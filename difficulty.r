library(combinat)
library(sp)
library(ggplot2)
library(fpc)

## Load required packages
library(maptools)
library(rgdal)

# set working directory
setwd('C:\\Users\\Nicolas\\Documents\\R')






###
# Compute accuracy by comparing observation to reference data
# parameters: min_dist = minimum distance so an observation is considered as matching a reference one
# output 2 columns:
# col 1 = matched_ref: 
# col 2= matched_clusters: 
# 
compute_accuracy <- function(obs, refs, min_dist = 0.01) {
	
	matched_ref = vector()	
	matched_clusters = vector()	
	pts=obs
	
	#for each reference annotations
	# compute distance with clusters/observations
	# select and store the matching clusters ( dist <min_dist)	
	for (i in 1:nrow(refs)) {
		ref = refs[i, ]
		dist = spDistsN1(pts, ref, longlat = TRUE)
		selected_idx = which(dist <= min_dist)
		not_assigned_yet = setdiff(selected_idx, matched_clusters)
		#check if already assigned
		if (length(not_assigned_yet) > 0) {
			#matched_ref = c(matched_ref, ref)
			matched_ref=c(matched_ref, i)
			idx=which.min(dist[not_assigned_yet])
			matched_clusters = c(matched_clusters, not_assigned_yet[idx])
		}
	}
	
	#number of true positive
	#true_positive = length(matched_ref) 	
	#number of missing buildings
	#false_negative = nrow(refs) - true_positive 	
	#number of wrongly identified buildings
	#false_positive = nrow(obs) - true_positive 	
	#precision=true_positive/nrow(obs) #ratio of wrongly identified buildings	
	#recall=true_positive/nrow(refs) # ratio of missing buildings	
	#fmeasure=2*(precision*recall)/(precision+recall) # aggregation

	return(cbind(matched_ref, matched_clusters))
}


# EPSG:3857 (Spherical Mercator projection)
ps="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Experiment1  data
#around 30 workers producing around 4000 annotations
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID
experiment <- read.csv("experiment1.csv", , encoding = "UTF-8")
experiment=SpatialPointsDataFrame(coords=experiment[,1:2], data=as.data.frame(experiment[,3]), proj4string=CRS(ps))
names(experiment) <- c("workerID")

#gold standard data 
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID(expertID)
reference <- read.csv("experiment1_reference.csv", encoding = "UTF-8")
reference=SpatialPointsDataFrame(coords=reference[,1:2], data=as.data.frame(reference[,3]), proj4string=CRS(ps))
names(reference) <- c("workerID")


########################## Preprocessing / Data cleaning #########


####  Delete outside a boundingbox box ####

#satellite imagery 
#bounding box 

#noth east point
n_e_lat=18.5504150
n_e_lng=-72.2570801

#south west point
s_w_lat=18.5614014
s_w_lng=-72.2515869

experiment=experiment[experiment@coords[,2]>n_e_lat,]
experiment=experiment[experiment@coords[,2]<s_w_lat,]
experiment=experiment[experiment@coords[,1]>n_e_lng,]
experiment=experiment[experiment@coords[,1]<s_w_lng,]


########  CLEAN DATA ########

### fct declaration ##

# find the minimum distance according to the reference dist
compute_min_dist=function(ref){
	dist_min= spDists(ref, ref, longlat = TRUE)
	dist_min=dist_min[-which(dist_min<0.0001)]
	dist_min=dist_min[!duplicated(dist_min)]
	distance=seq(0.0, 1.0, by=0.001) 
	r=sapply(t(distance),function(x){sum(dist_min>x)})
	r=r/length(dist_min)
	res=as.data.frame(cbind(distance,r))
	# s=ggplot(res[res$r>.9999,], aes(distance,r))
	# s+geom_point()
	#print(res[res$r>=1.0,])
	return (max(res[res$r==1.0,1]))
}

# minimum amount of annorations to accept a worker
LOW_ANNOTATIONS = 80

# Delete bad workers with low contribution
preprocessing=function(experiment,min_dist){
	workers = unique(experiment$workerID)
	total_workers = nrow(workers)
	i = 1
	cleaned_experiment=c()
	for (worker in workers) {
		#print(sprintf("worker %i", worker))
		worker_annotations = experiment[experiment$workerID == worker, ]	
		worker_annotations=clean(worker_annotations,min_dist)
		num_annotations = nrow(worker_annotations)	
		if (num_annotations >= LOW_ANNOTATIONS) {
			#reindexing
			worker_annotations$workerID[worker_annotations$workerID==worker]= i
			i=i+1
			# add
			if(length(cleaned_experiment)==0){
				cleaned_experiment=worker_annotations
			}else{
				cleaned_experiment=rbind(cleaned_experiment,worker_annotations)
			}
		}	
	}
	return(cleaned_experiment)
}

# Aggregating all points < dist_min
clean=function(data,dist_min){
	deg_min=dist_min/111.12 # distance in meter to lat degree
	print(sprintf("before cleaning: row %i (min degree: %f)", length(data),deg_min))
	tmp=dbscan(data@coords, deg_min,  MinPts = 2,method='hybrid') 
	data$cluster=tmp$cluster
	tmp=unique(data$cluster)
	tmp=tmp[tmp!=0]
	good_points=data[!data$cluster %in% tmp,]
	if (length(tmp)>0){  
		to_cluster=data[data$cluster %in% tmp,]
		clustered=internal_merge(to_cluster)
		good_points=rbind(good_points,clustered)
	}
	print(sprintf("after cleaning: row %i",length(good_points)))
	return(good_points)
}

###
# Clustering the observations (according to a min num of points, cf DBSCAN algo)
# finding the center 
#
merge=function(data,dist_min, num_voters){

   deg_min=dist_min/111.12 # distance in meter to lat degree
   workers_name=paste(unique(data$workerID), collapse=" ")

   #detecting clusters
   tmp=dbscan(data@coords, deg_min, MinPts = num_voters)

   data$clusterID=tmp$cluster
   d=aggregate(list(data@coords[,1],data@coords[,2]), list(data$clusterID), mean)
   names(d)=c("clusterID","lon","lat")
   d=d[c("lon", "lat", "clusterID")]
   d$workerID=workers_name
   if (nrow(d)==1){
      #coord=t(d[,1:2])
	coord=d[,1:2]	
   }else{
      coord=d[,1:2]
   }
   d=SpatialPointsDataFrame(coords=coord, data=d[,3:4], proj4string=CRS(ps))
   return(d)
}

##
#
# check if cluster has been wrongly formed by the same workers

check_instrument=function(data, deg_min, num_voters){
   tmp=dbscan(data@coords, deg_min, MinPts = num_voters)
   data$clusterID=tmp$cluster
   workers=unique(data$workerID)
   clusters=unique(data$clusterID[data$clusterID!=0])
   print("cluster ")
   print(clusters)
   for (cluster in clusters){
 	error=0;	
   	for (worker in workers){
	ln=length(which(exp$clusterID[exp$workerID==worker]==cluster))
   		if (ln>1){
			print(sprintf("error: %d",ln))
			error=1;
   		}
	}
	print(sprintf("cluster %d:  %d",cluster, error))
   }
}
######## END fct #####

breaks = c(0, 2, 4, 6, 8, 1)

min_dist=0.007
workers=unique(experiment$workerID)
print(sprintf("before %d participants producing %d", length(workers), nrow(experiment)))
experiment=preprocessing(experiment, min_dist)
print(sprintf("after %d participants producing %d", length(workers), nrow(experiment)))


##
# Compute false negative (buildings not identified)
#
compute_false_negative=function(){

	reference$matched=0	

	workers=unique(experiment$workerID)

	for (worker in workers){

		worker_contribution=experiment[experiment$workerID==worker,]

 		result=compute_accuracy(worker_contribution,reference, 0.007)

		reference$matched[result[,1]]=reference$matched[result[,1]]+1
	}

	#preparation / discretization
	reference$matched=reference$matched/length(workers)	
	reference$difficulty=cut(1-reference$matched, c(0,0.3,0.6,0.9,1), labels=c("easy(>70%)", "medium [40,70%]","hard [10%,40%]","very hard(<10%)"), include.lowest = TRUE)
	return(reference)		
}


##
# Graphics to study false negative errors 
# compute the histogram of false negative difficulty (% buildings that were easy to identify (>70% people get it)
# compute the spatial distribution of the difficulty of the false positive errors
#
fn_graphic=function(reference){

	# graphics 
	d=ggplot(as.data.frame(reference),aes(x=difficulty, fill=difficulty)) +geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1)+xlab("")+ylab("% buildings")+opts(axis.text.x =theme_blank())
	#d=ggplot(as.data.frame(reference)) + geom_point(aes(x = lon,y = lat,colour=difficulty))
	#d=grid.arrange(d1,d2+ opts(legend.position="none"), ncol=2)
	
	return(d)
}


##
# Compute the false positive errors 
# for all the individual workers
#
compute_false_positive=function(){
	
	# by default all the annotations are wrong annotations
	experiment$error=1

	workers=unique(experiment$workerID)

	# for each worker , comparaison of his annotation and the experts one
	# compute all the wrong annotations (false positive)
	# and 
	for (worker in workers){

		worker_contribution=experiment[experiment$workerID==worker,]

      	result=compute_accuracy(worker_contribution,reference, 0.007)

		# we set in the 'error' column the value 0 to the annotations that matched
		# so the others ($error=1) are the wrong ones
		experiment$error[experiment$workerID==worker][result[,2]]=0
		
		#	worker_contrib$error[result[,2]]=0
		#	writeOGR(worker_contrib[worker_contrib$error==1,], dsn=sprintf("test_exp1_%i_fp.kml",worker), layer= "cycle_wgs84", driver="KML", dataset_options=c("NameField=name"))
		#	print(worker_contrib)
	}

	# return only the wrong annotation
	return(experiment[experiment$error==1,])
}



compute_false_positive_difficulty=function(){
	exp_fp=compute_false_positive()
	nb_workers=length(unique(experiment$workerID))
	
	# We cluster mistakes on which at least {nb_worker} workers have done it
	# to get the exact result for a given number of worker, we remove the clusters from previously 
	# computed clusters (having a higher number of workers)
	
# from mistakes done by everybody  to mistakes done by only 1 or 2 people.
	for (nb_worker in nb_workers:1){

		new_=merge(exp_fp,0.007, nb_worker)
		if (nb_worker<nb_workers){

		}
		sp=spDists(d1,d2,longlat=TRUE)
	}
}
