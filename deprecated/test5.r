library(combinat)
library(sp)
library(ggplot2)
library(fpc)

## Load required packages
library(maptools)
library(rgdal)

LOW_ANNOTATIONS = 80
MAX_SAMPLE = 10

############################# FUNCTION DECLARATION ####################
# Generate all combinaison of [N,K]
# If there is too many possible combinaisons, 
# a random subset of {max_sample} unique combinaisons is returned

generate_combinaisons<-function(n,k,max_sample=MAX_SAMPLE){	
	#compute number of possible combinaison [n,k]
	num = choose(n, k)	
	if (num > max_sample) {		
		print(sprintf("Too many possible combinasons (%i), get only %i (unique) samples", num, max_sample))
		# get a sample of 20?boxplo0 randoms but unique combinaisons
		comb=matrix(ncol=k, nrow=0)
		while (nrow(comb)<MAX_SAMPLE)  #filling 1 by 1
		comb=unique(rbind(comb,t(replicate(1, sample(n, k)))))
		#alternative: non unique sample solution 
		#comb_i_workers = replicate(MAX_SAMPLE, sample(total_workers, num_workers))
	}
	else {
		# generate all possible combinaisons 
		comb = t(combn(n, k))
	}	
	return(comb)
}



intern_accuracy_dist <- function(data, reference) {

	accuracy <- data.frame()

	# output result
	# NAP=number of annotations produced
	# NAF=number of annotations references

	user_groups=unique(data$workerID)
	for (user_group in user_groups){
	 clusters=data[data$workerID==user_group,]
	 accuracy <- rbind(accuracy,compute_accuracy(obs = clusters,reference,min_dist))
	}
	names(accuracy) <- c("precision", "recall", "fmeasure", "matched", "omission", "false positive")	

   return(accuracy)
}

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
			matched_ref = c(matched_ref, ref)
			matched_clusters = c(matched_clusters, not_assigned_yet[1])
		}
	}
	
	# Card(A inter B) / Card(A union B)
	#= matching points / ((Card(A exclude from B)+ Card(A inter B)+Card(B exclude from A))
	#= matching points / omission + matching points + false positive
	matched = length(matched_ref)
	omission = nrow(refs) - matched
	falsepositive = nrow(obs) - matched
	coef = matched/(matched + omission + falsepositive)

	precision=length(matched_ref)/nrow(obs)
	recall=length(matched_ref)/nrow(refs)
	fmeasure=2*(precision*recall)/(precision+recall)

	return( c(precision,recall, fmeasure, matched/nrow(refs), omission/nrow(refs), falsepositive/nrow(obs)))
}

accuracy_dist <- function(data, reference, num_workers=1,num_voters=3,min_dist=0.01) {

	clusters=cluster_dist(data,reference,num_workers, num_voters, min_dist)
	accuracy=intern_accuracy_dist(clusters,reference)	
	accuracy$num_voters=num_voters
	accuracy$num_workers=num_workers
	return(accuracy)
}


cluster_dist <- function(data, reference, num_workers=1,num_voters=3,min_dist=0.01) {

	total_workers=length(unique(data$workerID))

	# to be sure 
	num_voters=min(num_voters,num_workers)

	#Generate all (or a subset) combinaisons of {num_workers} workers
	comb_i_workers=generate_combinaisons(n=total_workers, k=num_workers)

	lcomb=nrow(comb_i_workers)
	#print(sprintf("number of combinaison tested: %i",lcomb))

	clusters=c()
	#for a given combinaison of selected workers
	for (j in 1:lcomb) {

		# select the annotations according to the selected workers		
		selected_points = data[which(data$workerID %in% comb_i_workers[j,]), ]

		print(sprintf("%i/%i - compute cluster for workers %s (min dist=%f, min vote=%i)",j,	lcomb, toString(comb_i_workers[j,]),min_dist,num_voters))	
		#compute clusters
		if (length(clusters)==0){
			clusters=merge(selected_points, min_dist,num_voters)
		}
		else	{
			clusters=rbind(clusters,merge(selected_points, min_dist,num_voters))
		}

	}
	return(clusters)	
}


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
      coord=t(d[,1:2])
   }else{
      coord=d[,1:2]
   }
   d=SpatialPointsDataFrame(coords=coord, data=d[,3:4], proj4string=CRS(ps))
   return(d)
}


to_kml=function(data){
writeOGR(data, dsn="test2.kml", layer= "cycle_wgs84", driver="KML", dataset_options=c("NameField=name"))
}



internal_merge=function(data){
   clustersID = unique(data$cluster)   
   d=matrix(nrow = 0, ncol = 4)
   for (clusterID in clustersID) {	
	   tmp=data[data$cluster==clusterID,]
	   #workers=unique(tmp$workerID)
	   lat=mean(tmp@coords[,2])
	   lng=mean(tmp@coords[,1])
	   d=rbind(d, c(lng, lat,data$workerID[1], clusterID))
  }
	if (nrow(d)==1){
	  coord=t(d[,1:2])
       data=t(d[,3:4])
	}else{
	  coord=d[,1:2]
        data=d[,3:4]
	}
  output=SpatialPointsDataFrame(coords=coord, data=as.data.frame(data), proj4string=CRS(ps))
  names(output)=c("workerID", "cluster")
  return(output)
}

summary=function(data){
  s=by(output[3:6], output$num_workers, mean)
}

save= function(clusters, num_workers, num_voters, dist_min){
   write.csv(clusters, file=sprintf("clusters_nw%i_%i_%i.csv",num_workers,num_voters,dist_min*1000))
}

############################# PLOT ##############################

plot_output=function(output){
#boxplot(accuracy ~ num_workers,output)
p <- ggplot(output, aes(accuracy, ..count..)) +
  geom_histogram(binwidth = 0.1) 
p + facet_wrap(~ num_workers) 
}

plot_map=function(obs, ref, file){
obs_df=as.data.frame(obs)
ref_df=as.data.frame(ref)
s=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) +
geom_point(aes(x = lon,y = lat),data=obs_df,shape = 22,colour = '#0000ff',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
facet_wrap(facets = ~workerID ) +
#add geom point expert 
geom_point(aes(x = lon,y = lat),data=obs_df,shape = 22,colour = '#ff0000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
ggsave(s, filename=file, height=18,width=24,dpi=72)
}

plot_error_map=function(obs, ref){
matching=obs[obs$matching!=0,]
error=obs[obs$matching==0,]
s=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) +
geom_point(aes(x = lon,y = lat),data=as.data.frame(ref),shape = 21,colour = '#000000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
geom_point(aes(x = lon,y = lat),data=as.data.frame(matching),shape = 22,colour = '#00FF00',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
geom_point(aes(x = lon,y = lat),data=as.data.frame(error),shape = 22,colour = '#FF0000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
ggsave(s, filename=sprintf("error_map.png"), height=18,width=24,dpi=72)
}

######################   MATCHING ###########################



matched=function(obs, ref,min_dist){
  ref$taken=0
  #obs$matching=0
  for(i in 1:length(obs)) {
	ref$dist= spDistsN1(ref, obs[i,], longlat = TRUE)
	selected=which(ref$dist<=min_dist & ref$taken==0)
	if (length(selected)>0){		
		obs$matching[i]=selected[1]
		ref$taken[selected[1]]=1
	}
  }
  #ref$taken=NULL
  #t=sum(obs$matching!=0)
  #obs$matching=NULL
  return(obs)
}

############################# MAIN ##############################

# DATA 
# setwd("R")
# source("test3.r")l

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


######## Preprocessing / Data cleaning #########

###################  Delete outside a boundingbox box ######################

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


###################  CLEAN DATA ######################

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

#aggregating all points < dist_min
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


min_dist=compute_min_dist(reference)
print(min_dist)
#plot_map(experiment,reference, "plot_before.png")
experiment=preprocessing(experiment, min_dist)
#plot_map(experiment,reference, "plot_after.png")

#################################################


#dists=seq(0.001,0.04, 0.001)
#dists=c(0.008)
#ref=reference
#results=c()
#experiment$matching=0
#obs=experiment[experiment$workerID==12,]
#for (dist in dists){
#  tmp=compute_accuracy(obs,reference,dist)

#tmp=matched(obs,reference, dist)
#plot_error_map(tmp, reference)

#results=rbind(results,  c(dist,tmp))
#}
#print(results)

# plot 
#s=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) + facet_wrap(facets = ~workerID, nrow = 3, ncol = 8, scales = 'free') + geom_point(aes(x = lon,y = lat),data=exp1,shape = 22,colour = '#0000ff',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
#ggsave(s, filename="image.png", height=18,width=24,dpi=72)


#################################  COMPUTATION ########################

# example
#print(length(unique(experiment$workerID)))

#output = accuracy_dist(experiment, reference, num_workers=8,num_voters=2, min_dist=0.01)
#print(output)
?out
# we don't compute for each number of worker  1 , 2 , 3, 4 ,5 ,6
# but for a sub set 1 ,2 ,4, 6, etc.. to be faster
num_workers=c(1,2,3,4,5,8,10,14,16)

#num_voters=c(1,1,1,2,2,2,2,2,2,2)
num_voters=c(1,2,3,4,5,6)
output=c()
#for (number_workers in number_workers_candidates){
for (num_worker in num_workers){
  for (num_voter in num_voters){
	if (num_voter<=num_worker){
   		output=rbind(output,accuracy_dist(experiment, reference, num_workers=num_worker, num_voters=num_voter, min_dist=0.01))
	}
  }
}
boxplot(precision ~ num_workers,output)
