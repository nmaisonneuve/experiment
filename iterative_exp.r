

############################# MAIN ##############################

# EPSG:3857 (Spherical Mercator projection)
ps="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Experiment1  data
#around 30 workers producing around 4000 annotations
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID
experiment <- read.csv("experiment3.csv", , encoding = "UTF-8")
experiment=SpatialPointsDataFrame(coords=experiment[,1:2], data=as.data.frame(experiment[,3:6]), proj4string=CRS(ps))

#gold standard data 
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID(expertID)
reference <- read.csv("experiment1_reference.csv", encoding = "UTF-8")
reference=SpatialPointsDataFrame(coords=reference[,1:2], data=as.data.frame(reference[,3]), proj4string=CRS(ps))
names(reference) <- c("workerID")

plot_map=function(obs, ref, file){
ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) +geom_point(aes(x = lon,y = lat,colour = factor(status)),data=as.data.frame(obs)) + coord_map(projection = 'mercator')+facet_grid(trackID ~ workerID ) #+
#add geom point expert 
#geom_point(aes(x = lon,y = lat),data=as.data.frame(ref),shape = 22,colour = '#ff0000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
ggsave(s, filename=file,dpi=100)
}


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

get_number_of_iterations=function(experiment,trackID){
 return (length(unique(experiment[experiment$trackID==trackID,]$workerID)))
}

select_data=function(experiment, trackID, num_iter){
	# only this track

	obs=experiment[experiment$trackID==trackID,]
	# 

	obs=obs[obs$workerID==num_iter,]
	# remove deleted annotations

	obs=obs[obs$status!=3,]
	return(obs)
}

compute_accuracy <- function(experiment, trackID, num_iter, refs, min_dist = 0.01) {

	# select the right data 
	obs=select_data(experiment,trackID,num_iter)
	
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

	#number of true positive
	true_positive = length(matched_ref) 
	
	#number of missing buildings
	false_negative = nrow(refs) - true_positive 

	#number of wrongly identified buildings
	false_positive = nrow(obs) - true_positive 
	
	precision=true_positive/nrow(obs) #ratio of wrongly identified buildings
	recall=true_positive/nrow(refs) # ratio of missing buildings
	fmeasure=2*(precision*recall)/(precision+recall) # aggregation

	result=data.frame(cbind(precision,recall,fmeasure))
	names(result)=c("precision","recall","fmeasure")
	result$iteration=num_iter
	result$dist=min_dist
	result$trackID=trackID
	return(result)
}

# minimum amount of annorations to accept a worker
LOW_ANNOTATIONS = 80

min_dist=0.007
num_iterations=seq(1:7)

select_iterations_greater_than=function(num_iteration){
selected=c()
	trackIDs=unique(experiment$trackID)
	for (trackID in trackIDs){
		if (get_number_of_iterations(trackID)>=num_iteration){
			selected=c(selected, trackID)
		}
	}
	return(selected)
}

best_tracks=select_iterations_greater_than(7)
best=experiment[experiment$trackID %in% best_tracks,]
#plot_map(best,reference,"best_iterative_model.png")

trackIDs=unique(best)
output=c()
for (num_iteration in num_iterations){
	for (trackID in trackIDs){
		if (get_number_of_iterations(best,trackID)>=num_iteration){
		  output=rbind(output,compute_accuracy(best,trackID, num_iteration,reference, min_dist))
		}
	}
}



print(output)

#d=ggplot(output,aes(factor(iteration))) + geom_boxplot(aes(y=fmeasure)) + ylim(0,1)
#d1=ggplot(output,aes(factor(iteration))) + geom_boxplot(aes(y=precision)) ylim(0,1)
#d2=ggplot(output,aes(factor(iteration))) + geom_boxplot(aes(y=recall)) + ylim(0,1)
#grid.arrange(d, d1,d2, ncol=3)

#ggsave(s, filename="exp1_exp2_recall.png", dpi=70)



