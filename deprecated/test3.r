library(combinat)
library(sp)
library(ggplot2)

LOW_ANNOTATIONS = 40
MAX_SAMPLE = 200

############################# FUNCTION DECLARATION ####################
# Generate all combinaison of [N,K]
# If there is too many possible combinaisons, 
# a random subset of {max_sample} unique combinaisons is returned

generate_combinaisons<-function(n,k,max_sample=MAX_SAMPLE){
	
	#compute number of possible combinaison [n,k]
	num = choose(n, k)	

	if (num > max_sample) {
		
		print(sprintf("Too many possible combinasons (%i), get only %i (unique) samples", num, max_sample))

		# get a sample of 200 randoms but unique combinaisons
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

# Return the accuracy distribution according to the number of workers
# How:
#  for a given number of workers, 
# -compute all the possible combinaison of workers,
# -for each given combinaison of workers, 
#  +aggregating the result (=finding spatial clusters with a 
#   given minimum distance and a minimum number of workers aggreed)
#  +compute the accuracy from the centroid of the clusters according to reference data
#  +save result in a matrix (each row = accuracy result of a given combinaison)
accuracy_dist <- function(data, reference, num_workers=1,num_voters=3,min_dist=0.01) {


	total_workers=length(unique(data$workerID))

	# to be sure 
	num_voters=min(num_voters,num_workers)

	# output result
	accuracy <- matrix(nrow = 0, ncol = 8)

	#NAP=number of annotations produced
	#NAF=number of annotations references
	colnames(accuracy) <- c("NAP", "NAF", "accuracy", "matched", "omission", "false positive", "num_workers","num_voters")
				
	#Generate all (or a subset) combinaisons of {num_workers} workers
	comb_i_workers=generate_combinaisons(n=total_workers, k=num_workers)
	lcomb=nrow(comb_i_workers)
	
	#print(sprintf("number of combinaison tested: %i",lcomb))

	#for a given combinaison of selected workers
	for (j in 1:lcomb) {
		
		print(sprintf("%i/%i - compute cluster and its accuracy for workers %s (min dist=%f, min vote=%i)",j,	lcomb, toString(comb_i_workers[j,]),min_dist,num_voters))

		# select the annotations according to the selected workers		
		selected_points = data[which(data$workerID %in% comb_i_workers[j,]), ]
							
		#compute clusters
		clusters = compute_cluster(obs = selected_points, min_dist, num_voters)
		print(sprintf("%i clusters found", nrow(clusters)))
				
		# compute accuracy
		accuracy <- rbind(accuracy, c(compute_accuracy(obs = clusters,reference), num_workers, num_voters))
		#print(accuracy)
	}
	return(accuracy)	
}



compute_cluster <- function(obs, min_dist = 0.01, min_votes = 3) {
	
	# create empty clusters structure
	clusters <- matrix(nrow = 0, ncol = 4)
	colnames(clusters) <- c("lon", "lat", "voters","markers")

	# for each observations
	# associate it with a new/existing cluster
	for (i in 1:nrow(obs)) {
		clusters <- add_to_closest_cluster(clusters, obs[i,], i, obs, min_dist)
	}
	
	#remove clusters with not enought decision/vote support
	clusters.support=as.matrix(lapply(clusters[,3], length)) 
    clusters<-clusters[which(clusters.support>=min_votes),]

	#weird but required to preserve structure
	if (class(clusters)=="list"){
		clusters<-t(data.matrix(clusters))
	}	
	return(clusters)
}

add_to_closest_cluster <- function(clusters, row, idx,obs, threshold_dist) {
	

	pts = clusters[, 1:2]
	#weird but required to preserve structure
	if (length(pts) == 2){
		pts<-t(data.matrix(pts))
	}
	mode(pts) = "numeric"

	#compute distance between all clusters and a given annotation	
	dist = spDistsN1(pts, row, longlat = TRUE)
	min_dist = min(dist)

	# warning if dist empty =no cluster = first time loop
	if (min_dist <= threshold_dist) {

		selected_idx = which(dist == min_dist)[1] 		#the [1] = in case of several minima, pick up the first one
		closest_cluster = clusters[selected_idx, ]

		#already voted for this cluster? 
		no_voted_yet=(length(which(closest_cluster$voters == row$workerID)) == 0)
		if (no_voted_yet) {		
   		# add annotation to this cluster
			clusters[selected_idx, ] = add_marker(clusters[selected_idx, ], row,idx) 	
		}else{
			print(sprintf("warning: at least 2 points belonging to the same worker belong to the same clusters (=are very/too close)"))
		}
	}
	else {
		# create a new cluster
		clusters <- rbind(clusters, matrix(list(row@coords[1], 	row@coords[2], c(row$workerID), idx), nrow = 1))
	}
	return(clusters)
}

# Adding the marker to the cluster 
# and updating its centroid
add_marker <- function(cluster, row, idx) {
	
	# add worker in the list of voters
	cluster$voters = c(cluster$voters, row$workerID)
	cluster$markers = c(cluster$markers, idx)
	l = length(cluster$voters)
	
	#recompute centroid
	cluster$lat = (cluster$lat * (l - 1) + row@coords[2])/l
	cluster$lon = (cluster$lon * (l - 1) + row@coords[1])/l
	
	return(cluster)
}


compute_accuracy <- function(obs, refs, min_dist = 0.01) {
	
	matched_ref = vector()
	matched_clusters = vector()

	# fix class and mode for spDistsN1
	pts = data.matrix(obs)

#	pts<- ifelse(nrow(pts) == 1, t(data.matrix(obs[, 1:2])), pts[, 1:2])
#  doesnt work..?
	if (nrow(pts) == 1){
		pts<-t(data.matrix(obs[, 1:2]))
	}
	else{
		pts<-pts[, 1:2]
	}
	mode(pts) = "numeric"
	
	
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
	return( c(nrow(obs), nrow(refs), coef, matched/nrow(refs), omission/nrow(refs), falsepositive/nrow(obs)))
}



############################# MAIN ##############################

# DATA 
# setwd("R")
# source("test3.r")

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

# Delete spam 

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


# Delete bad workers with low contribution
workers = unique(experiment$workerID)
total_workers = nrow(workers)
i = 1
for (worker in workers) {
	#print(sprintf("worker %i", worker))
	worker_annotations = experiment[experiment$workerID == worker, ]
	
	#remove duplicated points	 
	worker_annotations=worker_annotations[!duplicated(worker_annotations@coords),]
	experiment= experiment[!duplicated(worker_annotations@coords),]

	num_annotations = nrow(worker_annotations)
	if (num_annotations <= LOW_ANNOTATIONS) {
		#remove
		experiment=experiment[-which(experiment$workerID==worker),]
	}else{
		#reindexing 
		experiment$workerID[experiment$workerID==worker]= i
		i=i+1
	}	
}
workers=unique(experiment$workerID)
total_workers = nrow(workers)

#print(sprintf("data cleaned: %i ", total_workers))

#################################  COMPUTATION ########################

# example 

output = accuracy_dist(experiment, reference, num_workers=3,num_voters=2)
output=as.data.frame(output)

#print(output)

# we don't compute for each number of worker  1 , 2 , 3, 4 ,5 ,6
# but for a sub set 1 ,2 ,4, 6, etc.. to be faster
#number_workers_candidates=c(1,2,3,4,5,8,10,12,14,16,18,22,24)
#for (number_workers in number_workers_candidates){
#  output=accuracy_dist(experiment, reference, num_workers=j, num_voters=i, min_dist=0.005)
#}


#clusters=compute_cluster(obs=experiment$workerID==1,], min_dist=0.001,1)
#for (i in 2:6){
# if (j==1 && i==2)
#  output=accuracy_dist(experiment, reference, num_workers=j, num_voters=i, min_dist=0.005)
#else
#   output=rbind(output,accuracy_dist(experiment, reference, num_workers=j, num_voters=i, min_dist=0.005))
#}
#}

write.csv(output,"output.csv")

#qplot(num_obs, data = data.frame(output), geom = "histogram")

#cluster support
#clusters.support=as.matrix(lapply(clusters[,3], length)) 

# plot function 

# plot accuracy vs number of annotations

# distribution of vote per cluster


#clusters<-matrix(nrow=0, ncol=3)
#wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

+ xlim(0.0, 0.1) 
plot_result<-function(output){
pg <- ggplot(data=output, aes(x = accuracy, group=num_voters, fill=num_voters)) + geom_density( alpha=0.2) 
+ xlim(0.0, 0.1)
+ opts(aspect.ratio = 1)
+ facet_grid(.~c(accuracy,matched),space="free",scales="free_x")
pg
}

plot_result<-function(output){
pg <- ggplot(data=test_ouput) + geom_density( alpha=0.2, aes(y = ..scaled..,x = accuracy, group=num_voters, fill=num_voters)) + xlim(0,1.0)+ opts(aspect.ratio = 1)+ facet_grid(.~c(accuracy,matched),space="free",scales="free_x")
pg <- ggplot(data=test_ouput) + geom_density( alpha=0.2, aes(y = ..scaled..,x = c(accuracy,matched), group=num_voters, fill=num_voters)) + xlim(0,1.0)+ opts(aspect.ratio = 1)
pg <- ggplot(data=test_ouput) + geom_density(alpha=0.2, aes(x = matched, group=num_voters, fill=num_voters)) + xlim(0,1.0)+ opts(aspect.ratio = 1)
pg <- ggplot(data=test_ouput) + geom_density(aes(y=..scaled.., x=accuracy, ylab="test")) + geom_density(aes(y=..scaled.., x=matched, color="red"))+ geom_density(aes(y=..scaled.., x=omission, color="blue")) + geom_density(aes(y=..scaled.., x=false.positive, color="yellow")) + facet_wrap (~ num_voters, ncol = 5)
plot(experiment[experiment$workerID==3,]@coords)
# plot 
ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) + facet_wrap(facets = ~workerID, nrow = 2, ncol = 12, scales = 'free') + geom_point(aes(x = longitude,y = latitude),data=experiment1_cleaned,shape = 22,colour = '#0000ff',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
title = "Frequency distribution of Accuracy"
 m <- ggplot(data.frame(output), aes(x=accuracy))
# m+ geom_histogram(binwidth=0.1)+ opts(title=title,plot.title=theme_text(size=title.size))
 m+ geom_histogram(binwidth=0.1)+ opts(title=wrapper(title, width = 50))
}

## for testing (bad UnitTest)
#quick_compute<-function(data,workers, reference, min_dist = 0.01, num_voters = 3){
#		# select the annotations according to the selected workers		
#		selected_points = data[which(data$workerID %in% workers), ]
							
		#compute clusters
		#special case of one , we don't aggregate (aggregated points for 1 worker != all of his points)
		clusters = compute_cluster(obs = selected_points, min_dist, num_voters)
		print(sprintf("%i clusters found", nrow(clusters)))
				
		# compute accuracy
		return(compute_accuracy(obs = clusters,reference))
}

#d=as.matrix(lapply(clusters[,3], length))


# Raster Manipulation
#library(raster) 
# etopo1 <- raster("after_clip2.tif")
# etopo1 <- brick("AOI1_before.tif")
# e <- extent(500, 2800, 1200, 3900) 
# e <- extent(-72.3448, -72.342, 18.552, 18.5554) 
# crop<-crop(etopo1,e)
# plot(crop)
#writeRaster(crop, "croped_AOI1_before.tif", datatype='INT1U' ,format = "GTiff")