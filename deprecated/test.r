setClass("cluster", 
	representation(lat="numeric", lng="numeric", workers="vector" ,l="numeric"),
      prototype(lat=0, lng=0, l=0, workers=c())
)



already_voted<-function(cluster,workerID) {
      any(cluster@workers==workerID)
}

clustering<-function(clusters){
	apply(clusters,add_to_closest_cluster)
}
clusters=c()
closest_cluster<-function(row){
min_dist=0.01
closest_cluster=N	A
}

 selected_points

  for (i in 1:nrow(data))
#      distVec <- spDistsN1(,clusters[i,],longlat = TRUE)
#      minDistVec[i] <- min(distVec)
#      closestSiteVec[i] <- which.min(distVec)
#}
	for (j in length(combs[1,])) {
		idx=
		d=data.frame("data"=selected_jobs, "collective"=collective)
	}
}
#closestSiteVec <- vector(mode = "numeric",length = nrow(densityPoints))
#minDistVec     <- vector(mode = "numeric",length = nrow(densityPoints))


######### DEPRECATED ############
#setGeneric("add_marker",   function(this, lat,lng, workerID) standardGeneric("add_marker"))
#setMethod(votes, "cluster")
#setMethod(already_voted, "cluster")
#setMethod(avg_center, "cluster")
#setMethod("add_marker", "cluster",
#   function(this, lat,lng, workerID){
#   }
#)


###################  #################
# 
library(combinat)
library(sp)

# read with utf 8
experiment<-read.csv("experiment1.csv",,encoding="UTF-8")

# renaming columns
names(experiment)<-c("longitude", "latitude", "workerID")

# promoting the input lists to SpatialPointsDataFrames
coordinates(experiment) <- c("longitude", "latitude")

# EPSG:3857 (Spherical Mercator projection)
experiment@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Plot perspective: del /spam 
experiment=experiment[-218,]

i=1
# number of workerID
workers=unique(experiment$workerID)
total_workers=length(workers)

# reindex worker for simplication
# for manipulate combinaison
for (worker in workers){
  experiment$workerID[experiment$workerID==worker]=i
  i=i+1	
}


# compute accuracy dist 
for (i in 1:total_workers){
    dist=accuracy_dist(experiment,i, total_workers)
}

# Return the accuracy distribution according to the number of workers
# for a given number, 
# -compute all the possible combinaison of workers,
# -for each given combinaison of workers, 
#  +aggregating the result (=finding spatial clusters with a 
#   given minimum distance and a minimum number of vote)
#  +compute the accuracy from the centroid of the clusters 
#   found according to reference data
#  +save result in a list
accuracy_dist<-function(data, num_workers, total_workers){

  # for a given number of workers
  # generate all possible combinaison {total_workers, i}
  comb_i_workers=combn(total_workers,num_workers)
	
  #for a given combinaison of i workers
  for (j in 1:nrow(comb_i_workers)){

	# select the points 
	data=experiment
	selected_points=data[data$workerID %in% comb_i_workers[,j],]

	#create empty cluster data 
	clusters<<-matrix(list(0,0,c(0)), nrow=1)
	clusters<<-rbind(clusters,matrix(list(0,0,c(0)), nrow=1))
	colnames(clusters) <- c("lon", "lat", "voters")
	
	#compute clusters
	compute_cluster(data=selected_points, min_dist=0.01,min_vote=3)

	#plot points with different colors
	#plot the cluster

	# compute accuracy
	accuracy=compute_accuracy(clusters,reference)

	#save i , j , the given accuracy
	results[length(results)+1]=c(i, j, accuracy)
  }
}

compute_cluster<- function(data,min_dist=0.01, min_votes=3){
	for (i in 1:nrow(data)){
		add_to_closest_cluster(data[i,],min_dist)
	}
}

add_to_closest_cluster<-function(row,threshold_dist){	
	#ugly but working
	pts=clusters[,1:2]
	mode(pts)="numeric"

	#dist=spDistsN1(pts, selected_points[1,],longlat=TRUE)
      dist=spDistsN1(pts, row,longlat=TRUE)
	min_dist=min(dist)
	if (min_dist<=threshold_dist){
	
		print(row)
		selected_idx=which(dist==min_dist)[1]
		closest_cluster=clusters[selected_idx,] #the [1] = in case of several minima, pick up the first one

		if (length(which(closest_cluster$voters==row$workerID))==0){
			print("update cluster")
			clusters[selected_idx,]=add_marker(closest_cluster,row)
		}else{
			print ("by pass cluster")
		}

	}else{
		print("add new cluster")
		clusters<<-rbind(clusters,matrix(list(row@coords[1],row@coords[2],c(row$workerID)),nrow=1))
	}
}

# Adding the marker to the cluster 
# and updating its centroid
add_marker<-function(cluster, row){
  print(cluster)
  # add worker in the list of voters
  cluster$voters=c(cluster$voters, row$workerID)
  l=length(cluster$voters)
  print(cluster)

  # compute centroid
  cluster$lat=(cluster$lat * (l - 1.0) + row@coords[2]) / l
  cluster$lon=(cluster$lon * (l - 1.0) + row@coords[1]) / l
	print(cluster)
  # cluster[2]=(cluster@coords[1,][2] * (l - 1.0) + row@coords[2]) / l
	return(cluster)
}

%combn(total_job,nb_worker)
% combn(5 ,3, fun=NULL, simplify=FALSE)

#lnglat=SpatialPointsDataFrame(exp1[,1:2],data.frame(exp1clus$taskID),proj4string=CRS("+proj=longlat"))
#clusters=SpatialPointsDataFrame(c(),data.frame("l"),proj4string=CRS("+proj=longlat"))

clusters=SpatialPointsDataFrame(matrix(0, ncol=2),data.frame(l=0,workers=c(NA)),proj4string=CRS("+proj=longlat"))


#subset(exp1, exp1$taskID %in% c(1,2))
#lnglat[lnglat$exp1.taskID==1,]%in% c(1 2)

## S3 method for class 'SpatialPointsDataFrame'
geodata=as.geodata(lnglat, data.col = 1)
 exp1[exp1$taskID==5,]
d=data.frame({}
workers=unique(exp1$taskID)
for (worker in workers){
 	t=exp1[exp1$taskID==worker,]
setClass("track", representation(x="numeric", y="numeric"))
myTrack <- new("track", x = -4:4, y = exp(-4:4))
myTrack@x
myTrack@y<-rbind(myTrack@y, 2);
apply(m, 1, sum) 

mylist[[length(mylist)+1]] <- obj

push <- function(l, x) {
  assign(l, append(eval(as.name(l)), x), envir=parent.frame())
}

	#reindexing for combinaison
	#replace(exp1 , which(exp1==3), 6)
# projection to  EPSG:3857
# NwCountiesGMap <- spTransform(experiment,epsg4326String)

https://stat.ethz.ch/pipermail/r-sig-geo/2010-July/008838.html
But, far easier is to use the raster package, which will let you deal
with the entire Etopo1 if you want by accessing the file as needed,
and there are simpler methods to crop the big raster - and coerce to
SpatialGridDataFrame if need be.

E.g.
## all this works pretty fast as raster takes care of the memory
library(raster)
etopo1 <- raster("Etopo1.tif")
 e <- extent(-90, -32, -60, 15)
r <- crop(etopo1, e)
plot(r)

library(raster)
etopo1 <- brick("Etopo1.tif") # change here
e <- extent(-90, -32, -60, 15)
r <- crop(etopo1, e)
