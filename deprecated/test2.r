rm(list=ls(all=TRUE)) 

library(combinat)
library(sp)

#to remember: setwd("c:/myfiles")

# Return the accuracy distribution according to the number of workers
# How:
#  for a given number, 
# -compute all the possible combinaison of workers,
# -for each given combinaison of workers, 
#  +aggregating the result (=finding spatial clusters with a 
#   given minimum distance and a minimum number of vote)
#  +compute the accuracy from the centroid of the clusters 
#   found according to reference data
#  +save result in a matrix (each row = accuracy result of a given combinaison)
accuracy_dist<-function(data, reference, num_workers, total_workers){

   #matrix(coef,match,fp, omission)
   accuracy<-matrix(nrow=0, ncol=5)
   colnames(accuracy) <- c("num_obs","coef", "matched", "omission","false positive")

  # for a given number of workers
  
  MAX_SAMPLE=200
  num=choose(total_workers,num_workers)
  
  if (num>MAX_SAMPLE){
      # get a sample of 200 sample combinaison
      comb_i_workers=replicate(MAX_SAMPLE, sample(total_workers, num_workers))
  }else{
  	# generate all possible combinaison {total_workers, i}
	comb_i_workers=combn(total_workers,num_workers)
  }

  #for a given combinaison of i workers
  for (j in 1:ncol(comb_i_workers)){

	# select the points 
	#sprintf("computer accuracy for workers %s",toString(comb_i_workers[,j]))
	
	selected_points=data[data$workerID %in% comb_i_workers[,j],]

	#filter low contribution
	if (nrow(selected_points)>(num_workers*60)){

	#compute clusters
	clusters=compute_cluster(obs=selected_points, min_dist=0.01,min_vote=3)

	#plot points with different colors
	#plot the cluster
  	#write.csv(clusters, paste("clusters",1))

	# compute accuracy
	accuracy<-rbind(accuracy, compute_accuracy(obs=clusters,reference))

	}else{
		print("skipping bad workers")
	}
  }

  write.csv(accuracy, paste("accuracy",1))

 # return list(ac=accuracy)
 return(accuracy)

}


# given a "clusters" list
# each row have the following structure: lat, lng , voters = list of  users ID having "voted for the creation of this cluster
compute_cluster<- function(obs,min_dist=0.01, min_votes=3){
	
  # create empty clusters structure
  clusters<-matrix(nrow=0, ncol=3)
  colnames(clusters) <- c("lon", "lat", "voters")
	
  # for each observations
  # associate it with a new/existing cluster
  for (i in 1:nrow(obs)){
  	clusters<-add_to_closest_cluster(clusters,obs[i,],min_dist)
  }
  
  if (nrow(clusters)>1)
	 clusters=clusters[-2,] # remove 2nd fake line (see add_to_closest_cluster)	



#  for (i in 1:nrow(clusters)){
#	clusters=clusters[i,]$voters
#  }

  return(clusters)
}

add_to_closest_cluster<-function(clusters,row,threshold_dist){	

	#weird but required to preserve structure
	# see also the end of the function
	if(nrow(clusters)==1){
		clusters<-rbind(clusters,matrix(list(0,0,c(1)),nrow=1))
	}	

	pts=clusters[,1:2]	
	mode(pts)="numeric"
      dist=spDistsN1(pts, row,longlat=TRUE) 
	min_dist=min(dist) # warning if dist empty =no cluster = first time loop
	if (min_dist<=threshold_dist){
		selected_idx=which(dist==min_dist)[1]
		closest_cluster=clusters[selected_idx,] #the [1] = in case of several minima, pick up the first one
		if (length(which(closest_cluster$voters==row$workerID))==0){
			#print("update cluster")			
			clusters[selected_idx,]=add_marker(clusters[selected_idx,],row)
		}
	}else{
		#print("add new cluster")
		clusters<-rbind(clusters,matrix(list(row@coords[1],row@coords[2],c(row$workerID)),nrow=1))		
	}
	return(clusters)
}

# Adding the marker to the cluster 
# and updating its centroid
add_marker<-function(cluster, row){

  # add worker in the list of voters
  cluster$voters=c(cluster$voters, row$workerID)
  l=length(cluster$voters)

  #recompute centroid
  cluster$lat=(cluster$lat * (l - 1.000) + row@coords[2]) / l
  cluster$lon=(cluster$lon * (l - 1.000) + row@coords[1]) / l

  return(cluster)
}

compute_accuracy<-function(obs, refs, min_dist=0.01) {

    matched_ref=vector()
    matched_clusters=vector()

   pts=data.matrix(obs)    
   # mode(obs)="numeric"        
   if(nrow(obs)==1){
	pts=t(data.matrix(obs[,1:2]))
    }else{
	pts=obs[,1:2]
   }
   mode(pts)="numeric"

   for (i in 1:nrow(refs)) {
	ref=refs[i,]
      dist=spDistsN1(pts, ref, longlat=TRUE)
    	selected_idx=which(dist<=min_dist)
      not_assigned=setdiff(selected_idx,matched_clusters) #check if already assigned
	if (length(not_assigned)>0){
	  matched_ref=c(matched_ref, ref) 	    
   	    matched_clusters=c(matched_clusters,not_assigned[1])
 	} 
    }
    
    matched=length(matched_ref)
    omission   = nrow(refs)-matched
    falsepositive=nrow(obs)-matched
    coef=matched/(matched+omission+falsepositive)

    return (c(nrow(obs),coef,matched,omission,falsepositive))
}

# read with utf 8
experiment<-read.csv("experiment1.csv",,encoding="UTF-8")
reference<-read.csv("experiment1_reference.csv",,encoding="UTF-8")

# renaming columns
names(experiment)<-c("longitude", "latitude", "workerID")
names(reference)<-c("longitude", "latitude", "workerID")

# promoting the input lists to SpatialPointsDataFrames
coordinates(experiment) <- c("longitude", "latitude")
coordinates(reference) <- c("longitude", "latitude")

# EPSG:3857 (Spherical Mercator projection)
experiment@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
reference@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Plot perspective: del /spam 
experiment=experiment[-218,]

i=1

# number of workerID
workers=unique(experiment$workerID)
total_workers=length(workers)

sprintf("reindexing workerID column for simplication") 
# reindex worker for simplication

# for manipulate combinaison
for (worker in workers){
  experiment$workerID[experiment$workerID==worker]=i
  i=i+1	

}

# compute accuracy dist 
#for (i in 1:total_workers){
#dists=data.framelist(a=matrix,)

output=accuracy_dist(experiment,reference, 2, total_workers)

# number of annotations vs accuracy
# print(qplot(num_obs, coef, data = data.frame(output),geom=c("point","smooth")))


#cluster support
#clusters.support=as.matrix(lapply(clusters[,3], length)) 
#hist(clusters.support[,1], breaks=20, main="cluster vote distribution", xlab="Votes per cluster/Support",  ylab="num of clusters",col = "red",  xlim=c(1, 20))


#clusters<-matrix(nrow=0, ncol=3)
#colnames(clusters) <- c("lon", "lat", "voters")
#clusters=compute_cluster(data=experiments,clusters=clusters)



#d=as.matrix(lapply(clusters[,3], length))
#plot(density(d[,1]), main="cluster vote distribution", xlab="Votes per cluster", ylab="num of clusters",col = "red", lwd=4)
#  type="h", 
#hist(d[,1], main="cluster vote distribution", xlab="Votes per cluster", ylab=


