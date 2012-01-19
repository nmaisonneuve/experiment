library(spatstat) #owin
library(fpc)

### Constants ##

window_island=owin(c(34.17449,34.18145),c(-0.39021,-0.38439))
window_haiti=owin(c(-72.2570801,-72.2515869),c(18.5549150,18.5603014))
window_haiti2=owin(c(-72.34436,-72.34235),c(18.55314,18.55500))

# EPSG:3857 (Spherical Mercator projection)
ps="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
read_input=function(filename, min_points, min_distance){
  experiment <- read.csv(filename, , encoding = "UTF-8")  
  
  coords<-cbind(experiment$lon,experiment$lat)  
  
  #extra_data<-as.data.frame(cbind(experiment$assignID,experiment$duration,experiment$difficulty))
  #names(extra_data)<-c('workerID','duration','difficulty')  
  extra_data<-as.data.frame(experiment$assignID)
  #extra_data<-as.data.frame(experiment$workerID)
  names(extra_data)<-'workerID'  
  #print(head(extra_data))
  
  experiment=SpatialPointsDataFrame(coords=coords, data=extra_data, proj4string=CRS(ps))
  #print(head(experiment))
  #removing some useless data
  #remove<-c('cluster', 'x')
  #experiment[-match(remove, names(experiment)),]
  if (min_distance>-1){ # to avoid preprecossing set min_distance =-1
    experiment=preprocessing(experiment,min_points, min_distance)
  }
  return(experiment)
}

########  CLEAN DATA ########
spatial_filter=function(data,res,i,j){
#cutting outside
#noth east point
  r=(data@coords)
lat=range(r[,1])
lng=range(r[,2])
#print(lat)
#print(lng)

lat_res=(lat[2]-lat[1])/res
lng_res=(lng[2]-lng[1])/res

#print(lat_res)
#print(lng_res)

lat=seq(lat[1],lat[2],lat_res)
lng=seq(lng[1],lng[2],lng_res)

data=data[data@coords[,1]>lat[i],]
data=data[data@coords[,1]<lat[i+1],]
data=data[data@coords[,2]>lng[j],]
data=data[data@coords[,2]<lng[j+1],]

  return (data)
}
#print(nrow(filter(data,10,1,1)))

cut_outside_haiti=function(experiment){
#cutting outside
#noth east point
n_e_lat=18.5549150
n_e_lng=-72.2570801
s_w_lat=18.5603014
s_w_lng=-72.2515869
experiment=experiment[experiment@coords[,2]>n_e_lat,]
experiment=experiment[experiment@coords[,2]<s_w_lat,]
experiment=experiment[experiment@coords[,1]>n_e_lng,]
experiment=experiment[experiment@coords[,1]<s_w_lng,]

return (experiment)
}

cut_outside_haiti2=function(experiment){
#cutting outside
#noth east point
n_e_lat=18.55314
n_e_lng=-72.34436
s_w_lat=18.55500
s_w_lng=-72.34235
experiment=experiment[experiment@coords[,2]>n_e_lat,]
experiment=experiment[experiment@coords[,2]<s_w_lat,]
experiment=experiment[experiment@coords[,1]>n_e_lng,]
experiment=experiment[experiment@coords[,1]<s_w_lng,]
return (experiment)
}



# compute the average nearest neighboor distance
compute_mean_nndist=function(ref){
   dist_matrix= spDists(ref, ref, longlat = TRUE)
   #dist_min=dist_min[-which(dist_min<0.0001)]
   dist_matrix[which(dist_matrix<0.0001)]<-NA
   nndist=apply(dist_matrix, 2, function(x) {min(x, na.rm=TRUE)})
   hist(nndist, breaks=20, xlim=c(0,0.05), lwd = 2)
   min_dist=mean(apply(dist_matrix, 2, function(x) min(x, na.rm=TRUE) ))   
  return (min_dist)
}

# minimum amount of annorations to accept a worker

# Delete bad workers with low contribution
preprocessing=function(experiment,low_annotation, min_dist){
 workers = unique(experiment$workerID)
 total_workers = nrow(workers)
 i = 1
 cleaned_experiment=c()
 for (worker in workers) {
	#print(sprintf("worker %i", worker))
	worker_annotations = experiment[experiment$workerID == worker, ]	
	
  # merging closed points
  if (min_dist>0){
    worker_annotations=clean(worker_annotations,min_dist)
	}
  
	# filtering by number of points
  num_annotations = nrow(worker_annotations)	
	if (num_annotations >= low_annotation) {
		#reindexing
		worker_annotations$workerID[worker_annotations$workerID==worker]= i
		i=i+1
		# add
		if(length(cleaned_experiment)==0){
		  cleaned_experiment=worker_annotations
 		}else{
		  cleaned_experiment=rbind(cleaned_experiment,worker_annotations)
		}
	}else{
	#print(sprintf(" filtered (not enough markers %i)",num_annotations))
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
######## END fct #####
