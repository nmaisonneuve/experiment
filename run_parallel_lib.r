source('parallel_cluster.r')
source('algo_dbscan.r')
source('algo_dca.r')
source('algo_grid.r')

  #haiti2
  #dist_merging=0.005
  #dist_matching=0.005
  
  #haiti
  #dist_merging=0.007
  #dist_matching=0.005
  
  #dist_matching
  #island
  #dist_merging=0.006
  #dist_matching=0.01

#############################  VERY IMPORTANT CLUSTER  ################
# we don't compute for each number of worker  1 , 2 , 3, 4 ,5 ,6
# but for a sub set 1 ,2 ,4, 6, etc.. to be faster
compute_all_accuracy=function(experiment, reference, algo, dist_matching=0.007, dist_merging=0.007, num_workers=c(1,2,3,4,6,8,10,15,20)){
output=c()

 for (num_worker in num_workers){  
   num_voters=unique(ceiling(c(0.2,0.25,0.3,0.4,0.5)*num_worker))
   num_voters=compute_agreement_vector(num_worker)
   for (num_voter in num_voters){  
          tmp=accuracy_dist(experiment, reference, num_workers=num_worker, num_voters=num_voter, min_dist=dist_matching, min_cluster=dist_merging, method=algo)
     	    output=rbind(tmp,output)
   }   
 }
#output=output[order(-output$fmeasure),]
#output$workerID=seq(1:nrow(output))
return (output)
}


##
# Creating the data to process 
#
compute_clusters <- function(data, num_workers,num_voters,min_dist, method='density') {
  if (num_workers==1){
    clusters=data      
  } else{
    
  total_workers=length(unique(data$workerID))
	# to be sure 
	num_voters=min(num_voters,num_workers)
	
  #Generate all (or a subset) combinaisons of {num_workers} workers
	groups=generate_combinaisons(n=total_workers, k=num_workers)
	#clusters=cbind(clusters,workerID=j) # workerID = groupIdx actually
  #print(sprintf("%i/%i - compute cluster for workers %s (min dist=%f, min vote=%f, total annotations:%f, aggregated: %f)",j,  lcomb, toString(comb_i_workers[j,]),min_dist,num_voters, nrow(selected_points),nrow(cluster)))
  #apply(cbind(volunteers=groups,idx=1:length(groups)),1,FUN=function(group){ 
  size=nrow(groups)
  cluster_points.list=list(size)
   for (i in 1:size)     {
    selected_points<- data[which(data$workerID %in% groups[i,]), ] 
    if (method=='density'){
    clusters<-density_clustering(selected_points, min_dist,num_voters)    
    }else{
    clusters<-democratic_clustering3(selected_points, min_dist,num_voters)    
    }
    clusters$workerID=i
    cluster_points.list[i]=clusters
    print(sprintf("%i /%i compute cluster for workers [%s] (min dist=%f, min vote=%i, total annotations:%f, aggregated: %f)", i,size, toString(groups[i,]),min_dist,num_voters, nrow(selected_points),nrow(clusters)))    
   # return(clusters)
	#})
  }
  clusters=do.call("rbind",cluster_points.list)  
  #clusters=SpatialPointsDataFrame(cluster_points[1:2], data=cluster_points[3:1], proj4string=CRS(ps))
  }
  clusters$num_workers=num_workers
  clusters$num_voters=num_voters
  clusters$cluster_dist=min_dist
return (clusters)
}