clusters_2=c()
source('parallel_cluster.r')

democratic_density=function(data,point_idx,worker,k){
  others=setdiff(unique(data$workerID),worker)
  #others=unique(data$workerID)
  m=c()
  for (other in  others) {
    density=mean(local_density(data,point_idx,other,2))    
      m=c(density,m)
  } 
  return (median(m))
}

local_density=function(data,point_idx,worker,k){
  selected_points=setdiff(which(data$workerID==worker),point_idx)
  if (length(selected_points)>0){
    dist = spDistsN1(data[selected_points,], data[point_idx,], longlat = TRUE)
    return (sort(dist)[1:k])
  }else
    return(NA)
}

nearest_neighbor=function (data,unclassified,point_idx, max_dist, worker){
  worker_set=which(data$workerID==worker)
  data$cluster[point_idx]=1
  #cat("pivot: " , data[point_idx,]@coords ," ",  data$workerID[point_idx],"\n")
  
  selected_points=intersect(worker_set,unclassified)   
  if (length(selected_points)>0){
    
    dist = spDistsN1(data[selected_points,], data[point_idx,], longlat = TRUE)
    
    idx=which(dist <= (max_dist))    
    if (length(idx)>0){#print(idx)
    #  print(data[selected_points[idx],])  
      sort=sort(dist,index.return = TRUE)$ix
      inter=intersect(sort,idx)
     # print(inter)
#      data$cluster[selected_points[idx]]=2
    #  print(plot_volunteers2(data[unclassified,]))
   #   if (readline(prompt = "enter")=="stop")
    #  stop("Message")
      t1=selected_points[inter[1]]
      #t2=selected_points[which.min(dist[idx])]
      #cat (t1, " ", t2,"\n")        
      return (t1)
     }   
  }
  #cat("nothing found for ",worker)
  #print(plot_volunteers2(data[unclassified,]))
  #  if (readline(prompt = "enter")=="stop")
  #    stop("Message")
  return (NA)
}

democratic_nearest_neighbor=function(data,unclassified_idx,point_idx,density_method="fixed"){
      point=data[point_idx,]    
      max_dist=0.005
      others=setdiff(unique(data$workerID),point$workerID)      
      points_idx=c(point_idx)
 
      #if (density_method=="variable"){
        #max_dist=mean(local_density(data,point_idx,point$workerID,1))       
      #}
      #if (density_method=="hybrid"){
        #max_dist=max(0.010,democratic_density(data,point_idx,point$workerID,1))
      
      #}
      
      #dist_ex=c(dist_ex,max_dist)
      #max_dist=0.004
      #cat("max dist: ",max_dist)
      #max_dist=0.007
      for (other in others) {
        nb=nearest_neighbor(data,unclassified_idx,point_idx,max_dist,other)
        if (!is.na(nb)){
          points_idx=append(nb,points_idx)
        }
      }
      return (points_idx)
}

democratic_clustering4=function (data,unclassified_idx=1:nrow(data),min_volunteers=2,method="better"){  
  data$cluster=0 # by default all are noises  
  cluster_idx=1  
  workers=unique(data$workerID)
  dist_ex=c()
  #for (min_vol in length(workers):min_volunteers) {
  while(length(unclassified_idx)>0) {

      rand_point_idx=unclassified_idx[sample.int(length(unclassified_idx),1)]
      #rand_point_idx=unclassified_idx[1]      
      point=data[rand_point_idx,]
      
      #check 
      points_idx=democratic_nearest_neighbor(data,unclassified_idx,rand_point_idx)
      
      if (method=='better'){
        #print("better mode")
        before=length(points_idx)
        for (better_pivot in setdiff(points_idx,rand_point_idx)){
          points2_idx=democratic_nearest_neighbor(data,unclassified_idx,better_pivot)
          #points2_idx=local_density(data,better_pivot,3)
          if (any(points2_idx==rand_point_idx) & (length(points2_idx)>before)){
                print("better")  
              points_idx=points2_idx
              before=length(points_idx)
            }
          }
      }
      #cat('local result', points_idx,"\n")
      
      #validate
      if (length(points_idx)>=min_volunteers){
       # cat(points_idx," ",max_dist,"worker:", point$workerID, " others: ", others, "\n")
        data$cluster[points_idx]=cluster_idx
        cluster_idx=cluster_idx+1
        unclassified_idx=setdiff(unclassified_idx,points_idx)
      }else{        
        unclassified_idx=setdiff(unclassified_idx,rand_point_idx)
      }                    
     }
    
  data=data[data$cluster!=0,]
  clusters_2<<-plot_clusters(data)
  output=compute_centroid(data)
  return (output)
}

democratic_clustering3=function (data,min_dist=0.07,min_volunteers=2){  
  #data=as.matrix(as.data.frame(data))
  data$free=TRUE
  data$cluster=0 # by default all are noises  
  workers_idx=unique(data$workerID)
  
  size=length(workers_idx)-min_volunteers+1    
  clusters=list()
  
  for (v in 1:size) {

    my_idx=which(data$workerID==workers_idx[v] & data$free==TRUE)          
    my_markers=data[my_idx,]          
    #my_idx2 to remove
    #my_idx2=which(data$workerID==workers_idx[v] & data$free==FALSE)
    #cat(workers_idx[v],"nb:", nrow(my_markers),"already",length(my_idx2), '\n')     
    
    matches=c()    
    clusters_v=c()
     if (nrow(my_markers)>0){
        others_idx=workers_idx[-c(1:v)]     #only the next      
        
      for (other in others_idx){ #for each other volunteers                        
            other_idx=which(data$workerID==other & data$free==TRUE)
            others_selected=data[other_idx,]                        
            match=zerodist2(others_selected, my_markers,min_dist/112)
            match2=subset(match, !duplicated(match[,2])) 
            if (nrow(match)!=nrow(match2)){
              cat ("lost:", nrow(match2)/nrow(match),"\n")
            }
            match=match2
            
            #cat("with other",other, "nb: ",length(others_selected),"matching",nrow(match),"\n")            
            #print(match)
            if (nrow(match)>0){
              matches=rbind(cbind(other_idx[match[,1]], match[,2], other),matches)
            }
     }
    if (length(matches)>0 && nrow(matches)>0){
    matches[,2]=my_idx[matches[,2]]
    matches=rbind(matches,cbind(my_idx,my_idx,workers_idx[v]))   # we add the volunteer's points
    }else{
      matches=cbind(my_idx,my_idx,workers_idx[v])
    }
    clusters_v=aggregate(matches[,1], list(matches[,2]),function(x){
    if (length(x)>=(min_volunteers)){ 
        data$free[x]<<-FALSE
      return (c(mean(data@coords[x,1]),mean(data@coords[x,2]), length(x)))  
    }      
    else {
      return(NA)
    }}, simplify=FALSE)$x        

    if (length(clusters_v)>0){
       clusters_v=clusters_v[!is.na(clusters_v)]
    }
    if (length(clusters_v)>0){        
        clusters=append(clusters,clusters_v)       
    }
   }
  }
  
  #if (class(clusters)!="matrix"){
  if (length(clusters)>0){
    clusters=matrix(unlist(clusters), ncol=3, byrow=TRUE)        
    if (nrow(clusters)==1){
      output=SpatialPointsDataFrame(coords=t(clusters[,1:2]), data=as.data.frame(t(clusters[,3])), proj4string=CRS(ps))
    }else{
      output=SpatialPointsDataFrame(coords=clusters[,1:2], data=as.data.frame(clusters[,3]), proj4string=CRS(ps))  
    }
  }else{
    output=SpatialPointsDataFrame(coords=cbind(0,0), data=data.frame(0), proj4string=CRS(ps))  
  }  
  names(output)=c('support')  
  return (output)
}


democratic_clustering=function (data,min_dist=0.07,min_volunteers=2){  
  data$taken=0
  data$cluster=0 # by default all are noises  
  cluster_idx=1
  size=nrow(data)  
  workers=unique(data$workerID)
  
  for (i in 1:size){        
    if (data$taken[i]==0){      
#       # select the seed
      point=data[i,]
      # select other potential points
      idx2=which(data$workerID!=point$workerID & data$taken==0)            
      
      if (length(idx2)>0){
        pts=data[idx2,]      
        
        # compute distance
        dist = spDistsN1(pts, point, longlat = TRUE)
    		    
        # if points from others within a given distance
        idx=which(dist <= min_dist)    
        others=unique(pts$workerID[idx])
        if ((length(others)+1)>=min_volunteers){        
          # detailed version
          # pts$dist=dist
          # pts=pts[order(dist),]
          # for (w in others){           
          #      w_idx=which(pts$workerID[idx]==w)[1]              
          #      data$cluster[idx2][idx][w_idx]=cluster_idx    
          #      data$taken[idx2][idx][w_idx]=TRUE              
          # }
          #print(data$workerID[idx2][idx][w_idx])                  
          data$cluster[idx2][idx]=cluster_idx    
          data$taken[idx2][idx]=1            
          data$cluster[i]=cluster_idx
          cluster_idx=cluster_idx+1
        }
      }    
      data$taken[i]=1
     }
  }
  
  return (compute_centroid(data))
}

