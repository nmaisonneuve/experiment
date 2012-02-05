source('input_data.r')
source('plot_map.r')
source('algo_dca.r')


input_root="haiti/haiti"
input_volunteer=sprintf("%s_volunteer.csv",input_root)
input_gold=sprintf("%s_reference.csv",input_root)

plot_density=function(data,worker,k=1){
  points=which(data$workerID==worker) 
  output=SpatialPointsDataFrame(coords=data@coords[points,], data=as.data.frame(rep(0,length(points))), proj4string=CRS(ps))  
  names(output)=c("density")  
  for (i in 1:length(points)){
    output$density[i]=democratic_density(data,points[i],worker,1)
  }
  output$workerID=1  
  input=as.data.frame(output)
  print(qplot(density, data=input, geom="histogram"))  
  p=ggplot(input, aes(coords.x1, coords.x2))+  geom_point(aes(size=density,colour=density))+ blank+
    xlim(lat_range[1],lat_range[2])+ylim(lng_range[1],lng_range[2])
  print(p)
}

data=read_input(input_volunteer,50, 0)
ref=read_input(input_gold,0, -1)
#data=data[data$workerID %in% c(1,2,3,4,6, 10,11,12,13,14,15,19,26,25,27,28,30,31,34,35),]
lat_range=range(ref@coords[,1])
lng_range=range(ref@coords[,2])

#data=data[data$workerID %in% c(1,2,3,4),]
#data=spatial_filter(data,4,3,2)
#ref=spatial_filter(ref,4,3,2)
#plot_density(data,1,1)


n=length(unique(data$workerID))

ptm <- proc.time()
clustered_data=democratic_clustering_internal(data,0.007,min_volunteers=1)
#clustered_data=democratic_clustering3(data,0.007,min_volunteers=1)

print(plot_volunteers(clustered_data))


dd=clustered_data
workers=unique(dd$workerID)

results=c()
for (i in unique(dd$cluster)){
  positive=dd$workerID[dd$cluster==i]
  negative=setdiff(workers,positive)
  if (length(positive)>0){
    positive=cbind(i,positive,1)
  }else{
    positive=c()
  }
  if (length(negative)>0){
    negative=cbind(i,negative,0)
  }else{
    negative=c()
  }
  
  #print(positive)
  #print(negative)
  in_results=rbind(positive,negative)
  results=rbind(in_results,results)
}
results=as.data.frame(results)
results=data.frame(worker=paste("worker",results[,2],sep=""),cluster=paste("url",results[,1],sep=""),label=paste("porn",results[,3],sep=""))
write.table(results,file = "test-unlabeled.txt",sep="\t", quote=FALSE,row.names=FALSE)
#clustered_data=clustered_data[clustered_data$cluster!=0,]
#output=compute_centroid(clustered_data)
#print(plot_result(output))
