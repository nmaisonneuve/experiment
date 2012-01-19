library(ggplot2)
source('input_data.r')
source('plot_map.r')
source('parallel_cluster.r')
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
data=data[data$workerID %in% c(1,2,3,4, 19, 27, 28,25,30,31),]
#data=data[data$workerID %in% c(1,2,3,4),]


#data=spatial_filter(data,4,3,2)
#ref=spatial_filter(ref,4,3,2)





p_ref=plot_volunteers(ref)
#plot_density(data,1,1)
p_v=plot_volunteers(data)

ptm <- proc.time()
#output0=density_clustering(data,0.005,min_volunteers=3) 
print(proc.time()-ptm)

ptm <- proc.time()
output_before=democratic_clustering3(data,0.005,min_volunteers=3)
print(proc.time()-ptm)
# 
# ptm <- proc.time()
#unclassified=sample(nrow(data),nrow(data))
#output1=democratic_clustering4(data,min_volunteers=3,method='better')
#print(proc.time()-ptm)

#output2=democratic_clustering4(data,unclassified,2, method='better')
# print(proc.time()-ptm)
# 
# ptm <- proc.time()
# output2=democratic_clustering4(data,1,method='variable')
# print(proc.time()-ptm)
# 
# ptm <- proc.time()
 #output3=democratic_clustering4(data,1,method='hybrid')
# print(proc.time()-ptm)
# #ptm <- proc.time()
# #output1=democratic_clustering3(data,0.004,min_volunteers=2,method='hybrid')
# #print(proc.time()-ptm)
# 
# 
res=0.007
print(compute_accuracy(output0,ref,res))
print(compute_accuracy2(output0,ref,res))

print(compute_accuracy(output_before,ref,res))
print(compute_accuracy2(output_before,ref,res))
print(compute_accuracy(output1,ref,res))
#print(compute_accuracy(output2,ref,0.007))
 #print(compute_accuracy(output3,ref,0.006))

 #p0=plot_result(output0)
 p_before=plot_result(output_before)
print(p_before)
# print(plot_result(output1))
 p1=plot_result(output1)
# print(plot_result(output3))
multiplot(p_ref,p_v,clusters_2,p1,cols=2)