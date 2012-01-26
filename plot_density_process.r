source('input_data.r')
source('plot_map.r')
source('algo_dca.r')


input_root="island/island"
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

data=data[data$workerID %in% c(1,2,3,4),]
#data=spatial_filter(data,4,3,2)
#ref=spatial_filter(ref,4,3,2)

p_ref=plot_volunteers(ref)
#plot_density(data,1,1)
print(p_ref)

print(plot_volunteers(data))

n=length(unique(data$workerID))

ptm <- proc.time()
output_before=democratic_clustering4(data,0.01,min_volunteers=1)
print(proc.time()-ptm)

#output_before$support_backup=output_before$support
#output_before$support=cut(output_before$support_backup/n,breaks=c(0,0.2,0.4,0.6,0.8,1), labels=FALSE)


output_before$support_backup=output_before$support
output_before$support=cut(output_before$support_backup/n,breaks=seq(0,1,0.1))
p=ggplot(as.data.frame(output_before), aes(x = support)) + geom_bar(aes(y = (..count..)/sum(..count..))) +     scale_y_continuous(formatter = 'percent')

print(plot_result(output_before))
print(hist(output_before$support_backup))



#false positive
output=compute_accuracy_explained(output_before,ref,0.007)

print(plot_result_with_ref2(output,ref))

output$support_backup=output$support
output$support=output$support/n
output$support=cut(output$support,breaks=c(0,0.2,0.4,0.6,0.8,1))
fp_errors=output[output$matching==0,]
print(plot_result(fp_errors))

#false negative
output$support=(n-output$support_backup)/n
output$support=cut(output$support,breaks=c(0,0.2,0.4,0.6,0.8,1))
output$support <- factor(output$support, levels = c("0",levels(output$support)))
#output$support[is.na(output$support)]=factor(0)
fn_errors=output[output$matching==1 | output$matching==-1,]
print(plot_result(fn_errors))




#interval=seq(0.1,1,0.2)
#interval=c(0,0.10,0.25,0.4,0.55,0.70,0.85,1)
#interval=c(0,0.10,0.25,0.4,0.55,0.70,0.85,1)
#names(output)[7]=c("min_dist")
#print(output)

#test=
#res=0.007

#print(compute_accuracy_fast(test,ref,res))

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



#print(compute_accuracy(output_before,ref,res))
#print(compute_accuracy2(output_before,ref,res))
#print(compute_accuracy(output1,ref,res))
#print(compute_accuracy(output2,ref,0.007))
 #print(compute_accuracy(output3,ref,0.006))

 #p0=plot_result(output0)
# p_before=plot_result(output_before)
#print(p_before)
# print(plot_result(output1))
# p1=plot_result(output1)
# print(plot_result(output3))
#multiplot(p_ref,p_v,clusters_2,p1,cols=2)

plot_result=function(data_){
  input=as.data.frame(data_)  
  p=ggplot(input, aes(coords.x1, coords.x2))+  geom_point(aes(colour=factor(support),size=support))+blank  +
    xlim(lat_range[1],lat_range[2])+ylim(lng_range[1],lng_range[2]) +scale_colour_grey(start=0.8, end=0)  #scale_colour_gradient(low="gray", high="black", space="Lab")    
  if (FALSE){
    p=p+opts(legend.position = "right")
  }
return(p)
}
print(plot_result(output_before))
