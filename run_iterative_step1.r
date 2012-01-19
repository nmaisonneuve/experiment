library(ggplot2)

source('input_data.r')
source('cluster_map.r')
source('parallel_cluster.r')

  

# STATUS_NOT_ANALYSED=1
# STATUS_ACCEPTED=2
# STATUS_REJECTED =3
# STATUS_NEW =4

#input
input_root="haiti2/haiti2"
input_volunteer=sprintf("%s_serial_volunteer.csv",input_root)
input_gold=sprintf("%s_reference.csv",input_root)
output_volunteer=sprintf("%s_serial_volunteer_result_0.007.csv",input_root)
iteration_image=sprintf("%s_serial_image.png",input_root)

# EPSG:3857 (Spherical Mercator projection)
ps="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Experiment1  data
#around 30 workers producing around 4000 annotations
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID
experiment <- read.csv(input_volunteer, , encoding = "UTF-8")


extra_data=as.data.frame(experiment[,3:5])
experiment=SpatialPointsDataFrame(coords=experiment[,1:2], data=extra_data, proj4string=CRS(ps))
print(experiment[1:10,])

# loading reference data 
reference <- read.csv(input_gold, , encoding = "UTF-8")
#print(head(reference))
extra_data=as.data.frame(reference[,3])
names(extra_data)<-c("iteration")
reference=SpatialPointsDataFrame(coords=reference[,1:2], data=extra_data, proj4string=CRS(ps))


iteration_compute=function (experiment,reference){
#display all the instances' ID# for each instance
output <- data.frame()

for (inst in unique(experiment$instance)){
  exp=experiment[experiment$instance==inst,]    
  # for each iteration
  for (iter in unique(exp$iteration)){
    exp_iter=exp[exp$iteration==iter,]
    print(sprintf('instance: %f , iteration: %f %f',inst,iter,nrow(exp_iter)))        
      tmp=as.data.frame(t(compute_accuracy(exp_iter, reference,0.007)))
      tmp$iteration=iter
      tmp$instance=inst
     names(tmp) <- c("precision", "recall", "fmeasure","num_annotations",'iteration','instance')    
    output=rbind(output,tmp)
  }
}
  
  return(output)
}

result=iteration_compute(experiment, reference)
# to compare with parallel
#result=result[result$iteration %in% c(1,2,4,6,8),]
result2=as.data.frame(cbind(result$iteration,result$instance, result$precision, result$recall, result$fmeasure, result$num_annotations))
names(result2)<-c('iteration','instance' ,'precision','recall','fmeasure', 'num_annotations')
write.csv(result2,file=output_volunteer)
print(result2)
ggplot(result2, aes(y=fmeasure , x=iteration,group=iteration))+geom_boxplot()