library(ggplot2)
source('input_data.r')
source('parallel_cluster.r')


#input
input_root="island/island"
input_volunteer=sprintf("%s_volunteer2.csv",input_root)


# Experiment1  data
data=read_input(input_volunteer,50, 0)
#ptm <- proc.time()
#clusters=compute_clusters(data,5,1,0.007)
#print(proc.time()-ptm)
#print(system.time(compute_clusters(data,2,1,0.006)))
  
# select data from 10 people
data=data[data$workerID %in% c(10, 19, 27, 38),]
print("total")
print(nrow(data))

ptm <- proc.time()
data2=democratic_clustering3(data,0.010,4)
print(proc.time()-ptm)

ptm <- proc.time()
data3=density_clustering(data,0.007,4)
print(proc.time()-ptm)

#cat("democracy2",nrow(data2),"democracy1",nrow(data1),"democracy3",nrow(data3))

#spplot(data1,main='democracy1')
#spplot(data2,main='democracy2')
spplot(data3,main='density')

#data1=data1[data1$cluster!=0,]
#data2=data2[data2$cluster!=0,]
#data=data[sample(nrow(data), nrow(data)), ]
#qplot(y=coords.x2, x=coords.x1, data=as.data.frame(data3), color=support)
s=qplot(y=coords.x2, x=coords.x1, data=as.data.frame(data2), color=factor(support))+opts(aspect.ratio=1)
s1=qplot(y=coords.x2, x=coords.x1, data=as.data.frame(data3), color=factor(support))+opts(aspect.ratio=1)

#qplot(y=coords.x2, x=coords.x1, data=as.data.frame(data3), color=support)factor(facets=. ~ workerID)+ opts(legend.position = "none")

