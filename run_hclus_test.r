source('input_data.r')

input_root="haiti2/haiti2"
input_volunteer=sprintf("%s_volunteer.csv",input_root)
input_gold=sprintf("%s_reference.csv",input_root)


blank=opts(legend.position="none",panel.background=theme_blank(),
       axis.text.x=theme_blank(), axis.text.y=theme_blank(),           
       axis.title.x=theme_blank(), axis.title.y=theme_blank(),
       axis.ticks=theme_blank(),
      panel.grid.major = theme_blank(),
      panel.grid.minor = theme_blank(),
      panel.border=theme_rect(colour="black",size=0.71),
      aspect.ratio=1)

plot_cluster=function(data_){
p=ggplot(as.data.frame(data_), aes(coords.x1, coords.x2))+  
  geom_point(aes(size=1,colour = factor(cluster)))+
  blank  
return(p)
}

plot_points=function(data_){
  input=as.data.frame(data_)  
  p=ggplot(input, aes(coords.x1, coords.x2))+  geom_point(aes(size=1,colour=support))+blank  
return(p)
}


output=read_input(input_volunteer,50, 0)
workers=c(1,2,3,4)
data=output[output$workerID %in% workers,]
#data=data[sample.int(nrow(data)),]
#data=data[1:10,]
#print(data)
m=as.matrix(dist(data@coords))
#print(m)
#print(dim(m))

t=c()
for (worker in workers){
idx=which(data$workerID==worker)
idx_m=cbind(rep(idx,each=length(idx)),rep(idx,length(idx)))
t=rbind(idx_m,t)
}

#print(t)
print(m)
m=replace(m,t,100)
apres=apclusterLM(m,details=TRUE, lam=0.5)
print("after")
print(m)
ds=dbscan(m,(0.007/112),MinPts=3, countmode=NULL, method="dist")
data$cluster=ds$cluster
print(length(unique(ds$cluster)))
data=data[data$cluster!=0,]
data$taken=1
centroid=aggregate(data@coords, list(data$cluster), mean)
support=aggregate(data$taken, list(data$cluster), sum)
result=SpatialPointsDataFrame(coords=centroid[,2:3], data=as.data.frame(support[,2]), proj4string=CRS(ps))  
 names(result)=c("support") 

print(length(data))

print(plot_cluster(data))
print(plot_points(result))

#m[m[,2]>=5,]
#plot_nndist(output1)
#output1=output[output$workerID %in% c(1),]
#plot_nndist(output1)
#output1=output[output$workerID %in% c(3),]
#plot_nndist(output1)
#apply(output,workerID,FUN=function(x){length})
#x=output@coords
#dist=dist(output@coords)

#d=hclust(dist, method='centroid')
#output=outpout[output$mar]
#
## compute similarity matrix (negative squared Euclidean)
#sim <- negDistMat(x, r=2)
#print("ok")
## compute agglomerative clustering from scratch
#aggres1 <- aggExCluster(sim)
## show results
#show(aggres1)
## plot dendrogram
#plot(aggres1, x,k=150)