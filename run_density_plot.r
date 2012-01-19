library(ggplot2)
library(spatstat)
#source('algo_grid.r')
source('input_data.r')
source('parallel_cluster.r')

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
  blank+
  xlim(min(ref@coords[,1]),max(ref@coords[,1]))+ylim(min(ref@coords[,2]),max(ref@coords[,2]))
return(p)
}

plot_points=function(data_){
p=ggplot(as.data.frame(data_), aes(coords.x1, coords.x2))+  
  geom_point(aes(size=1))+blank+
  xlim(min(ref@coords[,1]),max(ref@coords[,1]))+ylim(min(ref@coords[,2]),max(ref@coords[,2]))
return(p)
}

par(mar = rep(0, 4)) #margin 0
#constants (window of the map != window of the data (buildings not at a corner))



#input
input_root="haiti2/haiti2"

#define window 
window=window_haiti2
input_volunteer=sprintf("%s_volunteer.csv",input_root)
input_ref=sprintf("%s_reference.csv",input_root)

data=read_input(input_volunteer,50, 0)
ref=read_input(input_ref,50, 0)
data=data[data$workerID %in% c(1,2,3,4, 19, 27, 28,25,30,31),]
#data=data[data$workerID %in% c(1,2,3),]
data_raw=data

#plot aggregated 
#data.ppp <- ppp_read(input_volunteer,min=50)
#ref.ppp<-ppp_read(input_ref)


#plot(data.ppp,main="",chars=20)

print("total")
print(nrow(data))

#plot cluster
#min_dist=0.002
#ds=dbscan(data@coords,min_dist/112,2)
#data$cluster=ds$cluster
#datac1=data[data$cluster!=0,]


min_dist=0.006
total=length(unique(data$workerID))
voters=ceiling(0.2*total)
cat(voters, "voters")
ds=dbscan(data@coords,min_dist/112,voters)

data$cluster=ds$cluster
datac3=data[data$cluster!=0,]

datac3_o=density_clustering(data,min_dist,voters)
  
print(nrow(datac3_o))


print(plot_points(ref))

print(plot_points(data_raw))

#print(plot_cluster(datac3))

print(plot_points(datac3_o))
#plot(ref.ppp,main="",chars=20)  
  