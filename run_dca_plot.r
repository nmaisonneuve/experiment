library(ggplot2)
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

#input
input_root="haiti2/haiti2"

#define window 
input_volunteer=sprintf("%s_volunteer.csv",input_root)
input_ref=sprintf("%s_reference.csv",input_root)

data_raw=read_input(input_volunteer,50, 0)
data=data_raw[data_raw$workerID %in% c(1,2,3,4, 19, 27, 28,25,30,31),]
#data=data[data$workerID %in% c(1,4, 19, 27, 28,25),]

#data=data[data$workerID %in% c(1,2,3),]

#plot aggregated 
#data.ppp <- ppp_read(input_volunteer,min=50)
#ref.ppp<-ppp_read(input_ref)


#plot(data.ppp,main="",chars=20)

print("total")
print(nrow(data))

#plot cluster
min_dist=0.006
min_voters=ceiling(0.2*length(unique(data$workerID)))
cat(min_voters,"min voters\n")
datac1=democratic_clustering3(data,min_dist,min_voters)

data=data_raw[data_raw$workerID %in% c(1,2,3),]
min_voters=ceiling(0.2*length(unique(data$workerID)))
cat(min_voters,"min voters\n")
datac2=democratic_clustering3(data,min_dist,min_voters)

print(nrow(datac1))
print(nrow(datac2))

#print(plot_points(data))

#print(plot_cluster(datac3))
print(plot_points(datac1))

print(plot_points(datac2))

#plot(ref.ppp,main="",chars=20)  
 