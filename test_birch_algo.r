library(birch)

#input
input_root="haiti2/haiti2"

#define window 
input_file=sprintf("%s_volunteer.csv",input_root)
data <- read.csv(input_file, , encoding = "UTF-8")
    
  #select only good workers
workers=unique(data$assignID)
size_per_worker=apply(t(workers), 2,FUN=function(x){ return(length(which(data$assignID==x)))})
good_workers=workers[which(size_per_worker>min_size)] 
  
# filter  
data=data[data$assignID %in% good_workers,]
  
#reindex worker
data$assignID=apply(t(data$assignID), 2,FUN=function(x){which(good_workers==x)})
 
#ddd=data[data$assignID %in% c(1,2),1:2]
ddd=data[,1:2]
clusters=birch(as.matrix(ddd),radius=(0.000000006))$members
print(length(clusters))
ddd$cluster=0
cluster_idx=1
for (i in 1:length(clusters)){
  ddd$cluster[clusters[[i]]]=sample(0:2000,1)
  cluster_idx=cluster_idx+1
}
p=ggplot(as.data.frame(ddd), aes(lon, lat))+
  geom_point(aes(colour = factor(cluster)))+
  opts(legend.position="none",panel.background=theme_blank(),
       axis.text.x=theme_blank(), axis.text.y=theme_blank(),           
       axis.title.x=theme_blank(), axis.title.y=theme_blank(),
       axis.ticks=theme_blank(), panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank())
