
library(ggplot2)
source('input_data.r')
source('algo_dca.r')


inter_agreement2=function(data, workerIDs,min_dist=0.007){  
      v1=data[data$workerID %in% workerIDs[1],]
      v2=data[data$workerID %in% workerIDs[2],]
      
      match=zerodist2(v1, v2,min_dist/111.1949)
      match=subset(match, !duplicated(match[,1]))
      match=subset(match, !duplicated(match[,2]))
      
      num_agree=nrow(match)
      
      num_disagree=nrow(v2)-length(match[,2])+nrow(v1)-length(match[,1])
      
      return (num_agree/(num_disagree+num_agree))
}


# function inter-agreement 
inter_agreement=function(data, workerIDs,max_dist=0.007){  
print(workerIDs)
  data=data[data$workerID %in% workerIDs,]
  points=democratic_clustering_internal(data, max_dist,2)
    
  #points = data[which(data$workerID %in% set_workerID), ]
  #ds=dbscan(points@coords,min_dist/111.20,2)
  #points$cluster=ds$cluster
  
  #number of cluster
  num_agree=length(unique(points$cluster))-1 #-1 to remove cluster==0 (= noisy points)
  
  #number of no cluster 
  num_disagree=length(which(points$cluster==0))
  
  #ratio
  ratio_agreement=num_agree/(num_disagree+num_agree)
  
  #points$cluster[which(points$cluster>0)]=-1  
  #d=points$workerID[which(points$cluster==0)]
  #points$cluster[which(points$cluster==0)]=d
  #points2=as.data.frame(points)
  #s=qplot(y=coords.x2, x=coords.x1, data=points2, color=factor(cluster))

return(ratio_agreement)
}
#  return (0)
#}

#on volutneers data
inter_agreements=c()
for (i in 1:3){
  cat( "compute spatial inter agreeement of map ", maps[i],"\n")
  input_root=maps[i]
  input_volunteer=sprintf("%s_volunteer.csv",input_root)
  
  # Experiment1  data
  data=read_input(input_volunteer,50, 0)  
  num_workers=length(unique(data$workerID))
  
  
  comb=generate_combinaisons(num_workers,2,-1)
  
  cat( "number of participants :", num_workers,", num comb:", nrow(comb),"\n")
  
  ratio=apply(comb,1, FUN=function(x){inter_agreement2(data,x,0.007)})
  ratio=as.data.frame(ratio)
  ratio$map=labels[i]
  
  inter_agreements=rbind(ratio,inter_agreements)
} 
inter1=inter_agreements[inter_agreements$map=="map1",]
print(mean(inter1$ratio)) #0.74
inter2=inter_agreements[inter_agreements$map=="map2",]
print(mean(inter2$ratio)) #0.27
inter3=inter_agreements[inter_agreements$map=="map3",]
print(mean(inter3$ratio)) #0.40

#p=qplot(ratio,..count.. / sum(..count..), data=inter_agreements, geom="histogram", group=map)+ xlim(0,1)

#histogram , same graph 
#ggplot()+geom_histogram(aes(y=..count.. / sum(..count..),x=ratio, alpha=0.1,fill=map),data=inter1)+ geom_histogram(aes(y=..count.. / sum(..count..),x=ratio,alpha=0.5,fill=map),data=inter2)+xlim(0,1)+geom_histogram(aes(y=..count.. / sum(..count..),x=ratio,alpha=0.1,fill=map),data=inter3)

#histogram , different graph
plot=ggplot()+geom_histogram(aes(y = ..ndensity..,x=ratio),data=inter1*100)+geom_density(aes(y=..scaled..,x=ratio, colour='red'),data=inter1)+  geom_histogram(aes(y=..count.. / sum(..count..),x=ratio),data=inter2)+xlim(0,1)+geom_histogram(aes(y=..count.. / sum(..count..),x=ratio),data=inter3)+facet_grid(. ~ map)+ blank+theme_bw()+opts(legend.position = "none")+xlab("spatial inter-agreement")+ylab("density")
print(plot)

#good but pb of scale 
inter_agreements$ratio=inter_agreements$ratio*100
plot=ggplot(data=inter_agreements,aes(x=ratio))+geom_histogram(aes(y=..density..))+ geom_density(colour='red',size=0.5) +ylab("density")+xlab("spatial inter-agreement")+xlim(0,100)+facet_grid(. ~ map)+ blank+theme_bw()+opts(legend.position = "none")
ggsave("spatial_inter_agreement.pdf",plot)

#Interleaved histogram
#ggplot(inter_agreements, aes(x=ratio, fill=map)) + geom_histogram(position=position_dodge())

#density function
#qplot(x=ratio, fill=map,data=inter_agreements, geom="density", alpha=0.2)

#
#qplot(x=ratio, data=inter_agreements, geom="density", facets=map,alpha=0.2)
#graphics
