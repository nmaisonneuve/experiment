#complementary

source('input_data.r')
source('cluster_map.r')
source('parallel_cluster.r')

agreement_ratio=function(data,set_workerID,min_dist){
  points = data[which(data$workerID %in% set_workerID), ]
  ds=dbscan(points@coords,min_dist/112,2)
  points$cluster=ds$cluster
  
  #number of cluster
  num_agree=length(unique(points$cluster[which(points$cluster>0)]))
  
  #number of no cluster 
  num_disagree=length(points$cluster[which(points$cluster==0)])
  
  #ratio
  ratio_agreement=num_agree/(num_disagree+num_agree)
  
  #points$cluster[which(points$cluster>0)]=-1  
  #d=points$workerID[which(points$cluster==0)]
  #points$cluster[which(points$cluster==0)]=d
  #points2=as.data.frame(points)
  #s=qplot(y=coords.x2, x=coords.x1, data=points2, color=factor(cluster))

return(ratio_agreement)
}


#input
input_root="haiti2/haiti2"
input_volunteer=sprintf("%s_volunteer.csv",input_root)
#input_gold=sprintf("%s_reference_cleaned_0_007.csv",input_root)
#output_filename=sprintf("%s_output_min_50_match_0_01_all.csv",input_root)
data=read_input(input_volunteer,50, 0)
avg_disagreement=function(data,num_volunteers){
# Experiment1  data
  num_workers=length(unique(data$workerID))
  d=split(data,data$workerID)
  #print(sapply(d, length))
  comb=generate_combinaisons(num_workers,num_volunteers,200)
  ratio=apply(comb,1, FUN=function(x){ agreement_ratio(data,x,0.007)})
  return(ratio) 
}
ratio=avg_disagreement(data,2)
ratio_haiti2=as.data.frame(ratio)
ratio_haiti2$map="haiti2"

#h=hist(ratio)
#plot(h)
#h.sum=h
#h.sum$counts     <- cumsum(h$counts)
#h.sum$density    <- cumsum(h$density)
#h.sum$itensities <- h$density
# plot , style 1
#qplot(ratio, data=as.data.frame(ratio), geom="histogram")+xlim(0,1)+facet_wrap(~ map, ncol=3) +opts(aspect.ratio=1)+ geom_histogram(aes(fill = ..count..))

# plot , style 2
ggplot(ratio, aes(ratio, fill = map))  +  geom_density(alpha = 0.2) + xlim(0, 1)+ xlab("inter-agreement amongs pairwises of volunteers")+ opts(axis.title.x = theme_text(size = 14, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14))

#print(points[1:10,])
#print(head(points2))


