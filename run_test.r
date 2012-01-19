library(spatstat)

source('input_data.r')

source('parallel_cluster.r')
source('plot_map.r')

#haiti2 
#min = 0.004

#haiti
#min= 0.004

#island
#min=0.008/0.007

#input
input_root="island/island"
input_volunteer=sprintf("%s_volunteer.csv",input_root)
input_gold=sprintf("%s_reference.csv",input_root)
#density_filename=sprintf("%s_reference_density.png",input_root)
#nnd_filename=sprintf("%s_reference_nnd.png",input_root)


plot_nndist=function(ref){
  dist=seq(0.001,0.02,0.0005)
  dist_matrix= spDists(ref, ref, longlat = TRUE)
  dist_matrix[which(dist_matrix<0.0001)]<-NA
  nndist=apply(dist_matrix, 2, function(x) {min(x, na.rm=TRUE)})
  print(nndist)
  print(length(nndist))
  freq=apply(as.matrix(dist),1,FUN=function(x){ return(length(which(nndist>x)))} )  
  freq=freq/nrow(ref)
  result=cbind(dist, freq)
  print(result)
  #cel_cum=cumsum(freq)
  #print(cel_cum)  
  print(plot(result))
  return (result)
}

density=function(){
  toto <- read.csv(input_gold, , encoding = "UTF-8")
  ref.ppp=ppp(toto[,4], toto[,5],c(min(toto[,4])-0.0001,max(toto[,4])+0.0001), c(min(toto[,5])-0.0001,max(toto[,5])+0.0001))
  h=hist(nndist(ref.ppp))
  h$counts=h$counts/sum(h$counts)
  
  png(filename=density_filename, width = 640, height = 640)
  title="density of map 2"
  plot(density(ref.ppp, sigma=0.0001), main=title)
  plot(ref.ppp,pch=16, cex=0.6, add=TRUE)
  dev.off()
  png(filename=nnd_filename, width = 640, height = 640)
  title="Stienen diagram of map 2"
  plot(ref.ppp %mark% (nndist(ref.ppp)/2), markscale = 1, main = title)
  dev.off()
}

check_result=function(reference){
  min_dists=c(0.001,0.003,.005,0.007,0.01)
  match_dists=c(0.01)
  for (match_dist in match_dists){
    for (min_dist in min_dists){
    collective=merge(points,min_dist,min_voters)
#to_kml(collective,sprintf('collective_%f_%f.kml',min_voters,min_dist))
#print("selection")
#print(nrow(points))
#print("collective")
#print(nrow(collective))
    result=compute_accuracy(collective,reference,match_dist)
    print(sprintf("\n match:%f, merging:%f",match_dist, min_dist))
    print(result)
      }
    }
}

# Experiment1  data
#data=read_input(input_volunteer,50, 0)
#print("num workers")
#print(length(unique(data$workerID)))
#d=split(data,data$workerID)
#print(sapply(d, length))

#print("experiment")
#print(nrow(data))

#gold standard data 
reference=read_input(input_gold,0, -1)
mean_nndist=compute_mean_nndist(reference)
plot_nndist(reference)
print(mean_nndist)
print("reference")
print(nrow(reference))
#to_kml(reference,'test.kml')
#min_voters=1

#t=c(1,3,4,5,8, 10)
#points = data[which(data$workerID %in% t), ]
#collective=merge(points,.006,1)
#collective2=merge(points,.003,2)

#result=compute_accuracy(collective, reference,0.01)
#print(result)
#result=compute_accuracy(collective2, reference,0.01)
#print(result)
#s=plot_map_with_expert(points, collective, collective2, reference)