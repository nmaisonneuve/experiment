library(fpc)

source('parallel_cluster.r')

density_clustering=function (data,min_dist=0.07,min_volunteers=2){
  ds=dbscan(data@coords,min_dist/111.19,min_volunteers)
  data$cluster=ds$cluster
  return (compute_centroid(data))
}