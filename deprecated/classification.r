setwd('C:/Users/nico2/Documents/R')
# Experiment1  data
#around 30 workers producing around 4000 annotations
experiment <- read.csv("photo.csv", , encoding = "UTF-8")
n_photo=nrow(experiment)
#n_photo=16
experiment$cluster=0
dist_ma=dist(experiment[1:n_photo,c(2,4,6,7)])
print(dist_ma)
z1=as.matrix(dist_ma)
n_cluster=1
for (i in 1:n_photo){
	idx=which(as.vector(z1[i,])<0.02)
	if (length(idx)>1){
	print(experiment$photo_id[idx])
	print(z1[i,idx])
	experiment$cluster[idx]=n_cluster
	n_cluster=n_cluster+1
	}
}
experiment[,c(1,8)]
