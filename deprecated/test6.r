library(combinat)
library(sp)
library(ggplot2)
library(fpc)

# parallel computing
library(doSNOW)
library(foreach)

## Load required packages
library(maptools)
library(rgdal)

############################# FUNCTION DECLARATION ####################
# Generate all combinaison of [N,K]
# If there is too many possible combinaisons, 
# a random subset of {max_sample} unique combinaisons is returned

generate_combinaisons<-function(n,k,max_sample=MAX_SAMPLE){	
	#compute number of possible combinaison [n,k]
	num = choose(n, k)	
	if (num > max_sample) {		
		print(sprintf("Too many possible combinasons (%i), get only %i (unique) samples", num, max_sample))
		# get a sample of 20?boxplo0 randoms but unique combinaisons
		comb=matrix(ncol=k, nrow=0)
		while (nrow(comb)<MAX_SAMPLE)  #filling 1 by 1
		comb=unique(rbind(comb,t(replicate(1, sample(n, k)))))
		#alternative: non unique sample solution 
		#comb_i_workers = replicate(MAX_SAMPLE, sample(total_workers, num_workers))
	}
	else {
		# generate all possible combinaisons 
		comb = t(combn(n, k))
	}	
	return(comb)
}

intern_accuracy_dist <- function(data, reference,min_dist) {

	accuracy <- data.frame()
	user_groups=unique(data$workerID)
	for (user_group in user_groups){
	 clusters=data[data$workerID==user_group,]
	 accuracy <- rbind(accuracy,compute_accuracy(obs = clusters,reference,min_dist))
	}
	names(accuracy) <- c("precision", "recall", "fmeasure","num_annotations")

   return(accuracy)
}

compute_accuracy <- function(obs, refs, min_dist) {
	
	matched_ref = vector()

	matched_clusters = vector()

	pts=obs

	#for each reference annotations
	# compute distance with clusters/observations
	# select and store the matching clusters ( dist <min_dist)	
	for (i in 1:nrow(refs)) {
		ref = refs[i, ]
		dist = spDistsN1(pts, ref, longlat = TRUE)
		selected_idx = which(dist <= min_dist)
		not_assigned_yet = setdiff(selected_idx, matched_clusters)
		#check if already assigned
		if (length(not_assigned_yet)>0) {
			matched_ref = c(matched_ref, ref)
			idx=which.max(dist[not_assigned_yet])
			#idx=1
			matched_clusters = c(matched_clusters, not_assigned_yet[idx])
		}
	}

	#number of true positive
	true_positive = length(matched_ref) 
	
	#number of missing buildings
	false_negative = nrow(refs) - true_positive 

	#number of wrongly identified buildings
	false_positive = nrow(obs) - true_positive 
	
	
	precision=true_positive/nrow(obs) #ratio of wrongly identified buildings
	recall=true_positive/nrow(refs) # ratio of missing buildings
	if ((precision==0)  || (recall==0)){
		fmeasure=0
	}else{
		fmeasure=2*(precision*recall)/(precision+recall) # aggregation
	}
	return(c(precision,recall, fmeasure,nrow(obs)))
}

accuracy_dist <- function(data, reference, num_workers=1,num_voters,min_dist, min_cluster) {
      clusters=cluster_dist(data,reference,num_workers, num_voters, min_cluster)	
	accuracy=intern_accuracy_dist(clusters,reference,min_dist)	
	accuracy$num_voters=num_voters
	accuracy$num_workers=num_workers
	accuracy$dist=min_dist
	accuracy$dist_cluster=min_cluster

	return(accuracy)
}

cluster_dist <- function(data, reference, num_workers,num_voters,min_dist) {

	total_workers=length(unique(data$workerID))

	# to be sure 
	num_voters=min(num_voters,num_workers)

	#Generate all (or a subset) combinaisons of {num_workers} workers
	comb_i_workers=generate_combinaisons(n=total_workers, k=num_workers)

	lcomb=nrow(comb_i_workers)
	#print(sprintf("number of combinaison tested: %i",lcomb))

	clusters=c()
	#for a given combinaison of selected workers
	for (j in 1:lcomb) {

		# select the annotations according to the selected workers		
		selected_points = data[which(data$workerID %in% comb_i_workers[j,]), ]

		#compute clusters


		if (num_workers==1){
			result=selected_points
		}else{
			result=merge(selected_points, min_dist,num_voters)
		}

		print(sprintf("%i/%i - compute cluster for workers %s (min dist=%f, min vote=%i, total annotations:%i, aggregated: %i)",j,	lcomb, toString(comb_i_workers[j,]),min_dist,num_voters, nrow(selected_points),nrow(result)))	

		if (length(clusters)==0){
			clusters=result
		}
		else	{
			clusters=rbind(clusters,result)
		}
	}
	return(clusters)	
}


merge=function(data,dist_min, num_voters){


   deg_min=dist_min/111.12 # distance in meter to lat degree
   workers_name=paste(unique(data$workerID), collapse=" ")

   #detecting clusters
   tmp=dbscan(data@coords, deg_min, MinPts = num_voters)

   data$clusterID=tmp$cluster
   d=aggregate(list(data@coords[,1],data@coords[,2]), list(data$clusterID), mean)
   names(d)=c("clusterID","lon","lat")
   d=d[c("lon", "lat", "clusterID")]
   d$workerID=workers_name
   if (nrow(d)==1){
      #coord=t(d[,1:2])
	coord=d[,1:2]
	
   }else{
      coord=d[,1:2]
   }
   d=SpatialPointsDataFrame(coords=coord, data=d[,3:4], proj4string=CRS(ps))
   return(d)
}


to_kml=function(data){
writeOGR(data, dsn="test2.kml", layer= "cycle_wgs84", driver="KML", dataset_options=c("NameField=name"))
}



internal_merge=function(data){
   clustersID = unique(data$cluster)   
   d=matrix(nrow = 0, ncol = 4)
   for (clusterID in clustersID) {	
	   tmp=data[data$cluster==clusterID,]
	   #workers=unique(tmp$workerID)
	   lat=mean(tmp@coords[,2])
	   lng=mean(tmp@coords[,1])
	   d=rbind(d, c(lng, lat,data$workerID[1], clusterID))
  }
	if (nrow(d)==1){
	  coord=t(d[,1:2])
       data=t(d[,3:4])
	}else{
	  coord=d[,1:2]
        data=d[,3:4]
	}
  output=SpatialPointsDataFrame(coords=coord, data=as.data.frame(data), proj4string=CRS(ps))
  names(output)=c("workerID", "cluster")
  return(output)
}

summary=function(data){
  s=by(output[3:6], output$num_workers, mean)
}

save= function(clusters, num_workers, num_voters, dist_min){
   write.csv(clusters, file=sprintf("clusters_nw%i_%i_%i.csv",num_workers,num_voters,dist_min*1000))
}

############################# PLOT ##############################

plot_output=function(output){
#boxplot(accuracy ~ num_workers,output)
p <- ggplot(output, aes(accuracy, ..count..)) + geom_histogram(binwidth = 0.1) 
p + facet_wrap(~ num_workers) 
}

plot_map=function(obs, ref, file){
obs_df=as.data.frame(obs)
ref_df=as.data.frame(ref)
s=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) +
geom_point(aes(x = lon,y = lat),data=obs_df,shape = 22,colour = '#0000ff',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
facet_wrap(facets = ~workerID ) +
#add geom point expert 
geom_point(aes(x = lon,y = lat),data=ref_df,shape = 22,colour = '#ff0000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
ggsave(s, filename=file, height=18,width=24,dpi=120)
}


###################### INDIVIDUAL PLOT MAP ###########################

plot_individual_performance=function(output){
num_instances=nrow(output)
output=output[order(-output$fmeasure),]
output$workerID=seq(1:nrow(output))
temp=c()
t1=cbind(output[,c("fmeasure","workerID")],replicate(num_instances,"fmeasure"))
names(t1)=c("measure", "workerID", "type")
t2=cbind(output[,c("precision","workerID")],replicate(num_instances,"precision"))
names(t2)=c("measure", "workerID", "type")
t3=cbind(output[,c("recall","workerID")],replicate(num_instances,"recall"))
names(t3)=c("measure", "workerID", "type")
temp=rbind(temp,t1)
temp=rbind(temp,t2)
temp=rbind(temp,t3)
temp=as.data.frame(temp)
temp$type=factor(temp$type)
temp$workerID=as.numeric(temp$workerID)
temp$measure=as.numeric(as.character(temp$measure))
s=qplot(type, measure, data=temp, fill=type)+geom_histogram()+ facet_grid(type ~ workerID)+ylim(0,1)+ opts(axis.ticks = theme_blank(),axis.text.x = theme_blank(), legend.position = "none")+xlab("")+ylab("")
return(s)
}

plot_individual=function(data){
# individual performance 
snug.opts <- opts(axis.ticks.x = theme_blank(), 
             axis.title.x = theme_blank(), 
		 axis.text.x = theme_blank(),
		 legend.position = "none",
             plot.margin = unit(c(0.2,0.5,0.2,0), "lines")) 

d1=qplot(factor(workerID), fmeasure, data=as.data.frame(data) , fill=fmeasure)+geom_bar()+ xlab("")+ylim(0,1)+snug.opts
d2=qplot(factor(workerID), precision, data=as.data.frame(data) , fill=precision)+geom_bar()+xlab("")+ylim(0,1)+snug.opts
d3=qplot(factor(workerID), recall, data=as.data.frame(data) , fill=recall)+geom_bar()+ ylim(0,1) +snug.opts
grid.arrange(d1,d2,d3, nrow=3)


# distribution 
snug.opts <- opts(axis.ticks.x = theme_blank(), 
		 legend.position = "none",
             plot.margin = unit(c(0.2,0.5,0.2,0), "lines")) 
d1=qplot(fmeasure, y=..count../sum(..count..), data=as.data.frame(data) , geom="histogram", binwidth=0.2)+ylim(0,1)+xlab("fmeasure")+ylab("% population")+snug.opts+opts(axis.ticks = theme_blank(),axis.text.x = theme_blank())+xlim(0,1)
d2=qplot(precision, y=..count../sum(..count..), data=as.data.frame(data) , geom="histogram", binwidth=0.2)+ylim(0,1)+xlab("precision")+ylab("% population")+snug.opts+opts(axis.ticks = theme_blank(),axis.text.x = theme_blank())+xlim(0,1)
d3=qplot(recall, y=..count../sum(..count..), data=as.data.frame(data) , geom="histogram", binwidth=0.2)+ylim(0,1)+xlab("recall")+ylab("% population")+snug.opts+xlim(0,1)
grid.arrange(d1,d2,d3, nrow=3)
}

###################### PLOT ERROR MAP ###########################

plot_error_map=function(obs, ref){
matching=obs[obs$matching!=0,]
error=obs[obs$matching==0,]
s=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) +
geom_point(aes(x = lon,y = lat),data=as.data.frame(ref),shape = 21,colour = '#000000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
geom_point(aes(x = lon,y = lat),data=as.data.frame(matching),shape = 22,colour = '#00FF00',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
geom_point(aes(x = lon,y = lat),data=as.data.frame(error),shape = 22,colour = '#FF0000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
ggsave(s, filename=sprintf("error_map.png"), height=30,width=40,dpi=120)
}

plot_error_map2=function(obs, ref){

ref$workerID=NULL
obs$matching[obs$matching!=0]= 1 
obs$matching=factor(obs$matching)
print("ok")

s=qplot(lon,lat,data=as.data.frame(obs), colour=matching, shape=22, facets=~workerID) + coord_map(projection = 'mercator')+ 
geom_point(data=as.data.frame(ref), colour="black", size=1)+
opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line())
ggsave(s, filename="error_map_all.pdf", height=40,width=50,dpi=100)
}



compute_all_error_map=function(){
workers = unique(experiment$workerID)
experiment$matching=0
for (worker in workers) {
		experiment=matched(experiment, worker, reference,0.01)
 }
return(experiment)
}

matched=function(exp, worker, ref,min_dist){
  ref$taken=0
  obs=exp[exp$workerID==worker,]
  for(i in 1:length(obs)) {
	ref$dist= spDistsN1(ref, obs[i,], longlat = TRUE)
	selected=which(ref$dist<=min_dist & ref$taken==0)
	if (length(selected)>0){		
		obs$matching[i]=selected[1]
		ref$taken[selected[1]]=1
	}
  }
  #ref$taken=NULL
  #t=sum(obs$matching!=0)
  #obs$matching=NULL
  exp$matching[exp$workerID==worker]=obs$matching
  return(exp)
}

########  CLEAN DATA ########

### fct declaration ##

# find the minimum distance according to the reference dist
compute_min_dist=function(ref){
   dist_min= spDists(ref, ref, longlat = TRUE)
   dist_min=dist_min[-which(dist_min<0.0001)]
   dist_min=dist_min[!duplicated(dist_min)]
   distance=seq(0.0, 1.0, by=0.001) 
   r=sapply(t(distance),function(x){sum(dist_min>x)})
   r=r/length(dist_min)
   res=as.data.frame(cbind(distance,r))
  # s=ggplot(res[res$r>.9999,], aes(distance,r))
  # s+geom_point()
  #print(res[res$r>=1.0,])
  return (max(res[res$r==1.0,1]))
}

# minimum amount of annorations to accept a worker

# Delete bad workers with low contribution
preprocessing=function(experiment,low_annotation, min_dist){
 workers = unique(experiment$workerID)
 total_workers = nrow(workers)
 i = 1
 cleaned_experiment=c()
 for (worker in workers) {
	#print(sprintf("worker %i", worker))
	worker_annotations = experiment[experiment$workerID == worker, ]	
	worker_annotations=clean(worker_annotations,min_dist)
	num_annotations = nrow(worker_annotations)	
	if (num_annotations >= low_annotation) {
		#reindexing
		worker_annotations$workerID[worker_annotations$workerID==worker]= i
		i=i+1
		# add
		if(length(cleaned_experiment)==0){
		cleaned_experiment=worker_annotations
 		}else{
		cleaned_experiment=rbind(cleaned_experiment,worker_annotations)
		}
	}	
 }
 return(cleaned_experiment)
}

# Aggregating all points < dist_min
clean=function(data,dist_min){
   deg_min=dist_min/111.12 # distance in meter to lat degree
   print(sprintf("before cleaning: row %i (min degree: %f)", length(data),deg_min))
   tmp=dbscan(data@coords, deg_min,  MinPts = 2,method='hybrid') 
   data$cluster=tmp$cluster
   tmp=unique(data$cluster)
   tmp=tmp[tmp!=0]
   good_points=data[!data$cluster %in% tmp,]
   if (length(tmp)>0){  
     to_cluster=data[data$cluster %in% tmp,]
     clustered=internal_merge(to_cluster)
     good_points=rbind(good_points,clustered)
   }
   print(sprintf("after cleaning: row %i",length(good_points)))
   return(good_points)
}

######## END fct #####


############################# MAIN ##############################

# DATA 
# setwd("R")


# EPSG:3857 (Spherical Mercator projection)
ps="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Experiment1  data
#around 30 workers producing around 4000 annotations
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID
experiment <- read.csv("experiment1.csv", , encoding = "UTF-8")
experiment=SpatialPointsDataFrame(coords=experiment[,1:2], data=as.data.frame(experiment[,3]), proj4string=CRS(ps))
names(experiment) <- c("workerID")

#gold standard data 
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID(expertID)
reference <- read.csv("experiment1_reference.csv", encoding = "UTF-8")
reference=SpatialPointsDataFrame(coords=reference[,1:2], data=as.data.frame(reference[,3]), proj4string=CRS(ps))
names(reference) <- c("workerID")



########################## Preprocessing / Data cleaning #########


####  Delete outside a boundingbox box ####

#satellite imagery 
#bounding box 

#noth east point
n_e_lat=18.5504150
n_e_lng=-72.2570801

#south west point
s_w_lat=18.5614014
s_w_lng=-72.2515869

experiment=experiment[experiment@coords[,2]>n_e_lat,]
experiment=experiment[experiment@coords[,2]<s_w_lat,]
experiment=experiment[experiment@coords[,1]>n_e_lng,]
experiment=experiment[experiment@coords[,1]<s_w_lng,]

#min_dist=compute_min_dist(reference)
#plot_map(experiment,reference, "plot_before_experiment1.png")
experiment=preprocessing(experiment, 150, 0.005)
#plot_map(experiment,reference, "plot_after_experiment1.png")

#################################################
#ref=reference
#results=c()
#experiment$matching=0
#obs=experiment[experiment$workerID==12,]
#for (dist in dists){
#  tmp=compute_accuracy(obs,reference,dist)
#
#tmp=matched(obs,reference, dist)
#plot_error_map(tmp, reference)

#results=rbind(results,  c(dist,tmp))
#}
#print(results)

# plot 
#s=ggplot() 
#+ opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) + facet_wrap(facets = ~workerID, nrow = 3, ncol = 8, scales = 'free') + geom_point(aes(x = lon,y = lat),data=exp1,shape = 22,colour = '#0000ff',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
#ggsave(s, filename="image.png", height=18,width=24,dpi=72)


#################################  COMPUTATION ########################

# example
#print(length(unique(experiment$workerID)))
#output = accuracy_dist(experiment, reference, num_workers=8,num_voters=2, min_dist=0.01)
#print(output)

MAX_SAMPLE = 10

c1=function(num_worker,num_voter){
	if (num_voter<=num_worker){	
		return(c(num_voter, num_worker))
		#accuracy_dist(experiment, reference, num_worker, num_voter, 0.01, 0.01)
	}
}
compute_all_accuracy_parallel=function(){
getDoParWorkers() 
getDoParName() 
registerDoSNOW(makeCluster(2, type = "SOCK")) 
getDoParWorkers() 
getDoParName() 
num_workers=c(1,2,3)
num_voters=c(1,2,3)
 output22<- foreach( num_worker = num_workers, .combine = "rbind") %:% foreach(num_voter = num_voters, .combine = "rbind") %dopar% {
	  c1(num_worker,num_voter)
 }
 return(output22)
}


#############################  VERY IMPORTANT CLUSTER  ################
# we don't compute for each number of worker  1 , 2 , 3, 4 ,5 ,6
# but for a sub set 1 ,2 ,4, 6, etc.. to be faster
compute_all_accuracy=function(){

num_dist=c(0.01)
num_workers=c(1,2,3,4,6,8,10,14)
num_voters=c(1,2,3,4,6,8)

#num_workers=c(1)
#num_voters=c(1)
output=c()
for (dist_cluster in num_dist){
 for (num_worker in num_workers){
    for (num_voter in num_voters){
	  if (num_voter<=num_worker){
		print(dist_cluster)
   		output=rbind(output,accuracy_dist(experiment, reference, num_workers=num_worker, num_voters=num_voter, min_dist=0.01, min_cluster=dist_cluster))
	}
   }
 }
}
#output=output[order(-output$fmeasure),]
#output$workerID=seq(1:nrow(output))
return (output)
}

#data=individual2
#data=data[order(-data$fmeasure),]
#data$workerID=seq(1:nrow(data))
#qplot(factor(workerID), fmeasure, data=as.data.frame(data) , fill=fmeasure)+geom_linerange(aes(ymin = 0,ymax = fmeasure, fill=fmeasure)) + ylim(0,1)+snug.ops
#d2=qplot(factor(workerID), precision, data=as.data.frame(data) , fill=precision)+geom_bar()+ opts(axis.ticks = theme_blank(),axis.text.x = theme_blank(),legend.position = "none")+xlab("")+ylim(0,1)
#d3=qplot(factor(workerID), recall, data=as.data.frame(data) , fill=recall)+geom_bar()+ opts(legend.position = "none")+ylim(0,1)+xlab("Participant Idx")






#write.csv(output,"experiment2_output.csv", , encoding = "UTF-8")

#qplot(name,x, data=result , geom="bar") 
#qplot(precision, recall, data=output, alpha = 1/20, colour = dist, facets=num_workers ~ num_voters) + xlim(0,1) +ylim(0,1)

#ggplot(r,aes(num_voters)) +geom_line(aes(y=recall, color="missed buildings (FalseNegative)" ))+ geom_line(aes(y=precision, color="wrongly identified buildings (FalsePositive)")) + geom_line(aes(y=fmeasure, color="fmeasure")) + facet_wrap(~num_workers, ncol=8)+ylab("accuracy")+xlab("Quorum to support decision)") + ylim(0,1)

#accuracy=aggregate(. ~ num_voters +num_workers, data = output, mean)
#precision=aggregate(. ~ num_voters +num_workers, data = output, sd)
#r$precision=1-r$precision
#r$recall=1-r$recall

#precision
#ggplot(precision,aes(num_voters)) +
#geom_line(aes(y=recall, color="missed buildings (FalseNegative)" ))+ 
#geom_line(aes(y=precision, color="wrongly identified buildings (FalsePositive)"))+
#geom_line(aes(y=fmeasure, color="fmeasure" )) +
#facet_wrap(~num_workers, ncol=8) +
#ylab("")+
#xlab("quorum (min. number of participants agreed to support decision)") +
#opts(title="num of workers")+
#scale_colour_manual(name="Accuracy indicators", values=c("fmeasure"="black", "missed buildings (FalseNegative)"="red", "wrongly identified buildings (FalsePositive)"="blue"))





#output1 <- read.csv("experiment1_output.csv", , encoding = "UTF-8")
#output2 <- read.csv("experiment2_output.csv", , encoding = "UTF-8")
#output$exp=1
#output2$exp=2
#output=rbind(output1,output2)
output$exp=1
ggplot(output,aes(factor(num_voters),fmeasure)) + geom_boxplot(aes(fill=factor(exp)),outlier.colour = "gray", outlier.size = 0.1) + facet_wrap(~num_workers, ncol=5) + ylim(0,1) + ylab("Fmeasure")+ xlab("Quorum / Cluster Support") +opts(title="Number of participants", aspect.ratio = 1)
#ggsave(s, filename="exp1_exp2_recall.png", dpi=70)

# num_voters vs num_workers plot
#ggplot(output, aes(num_voters, num_workers, fill=fmeasure))+theme_bw()+ geom_tile()+ scale_colour_identity()+ scale_fill_gradientn(colours= c("red", "white", "blue"), limits=c(0,1))+ geom_text(data=output, aes(label=paste(round(fmeasure,3)), size=1, angle=20))
#ggsave(s, filename="error2_numworker_numvoters_experiment1.png", height=10,width=24,dpi=100)
