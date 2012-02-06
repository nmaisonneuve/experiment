library(combinat)
library(sp)
## Load required packages
#for summarySE
library(doBy)

MAX_SAMPLE=200

compute_agreement_vector=function(num_workers,interval=0.15, max=1){
  return(unique(ceiling(seq(0.1,max,interval)*num_workers)))
}

generate_list_comb=function(n,vector_number_workers, max_sample=MAX_SAMPLE){
  size=length(workers)
  list_comb=list(size)
  for (i in 1:size){
    list_comb[[i]]=generate_combinaisons(n,i,max_sample)
  }
  return (list_comb)
}

generate_combinaisons_fast<-function(n,k, max_sample){
  if (exists("list_comb")){
    return(list_comb[[k]])
  }else{
    load("list_comb.data")
    #generate_list_comb(n,number)
  }
}
############################# FUNCTION DECLARATION ####################
# Generate all combinaison of [N,K]
# If there is too many possible combinaisons, 
# a random subset of {max_sample} unique combinaisons is returned
generate_combinaisons<-function(n,k,max_sample=MAX_SAMPLE){	
	#compute number of possible combinaison [n,k]
	num = choose(n, k)	
	if (num > max_sample & max_sample!=-1) {		
		print(sprintf("\nToo many possible combinasons (%f), get only %f (unique) samples", num, max_sample))
		# get a sample of 20?boxplo0 randoms but unique combinaisons
		comb=matrix(ncol=k, nrow=0)
		while (nrow(comb)<max_sample)  #filling 1 by 1
		comb=unique(rbind(comb,t(sort(sample(n, k)))))
    #sort(sample(10, 2))
		#alternative: non unique sample solution 
		#comb_i_workers = replicate(MAX_SAMPLE, sample(total_workers, num_workers))
	}
	else {
		# generate all possible combinaisons 
		comb = t(combn(n, k))
	}	
	return(comb)
}


compute_accuracy_explained <- function(obs, refs, min_dist){  
      match=zerodist2(obs, refs,min_dist/111.1949)
      match=subset(match, !duplicated(match[,1]))
      match=subset(match, !duplicated(match[,2]))
      obs$matching=0
      obs$matching[match[,1]]=1      
      fn_errors=refs[-match[,2],]
      fn_errors$support=0
      fn_errors$workerID=NULL
      fn_errors$matching=-1
      obs=rbind(obs,fn_errors)
  return(obs)
}

compute_accuracy_fast <- function(obs, refs, min_dist){  
      match=zerodist2(obs, refs,min_dist/111.1949)
      match=subset(match, !duplicated(match[,1]))
      match=subset(match, !duplicated(match[,2]))
      true_positive = length(match[,1]) 	
  return(recall_precision(true_positive,nrow(obs),nrow(refs)))
}
  
compute_accuracy <- function(obs, refs, min_dist) {
	
	#matched_ref = vector()
  obs$cluster=0
	clustered = c()
	#for each reference annotations
	# compute distance with clusters/observations
	# select and store the matching clusters ( dist <min_dist)	
	for (i in 1:nrow(obs)) {    
		dist = spDistsN1(refs@coords, obs[i,], longlat = TRUE)
		selected_idx = which(dist <= (min_dist))
		not_assigned_yet = setdiff(selected_idx, clustered)
    nb_results=length(not_assigned_yet)
		if (nb_results>0){
      if (nb_results==1) 
        idx=1
		  else    			
        idx=which.min(dist[not_assigned_yet])      
      obs$cluster[i]=not_assigned_yet[idx]
			clustered = c(clustered, not_assigned_yet[idx])      
		}
	}
  
  true_positive = length(clustered)
	return (recall_precision(true_positive,nrow(obs),nrow(refs)))
}

recall_precision=function (tp,nb_obs,nb_refs){

  #cat(tp, nb_obs, nb_refs,"\n")
  #number of missing buildings
	#false_negative = nb_refs - tp
	#number of wrongly identified buildings
	#false_positive = nb_obs - tp		
	if (tp==0) {
    precision=1 
	 }else{
    precision=tp/nb_obs #ratio of wrongly identified buildings
	}
  
  recall=tp/nb_refs # ratio of missing buildings  
  
  if (!is.na(precision) & !is.na(recall))
	  fmeasure=2*(precision*recall)/(precision+recall) 
  else
    fmeasure=NA
	return(c(precision,recall, fmeasure,nb_obs))  
}

accuracy_dist <- function(data, reference, num_workers=1,num_voters,min_dist, min_cluster, method='density') {
  
  clusters=compute_clusters(data,num_workers, num_voters, min_cluster, method)  
  clusters.list=lapply(unique(clusters$workerID),FUN=function(clusterID){
  cluster=clusters[clusters$workerID==clusterID,]
        return(c(clusterID,compute_accuracy_fast(cluster,reference,min_dist)))
  })
  accuracy=do.call("rbind",clusters.list)
	colnames(accuracy) <- c("workerID","precision", "recall", "fmeasure","num_annotations")
  accuracy=cbind(accuracy, dist=min_dist)
  accuracy=cbind(accuracy, dist_cluster=min_cluster)	
  accuracy=cbind(accuracy, num_workers=num_workers)
  accuracy=cbind(accuracy, num_voters=num_voters)
  return(accuracy)
}

compute_centroid=function(data){
 data=data[data$cluster!=0,]
  if (nrow(data)>0){
  centroid=aggregate(data@coords, list(data$cluster), mean)  
  support=aggregate(data$workerID, list(data$cluster), FUN=function(x){ return(length(unique(x)))})
  output=SpatialPointsDataFrame(coords=centroid[,2:3], data=as.data.frame(support[,2]), proj4string=CRS(ps))  
  }else{
    output=SpatialPointsDataFrame(coords=cbind(0,0),data=as.data.frame(c(0)), proj4string=CRS(ps))
  }
  names(output)=c('support')
 return(output)
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

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
    

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # Collapse the data
    formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
    #print(formula)
    datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)

    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}


############################### DEPRECATED
closest_pair=function(obs,ref,min){
  output=c()
  for (i in 1:nrow(obs)){
    dist = spDistsN1(ref@coords, obs@coords[i,], longlat = TRUE)
    idx=which(dist <= min)
    tmp=cbind(obs=rep(i,length(idx)),ref=idx,dist=dist[idx])
    output=rbind(tmp,output)
  }
  match=output
  match=subset(match, !duplicated(match[,1]))
  match=subset(match, !duplicated(match[,2]))

  output=match
  #output=as.data.frame(output)    
  #if (nrow(output)>0){
  #output=t(sapply(split(output,output$ref),FUN=function(x){return(x[which.min(x[,3]),])}))
  #mode(output)="numeric"  
  #output=as.data.frame(output)  
  #output=t(sapply(split(output,output$obs),FUN=function(x){return(x[which.min(x[,3]),])}))  
  #mode(output)="numeric"
  #}
  return(match)
}



#summary=function(data){  s=by(output[3:6], output$num_workers, mean)}
best_voters2=function(data){
  data_d=cbind(data$num_workers,data$num_voters)
  sum_data<-summarySE(data,measurevar="fmeasure", groupvars=c("num_workers","num_voters"))
  num_workers=unique(data$num_workers)
  best_voters=vapply(num_workers,1,FUN=function(x){
    max_idx=which.max(sum_data$fmeasure[sum_data$num_workers==x])
    voter=sum_data$num_voters[sum_data$num_workers==x][max_idx]
    return(voter)})
  best_parameters=cbind(num_workers,best_voters)
  idx=apply(data_d,1,FUN=function(x){return(length(which(best_parameters[,1] == x[1] & best_parameters[,2] == x[2]))>0)})
  return(data[idx,])
}
            
best_voters=function(data){
  data_d=cbind(data$num_workers,data$num_voters)
  num_workers=unique(data_d[,1])
  best_voters=vapply(num_workers, 1, FUN=function(x){ 
    idx=which(data_d[,1]==x)
    idx2=which.min(abs(data_d[idx,2]-0.26*x))
    return(data_d[idx,2][idx2])})
  print(best_voters)
  best_parameters=cbind(num_workers,best_voters)
  idx=apply(data_d,1,FUN=function(x){return(length(which(best_parameters[,1] == x[1] & best_parameters[,2] == x[2]))>0)})
  return(data[idx,])
}

compute_accuracy_fast_old <- function(obs, refs, min_dist){
    output=closest_pair(obs,refs,min_dist)    
    true_positive = length(output[,1])
    return(recall_precision(true_positive,nrow(obs),nrow(refs)))
}