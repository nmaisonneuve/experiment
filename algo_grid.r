library(spatstat)
source('parallel_cluster.r')


#real input file into a ppp object 
ppp_read=function(input_file, min_size=50){
  data <- read.csv(input_file, , encoding = "UTF-8")
    
  #select only good workers
  workers=unique(data$assignID)
  size_per_worker=apply(t(workers), 2,FUN=function(x){ return(length(which(data$assignID==x)))})
  good_workers=workers[which(size_per_worker>min_size)] 
  
  # filter  by size of the contribution
  data=data[data$assignID %in% good_workers,]
  
  #reindex worker
  data$assignID=apply(t(data$assignID), 2,FUN=function(x){which(good_workers==x)})
  
  #filter by spatial window 
  data.ppp=ppp(data[,1], data[,2],window=window, marks=factor(data[,3]), check=TRUE)  
  data.ppp=as.ppp(data.ppp) #to remove rejected points     
  
  return (data.ppp)
}

discretize_all=function(data,res){
  output=c()  
  workers=unique(data$marks)  
  for (i in workers){    
    selected=data[data$marks==i,]         
    output=cbind(output,discretize(selected,res))
  }
  return (output)
}

discretize=function(data,res){
  selected.q=quadratcount(data, nx = res[1], ny = res[2])    
  return (ifelse(as.vector(selected.q)>=1, 1,0))
}

compute_agreement_vector=function(num_workers,interval=0.15){
  return(unique(ceiling(seq(0.1,1,interval)*num_workers)))
}

compute_accuracy=function(data,ref,num_workers,num_voters){
  
  #generating groups  
  #cat (num_workers,"workers on ", N,"\n")
  groups=generate_combinaisons(n=ncol(data), k=num_workers)    
  #benchmarks=list(nrow(groups))   
  benchmarks=list()
  for (i in 1:nrow(groups)){ 
    workers=groups[i,]
    if (length(workers)>1){
      crowd=ifelse(rowSums(data[,workers])>=num_voters,TRUE,FALSE)    
    }
    else {
      crowd=ifelse(data[,workers],TRUE,FALSE)
    }
    b=similarity(crowd,ref)
   # cat ("compute aggregation of group (", num_workers,"):", workers,"(num voters: ", min_volunteers,")","nb markers:",length(which(aggregation))," r:", b,"\n")  
    #benchmarks=append(benchmarks,b)    
    benchmarks=append(benchmarks,b)
  }
  
  #benchmarks=do.call(rbind, benchmarks)
  benchmarks=data.frame(matrix(unlist(benchmarks), ncol=4, byrow=T))
  benchmarks$num_voters=num_voters
  benchmarks$num_workers=num_workers
  names(benchmarks)[1:4]=c("precision","recall","fmeasure", "nb_obs")  
  #benchmarks=cbind(benchmarks, num_voters=num_voters,num_workers=num_workers)
  return (benchmarks)
}


similarity=function(obs,ref){  
  nb_ref=length(which(ref))
  nb_obs=length(which(obs))    
  diff=obs & ref  
  tp=length(which(diff))  
  return(recall_precision(tp,nb_obs,nb_ref))
  
}

resolution=function (window, min_dist){
  resolution=c(diff(window$xrange)/(min_dist/111.7), diff(
  window$yrange)/(min_dist/111.7))
  return (ceiling(resolution))
}


#########################deprecated

aggregate_=function(workers, data, min_volunteers, resolution){
  selected=data[data$marks %in% workers]  
  selected.q=quadratcount(selected, nx = resolution[1], ny = resolution[2])  
  return (ifelse(as.vector(selected.q)>=min_volunteers, TRUE,FALSE))
}

similarity2=function(ref,obs,density){
  
  true_idx=which(ref)
  false_idx=which(!ref)
  
  nb_positive_ref=length(true_idx)  
  nb_negative_ref=length(false_idx)
      
  nb_positive_obs=length(which(obs))  
  nb_negative_obs=length(which(!obs))
  
  diff_pos=obs[true_idx] & ref[true_idx]
  diff_neg=!(obs[false_idx] | ref[false_idx])
   
  tp=length(which(diff_pos))  
  tn=length(which(diff_neg))
  
  if (tp==0){
    precision=1
  }else{
    precision=tp/nb_positive_obs  #positive predictive value (or precision)
  }
  
  if (tn==0){
    NPV=1}
  else {
    NPV=tn/nb_negative_obs #negative predictive value
  }
  
  sensitivity=tp/nb_positive_ref 
  specificity=tn/nb_negative_ref #pb to identify negative 
  
  fmeasure=0
  #print("ok")
  #print(precision)
  #print(sensitivity)
  if (!is.na(precision) & !is.na(sensitivity))
    fmeasure=2*(precision*sensitivity)/(precision+sensitivity) 
  else{
    fmeasure=NA
  }
  
  precision_computed =sensitivity*density/(sensitivity*density+ (1-specificity)*(1- density))
  fmeasure_computed=2*(sensitivity*density)/(density*(sensitivity+specificity) - specificity +1)
  
  result=cbind("PPV"=precision,"sensitivity"=sensitivity, "fmeasure"=fmeasure,"NPV"=NPV, "specificity"=specificity,precision_computed,fmeasure_computed)
  return(result)    
}