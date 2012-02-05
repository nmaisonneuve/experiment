library(ggplot2)
source('algo_grid.r')
source('plot_map.r')

#constants (window of the map != window of the data (buildings not at a corner))
window_island=owin(c(34.17449,34.18145),c(-0.39021,-0.38439))
window_haiti=owin(c(-72.2570801,-72.2515869),c(18.5549150,18.5603014))
window_haiti2=owin(c(-72.34436,-72.34235),c(18.55314,18.55500))


# sensitivity bias (or recall): the pb that when there is a building the annotator identify it as a building 
# specificity bias: the pb when there is not building the annotator does not identify a building.
generate_data=function(nb,prevalence){  
  ref=sample(c(TRUE,FALSE),nb,rep=TRUE,prob=c(prevalence,1-prevalence))  
  return(ref)
}

generating_difficulty_map=function(ref, data){
  positive_votes_ratio=rowSums(data)/ncol(data)
  agreement=ifelse(ref,positive_votes_ratio,1-positive_votes_ratio)
  return (agreement)
}


generate_annotator=function(ref,sensitivity, specificity, difficulty=NULL){
  
  if (!is.null(difficulty)){
    perception=apply(cbind(ref,difficulty),1,function(x){ if(runif(1)<=x[2]) x[1] else (!x[1]) })
  }else{
    perception=ref
  }
  
  bias=vapply(perception,2,FUN=function(x) ifelse(x,runif(1)<=sensitivity,runif(1)>specificity))
  return(bias)
}

generate_unique_annotators=function(ref, sensitivity, specificity, difficulty=NULL){
  output=c()
  for (i in 1:length(sensitivity)){        
    fake_annotations=generate_annotator(ref,sensitivity[i],specificity[i],difficulty)    
    output=cbind(output,fake_annotations)
  }
  return(output)
}

generate_annotators=function(ref, sensitivity, specificity,times){
  output=c()
  for (i in 1:times){        
    output=cbind(output,generate_annotator(ref,sensitivity,specificity))
  }
  return(output)
}

generate_collective_output=function(annotators,q){
  n=ncol(annotators)
  aggregated_votes=(rowSums(annotators))/n
  #print(aggregated_votes)
  return (ifelse(aggregated_votes>=q,TRUE,FALSE))
}


compute_prevalence=function(ref,min_dist){
  res= resolution(window, min_dist)
  ref.q=quadratcount(ref, nx = res[1], ny = res[2])            
  ref.q=(ifelse(as.vector(ref.q)>=1, TRUE,FALSE))
  print(ref.q)
  real=length(ref.ppp$x)
  digitized=length(which(ref.q==TRUE))
  print(length(ref.q))
  print(digitized)
  print(digitized/length(ref.q))
  cat ("impact of resolution ", res[1],"x",res[2],"of ",min_dist, " km:", (real-digitized)/real*100, "% of points lost\n")
}
