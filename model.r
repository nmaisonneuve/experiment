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

generate_annotator=function(ref,sensitivity, specificity){
  #bias=vapply(ref,2,FUN=function(x) ifelse(x,runif(1)<=sensitivity,runif(1)<=specificity))  
  bias=vapply(ref,2,FUN=function(x) ifelse(x,runif(1)<=sensitivity,runif(1)>specificity))
  return(bias)
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
#input
input_root="haiti2/haiti2"
window=window_haiti2 #define window
input_ref=sprintf("%s_reference.csv",input_root)
output_file=sprintf("%s_collective_output_qc_virtual.csv",input_root)

#map 1 prevalence =
#

#map3(haiti2)
# min_dist= 0.065  for 5% lost 
# prevalence = 0.16% of building


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


  ref.ppp<-ppp_read(input_ref)
  max_workers=50 
  #min_dists=c(0.01,0.012,0.013)  
  min_dists=c(0.007)
  #num_workers=c(1,2,3,4,6,8,10,15,20,25,30)
#num_workers=c(25,30)
  num_workers=c(30)  
  specificity=0.9
  sensitivity=0.6
   
  benchmark=c()
  errors=seq(0.1,0.9,0.2)
  
  for (min_dist in min_dists){
    
    #compute resolution and result according to min dist
    res= resolution(window, min_dist)
    ref.q=quadratcount(ref.ppp, nx = res[1], ny = res[2])            
    ref.q=(ifelse(as.vector(ref.q)>=1, TRUE,FALSE))
    
    real=length(ref.ppp$x)
    digitized=length(which(ref.q==TRUE))
    cat ("impact of resolution ", res[1],"x",res[2],"of ",min_dist, " km:", (real-digitized)/real*100, "% of points lost\n")
    #for (sensitivity in errors){
      #for (specificity in errors){
    data.q=generate_annotators(ref.q,sensitivity,specificity,max_workers)    
    for (num_worker in num_workers){      
          num_voters=compute_agreement_vector(num_worker)    
          print(num_voters)
           for (num_voter in num_voters){      
              cat("error profil (",sensitivity, specificity,") compute accuracy for",num_worker,"workers with ",num_voter,"voters and min dist of",min_dist,"\n")
              r=compute_accuracy(data.q,ref.q,num_worker,num_voter)              
              benchmark=rbind(r,benchmark)      
           }
    }
     #   }
    #  }  
  }
output=benchmark
output$ratio=output$num_voters/output$num_workers
#output=do.call(rbind,benchmark)
#output=as.data.frame(output)