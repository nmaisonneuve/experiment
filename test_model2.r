
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