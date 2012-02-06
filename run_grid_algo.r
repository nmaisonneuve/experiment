source('input_data.r')
source('algo_grid.r')



la_totale=function (data.ppp,ref.ppp,min_dist){
  
  max_workers=length(unique(data.ppp$marks))
  #min_dists=c(0.01,0.012,0.013)  
#  min_dists=c(0.009,0.013,0.02) 
  num_workers=c(1,2,3,4,5,6,7,8,9,10,15,20,25,30)
  #num_workers=c(25,30)
  #num_workers=c()  
  output=c()
    
  #for (min_dist in min_dists){    
  #compute resolution and result according to min dist
    res= resolution(window, min_dist)
    ref.q=quadratcount(ref.ppp, nx = res[1], ny = res[2])            
    ref.q=(ifelse(as.vector(ref.q)>=1, TRUE,FALSE))
    
    real=length(ref.ppp$x)
    digitized=length(which(ref.q==TRUE))
    cat ("impact of resolution ", res[1],"x",res[2],"of ",min_dist, " km:", (real-digitized)/real*100, "% of points lost\n")
    data.q=discretize_all(data.ppp,res)
    
    for (num_worker in num_workers){
      
    num_voters=compute_agreement_vector(num_worker)
    #num_voter=ceiling(num_worker*0.3)
    
     for (num_voter in num_voters){      
        cat("compute accuracy for",num_worker,"workers with ",num_voter,"voters and min dist of",min_dist,"\n")
        r=compute_accuracy(data.q,ref.q,num_worker,num_voter)        
        output=rbind(r,output)      
     }
   }    
  #}
  return (output)
}


#input

for (input_root in maps){
  print(input_root)
  min_dist=dists[[input_root]]
  window=windows[[input_root]]
  input_volunteer=sprintf("%s_volunteer2.csv",input_root)
  input_ref=sprintf("%s_reference.csv",input_root)
  output_file=sprintf("%s_collective_output3_qc.csv",input_root)
  
  ref.ppp<-ppp_read(input_ref)
  data.ppp <- ppp_read(input_volunteer,min=50)
  
  output=la_totale(data.ppp,ref.ppp,min_dist)
  
  output=as.data.frame(output)
  output$ratio=output$num_voters/output$num_workers
  output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))
  
  write.csv(output, file=output_file)
}