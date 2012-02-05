source('algo_grid.r')

###
#Is grouping the best individuals create the best performance?
# if I take the best 2 individuals, what is the rank of the individual 
#

#constants (window of the map != window of the data (buildings not at a corner))
#define window 
#window=owin(c(min(data[,1])-0.0001, max(data[,1]))+0.0001,c(min(data[,2])-0.0001, max(data[,2]))+0.0001)

#= 0.0065 lost of 5 % for haiti2
#=0.0086 list for 5.0% for hoiti
#=0.013 list for 4.3% for island 


#input
input_root=maps[3]
#for (input_root in maps){
  print(input_root)
  min_dist=dists[[input_root]]
  window=windows[[input_root]]
  input_volunteer=sprintf("%s_volunteer.csv",input_root)
  input_ref=sprintf("%s_reference.csv",input_root)
  input_perf=sprintf("%s_individual_perf.csv",input_root)

  #individual performance 
  perf=as.data.frame(read.csv(input_perf))

  #output_file=sprintf("%s_collective_output2_qc.csv",input_root)
  
  ref.ppp<-ppp_read(input_ref)
  data.ppp <- ppp_read(input_volunteer,min=50)

  #building grid gold standard
  res= resolution(window, min_dist)
  ref.q=quadratcount(ref.ppp, nx = res[1], ny = res[2])            
  ref=(ifelse(as.vector(ref.q)>=1, TRUE,FALSE))
  
  # lost of precision 
  real=length(ref.ppp$x)
  digitized=length(which(ref.q==TRUE))
  cat ("impact of resolution ", res[1],"x",res[2],"of ",min_dist, " km:", (real-digitized)/real*100, "% of points lost\n")

  # density 
  density= digitized/length(ref.q)

  data=discretize_all(data.ppp,res)
    
  groups=generate_combinaisons(n=ncol(data), k=20,200)    
  
#benchmarks=list(nrow(groups))   
  benchmarks=list()
  benchmarks2=list()
num_voters=5
for (i in 1:nrow(groups)){
    
    crowd=majority_vote(groups[i,], data,num_voters)    
    crowd2=uncertain_vote2(groups[i,], data,density)
    #cat(length(which(crowd2==FALSE)), length(which(crowd2==TRUE)),length(which(crowd==FALSE)), length(which(crowd==TRUE)),"\n")
    
   # print(crowd)
    b=similarity2(ref,crowd,density)    
    b2=similarity2(ref,crowd2,density)
    
   # cat ("compute aggregation of group (", num_workers,"):", workers,"(num voters: ", min_volunteers,")","nb markers:",length(which(aggregation))," r:", b,"\n")  
    #benchmarks=append(benchmarks,b)     
    benchmarks=append(benchmarks,b)
    benchmarks2=append(benchmarks2,b2)
  }
  #}
  #benchmarks=do.call(rbind, benchmarks)
  #benchmarks=data.frame(matrix(unlist(benchmarks), ncol=4, byrow=T))
  #names(benchmarks)[1:4]=c("precision","recall","fmeasure", "nb_obs")

  benchmarks=data.frame(matrix(unlist(benchmarks), ncol=5, byrow=T))
  names(benchmarks)[1:5]=c("precision","sensitivity","fmeasure", "NPV","specificity")
  print(head(benchmarks[with(benchmarks, order(-fmeasure)), ]))
  #test=summarySE(benchmarks,measurevar="fmeasure", groupvars=c("X8"))

  benchmarks2=data.frame(matrix(unlist(benchmarks2), ncol=5, byrow=T))
  names(benchmarks2)[1:5]=c("precision","sensitivity","fmeasure", "NPV","specificity")
  print(head(benchmarks2[with(benchmarks2, order(-fmeasure)), ]))
#print(head(groups[with(benchmarks, order(-fmeasure)), ]))