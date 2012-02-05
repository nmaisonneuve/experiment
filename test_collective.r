source('algo_grid.r')

###
#Is grouping the best individuals create the best performance?
# if I take the best 2 individuals, what is the rank of the individual 
#

#constants (window of the map != window of the data (buildings not at a corner))
#define window 
window_island=owin(c(34.17449,34.18145),c(-0.39021,-0.38439))
window_haiti=owin(c(-72.2570801,-72.2515869),c(18.5549150,18.5603014))
window_haiti2=owin(c(-72.34436,-72.34235),c(18.55314,18.55500))
windows = list("haiti2/haiti2"=window_haiti2, "haiti/haiti"=window_haiti, "island/island"=window_island)
#window=owin(c(min(data[,1])-0.0001, max(data[,1]))+0.0001,c(min(data[,2])-0.0001, max(data[,2]))+0.0001)

#= 0.0065 lost of 5 % for haiti2
#=0.0086 list for 5.0% for hoiti
#=0.013 list for 4.3% for island 
dists = list("haiti2/haiti2"=0.0065, "haiti/haiti"=0.0085, "island/island"=0.010)

maps=c("haiti2/haiti2","haiti/haiti","island/island")

#input
input_root=maps[3]
#for (input_root in maps){
  print(input_root)
  min_dist=dists[[input_root]]
  window=windows[[input_root]]
  input_volunteer=sprintf("%s_volunteer.csv",input_root)
  input_ref=sprintf("%s_reference.csv",input_root)
  output_file=sprintf("%s_collective_output2_qc.csv",input_root)
  
  ref.ppp<-ppp_read(input_ref)
  data.ppp <- ppp_read(input_volunteer,min=50)


  res= resolution(window, min_dist)
  ref.q=quadratcount(ref.ppp, nx = res[1], ny = res[2])            
  ref=(ifelse(as.vector(ref.q)>=1, TRUE,FALSE))

  density= length(which(ref.q==TRUE))/length(ref.q)

  real=length(ref.ppp$x)
  digitized=length(which(ref.q==TRUE))
  cat ("impact of resolution ", res[1],"x",res[2],"of ",min_dist, " km:", (real-digitized)/real*100, "% of points lost\n")
  data=discretize_all(data.ppp,res)
  
  num_voters=1
  

  groups=generate_combinaisons(n=ncol(data), k=3,-1)    
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
   # print(crowd)
    b=similarity2(ref,crowd,density)
    
   # cat ("compute aggregation of group (", num_workers,"):", workers,"(num voters: ", min_volunteers,")","nb markers:",length(which(aggregation))," r:", b,"\n")  
    #benchmarks=append(benchmarks,b)    
    benchmarks=append(benchmarks,b)
  }
  
  #benchmarks=do.call(rbind, benchmarks)
  #benchmarks=data.frame(matrix(unlist(benchmarks), ncol=4, byrow=T))
  #names(benchmarks)[1:4]=c("precision","recall","fmeasure", "nb_obs")

  benchmarks=data.frame(matrix(unlist(benchmarks), ncol=7, byrow=T))
  names(benchmarks)[1:5]=c("precision","sensitivity","fmeasure", "NPV","specificity")
print(head(benchmarks[with(benchmarks, order(-fmeasure)), ]))
print(head(groups[with(benchmarks, order(-fmeasure)), ]))