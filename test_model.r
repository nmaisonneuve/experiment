source('model.r')

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
dists = list("haiti2/haiti2"=0.0060, "haiti/haiti"=0.007, "island/island"=0.010)

maps=c("haiti2/haiti2","haiti/haiti","island/island")

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
  
  print("generating unique annotators")
    
  data=discretize_all(data.ppp,res)
  
  difficulty=generating_difficulty_map(ref,data)
  #print(difficulty)
  
  fake_data1=generate_unique_annotators(ref,perf$sensitivity,perf$specificity)
  fake_data2=generate_unique_annotators(ref,perf$sensitivity,perf$specificity,difficulty)

  benchmarks=c()
  #benchmarks2=list()
  
  #benchmarks=list(nrow(groups))   
 # num_workers=c(1,2,3,4,5,7,10,15,20,25,30)
  num_workers=c(5,20)
    

for (num_worker in num_workers){
  groups=generate_combinaisons(n=ncol(data), k=num_worker,200)     
  num_voters=ceiling(0.30*num_worker)
  for (i in 1:nrow(groups)){
    
    crowd=majority_vote(groups[i,], data,num_voters)
    fake_crowd1=majority_vote(groups[i,], fake_data1,num_voters)
    fake_crowd2=majority_vote(groups[i,], fake_data2,num_voters)
    #print(crowd)
    #crowd=uncertain_vote2(groups[i,], data,num_voters/20)
    #cat(length(which(crowd2==FALSE)), length(which(crowd2==TRUE)),length(which(crowd==FALSE)), length(which(crowd==TRUE)),"\n")
    
   # print(crowd)
    data1=cbind(similarity2(ref,crowd,density),"type"=1,"num_workers"=num_worker)
    data2=cbind(similarity2(ref,fake_crowd1,density),"type"=2,"num_workers"=num_worker)
    data3=cbind(similarity2(ref,fake_crowd2,density),"type"=3,"num_workers"=num_worker)
    
    data4=rbind(data1,data2,data3)
    benchmarks=rbind(benchmarks,data4)    
    
    #b2=similarity2(ref,crowd2,density)    
    #cat ("compute aggregation of group (", num_workers,"):", workers,"(num voters: ", min_volunteers,")","nb markers:",length(which(aggregation))," r:", b,"\n")  
    #benchmarks=append(benchmarks,b)    
    
    #benchmarks2=append(benchmarks2,b2)
  }
}
  #benchmarks=do.call(rbind, benchmarks)
  #benchmarks=data.frame(matrix(unlist(benchmarks), ncol=4, byrow=T))
  #names(benchmarks)[1:4]=c("precision","recall","fmeasure", "nb_obs")

  #benchmarks=data.frame(matrix(unlist(benchmarks), ncol=7, byrow=T))
  
  #names(benchmarks)=c("precision","sensitivity","fmeasure", "NPV","specificity","type","num_workers")

entropy=function(p) {
  h=sum(ifelse(p>0, p*log(p), 0.0))  
  return(-h)
}
                

#print(head(benchmarks[with(benchmarks, order(-fmeasure)), ]))

  
#  benchmarks2=data.frame(matrix(unlist(benchmarks2), ncol=7, byrow=T))
#  names(benchmarks2)[1:5]=c("precision","sensitivity","fmeasure", "NPV","specificity")
#  print(head(benchmarks2[with(benchmarks2, order(-fmeasure)), ]))
#print(head(groups[with(benchmarks, order(-fmeasure)), ]))