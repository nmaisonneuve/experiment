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
  data=data[1:50,1:10]
  dd=as.data.frame(as.matrix(data))
  dd$id=1:nrow(data)
  d1=melt(dd, id=c("id"))
  d1$value=ifelse(d1$value,"porn","notporn")
d1=cbind(d1$variable, d1$id,d1$value)
write.table(d1, file = "foo.csv", sep = "\t",quote=FALSE,row.names=FALSE, qmethod = "double")

 