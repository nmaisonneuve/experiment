setwd("C:/dev/parallel/R")
source('algo_grid.r')
library(ggplot2)
source('plot_map.r')
#constants (window of the map != window of the data (buildings not at a corner))
window_island=owin(c(34.17449,34.18145),c(-0.39021,-0.38439))
window_haiti=owin(c(-72.2570801,-72.2515869),c(18.5549150,18.5603014))
window_haiti2=owin(c(-72.34436,-72.34235),c(18.55314,18.55500))

# sensitivity bias (or recall): the pb that when there is a building the annotator identify it as a building 
# specificity bias: the pb when there is not building the annotator does not identify a building.

#
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

#output$ratio=output$num_voters/output$num_workers
#output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))

sensitivity=0.5
specificity=0.6
density=0.1

precision =sensitivity*density/(sensitivity*density+ (1-specificity)*(1- density))

fm=2*(sensitivity*density)/(density*(sensitivity+specificity) - specificity +1)

cat("fmeasure=",fm,"precision=", precision, "recall=",sensitivity, "density=",density,"\n")

ref=generate_data(500,density)
annotators=generate_annotators(ref,sensitivity,specificity,500)

test=colSums(annotators)/nrow(annotators)
print(test)

result=c()

for (q in seq(0.1,1,0.01)){
  output=generate_collective_output(annotators,q)
  result=rbind(c(similarity2(ref,output, density),q=q),result)
}
result=as.data.frame(result)
names(result)=c("precision","recall","fmeasure","NPV","specificity","computed_p","computed_f","q")

p=ggplot(result)+geom_line(aes(y=recall,x=q,colour="red"))+geom_line(aes(y=fmeasure,x=q))+geom_line(aes(y=precision,x=q,colour="blue"))
roc=as.data.frame(cbind("sensitivity"=result$recall,"FPR"=1-result$specificity,"q"=result$q))
p2=ggplot(roc,aes(y=sensitivity, x=FPR))+geom_point()
multiplot(p,p2, cols=1)

error=1-((result$recall)*(1-density)+ result$specificity*density)
test=as.data.frame(cbind("fmeasure"=result$fmeasure,error=error, "q"=result$q))
print(test)
print(test[which.min(test$error),])