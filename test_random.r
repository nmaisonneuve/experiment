source('parallel_cluster.r')
library(ggplot2)
#volunteers=runif(50) 

repetition=1:1
#sample_sizes=c(10,50,100,1000)
sample_sizes=c(50,100,500)
group_sizes=c(1,2,3,4,10,15,20,25,30)
max_num_groups=100

test5=c()
for (repeation in repetition){
  results=c()
  for (sample_size in sample_sizes){
    
  # generate 'sample_size'  measures via a normal distribution
  individual_output=rnorm(sample_size, mean=0, sd=1)
  
  # for each  k-group 
  for (group_size in group_sizes){
    if (sample_size>=group_size){
    # for a given limit of sample 
          
      # we generate  J groups of k participants with J<min(max_num_groups, c(^k_n))
      groups=generate_combinaisons(n=sample_size, k=group_size,max_num_groups)     
      
      # for each group we compute their average perf 
      result=apply(groups, 1, FUN=function(x) mean(individual_output[x]))
    
      results=rbind(cbind("sample_size"=sample_size,"limit"=max_num_groups,"num_workers"=group_size,"result"=result),results)
      }
    }
 }
  results=as.data.frame(results)
  test4=summarySE(results,measurevar="result", groupvars=c("num_workers","sample_size"))
  test4$result_abs=abs(test4$result)
  test4$sd_abs=abs(test4$sd)
  test5=rbind(test4,test5)
}
#print(p)
#qplot(x=factor(num_workers),y=result, data=test5, geom = c("boxplot"))+ylim(-1,1)+ facet_wrap(~ sample_size)

test1=summarySE(results,measurevar="result", groupvars=c("num_workers","sample_size"))
p=ggplot(test, aes(x=num_workers, y=result))+  ylim(-2,2)+geom_errorbar(aes(ymin=result-sd, ymax=result+sd)) +geom_line() +        geom_point()
print(p)

  #test$result=abs(test$result)
#test$iteration=repeation
#tests=rbind(test,tests)
#p=qplot(x=factor(num_workers),y=result, data=results, geom = c("boxplot"))+ylim(-1,1)

#p=qplot(x=factor(num_workers),y=result, data=test5, geom = c("boxplot"))+ylim(-1,1)+ facet_wrap(~ sample_size)
#print(p)

#p=qplot(x=factor(num_workers),y=sd, data=test5, geom = c("boxplot"))+ylim(-1,1)+ facet_wrap(~ sample_size)
#print(p)


results2=c()
for (i in 1:1000){
for (num_worker in group_sizes){  
    #result=mean(runif(num_worker))  
    result=mean(rnorm(num_worker, mean=0, sd=1))
    results2=rbind(cbind("num_workers"=num_worker,"result"=result),results2)
  }
}

results2=as.data.frame(results2)

test2=summarySE(results2,measurevar="result", groupvars=c("num_workers"))
test2$sample_size=0
test2=rbind(test1,test2)
p=ggplot(test2, aes(x=num_workers, y=result))+  ylim(-2,2)+geom_errorbar(aes(ymin=result-sd, ymax=result+sd)) +geom_line() +        geom_point()+ facet_grid(. ~ sample_size) 
print(p)


 


