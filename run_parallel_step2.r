library(xtable)
library(ggplot2)
library(gridExtra)
source('plot_result.r')

#input
input_root="island/island"
input_filename=sprintf("%s_collective_output_dca.csv",input_root)
output_filename=sprintf("%s_parallel_best_output_dca.csv",input_root)
plot_result=sprintf("%s_parallel_best_output.png",input_root)

#summary=function(data){  s=by(output[3:6], output$num_workers, mean)}

best_parameters=function(data){
 output.mean=aggregate(. ~ num_voters +dist_cluster+num_workers, data = data, mean)
  #output.mean=aggregate(. ~ num_voters +dist_cluster+num_workers, data = data, sd)
  #print(output.mean)
 num_workers=as.data.frame(output.mean$num_workers)
 fmeasure=as.data.frame(output.mean$fmeasure)
 sel <- ave(fmeasure, num_workers, FUN = max) == fmeasure
 return(output.mean[sel,])
}

best_results=function(best_params,data){
  output=c()
  for (i in 1:nrow(best_params)) {    
    #print(best_params[i,])
    #print(best_params[i,]$num_workers)
    tmp=subset(data, data$num_workers==best_params[i,]$num_workers & data$dist_cluster==best_params[i,]$dist_cluster & data$num_voters==best_params[i,]$num_voters)    
    output=rbind(output,tmp)
  }
  return(output)
}  


       
# Experiment1  data
output <- read.csv(input_filename, , encoding = "UTF-8")
best=best_parameters(output)
best=best[-c(1:2),]
print(best)
#cbind(best[1:6,3],best[1:6,1:2],best[1:6,4:7])
#write.csv(best, 'best_parameter.csv')
#output.mean=aggregate(. ~ num_voters +num_workers, data = output, mean)
#output.std=aggregate(. ~ num_voters +num_workers, data = output, sd)
#output.std=aggregate(output.mean,by =c("num_voters","num_workers","dist_cluster"), FUN= output, sd)
#output.mean=aggregate(. ~ num_voters +num_workers, data = output.mean, max)

##FOR LATEX ##
best$X<-NULL
best$dist<-NULL
latex=xtable(cbind(best[1:7,3],best[1:7,1],best[1:7,4:7]))



  ex=output[output$num_worker==8,]
  s1=ggplot(ex,aes(factor(num_voters),precision))+  
    geom_boxplot() + ylab("Precision")+ xlab("q - # Voters") +  
    opts(axis.title.x = theme_text(size = 14, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) + 
    ylim(0,1)

  s2=ggplot(ex,aes(factor(num_voters),recall))+  
    geom_boxplot() + ylab("Recall")+ xlab("q - # Voters") +  
    opts(axis.title.x = theme_text(size = 14, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) + 
    ylim(0,1)

  s=ggplot(ex,aes(factor(num_voters),fmeasure))+  
    geom_boxplot() + ylab("F measure")+ xlab("q - # Voters") +  
    opts(axis.title.x = theme_text(size = 14, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) + 
    ylim(0,1)

  grid.arrange(s1,s2,s,nrow=1)
## ONLY RESULTS WITH BEST PARAMETERS
results=best_results(best,output)
#save data 
write.csv(results,output_filename)
# save  plot/image
s4=plot_best_accuracy(results)
#ggsave(s4, filename=plot_result)

