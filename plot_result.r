# projection of points in the 
# precision vs recall for each number of workers
plot_result<-function(output){
  qplot(precision, recall, data=output, alpha = 1/20, colour = dist, facets=num_workers ~ num_voters) + xlim(0,1) +ylim(0,1)
  #qplot(precision, recall, data=output, xlab="wrongly identified(FP)", ylab="missed (FN)", colour = dist, facets=num_workers ~ num_voters) + xlim(0,1) +ylim(0,1)
#qplot(num_voters, num_workers, data=output, xlab="wrongly identified(FP)", ylab="missed (FN)", colour=fmeasure) 
#ggsave(s, filename="error_numworker_numvoters_experiment1.png", height=18,width=24,dpi=100)

}

plot_result2<-function(output){
ggplot(data=output) +
geom_density( alpha=1.0,aes(x = fmeasure,  colour="accuracy")) +
geom_density( alpha=1.0,aes(x = num_workers, colour="num_workers" ))+
xlim(0, 1)+ opts(aspect.ratio = 1) + 
#facet_grid(.~c(accuracy,matched),space="free",scales="free_x")
facet_wrap (~ num_workers, scales="free")
}

plot_best_accuracy=function(results){
  s=ggplot(results,aes(factor(num_workers),fmeasure))+  
    geom_boxplot() + ylab("F measure")+ xlab("# Volunteers") +  
    opts(axis.title.x = theme_text(size = 14, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) + 
    ylim(0,1)
  return(s)
}
# right plot for a given parameter
plot_accuracy=function(output){
  ggplot(output,aes(factor(num_voters),fmeasure)) + 
    geom_boxplot(aes(fill=factor(exp)),outlier.colour = "gray", outlier.size = 0.1) + 
    facet_wrap(~num_workers, ncol=5) + ylim(0,1) + 
    ylab("Fmeasure")+ xlab("Cluster Support") +opts(title="Number of participants", aspect.ratio = 1)
  
}
# for differents distance 
plot_result3=function(output){
  d1=ggplot(output,aes(factor(dist),fmeasure)) + geom_boxplot() +ylim(0,1)
d2=ggplot(output,aes(factor(dist),precision)) + geom_boxplot()+ylim(0,1) 
d3=ggplot(output,aes(factor(dist),recall)) + geom_boxplot() +ylim(0,1)
 grid.arrange(d1,d2,d3, ncol=3)  
}
#pg <- ggplot(data=test_ouput) + geom_density(aes(y=..scaled.., x=accuracy, 
#ylab="test")) + geom_density(aes(y=..scaled.., x=matched, color="red"))
#+ geom_density(aes(y=..scaled.., x=omission, color="blue")) + geom_density(aes(y=..scaled.., x=false.positive, color="yellow")) + facet_wrap (~ num_voters, ncol = 5)
