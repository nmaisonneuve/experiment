source('input_data.r')
source('plot_map.r')
source('parallel_cluster.r')

individual_plot=function(data){
data=data[order(-data$fmeasure),]
data$workerID=seq(1:nrow(data))
qplot(factor(workerID), fmeasure, data=as.data.frame(data) , fill=fmeasure)+geom_linerange(aes(ymin = 0,ymax = fmeasure, fill=fmeasure)) + ylim(0,1)+snug.ops
d2=qplot(factor(workerID), precision, data=as.data.frame(data) , fill=precision)+geom_bar()+ opts(axis.ticks = theme_blank(),axis.text.x = theme_blank(),legend.position = "none")+xlab("")+ylim(0,1)
d3=qplot(factor(workerID), recall, data=as.data.frame(data) , fill=recall)+geom_bar()+ opts(legend.position = "none")+ylim(0,1)+xlab("Participant Idx")
}

individual_perf=function(data,ref,min_dist=0.007){
  result=c()
  for( worker in unique(data$workerID)){
  contrib=data[data$workerID==worker,]
  re=compute_accuracy_fast(contrib,ref,min_dist)
  result=rbind(result,c(re,workerID=worker))
  }
  result=as.data.frame(result)  
  names(result)<-c("precision","recall","fmeasure","nb_obs","workerID")
  return (result)
}

individual_average_perf=function (data,ref, min_dist){
  result=c()
  ptm <- proc.time()
  for( worker in unique(data$workerID)){
  contrib=data[data$workerID==worker,]
  re=compute_accuracy_fast(contrib,ref,min_dist)
  result=rbind(result,c(re,worker))
  }
  result=as.data.frame(result)
  names(result)=c("precision","recall","fmeasure","nb_obs","workerID")
  result$map=1
  sum_fm<-summarySE(result,measurevar="fmeasure", groupvars=c("map"))
  names(sum_fm)[3]="measure"
  sum_fm$type="fmeasure"
  
  sum_pr<-summarySE(result,measurevar="precision", groupvars=c("map"))
  names(sum_pr)[3]="measure"
  sum_pr$type="precision"
  
  sum_re<-summarySE(result,measurevar="recall", groupvars=c("map"))
  names(sum_re)[3]="measure"
  sum_re$type="recall"
  
  print(sum_fm)
  
  return(rbind(sum_pr,sum_re,sum_fm))
}

maps=c("haiti2","haiti","island")
map_label=c("map3","map2","map1")

result=c() 
i=1
  for (map in maps){
    input_volunteer=sprintf("%s/%s_volunteer.csv",map,map)
    input_ref=sprintf("%s/%s_reference.csv",map,map)
    data=read_input(input_volunteer,50, 0)
    ref=read_input(input_ref,0, 0)
    result_map=individual_average_perf(data,ref,0.007)
    result_map$map=map_label[i]
    result=rbind(result,result_map)
    i=i+1
  }

p=ggplot(result, aes(y=measure,x=map, fill=map))+
  geom_bar(position=position_dodge(),  size=.3)+
  geom_errorbar(aes(ymin=measure-sd, ymax=measure+sd),size=.3,  width=.2,  position=position_dodge(.9))+
  scale_y_continuous(breaks=seq(0,1,0.2)) + theme_bw()+ facet_wrap(~ type, ncol=3)+ ylab("")+ xlab("") +
  opts(axis.title.x = theme_text(size = 12, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 12), axis.text.y = theme_text(size = 12), strip.background = theme_rect(colour='white', fill='white'),strip.text.x = theme_text(size=16),legend.position='none',plot.margin = unit(c(0,0,-1,-1), "lines"))

print(p)
#ggsave("individual_perf_color.pdf",plot=p,width=5*1.1, height=2*1.1)
