
source('input_data.r')
source('parallel_cluster.r')

#summary=function(data){  s=by(output[3:6], output$num_workers, mean)}

input_serial=function(input_filename){
  serial <- as.data.frame(read.csv(input_filename, , encoding = "UTF-8"))
  serial$X<-NULL
  serial$instance<-NULL
  serial$num_workers<-serial$iteration
  serial$iteration<-NULL
  serial$type<-'serial'
  #print(head(serial))
  return(serial)
}

best_voters=function(data){
  data_d=cbind(data$num_workers,data$num_voters)
  num_workers=unique(data_d[,1])
  best_voters=vapply(num_workers, 1, FUN=function(x){ 
    idx=which(data_d[,1]==x)
    idx2=which.min(abs(data_d[idx,2]-0.25*x))
    return(data_d[idx,2][idx2])})
  
  best_parameters=cbind(num_workers,best_voters)
  idx=apply(data_d,1,FUN=function(x){return(length(which(best_parameters[,1] == x[1] & best_parameters[,2] == x[2]))>0)})
  return(data[idx,])
}

agreement_plot=function(output,measure){
  sum_data<-summarySE(output,measurevar="fmeasure", groupvars=c("ratio"))
  p=ggplot(sum_data, aes(x=ratio, y=fmeasure))+geom_point()+geom_errorbar(aes(ymin=fmeasure-ci, ymax=fmeasure+ci))+scale_colour_gradientn(colour = rainbow(7))+ylim(0,1)
  return(p)
}

plot_all=function(output){
sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","algo","map"))
names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
#sum_fm$type="fmeasure"

print(head(sum_fm))
#shape ,
p1=ggplot(sum_fm, aes(x=num_workers, y=measure, colour=algo, group=algo)) + 
      geom_errorbar(aes(ymin=measure-ci, ymax=measure+ci)) +
      geom_line() +        geom_point()+
      ylab("f-measure")+ xlab("number of volunteers")+
      ylim(0,1) +facet_grid(. ~map)+theme_bw()+
      scale_colour_hue(name="Algo")+
      opts(strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
           axis.title.x = theme_text(size=15),
            legend.position="right", 
        legend.direction="vertical", plot.margin = unit(c(0,0,0,-1), "lines"))
return (p1)
}

compute_iterative_map=function(){
  #input
input_root="island/island"
input_serial_filename=sprintf("%s_serial_volunteer_result_0.007.csv",input_root)
serial1=input_serial(input_serial_filename)
serial1$map<-"map1"

input_root="haiti/haiti"
input_serial_filename=sprintf("%s_serial_volunteer_result_0.007.csv",input_root)
serial2=input_serial(input_serial_filename)
serial2$map<-"map2"


input_root="haiti2/haiti2"
input_serial_filename=sprintf("%s_serial_volunteer_result_0.007.csv",input_root)
serial3=input_serial(input_serial_filename)
serial3$map<-"map3"

serial=rbind(serial1,serial2,serial3)
return (serial)
}

compute_map=function(map, min_dist=0.007, map_label){

input_root=sprintf("%s/%s",map,map)

input_dca=sprintf("%s_collective_output3_dca.csv",input_root)
output_dca <- read.csv(input_dca, encoding = "UTF-8")
output_dca=best_voters(output_dca)
output=output_dca
output$ratio=output$num_voters/output$num_workers

#names(output)[7]=c("min_dist")
#output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))
#output$fmeasure[is.na(output$fmeasure)]=0
output=output[output$num_workers<=20,]
output$precision[output$precision==0]=1
output$map=map_label

return (output)
}

iterative=compute_iterative_map()

output1=compute_map("island",c(0.007,0.007,0.010),"map1")
output2=compute_map("haiti",c(0.007,0.007,0.007),"map2")
output3=compute_map("haiti2",c(0.007,0.007,0.007),"map3")
output=rbind(output1,output2,output3)
output$type="DCA"

parallel=data.frame(precision=output$precision, recall=output$recall,fmeasure=output$fmeasure,num_annotations=output$num_annotations, num_workers=output$num_workers, type=output$type, map=output$map)

final=rbind(iterative,parallel)

sum_fm<-summarySE(final,measurevar="fmeasure", na.rm=TRUE, groupvars=c("num_workers","type","map"))
sum_recall<-summarySE(final,measurevar="recall", na.rm=TRUE, groupvars=c("num_workers","type","map"))
sum_precision<-summarySE(final,measurevar="precision", na.rm=TRUE, groupvars=c("num_workers","type","map"))

names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
sum_fm$measure_type="fmeasure"

names(sum_recall)[names(sum_recall)=="recall"]="measure"
sum_recall$measure_type="recall"

names(sum_precision)[names(sum_precision)=="precision"]="measure"
sum_precision$measure_type="precision"

sum=rbind(sum_fm,sum_recall,sum_precision)
sum=sum_fm
sum=sum[-which(sum$num_workers %in% c(5,7,9,10,15,20)),]
pd <- position_dodge(.1)

qplot(num_workers, measure, data = sum, fill=type, geom = c("boxplot"), group = round_any(num_workers, 1, floor))+ ylim(0.0,1)+theme_bw()+facet_grid(measure_type ~ (map+type)

p1=ggplot(sum, aes(x=num_workers, y=measure, colour=type)) +       
    #geom_line(position=pd) +        geom_point(position=pd)+
    #geom_point()+  
    geom_bar(position=position_dodge())+
        #geom_errorbar(aes(ymin=measure-ci, ymax=measure+ci))+
     # geom_bar(position=position_dodge())+
      #geom_pointrange(aes(ymin=measure-sd, ymax=measure+sd),width=.1, position=pd) +
      ylab("")+ xlab("number of volunteers")+ylim(0.0,1)+theme_bw()+      opts(strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),           axis.title.x = theme_text(size=15),            legend.position="right",         legend.direction="vertical", plot.margin = unit(c(0,0,0,-1), "lines"))+facet_grid(measure_type ~ map) 

    #  scale_colour_hue(name="Map")+

print(p1)
#names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
#ggsave("iterative_parallel.pdf",p1)
#ggsave("C:\\Users\\Nicolas\\Documents\\papers\\crowdsourcing satellite imagery\\iterative_parallel.pdf",p1)