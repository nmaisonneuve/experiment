
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

agreement_plot=function(output,measure){
  sum_data<-summarySE(output,measurevar="fmeasure", groupvars=c("ratio"))
  p=ggplot(sum_data, aes(x=ratio, y=fmeasure))+geom_point()+geom_errorbar(aes(ymin=fmeasure-ci, ymax=fmeasure+ci))+scale_colour_gradientn(colour = rainbow(7))+ylim(0,1)
  return(p)
}

plot_all=function(output){
sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","map","model_type"))
names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
sum_fm$type="fmeasure"

sum_recall<-summarySE(output,measurevar="recall", groupvars=c("num_workers","map","model_type"))
names(sum_recall)[names(sum_recall)=="recall"]="measure"
sum_recall$type="recall"

sum_precision<-summarySE(output,measurevar="precision", groupvars=c("num_workers","map","model_type"))
names(sum_precision)[names(sum_precision)=="precision"]="measure"
sum_precision$type="precision"


print(head(sum_fm))
#shape ,
p1=ggplot(sum_fm, aes(x=num_workers, y=ci, linetype=model_type)) + 
      #geom_errorbar(aes(ymin=measure-sd, ymax=measure+sd)) +
      geom_line(size=1) + #geom_point()+
    ylab("") +xlab("")+
      ylim(0,0.2) +facet_grid(. ~map)+theme_bw()+
      scale_colour_hue(name="Algo")+
      opts(aspect.ratio=1,strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
           axis.title.x = theme_text(size=15),
            legend.position="none", 
        legend.direction="vertical", plot.margin = unit(c(0,0,0,0), "lines"))

#sum_fm=rbind(sum_fm, sum_precision,sum_recall)

p2=ggplot(sum_fm, aes(x=num_workers, y=measure, linetype=model_type)) + 
      #geom_errorbar(aes(ymin=measure-sd, ymax=measure+sd)) +
      geom_line(size=1) +#geom_point()+
      ylab("")+ xlab("")+
      ylim(0.3,1) +facet_grid(. ~map)+theme_bw()+
      scale_colour_hue(name="Algo")+
      opts(aspect.ratio=1,strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
           axis.title.x = theme_text(size=15),
        legend.position="none", 
        legend.direction="vertical", plot.margin = unit(c(0,0,0,0), "lines"))
#ggsave("parallel_model1.pdf",p1, width=8,heigh=3.8)
ggsave("parallel_iterative_model1.pdf",p1, width=8,height=3, dpi=20)
ggsave("parallel_iterative_model2.pdf",p2,width=8,height=3, dpi=20)


# p1=ggplot(sum_fm, aes(x=num_workers, y=measure, colour=map, group=map)) + 
#       #geom_errorbar(aes(ymin=measure-ci, ymax=measure+ci)) +
#       geom_line() +        geom_point()+
#       ylab("")+ xlab("number of volunteers")+
#       ylim(0,1) +facet_grid(. ~type)+theme_bw()+
#       scale_colour_hue(name="Map")+
#       opts(strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
#            axis.title.x = theme_text(size=15),
#             legend.position="right", 
#         legend.direction="vertical", plot.margin = unit(c(0,0,0,-1), "lines"))
# 
return (multiplot(p2, p1, cols=2))
#return (p1)
}


plot2_all=function(output){
  sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","algo","map"))
  names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
#sum_fm$type="fmeasure"

print(head(sum_fm))
#shape ,
p1=ggplot(sum_fm, aes(x=num_workers, y=measure, linetype=algo, group=algo)) + 
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
serial$type=NULL
serial$model_type="basic iterative"
return (serial)
}

compute_map=function(map, map_label){

  input_root=sprintf("%s/%s",map,map)
  
  input_dca=sprintf("%s_collective_output3_dca.csv",input_root)
  output_dca <- read.csv(input_dca, encoding = "UTF-8")
  output_dca=best_voters2(output_dca)
  output=output_dca
  
  
  #names(output)[7]=c("min_dist")
  #output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))
  #output$fmeasure[is.na(output$fmeasure)]=0
  output=output[output$num_workers<=10,]
  output$precision[output$precision==0]=1
  output$map=map_label

return (output)
}

iterative=compute_iterative_map()

output1=compute_map("island","map1")
output2=compute_map("haiti","map2")
output3=compute_map("haiti2","map3")
output=rbind(output1,output2,output3)
output$model_type="DCA"

parallel=data.frame(precision=output$precision, recall=output$recall,fmeasure=output$fmeasure,num_annotations=output$num_annotations, num_workers=output$num_workers, model_type=output$model_type, map=output$map)
print(str(parallel))
print(str(iterative))
final=rbind(iterative,parallel)
p1=plot_all(final)
print(p1)
