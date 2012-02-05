source('plot_map.r')

library(gridExtra)


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
plot_all=function(output){
sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","map"))
names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
sum_fm$type="fmeasure"

sum_recall<-summarySE(output,measurevar="recall", groupvars=c("num_workers","map"))
names(sum_recall)[names(sum_recall)=="recall"]="measure"
sum_recall$type="recall"

sum_precision<-summarySE(output,measurevar="precision", groupvars=c("num_workers","map"))
names(sum_precision)[names(sum_precision)=="precision"]="measure"
sum_precision$type="precision"

sum_fm=rbind(sum_fm, sum_precision,sum_recall)
print(head(sum_fm))
#shape ,
p1=ggplot(sum_fm, aes(x=num_workers, y=measure, colour=map, group=map)) + 
      #geom_errorbar(aes(ymin=measure-ci, ymax=measure+ci)) +
      geom_line() +        geom_point()+
      ylab("")+ xlab("number of volunteers")+
      ylim(0,1) +facet_grid(. ~type)+theme_bw()+
      scale_colour_hue(name="Map")+
      opts(strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
           axis.title.x = theme_text(size=15),
            legend.position="right", 
        legend.direction="vertical", plot.margin = unit(c(0,0,0,-1), "lines"))
return (p1)
}


accuracy_plots=function(serial){
  s0=ggplot(serial,aes(factor(num_workers),precision))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
  s0=s0+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
  s1=ggplot(serial,aes(factor(num_workers),recall))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
  s1=s1+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
  s2=ggplot(serial,aes(factor(num_workers),fmeasure))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
  #s2=s2+opts(axis.title.x = theme_text(size = 14, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
  s2=s2+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
  #ggsave(s, filename=output_image_filename)
  grid.arrange(s0,s1,s2,ncol=1)
}


accuracy_plots=function(serial){
  
  s2=ggplot(serial,aes(factor(num_workers),fmeasure))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
  s2=s2+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
  
}

#input
input_root="island/island"
input_serial_filename=sprintf("%s_serial_volunteer_result_0.007.csv",input_root)
serial1=input_serial(input_serial_filename)
serial1$map<-"Map 1"

input_root="haiti/haiti"
input_serial_filename=sprintf("%s_serial_volunteer_result_0.007.csv",input_root)
serial2=input_serial(input_serial_filename)
serial2$map<-"Map 2"


input_root="haiti2/haiti2"
input_serial_filename=sprintf("%s_serial_volunteer_result_0.007.csv",input_root)
serial3=input_serial(input_serial_filename)
serial3$map<-"Map 3"

serial=rbind(serial1,serial2,serial3)
print(serial[1:10,])

plot=plot_all(serial)
print(plot)
ggsave("iterative_model.pdf",plot)

s1=ggplot(serial,aes(factor(num_workers),recall))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s1=s1+ blank+theme_bw()+xlab("nb of participants")
#opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14))
s2=ggplot(serial,aes(factor(num_workers),precision))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s2=s2+ blank+theme_bw()+xlab("nb of participants")
#opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14))

s3=ggplot(serial,aes(factor(num_workers),fmeasure))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s3=s3+ blank+theme_bw()+
  #opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14))
xlab("nb of participants")

#grid.arrange(s1,s2,s3,ncol=1)
#grid.arrange(s1,s2,ncol=1) 

#result=rbind(parallel,serial)
#print(result$type)
#s=ggplot(result_parallel,aes(factor(num_workers),fmeasure))+ geom_boxplot(aes(fill=factor(type)),outlier.colour="NA")+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
