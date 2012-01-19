source('plot_result.r')
library(gridExtra)


input_serial=function(input_filename){
  serial <- as.data.frame(read.csv(input_serial_filename, , encoding = "UTF-8"))
  serial$X<-NULL
  serial$instance<-NULL
  serial$num_workers<-serial$iteration
  serial$iteration<-NULL
  serial$type<-'serial'
  #print(head(serial))
  return(serial)
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

s1=ggplot(serial,aes(factor(num_workers),recall))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s1=s1+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")

s2=ggplot(serial,aes(factor(num_workers),precision))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s2=s2+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")


#s3=ggplot(serial,aes(factor(num_workers),fmeasure))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
#s3=s3+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
grid.arrange(s1,s2,ncol=1) 
#grid.arrange(s1,s2,s3,ncol=1)
#result=rbind(parallel,serial)
#print(result$type)
#s=ggplot(result_parallel,aes(factor(num_workers),fmeasure))+ geom_boxplot(aes(fill=factor(type)),outlier.colour="NA")+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
