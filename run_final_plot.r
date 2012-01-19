source('plot_result.r')
library(gridExtra)

input_parallel=function(input_filename){
  parallel <- as.data.frame(read.csv(input_parallel_filename, , encoding = "UTF-8"))
  parallel$X<-NULL
  parallel$X.1<-NULL
  parallel$dist<-NULL
  parallel$dist_cluster<-NULL
  parallel$num_voters<-NULL
  parallel$type<-'parallel'
  parallel=parallel[parallel$num_workers %in% c(1:20),]
  #print(head(parallel))
  return (parallel)
}

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


#input
input_root="haiti2/haiti2"
input_parallel_filename=sprintf("%s_parallel_best_output.csv",input_root)
input_serial_filename=sprintf("%s_serial_volunteer_result_0.01.csv",input_root)
haiti2_parallel=input_parallel(input_parallel_filename)
haiti2_serial=input_serial(input_serial_filename)
haiti2_parallel$map<-"Map 3"
haiti2_serial$map<-"Map 3"


input_root="haiti/haiti"
input_parallel_filename=sprintf("%s_parallel_best_output.csv",input_root)
input_serial_filename=sprintf("%s_serial_volunteer_result_0.01.csv",input_root)
haiti_parallel=input_parallel(input_parallel_filename)
haiti_serial=input_serial(input_serial_filename)
haiti_parallel$map<-"Map 2"
haiti_serial$map<-"Map 2"

input_root="island/island"
input_parallel_filename=sprintf("%s_parallel_best_output.csv",input_root)
input_serial_filename=sprintf("%s_serial_volunteer_result_0.01.csv",input_root)
island_parallel=input_parallel(input_parallel_filename)
island_serial=input_serial(input_serial_filename)
island_parallel$map<-"Map 1"
island_serial$map<-"Map 1"

#serial=serial[serial$num_workers %in% c(1,2,3,4,6,8,10),]
#print(head(serial))
result_parallel=rbind(island_parallel,haiti_parallel, haiti2_parallel)
result_serial=rbind(island_serial,haiti_serial, haiti2_serial)

#result=rbind(parallel,serial)
#print(result$type)
#s=ggplot(result_parallel,aes(factor(num_workers),fmeasure))+ geom_boxplot(aes(fill=factor(type)),outlier.colour="NA")+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s0=ggplot(result_parallel,aes(factor(num_workers),precision))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s0=s0+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
s1=ggplot(result_parallel,aes(factor(num_workers),recall))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
s1=s1+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
s2=ggplot(result_parallel,aes(factor(num_workers),fmeasure))+ geom_boxplot()+ ylim(0,1) +facet_wrap(~ map, nrow = 1)
#s2=s2+opts(axis.title.x = theme_text(size = 14, vjust = -0.5),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
s2=s2+opts(axis.title.x = theme_blank(),axis.title.y = theme_text(size = 14, angle=90, vjust = 0),  aspect.ratio=1, axis.text.x = theme_text(size = 14), axis.text.y = theme_text(size = 14)) +xlab("number of participants (parallel)")
#ggsave(s, filename=output_image_filename)
grid.arrange(s0,s1,s2,ncol=1)
