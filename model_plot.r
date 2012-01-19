library(xtable)

library(ggplot2)
library(gridExtra)

source('plot_result.r')




#input
input_root="haiti2/haiti2"
input_filename=sprintf("%s_collective_output_virtual.csv",input_root)
plot_result=sprintf("%s_collective_output_qc.png",input_root)



function_m1=function(sum_fm){
p1=ggplot(sum_fm, aes(x=num_workers, y=measure, colour=ratio, group=ratio)) + 
      geom_errorbar(aes(ymin=measure-ci, ymax=measure+ci)) +
      geom_line() +        geom_point()+
      ylab("")+ xlab("number of volunteers")+
      ylim(0,1) +facet_grid(p_precision ~p_recall)+theme_bw()+
      scale_colour_hue(name="Agreement")
return(p1)
}

      #opts(strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),

# Experiment1  data
output <- read.csv(input_filename, encoding = "UTF-8")
output$ratio=output$num_voters/output$num_workers
#interval=seq(0.1,1,0.2)
#interval=c(0,0.10,0.25,0.4,0.55,0.70,0.85,1)
#interval=c(0,0.10,0.25,0.4,0.55,0.70,0.85,1)
output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))
names(output)[8]=c("p_recall")
names(output)[9]=c("p_precision")

#precision = vertical
#output=output[-which(output$p_precision==0.7),]

#sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","ratio","p_recall","p_precision"))
sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","ratio","p_recall","p_precision"))
names(sum_fm)[6]=c("measure")
p1=function_m1(sum_fm)


#output=output[output$ratio<0.60 & output$ratio>0.10,]

      #     axis.title.x = theme_text(size=15),
      #      legend.position="right", 
      #  legend.direction="vertical", plot.margin = unit(c(0,0,0,-1), "lines"))


#p3=ggplot(sum_precision, aes(x=num_workers, y=precision, colour=ratio, group=ratio)) + 
#    geom_point() + geom_line() + geom_errorbar(aes(ymin=precision-ci, ymax=precision+ci))+ylim(0,1)+ facet_grid(. ~min_dist) #scale_colour_gradientn(colour = rainbow(7))+ylim(0.5,1) #+ coord_trans(x="log2")

#multiplot(p1, p2, p3, cols=1)
#p=agreement_plot(output)