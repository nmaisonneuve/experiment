#library(xtable) #latex

library(ggplot2)
source('plot_result.r')
source('parallel_cluster.r')



agreement_plot=function(output,measure){
  sum_data<-summarySE(output,measurevar="fmeasure", groupvars=c("ratio"))
  p=ggplot(sum_data, aes(x=ratio, y=fmeasure))+geom_point()+geom_errorbar(aes(ymin=fmeasure-ci, ymax=fmeasure+ci))+scale_colour_gradientn(colour = rainbow(7))+ylim(0,1)
  return(p)
}

plot_all=function(output){
sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","ratio"))
names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
sum_fm$type="fmeasure"

sum_recall<-summarySE(output,measurevar="recall", groupvars=c("num_workers","ratio"))
names(sum_recall)[names(sum_recall)=="recall"]="measure"
sum_recall$type="recall"

sum_precision<-summarySE(output,measurevar="precision", groupvars=c("num_workers","ratio"))
names(sum_precision)[names(sum_precision)=="precision"]="measure"
sum_precision$type="precision"

sum_fm=rbind(sum_fm, sum_precision,sum_recall)
print(head(sum_fm))
#shape ,
p1=ggplot(sum_fm, aes(x=num_workers, y=measure, colour=ratio, group=ratio)) + 
      geom_errorbar(aes(ymin=measure-ci, ymax=measure+ci)) +
      geom_line() +        geom_point()+
      ylab("")+ xlab("number of volunteers")+
      ylim(0,1) +facet_grid(. ~type)+theme_bw()+
      scale_colour_hue(name="Agreement")+
      opts(strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
           axis.title.x = theme_text(size=15),
            legend.position="right", 
        legend.direction="vertical", plot.margin = unit(c(0,0,0,-1), "lines"))
return (p1)
}

input_root="haiti/haiti"
input_filename=sprintf("%s_collective_output2_density.csv",input_root)

# Experiment1  data
output <- read.csv(input_filename, encoding = "UTF-8")
output$ratio=output$num_voters/output$num_workers
#interval=seq(0.1,1,0.2)
#interval=c(0,0.10,0.25,0.4,0.55,0.70,0.85,1)
#interval=c(0,0.10,0.25,0.4,0.55,0.70,0.85,1)
#names(output)[7]=c("min_dist")
output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))
output$precision[output$precision==0]=1


output=output[output$dist==0.007,]
print(output)
#output=output[output$ratio<0.60 & output$ratio>0.10,]

#best_parameters(output)


#p3=ggplot(sum_precision, aes(x=num_workers, y=precision, colour=ratio, group=ratio)) + 
#    geom_point() + geom_line() + geom_errorbar(aes(ymin=precision-ci, ymax=precision+ci))+ylim(0,1)+ facet_grid(. ~min_dist) #scale_colour_gradientn(colour = rainbow(7))+ylim(0.5,1) #+ coord_trans(x="log2")

#multiplot(p1, p2, p3, cols=1)
p1=plot_all(output)

#p=agreement_plot(output)