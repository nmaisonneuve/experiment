library(ggplot2)
library(gridExtra)
source('plot_map.r')

source('plot_result.r')



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
p1=ggplot(sum_fm, aes(x=num_workers, y=sd, colour=algo, group=algo)) + 
      #geom_errorbar(aes(ymin=measure-sd, ymax=measure+sd)) +
      geom_line(size=1) +        #geom_point()+
    ylab("") +xlab("")+
      ylim(0,0.15) +facet_grid(. ~map)+theme_bw()+
      scale_colour_hue(name="Algo")+
      opts(aspect.ratio=1,strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
           axis.title.x = theme_text(size=15),
            legend.position="none", 
        legend.direction="vertical", plot.margin = unit(c(0,0,0,0), "lines"))

p2=ggplot(sum_fm, aes(x=num_workers, y=measure, colour=algo, group=algo)) + 
      #geom_errorbar(aes(ymin=measure-sd, ymax=measure+sd)) +
      geom_line(size=1) +        #geom_point()+
      ylab("")+ xlab("")+
      ylim(0.3,1) +facet_grid(. ~map)+theme_bw()+
      scale_colour_hue(name="Algo")+
      opts(aspect.ratio=1,strip.text.x = theme_text(size = 15),strip.background = theme_rect(colour = 'white'),
           axis.title.x = theme_text(size=15),
        legend.position="none", 
        legend.direction="vertical", plot.margin = unit(c(0,0,0,0), "lines"))
#ggsave("parallel_model1.pdf",p1, width=8,heigh=3.8)
ggsave("parallel_model1.pdf",p1, width=8,height=3, dpi=20)
ggsave("parallel_model2.pdf",p2,width=8,height=3, dpi=20)

#ggsave("parallel_model2.pdf",grid.arrange(p1,p2))

return (multiplot(p2, p1, cols=2))
}


compute_map=function(map,map_label){

input_root=sprintf("%s/%s",map,map)

input_dca=sprintf("%s_collective_output3_dca.csv",input_root)
output_dca <- read.csv(input_dca, encoding = "UTF-8")
names(output_dca)[7]=c("min_dist")
tmp=output_dca


output_dca=data.frame(cbind(fmeasure=tmp$fmeasure,precision=tmp$precision,recall=tmp$recall,num_workers=tmp$num_workers, num_voters=tmp$num_voters,min_dist=tmp$dist))
#output_dca=output_dca[output_dca$min_dist==min_dist[1],]
output_dca=best_voters2(output_dca)
output_dca$algo="DCA"
output_dca$min_dist=NULL

input_density=sprintf("%s_collective_output3_density.csv",input_root)
output_density <- read.csv(input_density, encoding = "UTF-8")
names(output_density)[7]=c("min_dist")
tmp=output_density
output_density=data.frame(cbind(fmeasure=tmp$fmeasure,precision=tmp$precision,recall=tmp$recall,num_workers=tmp$num_workers, num_voters=tmp$num_voters,min_dist=tmp$dist))


#output_density=output_density[output_density$min_dist==min_dist[2],]
output_density=best_voters2(output_density)
output_density$algo="DBSCAN"
output_density$min_dist=NULL


input_grid=sprintf("%s_collective_output3_qc.csv",input_root)
output_grid <- read.csv(input_grid, encoding = "UTF-8")

tmp=output_grid

output_grid=data.frame(cbind(fmeasure=tmp$fmeasure,precision=tmp$precision,recall=tmp$recall,num_workers=tmp$num_workers, num_voters=tmp$num_voters,min_dist=tmp$min_dist))
#output_grid=output_grid[output_grid$min_dist==min_dist[3],]
output_grid=output_grid[output_grid$num_workers<=20,]
output_grid=best_voters2(output_grid)
output_grid$algo="Grid"
output_grid$min_dist=NULL




output=rbind(output_density,output_dca,output_grid)

print(unique(cbind(output$num_workers,output$num_voters, output$algo)))

output$ratio=output$num_voters/output$num_workers

#names(output)[7]=c("min_dist")
#output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))
#output$precision[output$precision==0]=1
output$map=map_label

return (output)
}

output1=compute_map("island","map1")

output2=compute_map("haiti","map2")

output3=compute_map("haiti2","map3")
#output=output[output$min_dist==0.007,]

#output=output[output$ratio<0.60 & output$ratio>0.10,]
output=rbind(output1,output2,output3)
#best_parameters(output)


#p3=ggplot(sum_precision, aes(x=num_workers, y=precision, colour=ratio, group=ratio)) + 
#    geom_point() + geom_line() + geom_errorbar(aes(ymin=precision-ci, ymax=precision+ci))+ylim(0,1)+ facet_grid(. ~min_dist) #scale_colour_gradientn(colour = rainbow(7))+ylim(0.5,1) #+ coord_trans(x="log2")



#pdf('mypdf.pdf');
p1=plot_all(output)
#print(p1)

#p=agreement_plot(output)