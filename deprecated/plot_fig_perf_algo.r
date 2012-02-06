library(xtable)

library(ggplot2)
library(gridExtra)
library(doBy)
source('plot_result.r')


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
    

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # Collapse the data
    formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
    print(formula)
    datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)

    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}


#summary=function(data){  s=by(output[3:6], output$num_workers, mean)}

best_parameters=function(data){
  #print(data)
  #output.mean<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","num_voters","min_dist"))
  output.mean=aggregate(. ~ num_voters +num_workers, data = data, mean)
  #output.mean=aggregate(. ~ num_voters +dist_cluster+num_workers, data = data, sd)
#print(output.mean)
 num_workers=as.data.frame(output.mean$num_workers)
 fmeasure=as.data.frame(output.mean$fmeasure)
 sel <- ave(fmeasure, num_workers, FUN = max) == fmeasure
 return(output.mean[sel,])
}

best_results=function(best_params,data){
  output=c()
  for (i in 1:nrow(best_params)) {    
    #print(best_params[i,])
    #print(best_params[i,]$num_workers)
    tmp=subset(data, data$num_workers==best_params[i,]$num_workers & data$dist_cluster==best_params[i,]$dist_cluster & data$num_voters==best_params[i,]$num_voters)    
    output=rbind(output,tmp)
  }
  return(output)
}  

agreement_plot=function(output,measure){
  sum_data<-summarySE(output,measurevar="fmeasure", groupvars=c("ratio"))
  p=ggplot(sum_data, aes(x=ratio, y=fmeasure))+geom_point()+geom_errorbar(aes(ymin=fmeasure-ci, ymax=fmeasure+ci))+scale_colour_gradientn(colour = rainbow(7))+ylim(0,1)
  return(p)
}

plot_all=function(output){
sum_fm<-summarySE(output,measurevar="fmeasure", groupvars=c("num_workers","algo","min_dist"))
names(sum_fm)[names(sum_fm)=="fmeasure"]="measure"
sum_fm$type="fmeasure"

sum_recall<-summarySE(output,measurevar="recall", groupvars=c("num_workers","algo","min_dist"))
names(sum_recall)[names(sum_recall)=="recall"]="measure"
sum_recall$type="recall"

sum_precision<-summarySE(output,measurevar="precision", groupvars=c("num_workers","algo","min_dist"))
names(sum_precision)[names(sum_precision)=="precision"]="measure"
sum_precision$type="precision"

sum_fm=rbind(sum_fm, sum_precision,sum_recall)
print(head(sum_fm))
#shape ,
p1=ggplot(sum_fm, aes(x=num_workers, y=measure, colour=algo, group=algo)) + 
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


#input
input_root="island/island"

input_dca=sprintf("%s_collective_output_dca.csv",input_root)
output_dca <- read.csv(input_dca, encoding = "UTF-8")
names(output_dca)[7]=c("min_dist")
tmp=output_dca
output_dca=data.frame(cbind(fmeasure=tmp$fmeasure,precision=tmp$precision,recall=tmp$recall,num_workers=tmp$num_workers, num_voters=tmp$num_voters,min_dist=tmp$dist))
output_dca=output_dca[output_dca$min_dist==0.007,]
output_dca=best_parameters(output_dca)
output_dca$algo="dca"

print("ok")
input_density=sprintf("%s_collective_output_density.csv",input_root)
output_density <- read.csv(input_density, encoding = "UTF-8")
names(output_density)[7]=c("min_dist")
tmp=output_density
print()
output_density=data.frame(cbind(fmeasure=tmp$fmeasure,precision=tmp$precision,recall=tmp$recall,num_workers=tmp$num_workers, num_voters=tmp$num_voters,min_dist=tmp$dist))
output_density=output_density[output_density$min_dist==0.007,]
output_density=best_parameters(output_density)
output_density$algo="density"

print("ok0")
input_grid=sprintf("%s_collective_output_qc.csv",input_root)
output_grid <- read.csv(input_grid, encoding = "UTF-8")
tmp=output_grid
output_grid=data.frame(cbind(fmeasure=tmp$fmeasure,precision=tmp$precision,recall=tmp$recall,num_workers=tmp$num_workers, num_voters=tmp$num_voters,min_dist=tmp$min_dist))
output_grid=output_grid[output_grid$min_dist==0.010,]
output_grid=best_parameters(output_grid)
output_grid$algo="grid"


output=rbind(output_density,output_dca,output_grid)
output$ratio=output$num_voters/output$num_workers
print("ok1")
#names(output)[7]=c("min_dist")
output$ratio=cut(output$ratio,breaks=c(0,0.2,0.4,0.6,0.8,1))
output$precision[output$precision==0]=1
#output=output[output$min_dist==0.007,]
print("ok2")


#output=output[output$ratio<0.60 & output$ratio>0.10,]

#best_parameters(output)


#p3=ggplot(sum_precision, aes(x=num_workers, y=precision, colour=ratio, group=ratio)) + 
#    geom_point() + geom_line() + geom_errorbar(aes(ymin=precision-ci, ymax=precision+ci))+ylim(0,1)+ facet_grid(. ~min_dist) #scale_colour_gradientn(colour = rainbow(7))+ylim(0.5,1) #+ coord_trans(x="log2")

#multiplot(p1, p2, p3, cols=1)
#p1=plot_all(output)
#p=agreement_plot(output)