source('input_data.r')
library(ggplot2)
#input
input_root="haiti2/haiti2"
input_volunteer=sprintf("%s_output_min_50_match_0_01_all.csv",input_root)
output <- read.csv(input_volunteer, , encoding = "UTF-8")
individual=output[output$num_workers==1,]
qplot(x=precision,y=recall, data=individual)+ opts(aspect.ratio=1)+xlim(0,1)+ylim(0,1)
#plot(x=individual$precision,y=individual$recall, xlab="Precision rate", ylab="Recall rate",  ylim=c(0,1), xlim=c(0,1), pch=20)
