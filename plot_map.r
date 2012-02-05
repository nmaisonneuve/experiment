library(ggplot2)
library(grid)
library(gridExtra)

blank=opts(legend.position="none",                 
      panel.background = theme_rect(fill = 'white'),
      axis.text.x=theme_blank(), 
      axis.text.y=theme_blank(),           
      axis.title.x=theme_blank(), axis.title.y=theme_blank(),
      axis.ticks=theme_blank(),
      panel.grid.major = theme_blank(),
      panel.grid.minor = theme_blank(),
      strip.background = theme_rect(colour = 'white'),
      panel.border=theme_rect(colour="black",size=0.71),
      aspect.ratio=1, plot.margin = unit(c(0,0,0,0), "lines"))

blank2 <- function (base_size = 12){
  structure(list(
    panel.background = theme_rect(size = 1, colour = "black"),
    #panel.background = theme_rect(fill = 'white'),
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), axis.ticks.margin = unit(0, "lines"), 
    legend.position = "none",     
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_rect(colour = 'white'), 
    plot.title = theme_text(size = base_size * 1.2), 
    plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
  ), class = "options")
}

#plot_grid 
# using vector
# tile c(30,32)= grid of 30x32
plot_count=function(ref.q, tile=c(ncol(ref.q),nrow(ref.q)), min_volunteer=0,legend=FALSE){

  #create the grid
  x=rep(seq(1:tile[1]),each=tile[2])
  y=rep(rev(seq(1:tile[2])),tile[1])
  
  z=as.vector(ref.q)    
  if (min_volunteer>=1)
    z=ifelse(z>=min_volunteer,TRUE,FALSE)
  
  p=ggplot(as.data.frame(cbind(x,y,z)), aes(x=x,y=y)) + geom_tile(aes(fill=z)) + 
    #scale_fill_gradient2(low="white", high="black",space="Lab",mid="gray",midpoint=1) +
    scale_fill_gradient(low="white", high="black",space="Lab") +
    scale_x_discrete(breaks=NA)+ scale_y_discrete(breaks=NA)+  opts(
        panel.background = theme_rect(size = 1, colour = "black"),
        panel.grid.major = theme_blank(),
        panel.grid.minor = theme_blank(),
        axis.line = theme_blank(),
        axis.text.x = theme_blank(),
        axis.text.y = theme_blank(),
        axis.title.x = theme_blank(),
        axis.title.y = theme_blank(), 
        axis.ticks = theme_blank(),
        strip.background = theme_blank(),
        strip.text.y = theme_blank()
        #strip.text.y = theme_text(size = 7, colour = "red", angle = 90)
    )
  if (!legend)
    p=p+opts(legend.position = "none")
  
  return(p)
}

# multiplot
multiplot <- function(..., plotlist=NULL, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)
    
    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
    
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
     
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }
     
}
#size=1,

plot_volunteers=function(data_){
  input=as.data.frame(data_)  
  p=ggplot(input, aes(coords.x1, coords.x2))+  geom_point(aes(colour=factor(workerID)))+blank
  if (exists("lat_range")){
    print("lat_range exist")
  p=p+xlim(lat_range[1],lat_range[2])+ylim(lng_range[1],lng_range[2])
  }
return(p)
}

plot_volunteers2=function(data_){
  input=as.data.frame(data_)  
  p=ggplot(input, aes(coords.x1, coords.x2))+  geom_point(aes(size=1,colour=factor(workerID),shape=factor(cluster))) +
    xlim(lat_range[1],lat_range[2])+ylim(lng_range[1],lng_range[2])
return(p)
}


plot_clusters=function(data_){
  input=as.data.frame(data_)
p=ggplot(input, aes(coords.x1, coords.x2))+  geom_point(aes(size=1,colour = factor(cluster)))+blank+
  xlim(lat_range[1],lat_range[2])+ylim(lng_range[1],lng_range[2])
return(p)
}



plot_result=function(data_){
  input=as.data.frame(data_)  
  p=ggplot(input, aes(coords.x1, coords.x2))+  geom_point(aes(colour=factor(support),size=support))+blank  +
    xlim(lat_range[1],lat_range[2])+ylim(lng_range[1],lng_range[2]) +scale_colour_grey(start=0.8, end=0)  #scale_colour_gradient(low="gray", high="black", space="Lab")    
  if (FALSE){
    p=p+opts(legend.position = "right")
  }
return(p)
}

#without matching info
plot_result_with_ref=function(data_,ref_){
  input=as.data.frame(data_)  
  ref=as.data.frame(ref_)
  p=ggplot()+ geom_point(data=input, aes(x=coords.x1, y=coords.x2,size=support))+ geom_point(data=ref,aes(x=coords.x1, y=coords.x2, colour='yellow')) + blank  +
    xlim(lat_range[1],lat_range[2]) +opts(legend.position = "none")
return(p)
}

#+ matching information 
plot_result_with_ref2=function(data_,ref_){
  input=as.data.frame(data_)  
  ref=as.data.frame(ref_)
  p=ggplot()+ geom_point(data=input, aes(x=coords.x1, y=coords.x2,size=support, colour=matching))+ geom_point(data=ref,aes(x=coords.x1, y=coords.x2)) + blank  +
    xlim(lat_range[1],lat_range[2])+ylim(lng_range[1],lng_range[2])+scale_colour_gradient(low="blue", high="red", space="Lab") +opts(legend.position = "none")
return(p)
}


############################# PLOT ##############################
to_kml=function(data,output){
writeOGR(data, dsn=output, layer= "cycle_wgs84", driver="KML", dataset_options=c("NameField=name"))
}

plot_map=function(obs, file){
obs_df=as.data.frame(obs)
s=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line())+ geom_point(aes(x = lon,y = lat),data=obs_df,shape = 22,colour = '#0000ff',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
facet_wrap(facets = ~workerID, nrow=7 )+ opts(aspect.ratio = 1) 
ggsave(s, filename=file, height=18,width=24,dpi=120)
} 

plot_map_with_expert=function(obs, collective,collective2, ref, file){
obs=as.data.frame(obs)
names(obs)<-c("workerID","lon", "lat")
agg=as.data.frame(collective)
agg$workerID<-100

agg2=as.data.frame(collective2)
agg2$workerID<-150

ref=as.data.frame(ref)
ref$workerID<-200
names(ref)<-c("workerID","lon", "lat")

origin=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank()) #,panel.grid.minor = theme_line()
s=origin+ geom_point(aes(x = lon,y = lat),data=obs,size=3,shape = 20,colour = '#0000ff',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+ facet_wrap(facets = ~workerID , ncol=3 ) + opts(aspect.ratio = 1) 
s=s+ geom_point(aes(x = lon,y = lat),data=agg,size=3, shape = 20,colour = '#ff00ff',na.rm = TRUE,position = position_jitter()) 
s=s+ geom_point(aes(x = lon,y = lat),data=agg2,size=3, shape = 20,colour = '#00ff00',na.rm = TRUE,position = position_jitter())
s=s+ geom_point(aes(x = lon,y = lat),data=ref,size=3,shape = 20,colour = '#ff0000',na.rm = TRUE,position = position_jitter())
#ggsave(s, filename=file, height=18,width=24,dpi=120)
return(s)
}


plot_output=function(output){
#boxplot(accuracy ~ num_workers,output)
p <- ggplot(output, aes(accuracy, ..count..)) + geom_histogram(binwidth = 0.1) 
p + facet_wrap(~ num_workers) 
}




###################### INDIVIDUAL PLOT MAP ###########################

plot_individual_performance=function(output){
num_instances=nrow(output)
output=output[order(-output$fmeasure),]
output$workerID=seq(1:nrow(output))
temp=c()
t1=cbind(output[,c("fmeasure","workerID")],replicate(num_instances,"fmeasure"))
names(t1)=c("measure", "workerID", "type")
t2=cbind(output[,c("precision","workerID")],replicate(num_instances,"precision"))
names(t2)=c("measure", "workerID", "type")
t3=cbind(output[,c("recall","workerID")],replicate(num_instances,"recall"))
names(t3)=c("measure", "workerID", "type")
temp=rbind(temp,t1)
temp=rbind(temp,t2)
temp=rbind(temp,t3)
temp=as.data.frame(temp)
temp$type=factor(temp$type)
temp$workerID=as.numeric(temp$workerID)
temp$measure=as.numeric(as.character(temp$measure))
s=qplot(type, measure, data=temp, fill=type)+geom_histogram()+ facet_grid(type ~ workerID)+ylim(0,1)+ opts(axis.ticks = theme_blank(),axis.text.x = theme_blank(), legend.position = "none")+xlab("")+ylab("")
return(s)
}

plot_individual=function(data){
# individual performance 
snug.opts <- opts(axis.ticks.x = theme_blank(), 
             axis.title.x = theme_blank(), 
		 axis.text.x = theme_blank(),
		 legend.position = "none",
             plot.margin = unit(c(0.2,0.5,0.2,0), "lines")) 

d1=qplot(factor(workerID), fmeasure, data=as.data.frame(data) , fill=fmeasure)+geom_bar()+ xlab("")+ylim(0,1)+snug.opts
d2=qplot(factor(workerID), precision, data=as.data.frame(data) , fill=precision)+geom_bar()+xlab("")+ylim(0,1)+snug.opts
d3=qplot(factor(workerID), recall, data=as.data.frame(data) , fill=recall)+geom_bar()+ ylim(0,1) +snug.opts
grid.arrange(d1,d2,d3, nrow=3)


# distribution 
snug.opts <- opts(axis.ticks.x = theme_blank(), 
		 legend.position = "none",
             plot.margin = unit(c(0.2,0.5,0.2,0), "lines")) 
d1=qplot(fmeasure, y=..count../sum(..count..), data=as.data.frame(data) , geom="histogram", binwidth=0.2)+ylim(0,1)+xlab("fmeasure")+ylab("% population")+snug.opts+opts(axis.ticks = theme_blank(),axis.text.x = theme_blank())+xlim(0,1)
d2=qplot(precision, y=..count../sum(..count..), data=as.data.frame(data) , geom="histogram", binwidth=0.2)+ylim(0,1)+xlab("precision")+ylab("% population")+snug.opts+opts(axis.ticks = theme_blank(),axis.text.x = theme_blank())+xlim(0,1)
d3=qplot(recall, y=..count../sum(..count..), data=as.data.frame(data) , geom="histogram", binwidth=0.2)+ylim(0,1)+xlab("recall")+ylab("% population")+snug.opts+xlim(0,1)
grid.arrange(d1,d2,d3, nrow=3)
}


plot_error_map=function(obs, ref){
matching=obs[obs$matching!=0,]
error=obs[obs$matching==0,]
s=ggplot() + opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line()) +
geom_point(aes(x = lon,y = lat),data=as.data.frame(ref),shape = 21,colour = '#000000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
geom_point(aes(x = lon,y = lat),data=as.data.frame(matching),shape = 22,colour = '#00FF00',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')+
geom_point(aes(x = lon,y = lat),data=as.data.frame(error),shape = 22,colour = '#FF0000',na.rm = TRUE,position = position_jitter()) + coord_map(projection = 'mercator')
ggsave(s, filename=sprintf("error_map.png"), height=30,width=40,dpi=120)
}

plot_error_map2=function(obs, ref){

ref$workerID=NULL
obs$matching[obs$matching!=0]= 1 
obs$matching=factor(obs$matching)
print("ok")

s=qplot(lon,lat,data=as.data.frame(obs), colour=matching, shape=22, facets=~workerID) + coord_map(projection = 'mercator')+ 
geom_point(data=as.data.frame(ref), colour="black", size=1)+
opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank(),panel.grid.minor = theme_line())
ggsave(s, filename="error_map_all.pdf", height=40,width=50,dpi=100)
}

