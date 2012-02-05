
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