source('plot_map.r')
source('input_data.r')
library(sp)
# STATUS_NOT_ANALYSED=1
#  STATUS_ACCEPTED=2
#  STATUS_REJECTED =3
#  STATUS_NEW =4



#input
input_root="haiti2/haiti2"
input_volunteer=sprintf("%s_serial_volunteer.csv",input_root)
input_gold=sprintf("%s_reference.csv",input_root)
iteration_image=sprintf("%s_iterative_sample.pdf",input_root)

# EPSG:3857 (Spherical Mercator projection)
ps="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Experiment1  data
#around 30 workers producing around 4000 annotations
# 1 row = 1 geo_annotation = {lat, lng , the worker's ID}
experiment <- read.csv(input_volunteer, , encoding = "UTF-8")
reference <- read.csv(input_gold, , encoding = "UTF-8")

print(head(experiment))

#display all the instances' ID
print("instances")
print(unique(experiment$instance))

#number of iteration
#experiment=experiment[experiment$instance==110,]
#print(unique(experiment$iteration))

#experiment=experiment[-which(experiment$iteration>=9),]
#print(head(experiment))

extra=as.data.frame(experiment[,3:5])
names(extra)<-c('state','iteration','instance')
experiment2=SpatialPointsDataFrame(coords=experiment[,1:2], data=extra, proj4string=CRS(ps))

experiment2=cut_outside_haiti2(experiment2)

exp=as.data.frame(experiment2)


#instances=setdiff(unique(exp$instance),c(124,111,125,106,128))
#exp=exp[exp$instance %in% instances,]
#print(exp)

ref=as.data.frame(reference)
ref$X=NULL
ref$cluster=NULL
ref$workerID=NULL
ref$assignID=NULL
ref$iteration=0
ref$instance=0
ref$state=1

#
# add the reference at the end of the instance
instances=c()
#instances=unique(exp$instance)
for (i in instances){
  
  test=exp[exp$instance==i,]  
  ref$iteration=max(test$iteration)+1
  ref$instance=i
  
  new=ref[rep(seq_len(nrow(ref)), 1), ]
  
  #print(str(test))
  #print(str(ref))
  #print(max(test$iteration))
  #print(as.data.frame(new))
  
  exp=rbind(exp,as.data.frame(new))
  #print(nrow(exp[exp$instance==i,]))
  #print(max(exp[exp$instance==i,]$iteration))
}
s=ggplot() + theme_bw()+blank #opts(axis.line = theme_blank(),axis.text.x = theme_blank(),axis.text.y = theme_blank(),axis.ticks = theme_blank()) #,panel.grid.minor = theme_line()

#s=s+geom_point(aes(x = lon,y = lat,colour=factor(state), pch=factor(state)),data=exp)+ 
s=s+geom_point(aes(x = lon,y = lat,colour=factor(state), pch=20),data=exp)+
  facet_grid(instance ~ iteration) + opts(aspect.ratio = 1)+scale_colour_manual(values=c("#AAAAAA","#00FF00","#FF0000","#3333FF"))
#s=s+ geom_point(aes(x = lon,y = lat),data=ref,size=3,shape = 20,colour = '#ff0000',na.rm = TRUE,position = position_jitter())
# aes(x=lon, y=lat))+ geom_point() # +geom_point(aes(x = lon,y = lat, colour=state),data=exp,position = position_jitter()) + coord_map(projection = 'mercator')+ facet_wrap(facets = ~iteration, ncol=3 ) + opts(aspect.ratio = 1) 
ggsave(s, filename=iteration_image, height=32,width=35,dpi=120)
#ggsave(s, filename=iteration_image)
#ggsave(s, filename=iteration_image, height=2.5,width=14,dpi=120)
#facet_wrap(facets = ~iteration, ncol=3 )
#print(s)