library(ggplot2)
library(ReadImages)
library(RgoogleMaps)



################################################################################
#################### preload functions for later use        ####################
################################################################################

ggimage <- function(image){
  require(ggplot2)
  
  if(length(dim(image)) == 2){
    message('creating black and white image...')
    image <- melt(image)
    names(image) <- c('row','column','fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_gradient(low = 'black', high = 'white')
  }
  
  if(length(dim(image)) == 3){
  	message('creating color image...')
  	image <- apply(image, 1:2, function(v) rgb(v[1], v[2], v[3]))
    image <- melt(image)
    names(image) <- c('row', 'column', 'fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_identity()  	
  }

  #return(plot) # remove first pound for the image in the case study
  p=plot 
#   +  opts(axis.line = theme_blank(), 
#       axis.ticks = theme_blank(),
#       axis.text.x = theme_blank(), 
#       axis.text.y = theme_blank(), 
#       axis.title.x = theme_blank(), 
#       axis.title.y = theme_blank(),
#       axis.ticks.length = unit(0, "lines"), 
#       axis.ticks.margin = unit(0, "lines"),
#       legend.position = "none", 
#       panel.background = theme_blank(), 
#       panel.border = theme_blank(), 
#       panel.grid.major = theme_blank(), 
#       panel.grid.minor = theme_blank(), 
#       panel.margin = unit(0, "lines"), 
#       plot.background = theme_blank(), 
#       plot.title = theme_blank()
#       #plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
#     )
  return (p)
}
#bb <- qbbox(c(40.702147,40.711614,40.718217),c(-74.015794,-74.012318,-73.998284),TYPE = "all", margin = list(m=rep(0,4), TYPE = c("perc", "abs")[1]));

range_lat=c(-72.2570801,-72.2515869)
range_lng=c(18.5549150,18.5603014)
MyMap <- GetMap.bbox(range_lat, range_lng, destfile = "MyTile3.jpg", maptype = "satellite",MINIMUMSIZE=TRUE)
map <- read.jpeg("MyTile3.jpg")
plot(map)

res=c(dim(MyMap$myTile)[1],dim(MyMap$myTile)[2])
#length(map)
#n_pix = 640
#PlotOnStaticMap(MyMap,lat=c(18.55761),lon=c(-72.25433), pch=20,cex =1,col='red',verbose=0)
map <- apply(map, 1:2, function(v) rgb(v[1], v[2], v[3])) 

# reshape map for plotting
  m_map <- melt(map)
  names(m_map) <- c('x','y','fill')
  m_map <- within(m_map,{
    x <- range_lat[1]+(x-1)*diff(range_lat)/res[1]
    y <- range_lng[1]+(y-1)*diff(range_lng)/res[2]
  }) 
  names(m_map) <- c('lat','lon','fill')
# geocode pixel references
#  s <- (-n_pix/2) : (n_pix/2 - 1)  
  
#plot(map)
#image <- read.jpeg("c:/dev/experiment/haiti.jpg")
#print(ggimage(image))