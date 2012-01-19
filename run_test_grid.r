setwd("C:/dev/parallel/R")
source('algo_grid.r')



par(mar = rep(0, 4)) #margin 0



#constants (window of the map != window of the data (buildings not at a corner))
window_island=owin(c(34.17449,34.18145),c(-0.39021,-0.38439))
window_haiti=owin(c(-72.2570801,-72.2515869),c(18.5549150,18.5603014))
window_haiti2=owin(c(-72.34436,-72.34235),c(18.55314,18.55500))

#input
input_root="haiti2/haiti2"

#define window 
window=window_haiti2
input_volunteer=sprintf("%s_volunteer.csv",input_root)
input_ref=sprintf("%s_reference.csv",input_root)


data.ppp <- ppp_read(input_volunteer,min=50)
min_dist=0.006
res= resolution(window, min_dist)
data.q=discretize_all(data.ppp,res)