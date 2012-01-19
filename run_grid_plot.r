source('algo_grid.r')
source('plot_map.r')

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


ref.ppp<-ppp_read(input_ref)
data.ppp <- ppp_read(input_volunteer,min=50)

#plot volunteer vs reference
plot(data.ppp,main="",chars=20)
plot(ref.ppp,main="", add=TRUE, chars=20, cols='green')

#plot reference
plot(ref.ppp,main="",chars=20)

min_dist=0.006
res= resolution(window, min_dist)

data.q=discretize_all(data.ppp,res)


# plot discret reference
ref.q=quadratcount(ref.ppp, nx = res[1], ny = res[2])
p1=plot_count(ref.q,min_volunteer=1)
print(p1)

#plot discret volunteer 
selected=data.ppp[data.ppp$marks %in% c(1)]
plot(selected,main="",chars=20)
vol.q=quadratcount(selected, nx = res[1], ny = res[2])
p=plot_count(vol.q,min_volunteer=1)
print(p)

selected=data.ppp[data.ppp$marks %in% c(2)]
plot(selected,main="",chars=20)
vol.q=quadratcount(selected, nx = res[1], ny = res[2])
p=plot_count(vol.q,min_volunteer=1)
print(p)

selected=data.ppp[data.ppp$marks %in% c(3)]
plot(selected,main="",chars=20)
vol.q=quadratcount(selected, nx = res[1], ny = res[2])
p=plot_count(vol.q,min_volunteer=1)
print(p)

selected=data.ppp[data.ppp$marks %in% c(1,2,3)]
plot(selected,main="",chars=20)
vol.q=quadratcount(selected, nx = res[1], ny = res[2])
p=plot_count(vol.q,min_volunteer=0)
print(p)
p=plot_count(vol.q,min_volunteer=1)
print(p)
#p=plot_count(vol.q,min_volunteer=2)
#print(p)
#p=plot_count(vol.q,min_volunteer=8)
#print(p)
#selected=
  #print(data$marks)
  #print(workers)
  #print(selected)
  #print(resolution)
  #print((max(selected$y)>max(window$yrange)))
 # selected.q=quadratcount(selected, nx = resolution[1], ny = resolution[2])  
  #return (ifelse(as.vector(selected.q)>=min_volunteers, TRUE,FALSE))