source('algo_grid.r')
source('input_data.r')
source('plot_map.r')


input_root="haiti/haiti"
#define window 
window=window_haiti
input_volunteer=sprintf("%s_volunteer.csv",input_root)
input_ref=sprintf("%s_reference.csv",input_root)
output_file=sprintf("%s_collective_output_qc.csv",input_root)


ref.ppp<-ppp_read(input_ref)
data.ppp <- ppp_read(input_volunteer,min=50)

min_dist=0.0064
 #compute resolution and result according to min dist
res= resolution(window, min_dist)
ref.q=quadratcount(ref.ppp, nx = res[1], ny = res[2])
ref.q=(ifelse(as.vector(ref.q)>=1, TRUE,FALSE))

    real=length(ref.ppp$x)
    digitized=length(which(ref.q==TRUE))
    cat ("impact of resolution ", res[1],"x",res[2],"of ",min_dist, " km:", (real-digitized)/real*100, "% of points lost\n")
    data.q=discretize_all(data.ppp,res)
    data.q=ifelse(data.q>=1,TRUE,FALSE)

print(length(which(ref.q))/length(ref.q)) #5% of buildings
result1=t(apply(data.q,2,FUN=function(x){return(similarity2(x,ref.q))}))
result=t(apply(data.q,2,FUN=function(x){return(similarity(x,ref.q))}))
r=as.data.frame(cbind(result,result1))
#names(r)=c("precision","recall","fmeasure","nb_obs","tp","tn","fp","fn")
names(r)=c("precision","recall","fmeasure","nb_obs","precision_t","precision_f")
#plot_count(data[])
print(r)