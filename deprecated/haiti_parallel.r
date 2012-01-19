# sourceDirectory("R/", modifiedOnly=TRUE);
#setwd("c:/dev/parallel/R")

source('input_data.r')
source('cluster_map.r')
source('parallel_cluster.r')

#input
input_volunteer='island/island_volunteer_cleaned_0_003.csv'
input_gold='island/island_reference_cleaned_0_007.csv'
output_filename='island/island_output_v_0_003_min_0_ref_0_007.csv'

# Experiment1  data
experiment=read_input(input_volunteer,0, 0)
print("experiment")
print(nrow(experiment))

#gold standard data 
reference=read_input(input_gold,0, 0)
print("reference")
print(nrow(reference))

print(Sys.time())

#############################  VERY IMPORTANT CLUSTER  ################
# we don't compute for each number of worker  1 , 2 , 3, 4 ,5 ,6
# but for a sub set 1 ,2 ,4, 6, etc.. to be faster
compute_all_accuracy=function(experiment, reference){
num_dist=c(0.01)
num_workers=c(1,2,3,4,6,8,10,12,14)
num_voters=c(1,2,3,4,6,8,10,12,14)
output=c()
for (dist_cluster in num_dist){
 for (num_worker in num_workers){
    for (num_voter in num_voters){
    if (num_voter<=num_worker){
		tmp=accuracy_dist(experiment, reference, num_workers=num_worker, num_voters=num_voter, min_dist=0.01, min_cluster=dist_cluster)
   		output=rbind(output,tmp)
	}
   }
 }
}
#output=output[order(-output$fmeasure),]
#output$workerID=seq(1:nrow(output))
return (output)
}

output2=compute_all_accuracy(experiment,reference)
write.csv(output2, file=output_filename)
print(Sys.time())
