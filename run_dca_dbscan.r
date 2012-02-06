# sourceDirectory("R/", modifiedOnly=TRUE);
source('input_data.r')
source('run_parallel_lib.r')


algos=c('density','dca')
algos=c('dca')
#num_voters=unique(ceiling(c(0.2,0.35,0.5)*num_worker))

maps=c("haiti/haiti")
#input_root="haiti2/haiti2"
for (input_root in maps){
  min_dist=dists[[input_root]]
    
  for (algo in algos){
  
  cat("compute map",input_root, "with algo",algo,"\n")
  
  input_volunteer=sprintf("%s_volunteer2.csv",input_root)
  input_gold=sprintf("%s_reference.csv",input_root)
  output_filename=sprintf("%s_collective_output3_%s.csv",input_root,algo,min_dist)
  
  
  # Experiment1  data
  experiment=read_input(input_volunteer,50, 0)
  
  
  print("experiment")
  print(nrow(experiment))
  
  cat("number of volunteer", length(unique(experiment$workerID)))
  
  #gold standard data 
  reference=read_input(input_gold,0, -1)
  print("reference")
  print(nrow(reference))
    
  ptm <- proc.time()
  output2=compute_all_accuracy(experiment,reference, algo=algo, dist_matching=0.007, dist_merging=min_dist)
  print(proc.time()-ptm)
  
  #print(output2)
  write.csv(output2, file=output_filename)
  #rm(output2)
  }
}