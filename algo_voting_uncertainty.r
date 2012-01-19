uncertainty_voting_process=function(positive_workers, group){
  negative_workers=setdiff(group,positive_workers)
  N=length(group)
  ind_precisions=individual$precision[individual$workerID %in% positive_workers]
  ind_recalls=individual$recall[individual$workerID %in% negative_workers]
  #print(ind_precisions)
  print(mean(ind_precisions))
  print(mean(ind_recalls))
  #print(sum(1-ind_recalls))
  t_group=(sum(ind_precisions)+sum(1-ind_recalls))/N #group certainty about the presence of buildings
  print(t_group)
  r_group=(sum(1-ind_precisions)+sum(ind_recalls))/N #group certainty about the absence of buildings
  print(r_group)
  if (t_group>r_group)
    return(1)
  else
    return(0)
}