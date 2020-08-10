get_stem_maximal_split = function(stems, cut_level = 1){
  s_ = unlist(strsplit(stems, ';'))
  s_ = s_[nchar(s_) > 0]
  if(length(s_) < cut_level){
    return(s_[length(s_)]) # maximum stem point
  }
  else{
    return(s_[cut_level])
  }
}

determine_default_combinations = function(comorbid_column){
  #determine the count of diagnoses
  counts = nchar(comorbid_column) - nchar(gsub('1', '', comorbid_column))
  # tabulate
  tt = table(counts)
  # select those
  max = tt[tt/length(comorbid_column)*100 >2.5]
  return(as.numeric(names(max[length(max)])))
}

