#' Determine default number of combinations
#' @description thing to determine best number of combinations
#' @param comorbid_column
#' @return
#' @examples
#' @export

determine_default_combinations = function(comorbid_column){
  #determine the count of diagnoses
  counts = nchar(comorbid_column) - nchar(gsub('1', '', comorbid_column))
  # tabulate
  tt = table(counts)
  # select those present >2.5% of the time
  max = tt[tt/length(comorbid_column)*100 >2.5]

  return(as.numeric(names(max[length(max)])))
}
