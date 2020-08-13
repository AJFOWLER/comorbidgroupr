#' Determine maximum combinations
#'
#' @description Determine the maximum number of combinations to be included.
#'
#' @param comorbid_column A vector of character strings made up of 0s and 1s, or of factors coercible to character, all should be identical lengths.
#' @param default A number representing what proportion of diagnostic counts should be included . i.e. if 1 in 100 records have four or more diagnoses, but 3 in 100 have three or more, then the selected maximum combinations would be 3 if the default (2.5) is maintained.
#'
#' @return Number of diagnosis counts that are present in the selected proportion of records (default is 1 in 50).
#'
#' @examples
#' comorbid_column <- c('00010', '01000', '01110', '11101')
#'
#' determine_default_combinations(comorbid_column)
#'
#' @export

determine_default_combinations = function(comorbid_column, default = 2.5){

  if(!is.numeric(default)){stop('default should be numeric')}
  # determine the count of diagnoses
  counts = nchar(comorbid_column) - nchar(gsub('1', '', comorbid_column))
  # tabulate
  tt = table(counts)/length(comorbid_column)

  if(default <= 0 | default > max(tt)*100){stop(paste0('default must be greater than 0 but less than the highest disease proportion which is ', max(tt)*100))}

  # select those present >=2.5% of the time
  max = tt[tt*100>=default]
  # select counts present >2.5% of the time.
  return(as.numeric(names(max[length(max)])))
}
