#' Helper functions to check strings and get count position
#'
#' @description Convert and check comorbid strings, count positions within them to get disease counts.
#'
#' @param comorbid_column A vector of character strings made up of 0s and 1s, or of factors coercible to character, all should be identical lengths.
#'
#' @examples
#' comorbid_column <- c('00010', '01000', '01110', '11101')
#'
#' comorbid_factors <- as.factor(comorbid_column)
#'
#' to_string(comorbid_factors) # make into string
#'
#' check_strings_equal(comorbid_column) # TRUE if strings all same length
#'
#' get_disease_counts(comorbid_column) # list of counts by position in comorbid string
#'
#' @name comorbid
NULL

#' @export
#' @rdname comorbid
#'
to_string = function(comorbid_column){
  # convert to string if factor
  if(is.factor(comorbid_column)){
    return(as.character(comorbid_column))
  }
  else{
    return(comorbid_column)
  }
}

#' @export
#' @rdname comorbid
check_strings_equal <- function(comorbid_column){
  # return TRUE if length of strings all the same
  return(isTRUE(length(unique(nchar(to_string(comorbid_column)))) ==1))
}

#' @export
#' @rdname comorbid
get_disease_counts <- function(comorbid_column){
  #get number of diagnoses
  n_diag = nchar(comorbid_column[1])
  #make a list of each row of data that has associated disease
  all_diseases = lapply(1:n_diag, function(x) which(substr(comorbid_column, x, x) == '1'))
  return(all_diseases)
}
