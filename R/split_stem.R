#' Process disease stems
#' @description Cut disease stems for different number of diseases.
#' @param stems character vector of stems to be processed which should each be separated by a semi-colon (\code{;}).
#' @param cut_level number indicating the level at which stems should be cut (i.e. the number of diagnoses).
#' @param maximal A logical that true if the 'maximal' stem is to be considered for each record, or if it is to be limited to only records with the same number of diagnoses as \code{cut_level}, default of \code{TRUE}.
#'
#' @return A vector of the stems cut at the correct number of diagnoses.
#' @examples
#'
#' stems <- c('4;5-6;3-5-6;;', '4;5-4;;;', '5;5-6;3-5-6;3-5-6-7;')
#'
#' split_stem(stems, cut_level = 3, maximal = TRUE)
#' @export

split_stem = function(stems, cut_level = 1, maximal = TRUE){
  # should have ; between stems
  if(!all(grepl(';', stems)))stop('all stems should have a ";" separator')
  if(!is.logical(maximal))stop('maximal should be a logical of either TRUE or FALSE')
  if(!is.numeric(cut_level))stop('cut_level should be a numeric value')

  cut_level <- round(cut_level, digits = 0)


  split_stems = strsplit(stems, ';')
  len = max(sapply(split_stems, length))
  # pad the split stems to make consistent length
  split_stems = lapply(split_stems, function(x) c(x, rep('', len - length(x))))
  splitted_stems = do.call(rbind,split_stems)

  if(isFALSE(maximal)){
    # if maximal is not wanted, just cut at that level (equates with column in matrix)
    cat('Returning non-maximal stem \n Records with less than', cut_level, 'diseases will not be included')
    return(splitted_stems[,cut_level])
  }
  else{
    cat('Returning maximal stem \n Records with less than', cut_level, 'diseases will have their maximal stem included')
    core = splitted_stems[,cut_level]
    missing = which(core=='')
    if(length(missing) == 1){
      additional = .return_tail(splitted_stems[missing,])
      core[missing] = ifelse(!length(additional), NA, additional)
      }
    else{
      additional = apply(splitted_stems[missing, ], 1, .return_tail)
      core[missing] = additional
      }
    #replace empty character with NA
    core[core == "character(0)"] <- NA

    return(unlist(core))}
}

.return_tail = function(x){
  return(tail(x[nchar(x)>0],1))
}
