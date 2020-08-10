#' Process stems after deriving
#' @description
#' @params stems vector of stems to be processed
#' @params cut_level the level at which stems should be cut (i.e. the number of )
#' @params maximal
#'
#' @example
#' stems <- c('4;5-6;3-5-6;;', '4;5-4;;;', '5;5-6;3-5-6;3-5-6-7;')

# apply
split_stem = function(stems, cut_level = 1, maximal = TRUE){
  # should have ; between stems
  if(!all(grepl(';', stems)))stop('all stems should have a ";" separator')
  if(!is.logical(maximal))stop('maximal should be a logical of either TRUE/FALSE')
  if(!is.numeric(cut_level))stop('cut_level should be a numeric value')

  cut_level <- round(cut_level, digits = 0)

  splitted_stems = do.call(rbind, strsplit(stems, ';'))

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
    }
    else{
      additional = apply(splitted_stems[core == '', ], 1, .return_tail)
    }
    core[missing] = additional

    return(core)}
}

.return_tail = function(x){
  return(tail(x[nchar(x)>0],1))
}
