#' Change split stems into named diseases
#'
#' @description Name stems once they have been split
#'
#' @param split_stems A vector of split stems outputted by \code{\link{split_stem}}
#' @param dis_names A list of disease names associated with each position as used in \code{\link{get_disease_counts}}
#' @param separator Character to divide diseases in a stem, default of '|'
#'
#' @return A character vector of split stems converted to names, with each disease in a stem separated by defined separator.
#'
#' @examples
#' cut_stems = c('1-3', '2-3','1-2')
#'
#' disease_names = list('Diabetes','Kidney Disease','Heart Failure')
#'
#' name_stems(cut_stems, dis_names = disease_names) # many stems
#'

name_stems = function(split_stems, dis_names, separator = '|'){
  if(any(grepl(';', split_stems))) stop('Have you split your stems yet? There should not be any ; in split stems')
  if(!is.character(separator)) stop('Separator must be a character variable.')
  named_stems = sapply(split_stems, function(x){
    # need == '' to enable summary to work properly
    if(is.na(x) | x == ''){return('')}
    else{
    single_dis = as.numeric(strsplit(x, '-')[[1]])
    if(max(single_dis)> length(dis_names)) stop('Stem cannot have items greater than the length of disease names')
    named_stem = paste(dis_names[single_dis], collapse= separator)
    return(named_stem)
    }
    }, USE.NAMES = F)
    return(named_stems)
}
