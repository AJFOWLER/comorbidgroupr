#' Summary method for stem
#' @param object An object created by make_stem
#'
#' @export

# neater summary of stems when created
summary.stem <- function(object, dis_names = NULL, ...){
 cat('Stems generatred using::', tools::toTitleCase(object[1,'freq_or_outcome']), '\n')
 cat('Unique comorbid strings:', nrow(object), '\n')
 cat('\n')
 common_stems <- object[order(-object$Freq), ][1:5,]

 if(is.null(dis_names)){
  cat('Did you know, if you pass disease names to the dis_name argument I can provide names of diseases! \n')
   cat('\n')
 }
 else{
 common_stems[, 'position'] <- sapply(common_stems[,'position'], function(x) paste(dis_names[x], collapse=', '))
 }
  pretty_stems <- common_stems[,c('comorbid_column', 'Freq', 'position', 'stem')]
  names(pretty_stems) <- c('Comorbid string', 'Frequency', 'Diseases', 'Stems')
  row.names(pretty_stems) <- NULL
 return(pretty_stems)
 }


