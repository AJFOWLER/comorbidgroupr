#' Summary method for stem
#' @param object An object created by make_stem
#' @param dis_names A list of disease names associated with each position as used in \code{\link{get_disease_counts}}.
#' @param ... Other arguments
#'
#' @export

# neater summary of stems when created

summary.stem <- function(object,dis_names = NULL, ...){
  cat('Stems generatred using::', tools::toTitleCase(object[1,'freq_or_outcome']), '\n')
  cat('Unique comorbid strings:', nrow(object), '\n')
  # calculate the number of each disease, divided by length.
  core_cols <- c('comorbid_column', 'Freq', 'stem')
  dt <- object[,core_cols]

  # split out stem base R to avoid dependency overhead/for fun!
  splitted <- strsplit(dt$stem, ';')

  # pad
  len = max(sapply(splitted, length))
  splitted_pad = lapply(splitted, function(x) c(x, rep('', len - length(x))))

  # bind
  split_df <- cbind(dt, do.call(rbind, splitted_pad))

  # melt to long
  to_melt <- names(split_df)[!(names(split_df) %in% core_cols)]
  melted <- lapply(to_melt, function(x){ y <- split_df[,c('Freq', x)]; names(y)[2] <- 'stem'; return(y)})
  melted_df <- do.call(rbind, melted)
  # name stems
  if(is.null(dis_names)){
    cat('Did you know, if you pass disease names to the dis_name argument I can provide names of diseases! \n')
    cat('\n')
  }
  else{
    melted_df[, 'stem'] <- sapply(melted_df[,'stem'], function(x) name_stems(x, dis_names = dis_names, separator = '-'))
  }
  # sum across all stems
  maths <- aggregate(melted_df$Freq, by=list(melted_df$stem), function(x) sum(x))
  maths <- maths[order(-maths$x),]
  # order and present by length of disease stem (e.g. top 10 1, 2, 3)
  maths$number_of_conditions <- nchar(maths$Group.1) - nchar(gsub('-', '', maths$Group.1))+1
  maths[maths$Group.1 == '', c('number_of_conditions', 'Group.1')] <- c(0, 'No diseases')
  final_out <- lapply(1:len, function(x){
    top_ten = maths[maths$number_of_conditions == x, ][1:10,]
    top_ten$format <- paste0(top_ten$Group.1, ' (n:', top_ten$x,')')
    return(top_ten$format)
  })

  cleaned_final_out <- data.frame(do.call(cbind, final_out), stringsAsFactors = F)
  names(cleaned_final_out) <- do.call(paste, list('Number of diseases:', 1:len))

  cat('Commonest disease combinations across all strings; note patients may be represented multiple times:\n')

  return(cleaned_final_out)
  }
