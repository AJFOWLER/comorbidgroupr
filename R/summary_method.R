#' Summary method for stem
#' @param object An object created by make_stem
#' @param dis_names A list of disease names associated with each position as used in \code{\link{get_disease_counts}}.
#' @param ... Other arguments
#'
#' @export

# neater summary of stems when created

# This should be all stems, use the get_combinations and paste it onto the end of stems
# Then calculate the count by any present stems using the custom union_all

summary.stem <- function(object,dis_names = NULL, force_max = FALSE, ...){
  cat('Stems generated using::', tools::toTitleCase(object[1,'freq_or_outcome']), '\n')
  cat('Unique comorbid strings:', nrow(object), '\n')
  # calculate the number of each disease, divided by length.
  core_cols <- c('comorbid_column', 'Freq', 'position')
  dt <- object[,core_cols]

    max_combos <- nchar(gsub('[^;]', '', object$stem[1]))+1 #replace everything that isn't ';' with '',
  outlist <- list()

  for(i in 1:max_combos){
    cat('Doing', i, 'stem level \n')

    # generate unique combinations
    combos = unique_combos(dt$position, i)

    # if no combos; end loop.
    if(is.null(combos)){
      cat('No', i, 'combinations, ending process \n')
      break
    }
    combo_freq = calculate_group_frequency(combos, all_diseases = all_diseases, outcome_positions = 0, min_freq=0, tots = sum(object$Freq))
    outlist[[i]] <- combo_freq
  }

  # top ten of each list
  toplist <- lapply(outlist, function(x) x[order(-x[,'freq']),][1:10,!names(x) %in% c('outcome', 'propr_out')])

  if(is.null(dis_names)){
    cat('Did you know, if you pass disease names to the dis_name argument I can provide names of diseases! \n')
    cat('\n')
  }
  else{
    toplist <- lapply(toplist, function(x) data.frame(cbind(sapply(x[,!names(x) == 'freq'], function(y) disease_names[y]), 'freq'=x[,'freq']), stringsAsFactors = F))
  }

  # now collapse columns
  cleaned_top <- list()
  cleaned_top[[1]] <- toplist[[1]]
  names(cleaned_top[[1]]) <- c('unique_combinations', 'freq')
  for(item in 2:length(toplist)){
    x <- toplist[[item]]
    x[,'unique_combinations'] <- apply(x[,!names(x) == 'freq'],1, function(x) paste0(x, collapse='-'))

    cleaned_top[[item]] <- x[,c('unique_combinations', 'freq')]
  }

  clean_up <- function(freq) return(paste0(freq, ' (', round(freq/sum(object$Freq) *100, 1), ' %)'))
  cleaned_top_up <- lapply(cleaned_top, function(x) cbind(x[,'unique_combinations'], clean_up(as.numeric(x$freq))))
  cleaned_final_out <- data.frame(do.call(cbind, cleaned_top_up), stringsAsFactors = F)
  # now name
  # number of diseases
  clean_names <- do.call(paste, list('Number of diseases:', 1:max_combos))
  # freq
  freq_names <- rep('Frequency (%)', max_combos)

  names(cleaned_final_out) <-c(rbind(clean_names, freq_names))

  cat('Commonest disease combinations across all strings; note patients may be represented multiple times:\n')
  return(cleaned_final_out)
  }
