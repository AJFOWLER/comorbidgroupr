#' Make combinations of diseases ('stems') up to a maximum level
#' @description Generate disease stems from a vector of character strings.
#' @param comorbid_column A vector of character strings made up of 0s and 1s, or of factors coercible to character, all should be identical lengths.
#' If data contains elements with multiple states (i.e. where a character element may have a value of >1), then please see \code{\link{multiple_state_processor}}.
#' @param max The maximum number of combinations to be considered, if left as 'default' then a maximum based on code frequency will be calculated using \code{determine_default_combinations()}.
#' @param min_freq Number between 0 and 1; minimum proportion of code combinations to be included in the stem. If \code{outcome_column} is passed, \code{min_freq} is the minimum event rate per combination to be considered.
#' @param outcome_column A numeric vector one if outcome occurred and zero if outcome did not occur. Should be the same length as \code{comorbid_column} with each element relating to the same record as the \code{comorbid_column}. If \code{outcome_column} is passed, then the stem will be generated based on combinations with the highest event rate.
#' @param use_outcome Logical if to use outcome variable for stem generation.
#' @return data.frame with one row per unique \code{comorbid_string} pattern and columns: \code{comorbid_string} pattern, frequency of string pattern, positions within the pattern, stem and if frequency or outcome was used.
#' @examples
#'
#' comorbid_column <- c('00010', '01000', '01110', '11101')
#'
#' make_stem(comorbid_column, max='default', min_freq = 0, outcome_column=NULL)
#'
#' @export

make_stem <- function(comorbid_column,
                     max='default',
                     min_freq = 0,
                     outcome_column = NULL,
                     use_outcome = FALSE){
  # first check data
  if(!check_strings_equal(comorbid_column)){stop('comorbid column must be the same length for each record')}

  if(!is.numeric(max) && max != 'default'){stop('max must be a single number or deafult')}

  if(!is.numeric(min_freq)){stop('min_freq must be a single number number')}

  if(.identify_multiple_states(comorbid_column) == 'multiple')stop('The provided data is not just made up of 1s and 0s.\n Please see the multiple_state_processor documentation for how to handle this \n')

  if(!is.null(outcome_column)){
    if(is.factor(outcome_column)){outcome_column <- as.numeric(as.character(outcome_column))}

    if(is.character(outcome_column)){outcome_column <- as.numeric(outcome_column)}

    if(!all(sort(unique(outcome_column)) == c(0,1))){stop('outcome column should be numeric ones and zeros')}

    if(length(outcome_column) != length(comorbid_column)){stop('outcome column should have the same number of observations as comorbid_column')}

    outcome_positions = which(outcome_column == 1)
  }
  else
    {outcome_positions = 0}

  # identify unique combinations
  dt = .get_unique_sets(comorbid_column)

  # get disease counts
  all_diseases = get_disease_counts(comorbid_column)

  dt$position = apply(dt['comorbid_column'],1, .get_locales)

  if(max == 'default'){
    max_combos = determine_default_combinations(comorbid_column, default = 2.5)
  }
  else{
    max_combos = max
  }

  cat('Using', max_combos, 'combinations \n')

  dt$stem = stem_generator(dt$position, max_combos = max_combos, all_diseases = all_diseases, outcome_positions = outcome_positions, min_freq = min_freq, tots = length(comorbid_column), use_outcome = use_outcome)

  # if no diseases (position == '-1') then step is just empty
  dt[dt$position == '-1', c('position', 'stem')] = c('', paste0(rep(';', max_combos-1), collapse=''))

  # flag
  dt$freq_or_outcome = ifelse(length(outcome_positions) == 1 | use_outcome == FALSE, 'frequency', 'outcome')

  # If outcomes data is provided, then give count
  if(length(outcome_positions) > 1){
  # On a per pattern basis, add in outcomes (this is for cardinality assessment)
  # because this is now effectively at a pattern level, we can do some basic grouping by pattern to get frequency:
  outcomes <- tapply(outcome_column, comorbid_column, sum)
  # now turn the array into a data.frame
  outcome_bind <- data.frame(outcomes)
  # name appropriately
  outcome_bind$comorbid_column = row.names(outcomes)
  # merge with a left join
  dt <- merge(dt, outcome_bind, by.x='comorbid_column', by.y='comorbid_column', all.x=T)
  }

  # Add S3 class of stem
  class(dt) <-append('stem', class(dt))

  return(dt)
  }

# helper functions below #
.get_unique_sets = function(comorbid_column){
  # return unique values of comorbid_column
  d = data.frame(t(table(comorbid_column)))
  return(d[,2:3])
}

.get_locales = function(str_){
  return(unlist(gregexpr('1', str_)))
}


.make_zero <- function(x){if(identical(x,numeric(0))){0}else{x}}
# sometimes if missing outcome columns, the exclusive setup becomes messy, so change empty
# numeric inot 0 with this helper
