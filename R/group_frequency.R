#' Calculate group frequency for all unique combinations
#' @description Generate an ordered data.frame of different disease combinations on the basis of the most frequent or most strongly associated with outcomes.
#'
#' @param unique_combinations List of unique combinations of disease positions for a given number of combinations, generated using unique_combos
#' @param all_diseases List of positions associated with each disease, generated using get_disease_counts().
#' @param outcome_positions Numeric vector where each element refers to a record that suffered a particular outcome.
#' @param min_freq Number between 0 and 1; minimum proportion of code combinations to be included in the stem. If outcome_column is passed, min_freq is the minimum event rate per combination to be considered.
#' @param tots Numeric, total length of comorbid_column initially profiled to calculate frequency proportions to compare against min_freq
#'
#' @return data.frame ordered from lowest to highest proportion of those suffering outcomes (if outcome_positions entered) or number of records associated with that combination (if no outcome_positions entered)
#'
#' @examples
#' disease_counts = list(c(1), c(1,4), c(1,2,3,4), c(1))
#' unique_pos = structure(c(1, 1, 1, 2, 2, 3, 2, 3, 4, 3, 4, 4), .Dim = c(6L, 2L))
#' calculate_group_frequency(unique_combinations = unique_pos, all_diseases = disease_counts, tots = 4)
#'
#' @export


calculate_group_frequency <- function(unique_combinations, all_diseases, outcome_positions, min_freq=0, tots){
  # logic for parameter entry:
  #set up disease list
  setups = get_list_pos(all_diseases)

  combos_outcome = data.frame(unique_combinations, stringsAsFactors = F)

  returned = apply(combos_outcome, 1, function(x) {
    # find all rows
    all = reduce_set_overlap(sapply(x, FUN = setups, simplify = F))
    # intersection of all and outcomes
    outcome_bg = length(intersect(all, outcome_positions))
    return(cbind(length(all), outcome_bg))

  })
  combos_outcome = cbind(combos_outcome, t(returned))
  names(combos_outcome)[(ncol(combos_outcome)-1):ncol(combos_outcome)] = c('freq', 'outcome')
  combos_outcome$propr_out = combos_outcome$out/combos_outcome$freq

  # no ordering
  # no deletions
  # returns COUNTS
  if(outcome_positions == 0){
    # if no outcomes data, then just do based on freq
    combos_outcome = combos_outcome[combos_outcome$freq/tots >= min_freq, ]

    return(combos_outcome[order(combos_outcome$freq, decreasing = F),])
  }else{
    combos_outcome = combos_outcome[combos_outcome$propr_out >= min_freq, ]
    # if outcomes data, then do based on outcome, then freq
  return(combos_outcome[order(combos_outcome$propr_out, combos_outcome$freq, decreasing = F),])
    }
}


# helper functions below #
reduce_set_overlap <- function(vector_list){Reduce(intersect, vector_list[lengths(vector_list)>0])}

get_list_pos <- function(list){
  get_pos <- function(pos){
    if(pos == 0){
      return()
    }
    else{
      return(list[[pos]])}
  }
}
