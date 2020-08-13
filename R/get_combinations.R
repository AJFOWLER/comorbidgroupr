#' Get all patterns of diseases.
#' @description Return combinations of different patterns of diseases to a specified number of combinations
#'
#' @param positons Vector of all positions for each element of comorbid_column generated using .get_locales
#' @param combinations Maximum number of disease combinations to be generated (default = 2)
#' @return matrix of unique combinations (if unique_combos, else all combinations)
#' @examples
#' positions = list(c(1,2,3,4), c(3), c(3), c(2,3))
#' unique_combos(positions, combinations = 2)
#'
#' @export
#' @name combinations
NULL

#' @rdname combinations
unique_combos = function(positions, combinations=2){
  # lapply to positions the get_combos function
  all_combos = lapply(positions, function(x) get_combos(x, combinations = combinations))
  # remove NA lists (from error capture in get_combos)
  all_combos_nan = all_combos[!is.na(all_combos)]
  # generate matrix of unique combinations
  unique_combos = unique(do.call(rbind, all_combos_nan))
  return(unique_combos)
}

#' @rdname combinations
get_combos = function(positions, combinations = 2){
  #take all positions and return unique combinations
  position_ = unlist(positions)
  # determine length of position (as combn cannot accept n<m)
  len_pos_ = length(position_)

  if(len_pos_ == 1|| combinations > len_pos_){
    #NA if n<m or len_pos_ 1 (only one combination of 1!)
    return(NA)
  }
  else{
    # generate all combinations if n>m
    return(t(utils::combn(position_, combinations)))
  }
}
