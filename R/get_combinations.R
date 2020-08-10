#' Get all combinations and unique combinations
#' @description thing to do
#' @param positons
#' @param combinations = 2
#' @name combinations
#' @return
#' @examples
#' positions =
#' get_combos()
#' unique_combos()
#'
#' @export
#'
NULL

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
#' @rdname combinations
unique_combos = function(positions, combo_numbers=2){
  # lapply to positions the get_combos function
  all_combos = lapply(positions, function(x) get_combos(x, combinations = combo_numbers))
  # remove NA lists (from error capture in get_combos)
  all_combos_nan = all_combos[!is.na(all_combos)]
  # generate matrix of unique combinations
  unique_combos = unique(do.call(rbind, all_combos_nan))
  return(unique_combos)
}
