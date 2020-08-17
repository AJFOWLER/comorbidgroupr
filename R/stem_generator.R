#' Generate disease stems
#' @description Generate disease stems on the basis of combination frequency (if no \code{outcome_positions} is passed) or the proportion of records associated with an outcome event (if \code{outcome_positions} is passed).
#' @param poscolumn Vector of numbers which describe position of 1s in a comorbid string, generated using \code{.get_locales()}.
#' @param max_combos Number of maximum combinations to attempt.
#' @param all_diseases List of positions associated with each disease, generated using \code{.get_disease_counts()}.
#' @param min_freq Number between 0 and 1; minimum proportion of code combinations to be included in the stem. If \code{outcome_column} is passed, \code{min_freq} is the minimum event rate per combination to be considered.
#' @param outcome_positions Numeric vector where each number refers to a record that suffered a particular outcome.
#' @param tots Total number of records included to allow calculation of disease frequency.
#' @returns A dataframe with the \code{main_stem} for each pattern up to a maximum set in \code{max_combos}.
#' @examples
#' positions = list(c(1,2,3,4), c(3), c(3), c(2,3))
#' disease_counts = list(c(1), c(1,4), c(1,2,3,4), c(1))
#'
#' # generate stems on basis of frequency
#' stem_generator(poscolumn = positions,
#' max_combos = 2,
#' all_diseases = disease_counts,
#' outcome_positions = 0, tots=4)
#'
#' # generate stems on basis of outcome
#' stem_generator(poscolumn = positions,
#'  max_combos = 2,
#'  all_diseases = disease_counts,
#'  outcome_positions = c(2,3,4), tots = 4)
#'
#'
#' @export

# TIES

stem_generator = function(poscolumn, max_combos = 3, all_diseases,
                          outcome_positions = 0, min_freq = 0, tots){
  all_dis_count = sapply(all_diseases, length)

  # generate the base of the stem
  main_stem = sapply(poscolumn, function(x) {
    a = all_dis_count[x]
    # if only one disease, that is the base.
    if(length(a) == 1){
      return(x)}
    else{
      # else get the largest of those you select.
      return(which(all_dis_count == max(a))[1])}
  })

  # deal with 0000 later

  # work through each combination
  for(i in 2:max_combos){
    # generate empty 'working stem'
    working_stem = rep('', length(poscolumn))
    #record
    cat('Doing', i, 'stem level \n')
    # generate unique combinations
    combos = unique_combos(poscolumn, i)
    # if no combos; end loop.
    if(is.null(combos)){
      cat('No', i, 'combinations, ending process \n')
      break
    }
    # if only one combination, update.
    else if(nrow(combos) == 1){
      combo_freq = calculate_group_frequency(combos, all_diseases = all_diseases, outcome_positions = outcome_positions, min_freq=min_freq, tots = tots)

      rows_relevent = which(sapply(poscolumn, function(x) all(combos[1:i] %in% x)))
      if(nrow(combo_freq) == 0)
        {next}
      else{
        #logic needed here to enforce single combinations and associated frequency is accurate
        working_stem[rows_relevent] = paste0(combos[,1:i], collapse='-')}
    }

    else{
      combo_freq = calculate_group_frequency(combos, all_diseases = all_diseases, outcome_positions = outcome_positions, min_freq=min_freq, tots = tots)
      # work through each row (this has been ordered ascending so last item should be most frequent)
      for(rows in 1:nrow(combo_freq)){
        # select relevant combos
        rowers = combo_freq[rows, 1:i]
        # find relevant rows for that combination
        rows_relevant = which(sapply(poscolumn, function(x) all(rowers %in% x)))
        # if none relevant, next iteration.
        if(length(rows_relevant) == 0){
          next
        }
      # paste those rows onto working_stem
      working_stem[rows_relevant] = paste0(rowers, collapse='-')
      }}
    #at each loop for max_combo, paste on to the main stem
    main_stem = paste(main_stem, working_stem, sep=';')
  }
  # return main_stem when all done.
  return(main_stem)
}
