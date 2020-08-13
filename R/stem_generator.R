#' Generate disease stems
#' @description add something better here
#' @param poscolumn Vector of numbers which describe position of 1s in a comorbid string, generated using get_locales().
#' @param max_combos Number of maximum combinations to attempt.
#' @param all_diseases List of positions associated with each disease, generated using get_disease_counts().
#' @returns main_stem for each pattern up to a maximum set in max_combos.
#'
#' @export

# needs examples
# dealing with ties is hard

stem_generator = function(poscolumn, max_combos = 3, all_diseases,
                          min_freq = 0){
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
      rows_relevent = which(sapply(poscolumn, function(x) all(combos[1:i] %in% x)))
      working_stem[rows_relevent] = paste0(combos[,1:i], collapse='-')
    }

    else{
      combo_freq = calculate_group_frequency(combos, all_diseases)
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

# helper functions below #

calculate_death_proportion <- function(unique_combinations, disease_list, outcomes = 0){
  #set up disease list
  setups = get_list_pos(disease_list)
  combos_outcome = as.data.frame(unique_combinations, stringsAsFactors = F)
  combos_outcome[, c('freq', 'out')] = apply(unique_combinations, 1, function(x) {
    # find all rows
    all = reduce_set_overlap(sapply(x, FUN = setups))
    # intersection of all and outcomes
    outcome_bg = length(intersect(all, outcomes))
    return(c(length(all), outcome_bg/length(all)))
    })
  #drop no outcomes

  #reorder
  return(combos_outcome[order(combos_outcome[,ncol(combos_outcome)], decreasing = F),])
}

calculate_group_frequency = function(unique_combinations, disease_list){
  # determine the frequency of each unique combinations of groups
  setups = get_list_pos(disease_list)
  # setups does initial processing of the disease list
  combos_frequency = cbind(unique_combinations, apply(unique_combinations, 1, function(x) length(reduce_set_overlap(sapply(x, FUN = setups)))))
  # find the count of records associated with each combination of diseases
  # return these ordered
  return(combos_frequency[order(combos_frequency[,ncol(combos_frequency)], decreasing = F),])
}

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
