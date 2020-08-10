#' Make combinations of diseases ('stems') up to a maximum level.
#' @description Generate disease stems from a vector of character strings.
#' @param comorbid_column A vector of character strings made up of 0s and 1s, or of factors coercible to character, all should be identical lengths.
#' @param max The maximum number of combinations to be considered, if left as 'default' then a maximum based on code frequency will be used.
#' @param min_freq Numeric; minimum frequency of code combinations to be included in the stem.
#' @return data.frame with four columns, one row per unique comorbid_string pattern, comorbid pattern, Frequency of pattern, positions of diseases, and full stem.
#' @examples
#' comorbid_column <- c('00010', '01000', '01110', '11101')
#' make_stem(comorbid_column, max='default')
#'
#' @export

make_stem = function(comorbid_column,
                     max='default',
                     min_freq = 0){
  # first check data
  if(!check_strings_equal(comorbid_column)){stop('comorbid column must be the same length for each record')}

  if(!is.numeric(max) && max != 'default'){stop('max must be a single number or deafult')}

  if(!is.numeric(min_freq))stop('min_freq must be a single number number')

  # identify unique combinations
  dt = .get_unique_sets(comorbid_column)

  # get disease counts
  all_diseases = get_disease_counts(comorbid_column)

  dt$position = apply(dt['comorbid_column'],1, .get_locales)

  if(max == 'default'){
    max_combos = determine_default_combinations(comorbid_column)
  }
  else{
    max_combos = max
  }

  cat('Using', max_combos, 'combinations \n')

  dt$stem = stem_generator(dt$position, max_combos = max_combos, all_diseases, min_freq = min_freq)

  return(dt)
  }

# helper functions below #
.get_unique_sets = function(comorbid_column){
  # return unique values of comorbid_column
  d = data.frame(t(table(comorbid_column)))
  return(d[,2:3])
}

.get_locales = function(str_){
  #find all '1' in each comorbid_column
  return(unlist(gregexpr('1', str_)))
}
