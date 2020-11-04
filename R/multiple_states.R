#' Multiple states
#'
#' @description Sometimes, diseases may have multiple states, for example a different stage of cancer or kidney disease. This function processes comorbid strings that have multiple states
#' to enable them to be used within the make_stems algorithm
#' @param comorbid_column A vector of character strings made up of numbers 0-9 or of factors coercible to character, all should be identical lengths.
#' @param dis_names A list of disease names associated with each position as used in \code{\link{get_disease_counts}}
#'
#' @returns A list, the first element of which is a data.frame which includes \code{master_str}, which is can be joined with original data.
#' @examples
#'
#' @export

#' a<-c('3003', '2100', '3103', '0114')

multiple_state_processor <- function(comorbid_column, dis_names = NULL){
  if(!check_strings_equal(comorbid_column)){stop('comorbid column must be the same length for each record')}

  comorbid_column <- to_string(comorbid_column)
  # number of items in string
  n <- nchar(comorbid_column[1])

  # maximal state per item
  max_by_po <- sapply(1:n, function(x) max(as.numeric(substr(comorbid_column, x, x))))

  # remake comorbid_column based on max, value becomes relevant pos that ==1
  # e.g. for a position with max 3, 000 = no disease, 100 = 1, 010 = 2, 001 = 3

  # define a new position for each string
  tt <- data.frame(table(comorbid_column))

  # now we need to assign elements
  tt$master_str <- character(nrow(tt))

  for(pos in 1:n){
    max_n <- max_by_po[pos]
    element <- as.numeric(substr(tt$comorbid_column, pos, pos))
    #find which rows have which elements
    if(max_n == 1){
      str_positions <- which(element=='1')
    }else{
    str_positions <- sapply(1:max_n, function(x) which(element == x))
    }# now replace those elements

    tt$str <- paste0(rep('0', max_n), collapse='')

    if(max_n == 0){

      cat("empty column - I'll fill it with 0")
      tt$str <- paste0(rep('0', 1), collapse='')

      }else{

        for(position in 1:length(str_positions)){

      rows <- str_positions[[position]]

      if(length(rows) == 0){next}
      if(!is.list(str_positions)){ # which returns a vector outside lapply
        rows <- str_positions
        }

      int_str <- tt$str[rows]
      substr(int_str, position, position) <- '1'
      tt[rows, 'str'] <- int_str
    }
    }
    tt[,'master_str'] <- apply(tt, 1, function(x) paste0(x['master_str'], x['str']))

    tt$str <- NULL
  }
  if(!is.null(dis_names)){
    if(length(dis_names) != n)stop('Disease names should be the same length as comorbid string')
    d_name <- rep(dis_names, max_by_po)
    numbering <- lapply(max_by_po, function(x) 1:x)
    numbers_ready <- Reduce(c, numbering)
    new_names <- do.call(paste0, list(d_name, numbers_ready))
    return(list(tt, new_names))
  }
  cat('If you provide column names I will reassign them for you \n')
  return(tt)
}

.identify_multiple_states <-function(comorbid_columns){
  # staged identification
  # find any strings with non-numeric, these should stop
  if(any(grepl('[^0-9]',comorbid_columns) == TRUE))stop('There are non-numeric characters in the comorbid strings, please remove')

  ##find those that aren't 0-1
  if(any(grepl('[^0-1]', comorbid_columns) == TRUE))
    # If we find non 0-1 --> multiple states
  {return('multiple')}

  else
    # Otherwise it is single state
  {return('single')}
}
