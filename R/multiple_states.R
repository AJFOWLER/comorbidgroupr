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

 #a<-c('3003', '2100', '3103', '0114')

multiple_state_processor <- function(comorbid_column, dis_names = NULL){
  if(!check_strings_equal(comorbid_column)){stop('comorbid column must be the same length for each record')}
  if(any(grepl('[^0-9]',comorbid_column) == TRUE))stop('There are non-numeric characters in the comorbid strings, please remove')

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
  # iterate over each position of the string
  for(pos in 1:n){
    # identify the maximal number for each position
    max_n <- max_by_po[pos]
    # identify the relevant element from the string
    element <- as.numeric(substr(tt$comorbid_column, pos, pos))
    #find which rows have which elements
    if(max_n == 1){
      # if 1/0, then rows == those where 1 is present
      rows <- which(element=='1')
    }else{
      #otherwise, make a vector of rows which have which values
    str_positions <- sapply(1:max_n, function(x) which(element == x))
    }# now replace those elements
    # create empty string to work with
    tt$str <- paste0(rep('0', max_n), collapse='')

    if(max_n == 0){
      # if there are NO values (i.e. 0 in all strings), then simply paste empty column to str
      cat("empty column - I'll fill it with 0")
      tt$str <- paste0(rep('0', 1), collapse='')
      }

    else{
      # if max_n >= 0
      # Iterate over each element up to the maximum for that setting
      for(position in 1:max_n){
        # if only one element, we made rows above, so skip this rows creation
        if(exists('rows')==FALSE){
        rows <- str_positions[[position]]
        }
        # If there aren't any rows with this position count, next loop after removing rows
        if(length(rows) == 0){rm(rows);next}

        #select rows where the criteria is met
        int_str <- tt$str[rows]
        # paste a '1' in the relevant position within the string
        substr(int_str, position, position) <- '1'
        # put back into tt
        tt[rows, 'str'] <- int_str
        # remove rows so we can test with exists above.
        rm(rows)
    }
    }
    # paste togther str and master_str to make new master_str
    tt[,'master_str'] <- apply(tt, 1, function(x) paste0(x['master_str'], x['str']))
    # clean up - remove str for this loop
    tt$str <- NULL
  }
  # dis_name handling
  if(!is.null(dis_names)){
    if(length(dis_names) != n)stop('Disease names should be the same length as comorbid string')
    # replicate each dis_name by the maximal position number
    d_name <- rep(dis_names, max_by_po)
    # create numbers
    numbering <- lapply(max_by_po, function(x) 1:x)
    # make to vector
    numbers_ready <- Reduce(c, numbering)
    # paste together elemtwise
    new_names <- do.call(paste0, list(d_name, numbers_ready))
    # return tt and names
    return(list(tt, new_names))
  }
  cat('If you provide column names I will reassign them for you \n')
  # only return tt
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
