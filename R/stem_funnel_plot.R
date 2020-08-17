#' Funnel plot of combinations at a given stem level.
#'
#' @description Create a funnel plot for a binary outcome for each combination of diseases.
#'
#' @param comorbid_column A vector of character strings made up of 0s and 1s, or of factors coercible to character, all should be identical lengths.
#' @param outcome_column A numeric vector one if outcome occurred and zero if outcome did not occur. Should be the same length as \code{comorbid_column} with each element relating to the same record as the \code{comorbid_column}. If \code{outcome_column} is passed, then the stem will be generated based on combinations with the highest event rate.
#' @param cut_level A number indicating the level at which stems should be cut (i.e. the number of diagnoses).
#' @param use_outcome A logical indicating if stem derivation should be based on outcome rates or on frequency, defaults to \code{FALSE}.
#' @param maximal A logical that true if the 'maximal' stem is to be considered for each record, or if it is to be limited to only records with the same number of diagnoses as \code{cut_level}, default of \code{TRUE}.
#' @param max_x A number indicating the maximum frequency on the x axis, if left \code{NULL} then the maximum in the observed data will be used.
#' @return A list, first element is a funnel plot with associated 95\% and 99\% control limits, second element is the underlying data.
#' @examples
#'
#' comorbid_column <- c('1110', '0010', '0010', '0110')
#'
#' outcome_column <- c(1,0,0,1)
#'
#' stem_funnel_plot(comorbid_column = comorbid_column,
#' outcome_column = outcome_column,
#' use_outcome = FALSE,
#' maximal = TRUE,
#' max_x = NULL)
#'
#' @export
#'

# names of diseases

# axis titles

# name those outside limits / get those outside limits

stem_funnel_plot = function(comorbid_column,
                            outcome_column,
                            cut_level = 2,
                            use_outcome = FALSE,
                            maximal = TRUE,
                            max_x = NULL){
   if(!all(sort(unique(outcome_column)) == c(0,1))){stop('outcome column should be numeric ones and zeros')}

   if(length(outcome_column) != length(comorbid_column)){stop('outcome column should have the same number of observations as comorbid_column')}

   if(!is.logical(use_outcome)){(stop('use_outcome should be either TRUE or FALSE'))}

   if(!is.logical(maximal))stop('maximal should be a logical of either TRUE or FALSE')

   if(!is.null(max_x) & !is.numeric(max_x)){stop('max_x should be either NULL or a number')}

   if(use_outcome == TRUE){
      # make stem with outcome
      a = make_stem(comorbid_column, max = cut_level, outcome_column = outcome_column)
   }
   else{
      # make stem without outcome
      a = make_stem(comorbid_column, max = cut_level, outcome_column = NULL)
   }
   # setup NULL objects for name space
   outcome_column.1 <- outcome_column.2 <- rate <- dt.cut_stem <- NULL
   seq_ <- number.l_lim1 <- number.u_lim1 <- number.l_lim2 <- number.u_lim2 <- baseline <-  NULL

   dt = data.frame('comorbid' = comorbid_column)
   # merge stem by comorbid disease pattern

   dt = merge(dt, a[,c('comorbid_column', 'stem')], by.x = 'comorbid', by.y = 'comorbid_column', all.x=T)

   # cut stem at given level
   dt$cut_stem = split_stem(dt$stem, cut_level = cut_level, maximal = maximal)

   # aggregate outcomes ~ stem cut pattern.
   output = aggregate(outcome_column ~ dt$cut_stem, FUN = function(x) c(length(x), length(which(x == 1))))

   clean_output = do.call(data.frame, output)

   rm(output)

   # calculate rate
   clean_output$rate = clean_output[,3]/clean_output[,2]

   # determine baseline
   bl = sum(clean_output[,3])/sum(clean_output[,2])

   # drop those with no events
   if(is.null(max_x)){
   lim_data = .baseline_limits(bl, max(clean_output$outcome_column.1))
   g1 = ggplot(aes(x=outcome_column.1, y=rate), data=clean_output[clean_output$rate >0,])+
        geom_point(colour='black', size = 4+0.5, shape = 21)+
        geom_point(size = 4, alpha=0.3, aes(fill = dt.cut_stem, colour = dt.cut_stem))+

      geom_line(aes(x = seq_, y = number.l_lim1), data = lim_data, colour='red',     linetype='dotted') +
      geom_line(aes(x = seq_, y = number.u_lim2), data = lim_data, colour='red',     linetype='dotted') +
      geom_line(aes(x = seq_, y = number.l_lim2), data = lim_data, colour='darkred', linetype='dotdash') +
      geom_line(aes(x = seq_, y = number.u_lim1), data = lim_data, colour='darkred', linetype='dotdash') +
      geom_hline(aes(yintercept = baseline), colour='black', data = lim_data) +
      coord_cartesian(ylim=c(0,1)) +
      scale_y_continuous(breaks=seq(0, 1, 0.1))
   }
   else{
      # if max_x !is.null, then use max_x to generate baseline limits.
     lim_data = .baseline_limits(bl, max_x)

     g1 = ggplot(aes(x=outcome_column.1, y=rate), data=clean_output[clean_output$rate >0 & clean_output$outcome_column.1 <= max_x,])+
       geom_point(colour='black', size = 4+0.5, shape = 21)+
       geom_point(size = 4, alpha=0.3, aes(fill = dt.cut_stem, colour = dt.cut_stem))+
       xlab('Frequency')+
       geom_line(aes(x = seq_, y = number.l_lim1), data = lim_data, colour='red',     linetype='dotted') +
       geom_line(aes(x = seq_, y = number.u_lim2), data = lim_data, colour='red',     linetype='dotted') +
       geom_line(aes(x = seq_, y = number.l_lim2), data = lim_data, colour='darkred', linetype='dotdash') +
       geom_line(aes(x = seq_, y = number.u_lim1), data = lim_data, colour='darkred', linetype='dotdash') +
       geom_hline(aes(yintercept = baseline), colour='black', data = lim_data) +
       coord_cartesian(ylim=c(0,1)) +
       scale_y_continuous(breaks=seq(0, 1, 0.1))+
       theme_bw() +
       theme(legend.position = 'none')

   }
   names(clean_output) = c('cut_stem', 'frequency', 'proportion_with_outcome', 'rate_of_outcome')
return(list(funnel_plot = g1, funnel_data = clean_output))
}


.baseline_limits = function(baseline, max_number, lim1 = 99, lim2 = 95){
  #determine baseline limits; adapted from funnelR
  # null objects for name_spacing.
  seq_ <- number.l_lim1 <- number.u_lim1 <- number.l_lim2 <- number.u_lim2 <- NULL
  seq_ = seq(0.001, max_number, max_number/75)

  number.l_lim1 <- baseline - abs(qnorm((1-(lim1/100))/2)) * sqrt((baseline*(1-baseline)) / (seq_))
  number.u_lim1 <- baseline + abs(qnorm((1-(lim1/100))/2)) * sqrt((baseline*(1-baseline)) / (seq_))

  number.l_lim2 <- baseline - abs(qnorm((1-(lim2/100))/2)) * sqrt((baseline*(1-baseline)) / (seq_))
  number.u_lim2 <- baseline + abs(qnorm((1-(lim2/100))/2)) * sqrt((baseline*(1-baseline)) / (seq_))

  lim_data <- data.frame(baseline, seq_, number.l_lim1, number.u_lim1, number.l_lim2, number.u_lim2)
  return(lim_data)
}
