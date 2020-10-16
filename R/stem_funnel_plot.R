#' Funnel plot of combinations at a given stem level.
#'
#' @description Create a funnel plot for a binary outcome for each combination of diseases. Uses functionality provided by the \code{\link[funnelR]{fundata}} and \code{\link[funnelR]{funscore}} functions in the \href{https://cran.r-project.org/web/packages/funnelR/funnelR.pdf}{funnelR} package.
#'
#' @param comorbid_column A vector of character strings made up of 0s and 1s, or of factors coercible to character, all should be identical lengths.
#' @param outcome_column A numeric vector one if outcome occurred and zero if outcome did not occur. Should be the same length as \code{comorbid_column} with each element relating to the same record as the \code{comorbid_column}. If \code{outcome_column} is passed, then the stem will be generated based on combinations with the highest event rate.
#' @param cut_level A number indicating the level at which stems should be cut (i.e. the number of diagnoses).
#' @param use_outcome A logical indicating if stem derivation should be based on outcome rates or on frequency, defaults to \code{FALSE}.
#' @param maximal A logical that true if the 'maximal' stem is to be considered for each record, or if it is to be limited to only records with the same number of diagnoses as \code{cut_level}, default of \code{TRUE}.
#' @param max_x A number indicating the maximum frequency on the x axis, if left \code{NULL} then the maximum in the observed data will be used.
#' @param dis_names A list of disease names associated with each position of the comorbid string. If null, then the simple string is returned.
#' @return A list, first element is a ggplot object; a funnel plot with associated 95\% and 99\% control limits, second element is the underlying data.
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
#' max_x = NULL,
#' dis_names = c('heart disease', 'hypertension', 'chronic kidney disease', 'cancer')
#' )
#'
#' @export
#'

stem_funnel_plot = function(comorbid_column,
                            outcome_column,
                            cut_level = 2,
                            use_outcome = FALSE,
                            maximal = TRUE,
                            max_x = NULL,
                            dis_names = NULL){
   if(suppressWarnings(!all(sort(unique(outcome_column)) == c(0,1)))){stop('outcome column should be numeric ones and zeros')}

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
   d <- r <- stem <- score <- up <- lo <- up2 <- lo2 <- benchmark <- bl <- NULL

   dt = data.frame('comorbid' = comorbid_column)
   # merge stem by comorbid disease pattern

   dt = merge(dt, a[,c('comorbid_column', 'stem')], by.x = 'comorbid', by.y = 'comorbid_column', all.x=T)

   # cut stem at given level
   dt$cut_stem = split_stem(dt$stem, cut_level = cut_level, maximal = maximal)

   # aggregate outcomes ~ stem cut pattern.
   output = aggregate(outcome_column ~ dt$cut_stem, FUN = function(x) c('total' = length(x), 'outcome' = length(which(x == 1))))

   clean_output = do.call(data.frame, output)

   # need numerator (n) and denominator (d) to determine control limits & to plot
   names(clean_output) <- c('stem', 'd', 'n')

   # calculate rate
   clean_output$r = clean_output[,"n"]/clean_output[,"d"]

   # determine baseline
   bl = sum(clean_output[,"n"])/sum(clean_output[,"d"])

   # drop those with no events

   # sort out max_x
   if(is.null(max_x)){max_x <- max(clean_output$d)}

   if(!is.null(dis_names)){
      clean_output$stem <- name_stems(clean_output$stem, dis_names)
   }

   # calculate limits
   lim_data <- funnelR::fundata(clean_output, benchmark = bl, alpha = 0.95, alpha2 = 0.975, method='approximate', step = 1)

   #calculate outliers
   outliers_dat <- funnelR::funscore(clean_output, benchmark = bl, alpha=0.95, alpha2 = 0.975, method='approximate')
   cols <- c('#E6D72A', '#375E97')
   g1 = ggplot(aes(x=d, y=r), data=outliers_dat[outliers_dat$r >0 & outliers_dat$d <= max_x,])+
       geom_point(colour='black', size = 4+0.5, shape = 21)+
       geom_point(size = 4, alpha=0.3, aes(group = stem, colour = score))+
       xlab('Frequency')+
       geom_line(aes(x = d, y = up), data = lim_data[lim_data$d <= max_x,], colour='red',     linetype='dotted') +
       geom_line(aes(x = d, y = lo), data = lim_data[lim_data$d <= max_x,], colour='red',     linetype='dotted') +
       geom_line(aes(x = d, y = up2), data = lim_data[lim_data$d <= max_x,], colour='darkred', linetype='dotdash') +
       geom_line(aes(x = d, y = lo2), data = lim_data[lim_data$d <= max_x,], colour='darkred', linetype='dotdash') +
       geom_hline(aes(yintercept = benchmark), colour='black', data = lim_data[lim_data$d <= max_x,]) +
       coord_cartesian(ylim=c(0,1)) +
       scale_y_continuous(breaks=seq(0, 1, 0.1))+
       theme_bw() +
       scale_colour_manual(values = cols, labels = c('Within', 'Outside'), name = 'Within 95% control limits?')

   # return what is within 95% CI
   outliers_dat <- outliers_dat[,-5]
   names(outliers_dat) <- c('stem', 'total_number', 'number_events', 'rate', 'inside_95%', 'inside_97.5%')
   cat('Note that this includes all data, including that excluded from the plot with max_x \n')
   return(list(funnel_plot = g1, funnel_data = outliers_dat[order(-outliers_dat$rate),]))
}
