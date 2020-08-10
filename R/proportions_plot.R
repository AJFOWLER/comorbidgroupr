#' Funnel plot of proportions
#'
#' @description
#'
#' @param
#'
#' @inheritParams make_disease_stem
#'
#' @export

stem_funnel_plot = function(comorbid_column, outcome, stem_cut = 2,x_lim=NULL){
  # names of diseases
  # axis titles
  # name those outside limits
  # use continuous outcome and standardise rate for age?

   a = make_stem(comorbid_column, max = stem_cut)

   dt = data.frame('comorbid' = comorbid_column)
   dt = merge(dt, a[,c('comorbid_column', 'stem')], by.x = 'comorbid', by.y = 'comorbid_column', all.x=T)

   dt$cut_stem = unlist(lapply(dt$stem, get_stem_maximal_split, stem_cut))
   output = aggregate(outcome ~ dt$cut_stem, FUN = function(x) c(length(x), length(which(x == 1))))

   clean_output = do.call(data.frame, output)
   rm(output)
   clean_output$rate = clean_output[,3]/clean_output[,2]

   bl = sum(clean_output[,3])/sum(clean_output[,2])

   # drop those with no events
   if(is.null(x_lim)){
   lim_data = baseline_limits(bl, max(clean_output$outcome.1))
   g1 = ggplot(aes(x=outcome.1, y=rate), data=clean_output[clean_output$rate >0,])+
        geom_point(colour='black', size = 4+0.5, shape = 21)+
        geom_point(size = 4, alpha=0.3, aes(fill = dt.cut_stem, colour = dt.cut_stem))+

      geom_line(aes(x = seq_, y = number.l_lim1), data = lim_data, colour='red',     linetype='dotted') +
      geom_line(aes(x = seq_, y = number.u_lim2), data = lim_data, colour='red',     linetype='dotted') +
      geom_line(aes(x = seq_, y = number.l_lim2), data = lim_data, colour='darkred', linetype='dotdash') +
      geom_line(aes(x = seq_, y = number.u_lim1), data = lim_data, colour='darkred', linetype='dotdash') +
      geom_hline(aes(yintercept = benchmark), colour='black', data = lim_data) +
      coord_cartesian(ylim=c(0,1)) +
      scale_y_continuous(breaks=seq(0, 1, 0.1))
   }
   else{
     lim_data = baseline_limits(bl, x_lim)

     g1 = ggplot(aes(x=outcome.1, y=rate), data=clean_output[clean_output$rate >0 & clean_output$outcome.1 <= x_lim,])+
       geom_point(colour='black', size = 4+0.5, shape = 21)+
       geom_point(size = 4, alpha=0.3, aes(fill = dt.cut_stem, colour = dt.cut_stem))+
       xlab('Frequency')+
       geom_line(aes(x = seq_, y = number.l_lim1), data = lim_data, colour='red',     linetype='dotted') +
       geom_line(aes(x = seq_, y = number.u_lim2), data = lim_data, colour='red',     linetype='dotted') +
       geom_line(aes(x = seq_, y = number.l_lim2), data = lim_data, colour='darkred', linetype='dotdash') +
       geom_line(aes(x = seq_, y = number.u_lim1), data = lim_data, colour='darkred', linetype='dotdash') +
       geom_hline(aes(yintercept = benchmark), colour='black', data = lim_data) +
       coord_cartesian(ylim=c(0,1)) +
       scale_y_continuous(breaks=seq(0, 1, 0.1))+
       theme_bw() +
       theme(legend.position = 'none')

   }
   # add logic to label those outside expected 99/95% CI

return(g1)

}


baseline_limits = function(baseline, max_number, lim1 = 99, lim2 = 95){
  seq_ = seq(0.001, max_number, max_number/75)

  number.l_lim1 <- baseline - abs(qnorm((1-(lim1/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))
  number.u_lim1 <- baseline + abs(qnorm((1-(lim1/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))

  number.l_lim2 <- baseline - abs(qnorm((1-(lim2/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))
  number.u_lim2 <- baseline + abs(qnorm((1-(lim2/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))

  lim_data <- data.frame(baseline, seq_, number.l_lim1, number.u_lim1, number.l_lim2, number.u_lim2)
  return(lim_data)
  }

