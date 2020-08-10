# if we let people generate 'death strings'?




# generate test_dat

#test_dat = read.csv("C:/Users/Alex/Dropbox/PhD/QoL/Routine_PROMS_data/HIP_PROMS/Hip_201718.csv")
#test_dat
# names(test_dat)
# comorbid_string = do.call(paste0, test_dat[,13:24])
# comorbid_string = gsub('9', '0', comorbid_string)
# dat = make_stem(comorbid_string, max = 6)
# o_dat = data.frame('comorbid'=comorbid_string)
# # #
#  o_dat = merge(o_dat, dat[, c('comorbid_column', 'stem')], by.x = 'comorbid', by.y = 'comorbid_column', all.x=T )
# # #
#
#  o_dat$maxstem3 = unlist(sapply(o_dat$stem, get_stem_maximal_split, 3))
# # tt = table(o_dat$maxstem6)
#
# o_dat$readmitted = test_dat$Post.Op.Q.Readmitted
# o_dat$wound = test_dat$Post.Op.Q.Bleeding
# o_dat$wound[o_dat$wound == 9] = 2
#
# output = aggregate(wound ~ maxstem2, data = o_dat, function(x) c(length(x), length(which(x == 1))))
# output = do.call(data.frame, output)
#
# # benchmark
# benchmark = sum(output$wound.2)/sum(output$wound.1)
#
# # limits of control
# lim1 = 99
# lim2 = 95
# seq_ = seq(0.001, 5000, 50)
#
# number.l_lim1 <- benchmark - abs(qnorm((1-(lim1/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))
# number.u_lim1 <- benchmark + abs(qnorm((1-(lim1/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))
#
# number.l_lim2 <- benchmark - abs(qnorm((1-(lim2/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))
# number.u_lim2 <- benchmark + abs(qnorm((1-(lim2/100))/2)) * sqrt((benchmark*(1-benchmark)) / (seq_))
#
# lim_data <- data.frame(benchmark, seq_, number.l_lim1, number.u_lim1, number.l_lim2, number.u_lim2)
#
# output$Rate = output$wound.2/output$wound.1
#
# g1 = ggplot(aes(x=wound.1, y=Rate), data=output[output$wound.1 <5000,])+
#   geom_point(colour='black', size = 4+0.5, shape = 21)+
#   geom_point(size = 4, alpha=0.3, aes(fill = maxstem2, colour = maxstem2))+
#
#   geom_line(aes(x = seq_, y = number.l_lim1), data = lim_data, colour='red',     linetype='dotted') +
#   geom_line(aes(x = seq_, y = number.u_lim2), data = lim_data, colour='red',     linetype='dotted') +
#   geom_line(aes(x = seq_, y = number.l_lim2), data = lim_data, colour='darkred', linetype='dotdash') +
#   geom_line(aes(x = seq_, y = number.u_lim1), data = lim_data, colour='darkred', linetype='dotdash') +
#   geom_hline(aes(yintercept = benchmark), colour='black', data = lim_data) +
#   coord_cartesian(ylim=c(0,1)) +
#   scale_y_continuous(breaks=seq(0, 1, 0.1))
#
# output[order(output$Rate, decreasing = T),]
#
# # observed/expected


#length(table(a))
# 545 unique combinations
# #t = table(b)
# #tt = data.frame(t[order(t, decreasing = T)], stringsAsFactors = F)
# #tt[,'b'] = as.character(tt[,'b'])
#
# # get_unique_sets = function(str_){return(table(str_))}
#
# get_locales = function(str_){
#   return(unlist(gregexpr('1', str_)))
# }
#
# #tt$pos = apply(tt['b'],1, get_locales)
#
# #t_pos = tt$pos[545]
#
# get_combos = function(positions, level_ers = 5){
#   position_ = unlist(positions)
#   len_pos_ = length(position_)
#   if(len_pos_ == 1|| level_ers > len_pos_){
#     return(NA)
#   }
#   else{
#     return(t(combn(position_, level_ers)))
#   }
# }
#
# unique_combos = function(positions, combo_numbers){
#   all_combos = lapply(positions, function(x) get_combos(x, level_ers = combo_numbers))
#   all_combos_nan = all_combos[!is.na(all_combos)] # drop NA lists
#   unique_combos = unique(do.call(rbind, all_combos_nan))
#   return(unique_combos)
# }
#
# #combos = unique_combos(l,2)
#
# calculate_group_frequency = function(combos, disease_list){
#   setups = get_list_pos(disease_list)
#   combos_c = cbind(combos, apply(combos, 1, function(x) length(reduce_set_overlap(sapply(x, FUN = setups)))))
#   return(combos_c[order(combos_c[,ncol(combos_c)], decreasing = F),])
# }
#
# #two_freq = calculate_group_frequency(combos, all_diseases)
#
# stem_generator = function(poscolumn, max_combos, all_diseases){
#
#   main_stem = sapply(poscolumn, function(x) {
#     a = all_dis_count[x]
#   if(length(a) == 1){
#     return(x)}
#     else{
#     return(which(all_dis_count == max(a)))}
#     })
#
#   #deal with 0000 later
#
#   for(i in 2:max_combos){
#     working_stem = rep('', length(poscolumn))
#     cat('Doing', i, 'stem level \n')
#     combos = unique_combos(poscolumn, i)
#     if(is.null(combos)){
#       cat('No', i, 'combinations, ending process \n')
#       break
#     }
#
#     else if(nrow(combos) == 1){
#       rows_relevent = which(sapply(poscolumn, function(x) all(combos[1:i] %in% x)))
#       working_stem[rows_relevant] = paste0(rowers, collapse='-')
#     }
#
#     else{combo_freq = calculate_group_frequency(combos, all_diseases)
#     for(rows in 1:nrow(combo_freq)){
#       rowers = combo_freq[rows, 1:i]
#       rows_relevant = which(sapply(poscolumn, function(x) all(rowers %in% x)))
#       if(length(rows_relevant) == 0){
#         next
#       }
#
#       working_stem[rows_relevant] = paste0(rowers, collapse='-')
#     }}
#     main_stem = paste(main_stem, working_stem, sep=';')
#   }
#   return(main_stem)
# }
#
# # stem processing
# get_stem_maximal_split = function(stems, cut_level = 1){
#   s_ = unlist(strsplit(stems, ';'))
#   s_ = s_[nchar(s_) > 0]
#   if(length(s_) < cut_level){
#     return(s_[length(s_)]) # maximum stem point
#   }
#   else{
#     return(s_[cut_level])
#   }
# }
# # tt$all_stems = stem_generator(tt$pos, 8, all_diseases = all_diseases)
# #
# # c = data.frame('string' = b)
# # d = merge(c, tt[,c('b', 'all_stems')], by.x = 'string', by.y = 'b', all.x=T)
# #
# # #lapply(tt$all_stems, get_stem_maximal_split, 5)
# # d$fivstem = unlist(lapply(d$all_stems, get_stem_maximal_split,5))
#
#   # for(rows in 1:nrow(two_freq)){
# #   rowers = two_freq[rows,1:2]
# #   rows_relevant = which(sapply(tt$pos, function(x) all(rowers %in% x)))
# #   tt[rows_relevant,'workingstem'] = paste0(rowers, collapse='-')
# # }
# #
# # tt$main_stem = paste0()
# #
# # apply(two_freq, 1, function(x) x[1:2] )
# #
# #
# # #
# #
# # # get all two way combinations and get unique combos
# # all_two_combos = apply(tt['pos'], 1, get_combos)
# # all_two_combos = all_two_combos[!is.na(all_two_combos)] # drop NA
# # unique_combos = unique(do.call(rbind, all_two_combos))
# #
# # # get all two way combinations and get unique combos
# # all_three_combos = apply(tt['pos'], 1, get_combos, 3)
# # all_three_combos = all_three_combos[!is.na(all_three_combos)] # drop NA
# # unique_combos = unique(do.call(rbind, all_three_combos))
# #
# #
# # all_three_combos = apply(tt['pos'], 1, get_combos, 4)
# # all_three_combos = all_three_combos[!is.na(all_three_combos)] # drop NA
# # unique_combos = unique(do.call(rbind, all_three_combos))
