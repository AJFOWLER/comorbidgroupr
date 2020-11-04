context('test multiple_states functionality')

error_str <- c('0111', '020', '0300')
letter_str <- c('A000', 'A023', '0011')
tstr <- c('3100', '0200','0110','1111', '2000')

test_that('data entry appropriate',{
  factor_str <- factor(c('0101', '3000', '3100'))

  expect_equal(nrow(multiple_state_processor(factor_str)), 3)

  expect_error(multiple_state_processor(letter_str), "There are non-numeric characters in the comorbid strings, please remove", fixed=T)

  expect_error(multiple_state_processor(error_str), "comorbid column must be the same length for each record", fixed=T)

  expect_error(multiple_state_processor(comorbid_column = factor_str, dis_names = c('first', 'second', 'third')), "Disease names should be the same length as comorbid string", fixed=T)
          })

test_that('multiple_states function works appropriately',{
  ms <- multiple_state_processor(tstr)
  expect_equal(nrow(ms), 5)
  expect_equal(ncol(ms), 3)
  # length of the string should be 7 (max are 3211)
  expect_equal(nchar(ms$master_str[[1]]), 7)
  # manually reviewed the cols, row 4 should be 0100000
  expect_equal(ms$master_str[4], '0100000')
  # row 5 should be 001100
  expect_equal(ms$master_str[5], '0011000')
  # ensure that the output str are identical lengths
  expect_equal(length(unique(nchar(ms[,'master_str']))), 1)
  })

test_that('adding dis_names works appropriately',{
  tst_names <- c('ckd', 'cancer', 'hf', 'another')
  ms <- multiple_state_processor(tstr, dis_names = tst_names)
  # three states for ckd, two for cancer, one for others
  # two element list
  expect_equal(length(ms), 2)

  expect_equal(ms[[2]], c("ckd1","ckd2", "ckd3", "cancer1", "cancer2", "hf1", "another1"))
  expect_equal(nchar(ms[[1]][1,'master_str']), length(ms[[2]]))

  # two element list
  expect_equal(length(ms), 2)

  #if you don't provide names, they should be flagged
  op <- capture.output(multiple_state_processor(tstr))
  expect_equal(op[[1]], "If you provide column names I will reassign them for you ")
  # if you do, then you should get a list.
  op <- capture.output(multiple_state_processor(tstr, tst_names))
  # should be a list
  expect_equal(op[[1]], '[[1]]')
  })

test_that('edge case failures',{
  #all numbers the same
  silly_str <- c('1111','1111', '1111', '2222')
  odd_str <- c('3300', '0033', '0000') # this causes a failure
  one_col_all_zero <- c('032', '022', '033') # as does this
  ms <- multiple_state_processor(silly_str)
  expect_equal(nrow(ms), 2) # three are the same, final one is solo
  expect_equal(ms$master_str[1], '10101010')
  expect_equal(multiple_state_processor(one_col_all_zero)$master_str[2], '0001010')
  expect_equal(multiple_state_processor(odd_str)$master_str[3], '001001000000')
})

test_that('identifying multiple states function works appropraitely', {
  expect_error(.identify_multiple_states(letter_str), "There are non-numeric characters in the comorbid strings, please remove", fixed=T)
  expect_equal(.identify_multiple_states(c('010','011','110')), "single")
  expect_equal(.identify_multiple_states(c('020','011','110')), "multiple")
  expect_equal(.identify_multiple_states(c('222','333','000')), "multiple")

})

test_that('multiple_state_processor integrates with make_stem',{
  # run this through to ensure no errors
  df <- data.frame(comorbid_column = c('23001', '11101', '21011', '22111', '23001'))
  # do multistate
  ms <- multiple_state_processor(df$comorbid_column, dis_names = c('first', 'second', 'third', 'fourth', 'fifth'))
  # merge back in
  df <- merge(df, ms[[1]][,c('comorbid_column', 'master_str')], by.x = 'comorbid_column', by.y='comorbid_column', all.x=T)
  # Make stem
  stem <- make_stem(df$master_str, max=2)
  # summary using ms[[2]] object as names
  sumstem <- summary(stem, dis_names = ms[[2]])
  # all looks correct
  expect_equal(sumstem[1,1], 'fifth1')
  expect_equal(nrow(ms[[1]]), 4)
  })
