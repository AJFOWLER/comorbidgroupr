context('grouping function')

unequal_strings = c('111', '0010', '0010', '0110')
equal_strings = c('1111', '0010', '0010', '0110')

fac_ml = factor(c('a','b','c','d','f'))
chr = c('a', 'b', 'a', 'b', 'b')

# outcomes
outcomes = c(1,0,0,1)

test_that("multiple states are captured",{
  multi_state <- c('0200', '0120', '1100')
  expect_error(make_stem(multi_state))
})

test_that("error messages are captured in make_stem", {
  expect_error(make_stem(equal_strings,max = 'a'), 'max must be a single number or deafult', fixed = T)
  expect_error(make_stem(unequal_strings, max='a'), 'comorbid column must be the same length for each record', fixed=T)
})

test_that("frequency or outcome are accurately reported", {
  expect_equal(make_stem(equal_strings, outcome_column =  NULL)[1,'freq_or_outcome'], 'frequency')
  expect_equal(make_stem(equal_strings, outcome_column = outcomes)[1,'freq_or_outcome'], 'outcome')
})

test_that('make stem works accurately', {
  expect_equal(make_stem(equal_strings, min_freq = 0.25)[2,'stem'], "3;2-3;;")
  expect_equal(make_stem(equal_strings, min_freq = 0.60)[3,'stem'], "3;;")
  expect_equal(make_stem(equal_strings, min_freq = 0)[3,'stem'], "3;2-3;2-3-4;1-2-3-4")

})

test_that('stems are an s3 class',{
  expect_s3_class(make_stem(equal_strings), 'stem')
})

test_that('ties are handled correctly',{
  tied_data <- c('0001', '0011', '0010', '1000', '1000')
  # in old version, this returned 1;3-4 for stem of row 3 which should be 3;3-4
  stem <- make_stem(tied_data)
  expect_equal(stem[3,'stem'], '3;3-4')
})

test_that('outcome_capture_correct', {
  cc_string <- c('1001', '0100', '1101', '1001') # by frequency stem for 3 should be 1;1-4
  outcome_string <- c('0', '1', '1', '0') # by outcome should be 1;2-4
  expect_equal(make_stem(cc_string, max=2)[3,4], "1;1-4")
  expect_equal(make_stem(cc_string, outcome_column = outcome_string, max=2)[3,4], "1;2-4")
})
