context('grouping function')

unequal_strings = c('111', '0010', '0010', '0110')
equal_strings = c('1111', '0010', '0010', '0110')

fac_ml = factor(c('a','b','c','d','f'))
chr = c('a', 'b', 'a', 'b', 'b')

# outcomes
outcomes = c(1,0,0,1)

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

test_that('summary.stem method works correctly',{
  stems <- make_stem(hip_data$comorbid_string[1:500])
  sumstem <- summary(stems)
  expect_equal(nrow(sumstem), 5)
  expect_equal(sumstem[3, 'Diseases'][[1]], '')

  #with names
  sumstem <- summary(stems, dis_names = disease_names)
  expect_equal(sumstem[5, 'Diseases'][[1]], 'Depression, Arthritis')
  #ensure that unique comorbid strings correctly captured
  op <- capture.output(summary(stems))
  expect_equal(as.numeric(regmatches(op[[2]], gregexpr("[[:digit:]]+", op[[2]]))[[1]]), nrow(stems))
})
