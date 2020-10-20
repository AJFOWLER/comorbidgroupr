context('summary function')

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
