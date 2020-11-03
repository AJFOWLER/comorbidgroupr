context('summary function')

test_that('summary.stem method works correctly',{
  stems <- make_stem(hip_data$comorbid_string[1:500])
  sumstem <- summary(stems)
  expect_equal(nrow(sumstem), 10)
  expect_equal(sumstem[3, 3], "10-11")
  expect_equal(sumstem[10,6], "3 (0.6 %)")

  #with names
  sumstem <- summary(stems, dis_names = disease_names)
  expect_equal(sumstem[3, 3],  "Depression-Arthritis")

  #ensure that unique comorbid strings correctly captured
  op <- capture.output(summary(stems))
  expect_equal(as.numeric(regmatches(op[[2]], gregexpr("[[:digit:]]+", op[[2]]))[[1]]), nrow(stems))
})

test_that('summary.stem method works to count of six diseases',{
  stems <- make_stem(hip_data$comorbid_string[1:5000], max=6)
  sumstem <- summary(stems)
  expect_equal(ncol(sumstem), 12)
  expect_equal(names(sumstem)[9], 'Number of diseases: 5')
})
