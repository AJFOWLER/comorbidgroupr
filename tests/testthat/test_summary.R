context('summary function')

test_that('summary.stem method works correctly',{
  stems <- make_stem(hip_data$comorbid_string[1:500])
  sumstem <- summary(stems)
  expect_equal(nrow(sumstem), 10)
  expect_equal(sumstem[3, 3], "1-9-11 (n:4)")

  #with names
  sumstem <- summary(stems, dis_names = disease_names)
  expect_equal(sumstem[3, 3],  "High.Bp-Cancer-Arthritis (n:4)")
  #ensure that unique comorbid strings correctly captured
  op <- capture.output(summary(stems))
  expect_equal(as.numeric(regmatches(op[[2]], gregexpr("[[:digit:]]+", op[[2]]))[[1]]), nrow(stems))
})
