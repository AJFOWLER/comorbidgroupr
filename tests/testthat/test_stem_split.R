context('test stem splitting')

stems <- c('4;5-6;3-5-6;;', '4;5-4;;;', '5;5-6;3-5-6;3-5-6-7;')

test_that("stems are split appropriately, exceptions captured", {
  expect_error(split_stem('4,5,6,7'), 'all stems should have a ";" separator', fixed = T)
  expect_error(split_stem(stems, cut_level = 'a', maximal = TRUE), 'cut_level should be a numeric value', fixed = T)
  expect_error(split_stem(stems, cut_level = 3, maximal = 1), 'maximal should be a logical of either TRUE or FALSE', fixed = T)})

test_that('stems correctly split depending on maximal true or false', {
  (expect_equal(split_stem(stems, cut_level = 1, maximal=TRUE), c('4','4','5')))
  (expect_equal(split_stem(stems, cut_level = 3, maximal=TRUE), c('3-5-6','5-4','3-5-6')))
  (expect_equal(split_stem(stems, cut_level = 3, maximal=FALSE), c('3-5-6','','3-5-6')))
})
