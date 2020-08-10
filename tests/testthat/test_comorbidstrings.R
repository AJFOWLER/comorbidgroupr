context('check comorbid strings')

x <- c('00100', '00100', '0100')
y <- c('010000', '020000', '000000')

z <- factor(c('00100', '01100', '01111110'))
a <- factor(c('00100', '01100', '11110'))

test_that("factors turn into characters", {
  expect_equal(to_string(factor('010101')), '010101')
})

test_that("inconsistent string length is captured", {
  expect_false(check_strings_equal(x))
  expect_true(check_strings_equal(y))
})

test_that("inconsistent factor length is captured", {
  expect_false(check_strings_equal(z))
  expect_true(check_strings_equal(a))
})

test_that('disease lists are the same length as each individual string',{
  expect_equal(length(get_disease_counts(y)), 6)
})

