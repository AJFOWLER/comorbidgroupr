context('grouping function')

unequal_strings = c('111', '0010', '0010', '0110')
equal_strings = c('1111', '0010', '0010', '0110')

fac_ml = factor(c('a','b','c','d','f'))
chr = c('a', 'b', 'a', 'b', 'b')

ex_list = list(c(1,2,3,4,5), c(2,3,4,5), c(3,4,5,6,1,2))

test_that("error messages are captured in make_stem", {
  expect_error(make_stem(equal_strings,max = 'a'), 'max must be a single number or deafult', fixed = T)
  expect_error(make_stem(unequal_strings, max='a'), 'comorbid column must be the same length for each record', fixed=T)
})

test_that("intersection over multiple sets works correctly", {
  expect_equal(reduce_set_overlap(list(fac_ml, chr)), c("a", "b"))
})

test_that("list position function works", {
  expect_equal(get_list_pos(ex_list)(1), c(1,2,3,4,5))
})
