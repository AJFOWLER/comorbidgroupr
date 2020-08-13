context('grouping function')

unequal_strings = c('111', '0010', '0010', '0110')
equal_strings = c('1111', '0010', '0010', '0110')

fac_ml = factor(c('a','b','c','d','f'))
chr = c('a', 'b', 'a', 'b', 'b')

# disease counts relevant to equal strings
disease_counts = list(c(1), c(1,4), c(1,2,3,4), c(1))

# positions relevant to equal strings at pair level
positions = list(c(1,2,3,4), c(3), c(3), c(2,3))
# unique positions
unique_pos = structure(c(1, 1, 1, 2, 2, 3, 2, 3, 4, 3, 4, 4), .Dim = c(6L,
                                                          2L))
# outcomes
outcomes = c(1,0,0,1)

#positions relevant to equal strings

test_that("error messages are captured in make_stem", {
  expect_error(make_stem(equal_strings,max = 'a'), 'max must be a single number or deafult', fixed = T)
  expect_error(make_stem(unequal_strings, max='a'), 'comorbid column must be the same length for each record', fixed=T)
})

test_that("intersection over multiple sets works correctly", {
  expect_equal(reduce_set_overlap(list(fac_ml, chr)), c("a", "b"))
})

test_that("list position function works", {
  expect_equal(get_list_pos(disease_counts)(3), c(1,2,3,4))
})
