context('check stem helper functions')

comorbid_column = c('001100', '001100', '010100', '010100', '111000', '000001')

positions = list(c(3,4), c(2,4), c(1,2,3), c(6))

unique_combinations = matrix(c(1,2,1,3,2,3), 3,2)

stems <- c('4;5-6;3-5-6;;', '4;5-4;;;', '5;5-6;3-5-6;3-5-6-7;')

test_that("stems are split appropriately, exceptions captured", {
  expect_error(split_stem('4,5,6,7'), 'all stems should have a ";" separator', fixed = T)
  expect_error(split_stem(stems, cut_level = 'a', maximal = TRUE), 'cut_level should be a numeric value', fixed = T)
  expect_error(split_stem(stems, cut_level = 3, maximal = 1), 'maximal should be a logical of either TRUE/FALSE', fixed = T)})

test_that('stems correctly split depending on maximal true or false', {
  (expect_equal(split_stem(stems, cut_level = 1, maximal=TRUE), c('4','4','5')))
  (expect_equal(split_stem(stems, cut_level = 3, maximal=TRUE), c('3-5-6','5-4','3-5-6')))
  (expect_equal(split_stem(stems, cut_level = 3, maximal=FALSE), c('3-5-6','','3-5-6')))
})

test_that("unique sets are the correct number",
          {expect_equal(nrow(get_unique_sets(comorbid_column)), 4)}
          )

test_that("local positions are identified",
          {expect_equal(get_locales(comorbid_column[1]), c(3,4))}
          )

test_that("Combinations if statement captures groups correctly",{
          expect_equal(dim(get_combos(positions[[3]], combinations = 2)), c(3,2))
          expect_equal(dim(get_combos(positions[[2]], combinations = 2)), c(1,2))
          expect_equal(get_combos(positions[[4]], combinations = 2), NA)
          })

test_that("Unique combinations correctly calculates",{
    expect_equal(dim(unique_combos(positions, combo_numbers=2)), c(5,2))
})

# need a group frequency test
