context('test combinations and unique combinations are accurate')

positions = list(c(3,4), c(2,4), c(1,2,3), c(6))

test_that("Combinations if statement captures groups correctly",{
  expect_equal(dim(get_combos(positions[[3]], combinations = 2)), c(3,2))
  expect_equal(dim(get_combos(positions[[2]], combinations = 2)), c(1,2))
  expect_equal(get_combos(positions[[4]], combinations = 2), NA)
})

test_that("Unique combinations correctly calculates",{
  expect_equal(dim(unique_combos(positions, combinations=2)), c(5,2))
  expect_equal(dim(unique_combos(positions, combinations = 3)), c(1,3))
})
