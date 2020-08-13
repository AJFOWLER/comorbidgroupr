context('ensure default combinations works appropriately')

comorbid_column = c('00110', '01000', '011000', '01110')

test_that('Errors are captured relating to default values', {
  expect_error(determine_default_combinations(comorbid_column, default = 60), 'default must be greater than 0 but less than the highest disease proportion which is 50', fixed=T)
  expect_error(determine_default_combinations(comorbid_column, default = 0), 'default must be greater than 0 but less than the highest disease proportion which is 50', fixed=T)})

test_that('Accurate values are returned', {
  expect_equal(determine_default_combinations(comorbid_column, default = 2.5), 3)
  expect_equal(determine_default_combinations(comorbid_column, default = 49.0),2)})
