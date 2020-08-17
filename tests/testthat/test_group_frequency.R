context('check group frequency and helpers functions')

comorbid_column = c('1111', '0010', '0010', '0110')

# disease counts relevant to equal strings
disease_counts = list(c(1), c(1,4), c(1,2,3,4), c(1))

# positions relevant to equal strings at pair level
positions = list(c(1,2,3,4), c(3), c(3), c(2,3))

# unique positions
unique_pos = structure(c(1, 1, 1, 2, 2, 3, 2, 3, 4, 3, 4, 4), .Dim = c(6L,
                                                                       2L))
# outcomes
outcomes = c(1,0,0,1)

test_that('frequency or outcome correctly captured',{
  expect_equal(names(calculate_group_frequency(unique_combinations = unique_pos, all_diseases = disease_counts, outcome_positions = 0, tots = 4))[3:5], c('freq', 'outcome', 'propr_out'))
  expect_equal(calculate_group_frequency(unique_combinations = unique_pos, all_diseases = disease_counts, outcome_positions = 0, tots = 4)[4,1], 2)
  expect_equal(calculate_group_frequency(unique_combinations = unique_pos, all_diseases = disease_counts, outcome_positions = outcomes, tots = 4)[1,'propr_out'], 0.5)
})

test_that('reduce overlap works', {
  expect_equal(.reduce_set_overlap(list(c(1,2,3), c(2,3,4), c(2,4))), 2)
  expect_equal(.reduce_set_overlap(list(c(1,0), c(2,1))), 1)
})


test_that("unique sets are the correct number",
          {expect_equal(nrow(.get_unique_sets(comorbid_column)), 3)}
          )

test_that("local positions are identified",
          {expect_equal(.get_locales(comorbid_column[1]), c(1,2,3,4))}
          )

test_that("list position function works", {
  expect_equal(.get_list_pos(disease_counts)(3), c(1,2,3,4))
})

# need a group frequency test
