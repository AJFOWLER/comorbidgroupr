context('test correlation generation is accurate')
comorbid_column <- hip_data$comorbid_string[1:1e4]

dt = .get_unique_sets(comorbid_column)

# get disease counts
dt$position = apply(dt['comorbid_column'],1, .get_locales)

dt$corr <- use_correlation(comorbid_column = comorbid_column, poscolumn = dt$position)

test_that("Correlations work correctly",{
  expect_equal(length(is.na(dt$corr)), 12) # should be 12 things with NA (these are single/no diseases)
  expect_equal(dt[4, 'corr'], 0.049867739) # manually calculated this to ensure works
  # need to check how correlations work amongst those with single outcomes?
})

test_that("correlations interact correctly with stem_generator",{

})

