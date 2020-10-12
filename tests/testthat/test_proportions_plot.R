context('test proportions plot works')

comorbid_column <- c('111', '0010', '0010', '0110')
outcome_column <- c(1,0,0,1)
short_outcome <- outcome_column[1:3]
wrong_outcome = c(1,2,0,1)

test_that('capture errors related to outcome column',{
  expect_error(stem_funnel_plot(comorbid_column=comorbid_column, outcome_column = wrong_outcome), 'outcome column should be numeric ones and zeros', fixed = T) #expect warning for object length
  expect_error(stem_funnel_plot(comorbid_column=comorbid_column, outcome_column = short_outcome), 'outcome column should have the same number of observations as comorbid_column', fixed = T)
  expect_error(stem_funnel_plot(comorbid_column, outcome_column, use_outcome = 1), 'use_outcome should be either TRUE or FALSE', fixed=T)
})

test_that('capture other errors',{
  expect_error(stem_funnel_plot(comorbid_column, outcome_column, use_outcome = FALSE, maximal = 1), 'maximal should be a logical of either TRUE or FALSE', fixed=T)
  expect_error(stem_funnel_plot(comorbid_column, outcome_column, use_outcome = FALSE, maximal = TRUE, max_x = TRUE), 'max_x should be either NULL or a number', fixed=T)
  expect_error(stem_funnel_plot(comorbid_column, outcome_column, use_outcome = TRUE, maximal = TRUE, max_x = 'a'),  'max_x should be either NULL or a number', fixed=T)
})

test_that('correct outliers are identified and named', {
  test_plot <- stem_funnel_plot(comorbid_column = hip_data$comorbid_string, outcome_column = hip_data$outcome, cut_level = 2)
  expect_equal(length(test_plot), 2)
  expect_equal(length(which(test_plot$funnel_data$`inside_95%` == 'Extreme')), 6)
  named_test_plot <- stem_funnel_plot(comorbid_column = hip_data$comorbid_string, outcome_column = hip_data$outcome, cut_level = 3, dis_names = disease_names)
  expect_equal(named_test_plot$funnel_data$stem[[1]], 'Lung.Disease | Diabetes | Cancer')
  })
