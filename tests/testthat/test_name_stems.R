context('test stem naming')

cut_stems <- c('5-6', '5-4', '3-5-6')
dis_names <- list('Diabetes', 'Chronic Kidney Disease', 'Hypertension', 'Congestive Cardiac Failure', 'Liver Disease', 'Dementia')

test_that("name_x_stem works appropriatly",{
    expect_equal(name_stems(cut_stems, dis_names = dis_names), c("Liver Disease | Dementia", "Liver Disease | Congestive Cardiac Failure", "Hypertension | Liver Disease | Dementia"))
    expect_equal(name_stems(c(NA, cut_stems), dis_names = dis_names), c("","Liver Disease | Dementia", "Liver Disease | Congestive Cardiac Failure", "Hypertension | Liver Disease | Dementia"))
})

test_that("errors are correctly captured",{
  expect_error(name_stems(c('1;9'), dis_names = dis_names), 'Have you split your stems yet? There should not be any ; in split stems', fixed=T)
  })

