## code to prepare `hip_data` dataset and names of diseases
#'
#'
#'
#'

# read hip data in
# hip_data = read.csv('data-raw/raw_hip.csv', stringsAsFactors = F, header = TRUE)
# disease_names <- names(hip_data)
# usethis::use_data(disease_names, overwrite=TRUE)

# columns are diseases (1:11) and column 12 is outcome of 'suffered complications'
# 1 indicates 'yes' and 0 'no'
# to generate the comorbid string we need to paste together disease columns

# hip_data$comorbid_string = do.call(paste0, hip_data[,1:11])

# we can drop the extraneous columns
# hip_data[,1:11] = NULL
# hip_data[,c(2,1)] #reorder
# names(hip_data)[2] = 'outcome'
# collapse
# usethis::use_data(hip_data, overwrite = TRUE)
