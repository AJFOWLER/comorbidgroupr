#' Example comorbidity and outcome data
#'
#' A dataset containing the comorbid string of 40,000 individuals
#' and which suffered any complications after hip replacement surgery.
#'
#' @format A data frame with two columns:
#' \describe{
#'   \item{comorbid_string}{A string of 0s and 1s indicating disease presence or absence, each position refers to a single disease}
#'   \item{outcome}{A binary 1/0 indicating if an outcome was suffered}
#'  }
#'
#' @source \url{https://digital.nhs.uk/data-and-information/publications/statistical/patient-reported-outcome-measures-proms/}
"hip_data"
