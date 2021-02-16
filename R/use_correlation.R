#' Use item correlation to combine diseases
#' First calculate the core matrix
#' https://stackoverflow.com/questions/54713342/correlation-coefficient-for-three-variables-in-r
#' @export

use_correlation <-function(comorbid_column,
                           poscolumn){
  # take cci_column
  cci <- strsplit(comorbid_column, '', fixed=T)

  # turn into a matrix
  cor_dat <- matrix(unlist(cci), ncol = nchar(comorbid_column[1]), byrow=T)

  # make into a number
  cor_dat_m <- apply(cor_dat, 2, FUN = as.numeric)

  # Pearson correlation coefficient to make matrix
  cor_mat <- cor(cor_dat_m[,c(10:11)])
  rm(cor_dat, cor_dat_m, cci) # keep clean
  #########
  # PAIRS #
  #########

  # then loop through combinations to identify ==2
  pairs <- which(lapply(poscolumn, length) == 2)
  paircols <- poscolumn[pairs]
  paircorrs <- c()

  for(combos in 1:length(paircols)){
    pos <- paircols[[combos]]
    cs <- cor_mat[pos[1], pos[2]]
    paircorrs[combos] <- cs
  }

  ######
  # >2 #
  ######
  pcol_which <- which(lapply(poscolumn, length) >2) # only if >2
  corrs <- c()
  pcol <- poscolumn[pcol_which]

  for(combos in 1:length(pcol)){
    pos <- pcol[[combos]]
    cs <- cor_mat[pos,pos]
    corrs[combos] <- .multiple_correlation(cs)
  }

  # Generate
  out_col <- rep(NA, length(poscolumn))

  # bind in pairs
  out_col[pairs] <- paircorrs

  # bind in >2
  out_col[pcol_which] <- corrs

  # what is the best thing to return here?

  # ? interject this at the point at which the stem is being constructed

}

.multiple_correlation <- function(cor_mat){
  cs <- cor_mat[upper.tri(cor_mat)]
  return(sqrt(sum(cs^2) - 2*prod(cs)))
}
