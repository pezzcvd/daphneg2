#' multivariate
#'
#' Internal intermediate function. Starts the procedure for multivariate analysis.
#'
#' @param mv.input character{1}. Name of the parameter of interest.
#' @param mv.par character{1}. Name of the covariate parameter.
#' @param mv.pheno data.frame. Either phenotypical or environmental.
#' @param mv.div FLag. If TRUE indicates the need of having both phenotypical and
#' environmental datasets (PxE analysis). (default F)
#' @param mv.env_altern data.frame. Environmental, loaded in case of PxE analysis.
#' @param mv.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. It's called by the preprocessing prodedure and it calls either "oneCov" or "nCov"
#' it's part of the preprocessing procedure of GWAS files in case of multivariate analysis.
#' @noRd
#'
#'
multivariate = function(mv.input, mv.par, mv.pheno, mv.div = F, mv.env_altern = NULL, mv.pw = normalizePath("~")) {
  # Input controls
  checkmate::assert_character(x = mv.input, any.missing = F, len = 1)
  checkmate::assert_character(x = mv.par, any.missing = F, len = 1)
  checkmate::assert_data_frame(x = mv.pheno)
  checkmate::assert_flag(x = mv.div)
  checkmate::assert_data_frame(x = mv.env_altern, null.ok = T)
  checkmate::assert_character(x = mv.pw, any.missing = F, len = 1)

  #noerr = paste0(mv.pw, "ciao")

  #debug_msg("Starting multivariate function. \n")

  # Filtering phenotype
  # Retrieves the data corresponding to the correct parameter and removes NAs
  input.line = which(colnames(mv.pheno) %in% mv.input )
  mv.x = mv.pheno[,input.line]
  mv.xn = which(!is.na(mv.x))

  # PxE case needs a second table for covariates
  # if pxe define cov table
  if (mv.div) {
    #debug_msg("Setting covariate table in case of pxe \n")
    mv.table = mv.env_altern
  } else {
    mv.table = mv.pheno
    # Remove param of interest from the table that will be used for covariates
    #debug_msg("Removing parameter of interest from covariate table \n")

    #debug_msg(paste0("Covariates table dimensions before filtering.
    #                ", nrow(mv.table), "x", ncol(mv.table), " \n"))
    parn = which(colnames(mv.pheno) == mv.input)
    mv.table = mv.pheno[,-parn]
    #debug_msg(paste0("Covariates table dimensions after filtering.
    #                 ", nrow(mv.table), "x", ncol(mv.table), " \n"))
  }

  # check case one covariate
  if (mv.par != "all") {
    #debug_msg(paste0("One covariate: ", mv.par, " \n"))
    oneCov(mv.input, mv.par, mv.table, mv.x, mv.xn, mv.pw)

    #check case more covariates --> linear model
  } else {
    #debug_msg("More covariates \n")
    nCov(mv.input, mv.table, mv.x, mv.xn, mv.pw)
  }

  #debug_msg("Multivariate function completed successfully \n")
  return()
}
