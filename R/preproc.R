#' preproc
#'
#' Internal function. Starts the procedure for proprocessing analysis.
#'
#' @param pp.par character. Name of the parameter of interest.
#' @param pp.option character. Label for which kind of analysis is needed (P, E, PxP, ExE, PxE)
#' @param pp.geno character. Rdata file with genotype information.
#' @param pp.cvt character. Name of the covariate. (default NULL)
#' @param pp.pw character. Output path. (default home folder).
#'
#' @return NULL. It strarts the preprocessing prodedure and it calls either "univariate" or
#' "multivariate" in order to produce GWAS files.
#' @noRd
#'
#' @examples
preproc = function(pp.par, pp.option, pp.geno, pp.cvt = NULL, pp.pw = normalizePath("~")) {
  # Loading genotype information
  checkmate::assert_file(pp.geno)
  load(pp.geno)

  # setting everything else according to option
  # Univariate options
  checkmate::assert_choice(x = pp.option, choices = c("P", "E", "PxP", "ExE", "PxE"))
  if (pp.option %in% c("P", "E")) {
    if (pp.option == "P") {
      pp.pheno = phenotypical
    }
    if (pp.option == "E") {
      pp.pheno = environmental
    }
    checkmate::assert_null(x = pp.cvt)
    # Those have to be checked in all cases
    # pheno table and parameter of interest (same as before)
    checkmate::assert_data_frame(x = pp.pheno)
    checkmate::assert_character(x = pp.par, any.missing = F, len = 1)
    checkmate::assert_choice(x = pp.par, choices = colnames(pp.pheno))

    # Call
    univariate(uv.par = pp.par, uv.pheno = pp.pheno,genotype = genotype,  uv.pw = pp.pw)
  }

  # Multivariate options
  if (pp.option %in% c("PxP", "ExE", "PxE")) {

    if (pp.option %in% c("PxP", "ExE")) {
      pxeFlag = F
      pp.ctab = NULL
      # Set up tables
      if (pp.option == "PxP") {
        pp.pheno = phenotypical
      }
      if (pp.option == "ExE") {
        pp.pheno = environmental
      }

      # Check that covariate table is a data.frame, covariate parameter is a string and
      # the parameter is either one of the columns in covariarte table or "all"
      #checkmate::assert_character(x = pp.cvt, any.missing = F, len = 1)
      checkmate::assert_choice(x = pp.cvt, choices = c("all", colnames(pp.pheno)))

    }

    if (pp.option == "PxE") {
      pp.pheno = phenotypical
      pp.ctab = environmental
      pxeFlag = T

      checkmate::assert_data_frame(x = pp.ctab)
      checkmate::assert_choice(x = pp.cvt, choices = c("all", colnames(pp.ctab)))
    }
    # Those have to be checked in all cases
    # pheno table and parameter of interest (same as before)
    checkmate::assert_data_frame(x = pp.pheno)
    #checkmate::assert_character(x = pp.par, any.missing = F, len = 1)
    checkmate::assert_choice(x = pp.par, choices = colnames(pp.pheno))

    # Call
    multivariate(mv.input = pp.par, mv.par = pp.cvt, mv.pheno = pp.pheno,
                 mv.div = pxeFlag, mv.env_altern = pp.ctab, mv.pw = pp.pw)

  }

  return()
}
