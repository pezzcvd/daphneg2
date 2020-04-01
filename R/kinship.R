#' kinship
#'
#' Internal function. It calculates the kinship matrix for a specific study using the
#' software GEMMA.
#'
#' @param k.input character{1}. Parameter name, used as a prefix in the files of interest.
#' @param k.gemma character{1}. Gemma executable name. Version or system may vary.
#' @param k.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. It automatically writes the relatedness matrix for this specific setting.
#' The matrix will be used for GWAS analysis.
#'
#' @noRd
#'
kinship = function(k.input, k.gemma, k.pw = normalizePath("~")) {
  # Input controls
  checkmate::assert_character(x = k.input, any.missing = F, len = 1)
  checkmate::assert_character(x = k.gemma, any.missing = F, len = 1)
  checkmate::assert_character(x = k.pw, any.missing = F, len = 1)

  # Looks for the path of gemma executable.
  #/home/pejo/gemma-0.98.1-linux-static
  #gemma = list.files(path = normalizePath("~"), pattern = gemma.name,
  #                   recursive = T, full.names = T)[1]

  # Calls the gemma software in order to create the kinship matrix.
  system(paste0(k.gemma," -g ", k.pw, "/geno_", k.input,
                " -p ", k.pw, "/pheno_", k.input,
                " -gk 1 -o kinship_", k.input, " -outdir ", k.pw,"/output"))

  return()
}
