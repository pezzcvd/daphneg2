#' kinship
#'
#'It creates the kinship matrix. (Calls Gemma)
#'
#' @param k.input character{1}. Parameter name, used as a prefix in the files of interest.
#' @param gemma.name character{1}. Gemma executable name. Version or system may vary.
#' @param k.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. It automatically writes the relatedness matrix for this specific setting.
#' The matrix will be used for GWAS analysis.
#'
#' @noRd
#'
kinship = function(k.input, gemma.name, k.pw = normalizePath("~")) {
  # Input controls
  checkmate::assert_character(x = k.input, any.missing = F, len = 1)
  checkmate::assert_character(x = gemma.name, any.missing = F, len = 1)
  checkmate::assert_character(x = k.pw, any.missing = F, len = 1)

  # Looks for the path of gemma executable.
  #/home/pejo/gemma-0.98.1-linux-static
  gemma = list.files(path = normalizePath("~"), pattern = gemma.name,
                     recursive = T, full.names = T)[1]

  # Calls the gemma software in order to create the kinship matrix.
  system(paste0(gemma," -g ", k.pw, "/geno_", k.input,
                " -p ", k.pw, "/pheno_", k.input,
                " -gk 1 -o kinship_", k.input, " -outdir ", k.pw,"/output"))

  return()
}
