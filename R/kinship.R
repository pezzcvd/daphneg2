#' kinship
#'
#' Internal function. It calculates the kinship matrix for a specific study using the
#' software GEMMA.
#'
#' @param k.input character{1}. Parameter name, used as a prefix in the files of interest.
#' @param k.gemma character{1}. Gemma executable name. Version or system may vary.
#' @param k.loco boolean(1). Leave one chromosome out approach. (default FALSE)-.
#' @param k.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. It automatically writes the relatedness matrix for this specific setting.
#' The matrix will be used for GWAS analysis.
#'
#' @noRd
#'
kinship = function(k.input, k.gemma, k.loco = F, k.pw = normalizePath("~")) {
  # Input controls
  checkmate::assert_string(x = k.input)
  checkmate::assert_string(x = k.gemma)
  checkmate::assert_string(x = k.pw)

  # Calls the gemma software in order to create the kinship matrix.
  if (k.loco == T) {
    for (c in 1:5) {
      print(paste0(k.gemma," -g ", k.pw, "/geno_notchr", c, "_", k.input,
                    " -p ", k.pw, "/pheno_", k.input,
                    " -gk 1 -o kinship_notchr", c, "_", k.input, " -outdir ", k.pw,"/output"))

      system(paste0(k.gemma," -g ", k.pw, "/geno_notchr", c, "_", k.input,
                    " -p ", k.pw, "/pheno_", k.input,
                    " -gk 1 -o kinship_notchr", c, "_", k.input, " -outdir ", k.pw,"/output"))
    }

  } else {

    print(paste0(k.gemma," -g ", k.pw, "/geno_", k.input,
                 " -p ", k.pw, "/pheno_", k.input,
                 " -gk 1 -o kinship_", k.input, " -outdir ", k.pw,"/output"))
    system(paste0(k.gemma," -g ", k.pw, "/geno_", k.input,
                  " -p ", k.pw, "/pheno_", k.input,
                  " -gk 1 -o kinship_", k.input, " -outdir ", k.pw,"/output"))
  }

  return()
}
