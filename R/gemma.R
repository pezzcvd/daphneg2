#' gemma
#'
#' Internal function. It calculates the SNP associations for a specific study using the
#' software GEMMA.
#'
#' @param g.input character{1}. Parameter name, used as a prefix in the files of interest.
#' @param g.gemma character{1}. Gemma executable name. Version or system may vary.
#' @param g.annot annotation
#' @param g.miss missingness
#' @param g.maf minor allele frequency
#' @param g.pw character{1}. Output path. (default home folder).
#' @param g.kinship precomputed kinship file name, default ""
#'
#' @return NULL. It automatically performs the GWAS analysis for this specific setting
#' and writes a result table.
#'
#' @noRd
#'
gemma = function(g.input, g.gemma, g.cv, g.annot, g.miss, g.maf, g.kinship = "", g.pw = normalizePath("~")) {
  # Input controls
  checkmate::assert_string(x = g.input)
  checkmate::assert_string(x = g.gemma)
  checkmate::assert_choice(x = g.cv, choices = c(0,1,2), null.ok = F)
  checkmate::assert_string(x = g.annot)
  checkmate::assert_file(x = g.annot)
  checkmate::assert_numeric(x = g.miss)
  checkmate::assert_numeric(x = g.maf)
  checkmate::assert_string(x = g.pw)

  # Looks for the path of gemma executable.
  #/home/pejo/gemma-0.98.1-linux-static
  #gemma = list.files(path = normalizePath("~"), pattern = gemma.name,
  #                   recursive = T, full.names = T)[1]

  if (g.kinship == "") {
    kin = ""
  } else {
    kin = paste0(g.pw, "/output/kinship_", g.input, ".cXX.txt")
  }

  #GWAS
  if (g.cv > 0) {
    system(paste0(g.gemma, " -g ", g.pw, "/geno_", g.input,
                  " -p  ", g.pw, "/pheno_", g.input,
                  " -k  ", kin,
                  " -c  ", g.pw, "/covar_", g.input,
                  " -a  ", g.annot,
                  " -miss ", g.miss, " -maf ", g.maf,
                  " -lmm 1 -o out_", g.input,
                  " -outdir ", g.pw,"/output"))

  } else {
    system(paste0(g.gemma, " -g  ", g.pw, "/geno_", g.input,
                  " -p  ", g.pw, "/pheno_", g.input,
                  " -k  ", kin,
                  " -a  ", g.annot,
                  " -miss ", g.miss, " -maf ", g.maf,
                  " -lmm 1 -o out_", g.input,
                  " -outdir ", g.pw,"/output"))
  }
  return()
}
