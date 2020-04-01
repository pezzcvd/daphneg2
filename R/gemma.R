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
#'
#' @return NULL. It automatically performs the GWAS analysis for this specific setting
#' and writes a result table.
#'
#' @noRd
#'
gemma = function(g.input, g.gemma, g.cv, g.annot, g.miss, g.maf, g.pw = normalizePath("~")) {
  # Looks for the path of gemma executable.
  #/home/pejo/gemma-0.98.1-linux-static
  #gemma = list.files(path = normalizePath("~"), pattern = gemma.name,
  #                   recursive = T, full.names = T)[1]

  #GWAS
  if (g.cv > 0) {
    system(paste0(g.gemma, " -g ", g.pw, "/geno_", g.input,
                  " -p  ", g.pw, "/pheno_", g.input,
                  " -k  ", g.pw, "/output/kinship_", g.input, ".cXX.txt",
                  " -c  ", g.pw, "/covar_", g.input,
                  " -a  ", g.annot,
                  " -miss ", g.miss, " -maf ", g.maf,
                  " -lmm 1 -o out_", g.input,
                  " -outdir ", g.pw,"/output"))

  } else {
    system(paste0(g.gemma, " -g  ", g.pw, "/geno_", g.input,
                  " -p  ", g.pw, "/pheno_", g.input,
                  " -k  ", g.pw, "/output/kinship_", g.input, ".cXX.txt",
                  " -a  ", g.annot,
                  " -miss ", g.miss, " -maf ", g.maf,
                  " -lmm 1 -o out_", g.input,
                  " -outdir ", g.pw,"/output"))
  }
  return()
}