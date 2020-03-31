#' gemma
#'
#' @param g.input
#' @param gemma.name
#' @param g.annot
#' @param g.miss
#' @param g.maf
#' @param g.pw
#'
#' @return
#' @noRd
#'
gemma = function(g.input, gemma.name, g.cv, g.annot, g.miss, g.maf, g.pw = normalizePath("~")) {
  # Looks for the path of gemma executable.
  #/home/pejo/gemma-0.98.1-linux-static
  gemma = list.files(path = normalizePath("~"), pattern = gemma.name,
                     recursive = T, full.names = T)[1]

  #GWAS
  if (g.cv > 0) {
    system(paste0(gemma, " -g ", g.pw, "/geno_", g.input,
                  " -p  ", g.pw, "/pheno_", g.input,
                  " -k  ", g.pw, "/output/kinship_", g.input, ".cXX.txt",
                  " -c  ", g.pw, "/covar_", g.input,
                  " -a  ", g.annot,
                  " -miss ", g.miss, " -maf ", g.maf,
                  " -lmm 1 -o out_", g.input,
                  " -outdir ", g.pw,"/output"))

  } else {
    system(paste0(gemma, " -g  ", g.pw, "/geno_", g.input,
                  " -p  ", g.pw, "/pheno_", g.input,
                  " -k  ", g.pw, "/output/kinship_", g.input, ".cXX.txt",
                  " -a  ", g.annot,
                  " -miss ", g.miss, " -maf ", g.maf,
                  " -lmm 1 -o out_", g.input,
                  " -outdir ", g.pw,"/output"))
  }
  return()
}
