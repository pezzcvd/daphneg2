#' univariate
#'
#' Internal intermediate function. Starts the preprocessing procedure for
#' univariate analysis. It matches available phenotype values for the selected parameter
#' with the available ecotypes in the dataset, it retrieves the corresponding genotipic
#' information and writes the files that will be fed to the GWAS analysis software.
#' (In this case only genotype and phenotype).
#'
#' @param uv.par character. Name of the parameter of interest.
#' @param uv.pheno data.frame. Either phenotypical or environmental.
#' @param genotype data.frame. Table with SNP occurrences among ecotypes.
#' @param uv.loco boolean. Leave one chromosome out approach. (default FALSE).
#' @param uv.pw character. Output path. (default home folder).
#'
#' @return NULL. It's called by the preprocessing prodedure and it automatically writes
#' phenotype and genotype files for GWAS in case of univariate analysis.
#' @noRd
#'
#'
#'
univariate = function(uv.par, uv.pheno, genotype, uv.loco = F, uv.pw = normalizePath("~")) {
  # input controls
  checkmate::assert_character(x = uv.par, any.missing = F, len = 1)
  checkmate::assert_data_frame(x = uv.pheno)
  checkmate::assert_character(x = uv.pw, any.missing = F, len = 1)

  # Filtering phenotype
  # Retrieves the data corresponding to the correct parameter and removes NAs
  y = uv.pheno[,uv.par]
  acces = rownames(uv.pheno[!is.na(y),])
  y = y[ !is.na(y)]

  # Checks  that data are normally distributed
  y = suppressWarnings(normal(y))

  # Writing phenotype file
  write.table(y, paste(uv.pw,"/pheno_", uv.par, sep = ""), sep = "\n",
              row.names = F, col.names = F,quote = F)

  #LOCO
  if (uv.loco == T) {
    for (c in 1:5) {
      col1 <- append(c("snpID", "alt", "ref"), acces)
      g1 <- genotype[substr(genotype$snpID, 4, 4) == c,colnames(genotype) %in% col1]
      g2 <- genotype[substr(genotype$snpID, 4, 4) != c,colnames(genotype) %in% col1]

      write.table(g1, paste(uv.pw,"/geno_chr", c, "_", uv.par, sep = ""), sep = ", ",
                  row.names = F, col.names = F, quote = F)
      write.table(g2, paste(uv.pw,"/geno_notchr", c, "_", uv.par, sep = ""), sep = ", ",
                  row.names = F, col.names = F, quote = F)

    }
  } else {
    col1 <- append(c("snpID", "alt", "ref"), acces)
    g1 <- genotype[,colnames(genotype) %in% col1]

    # Writing genotype file
    write.table(g1, paste(uv.pw,"/geno_", uv.par, sep = ""), sep = ", ",
                row.names = F, col.names = F, quote = F)
  }

  return()
}
