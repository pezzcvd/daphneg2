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
#' @param uv.pw character. Output path. (default home folder).
#'
#' @return NULL. It's called by the preprocessing prodedure and it automatically writes
#' phenotype and genotype files for GWAS in case of univariate analysis.
#' @noRd
#'
#'
#'
univariate = function(uv.par, uv.pheno, genotype, uv.pw = normalizePath("~")) {
  # input controls
  checkmate::assert_character(x = uv.par, any.missing = F, len = 1)
  #checkmate::assert_choice(x = uv.par, choices = c(env_explain$ID, phn_explain$ID), null.ok = F)
  checkmate::assert_data_frame(x = uv.pheno)
  #checkmate::assert_choice(x = uv.pheno, choices = c(environmental, phenotypical), null.ok = F)
  checkmate::assert_character(x = uv.pw, any.missing = F, len = 1)

  #debug_msg("Starting univariate function. \n")

  # Filtering phenotype
  # Retrieves the data corresponding to the correct parameter and removes NAs
  y = uv.pheno[,uv.par]
  #debug_msg(paste0("Parameter dimensions before filtering. ", length(y), " \n"))
  acces = rownames(uv.pheno[!is.na(y),])
  y = y[ !is.na(y)]
  #debug_msg(paste0("Parameter dimensions after filtering. ", length(y), " \n"))

  #debug_msg("Checking normality \n")
  # Checks  that data are normally distributed
  y = suppressWarnings(normal(y))

  # Writing phenotype file
  write.table(y, paste(uv.pw,"/pheno_", uv.par, sep = ""), sep = "\n",
              row.names = F, col.names = F,quote = F)
  #debug_msg("Phenotype file written \n")

  # Genotype filtering
  #debug_msg(paste0("Genotype table dimensions before filtering.
  #                ", nrow(genotype), " X ", ncol(genotype), " \n"))
  col1 <- append(c("snpID", "alt", "ref"), acces)
  g1 <- genotype[,colnames(genotype) %in% col1]
  #debug_msg(paste0("Genotype table dimensions after filtering.
  #                ", nrow(g1), " X ", ncol(g1), " \n"))

  # Writing genotype file
  write.table(g1, paste(uv.pw,"/geno_", uv.par, sep = ""), sep = ", ",
              row.names = F, col.names = F, quote = F)
  #debug_msg("Phenotype file written \n")

  #debug_msg("Univariate function completed successfully \n")
  return()
}
