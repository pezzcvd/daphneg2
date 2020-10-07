#' oneCov
#'
#' #' Internal function. It matches non-missing phenotype values with covariates,
#' it retrieves the corresponding genotipic information and writes genotype, phenotype and
#' covariate files that will be fed to the GWAS analysis software.
#'
#' @param oc.input character{1}. Name of the parameter of interest.
#' @param oc.par character{1}. Name of the covariate parameter.
#' @param oc.table dataframe. Either phenotypical or environmental.
#' @param x numeric{1}- Phenotype values.
#' @param xn numeric{1}. Phenotype non-null positions.
#' @param oc.loco boolean. Leave one chromosome out approach. (default FALSE).
#' @param oc.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. Phenotype, genotype and covariate files are produced. These are going to be
#' the input files for GWAS analysis
#'
#' @noRd
#'
#'
oneCov = function (oc.input, oc.par, oc.table, x, xn, oc.loco = F, oc.pw = normalizePath("~")) {
  # Controls on input
  checkmate::assert_string(x = oc.input)
  checkmate::assert_string(x = oc.par)
  checkmate::assert_data_frame(x = oc.table)
  checkmate::assertNumeric(x = x, len = 1131)
  checkmate::assertNumeric(x = xn, max.len = 1131)
  #debug_msg("Starting oneCov function. \n")

  # Assign the right column
  # Retrieves from the right dataset table the parameter of covariate (y) and
  # saves the positions of covariate non-missing values (yn)
  y = oc.table[,oc.par]
  yn = which(!is.na(y))

  # Intersects indexes of phenotype and covariate and retrieves corresponding ecotypes
  acces = rownames(oc.table[intersect(xn,yn),])
  #debug_msg(paste0("Parameter dimensions before filtering. ", length(x), " \n"))
  #debug_msg(paste0("Covariate dimensions before filtering. ", length(y), " \n"))

  # Updates phenotype and covariate, saving only values corresponding to common ecotypes
  x = x[intersect(xn,yn)]
  y = y[intersect(xn,yn)]
  #debug_msg(paste0("Parameter dimensions after filtering. ", length(x), " \n"))
  #debug_msg(paste0("Covariate dimensions after filtering. ", length(y), " \n"))

  #debug_msg("Checking normality \n")
  # For both checks normality (important condition for gwas)
  x = normal(x)
  y = normal(y)

  # Writing phenotype and covariate
  write.table(x, paste0(oc.pw, "/pheno_", oc.input, "_", oc.par), sep = "\n",
              quote = F, row.names = F, col.names = F)
  #debug_msg("Phenotype file written \n")
  # In covariate file adds intercept column
  write.table(as.data.frame(cbind(1, y)), paste0(oc.pw, "/covar_", oc.input, "_", oc.par), sep = "\t",
              quote = F, row.names = F, col.names = F)
  #debug_msg("Covariate file written \n")


  #LOCO
  if (oc.loco == T) {
    for (c in 1:5) {
      col1 <- append(c("snpID", "alt", "ref"), acces)
      #g1 <- genotype[substr(genotype$snpID, 4, 4) == i, colnames(genotype) %in% col1]
      #g2 <- genotype[substr(genotype$snpID, 4, 4) != i, colnames(genotype) %in% col1]
      g1 <- genotype[substr(genotype$snpID, 4, 4) == c,colnames(genotype) %in% col1]
      g2 <- genotype[substr(genotype$snpID, 4, 4) != c,colnames(genotype) %in% col1]

      write.table(g1, paste(oc.pw,"/geno_chr", c, "_", oc.input, "_", oc.par, sep = ""), sep = ", ",
                  row.names = F, col.names = F, quote = F)
      write.table(g2, paste(oc.pw,"/geno_notchr", c, "_", oc.input, "_", oc.par, sep = ""), sep = ", ",
                  row.names = F, col.names = F, quote = F)

    }
  } else {
      col1 <- append(c("snpID", "alt", "ref"), acces)
      g1 <- genotype[,colnames(genotype) %in% col1]
      #debug_msg(paste0("Genotype table dimensions after filtering.
      #                ", nrow(g1), " X ", ncol(g1), " \n"))


      # Writing genotype
      write.table(g1, paste0(oc.pw,"/geno_", oc.input, "_", oc.par), sep = ", ",
                  row.names = F, col.names = F, quote = F)
    }


  # Genotype filtering
  # FIltering out genotype table
  #debug_msg(paste0("Genotype table dimensions before filtering.
  #                ", nrow(genotype), " X ", ncol(genotype), " \n"))

  #debug_msg("Genotype file written \n")

  #debug_msg("oneCov function completed successfully \n")
  return()
}
