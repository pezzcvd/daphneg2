#' nCov
#'
#' Internal function. It filters out missing values from phenotype target.
#' Then it selects the best multiple covariate model. Each model subselects 10 random
#' covariates and calculates the AIC (information score). The best model will be the one
#' with the highest information content among 1000 models.
#' It retrieves the corresponding genotipic information and writes genotype, phenotype,
#' covariate (best model fitted values vector) and the model files that will be fed
#' to the GWAS analysis software.
#'
#' @param nc.input character{1}. Name of the parameter of interest.
#' @param nc.table dataframe. Either phenotypical or environmental.
#' @param xp numeric{1}- Phenotype values.
#' @param xn numeric{1}. Phenotype non-null positions.
#' @param genotype data.frame. Table with SNP occurrences among ecotypes.
#' @param nc.loco boolean. Leave one chromosome out approach. (default FALSE).
#' @param nc.pw character{1}. Output path. (default home folder).
#'
#' @return
#' @importFrom stats lm
#' @importFrom MASS stepAIC
#'
#' @noRd
#'
nCov = function(nc.input, nc.table, xp, xn, genotype, nc.loco = F, nc.pw = normalizePath("~")) {

  # Input controls
  checkmate::assert_string(x = nc.input)
  checkmate::assert_data_frame(x = nc.table)
  checkmate::assertNumeric(x = xp, len = 1131)
  checkmate::assertNumeric(x = xn, max.len = 1131)

  # First filtering. Removing ecotypes where phenotype has NAs.

  # x collects only the non-missing values from phenotype
  # cofCompl represents the whole collection of phenotypical/environmental data, where
  # ecotypes corresponding to NAs in phenotype are removed.
  x = xp[xn]
  cofCompl = nc.table[xn,]
  print(dim(cofCompl))

  # Fitting linear model

  # Random subsetting of 10 covariates, further filtering of the ecotypes in order to remove
  # all ecotypes from the chosen parameters, building of a linear model and scoring of the model
  # through Akaike's information criterion
  # It is retained the best model out of 1000
  modelsSet = list()
  for (i in 1:1000) {
    cofSub2 = cofCompl[,sample(2:ncol(cofCompl), 10)]
    print(sample(1:ncol(cofCompl), 10))
    newdf2 = as.data.frame(cbind("y" = x, cofSub2))
    newdf2 = newdf2[!is.na(rowMeans(newdf2)),]
    print(head(newdf2))
    full2 = lm(y ~ ., data = newdf2)
    print(full2)
    # Aikake's information criterion
    modelsSet[[i]] = stepAIC(full2)
    full2 = NULL
  }

  bestN = which.min(sapply(1:length(modelsSet), function(x)
    min(modelsSet[[x]]$anova$AIC)))
  bestModel = modelsSet[[bestN]]

  # Covariate vector is identified: fitted values of the best model
  y = normal(bestModel$model$y)
  cv = normal(bestModel$fitted.values)

  # Writing phenotype, covariate, model files
  write.table(y, paste0(nc.pw, "/pheno_", nc.input, "_all"), sep = "\n",
              quote = F, row.names = F, col.names = F)

  write.table(as.data.frame(cbind(1, cv)), paste0(nc.pw, "/covar_", nc.input, "_all"),
              sep = "\t", quote = F, row.names = F, col.names = F)

  write.table(bestModel$model, paste0(nc.pw, "/model_", nc.input, "_all"), sep = "\t", quote = F)

  #LOCO
  if (nc.loco == T) {
    for (c in 1:5) {
      acces = rownames(bestModel$model)
      col1 <- append(c("snpID", "alt", "ref"), acces)
      g1 <- genotype[substr(genotype$snpID, 4, 4) == c,colnames(genotype) %in% col1]
      g2 <- genotype[substr(genotype$snpID, 4, 4) != c,colnames(genotype) %in% col1]

      write.table(g1, paste(nc.pw,"/geno_chr", c, "_", nc.input, "_all", sep = ""), sep = ", ",
                  row.names = F, col.names = F, quote = F)
      write.table(g2, paste(nc.pw,"/geno_notchr", c, "_", nc.input, "_all", sep = ""), sep = ", ",
                  row.names = F, col.names = F, quote = F)

    }
  } else {
    acces = rownames(bestModel$model)
    col1 <- append(c("snpID", "alt", "ref"), acces)
    g1 <- genotype[,colnames(genotype) %in% col1]

    # Writing genotype
    write.table(g1, paste0(nc.pw,"/geno_", nc.input, "_all"), sep = ", ",
                row.names = F, col.names = F, quote = F)
  }

  return()
}
