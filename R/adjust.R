#' adjust
#'
#' Internal function. It adds adjusted P-values to the result table. The function adjustment
#' is calulated with False Discovery Rate values, calculated with Benjamin-Hochberg and
#' Benjamin-Yetutiely methods.
#'
#' @param adj.par character{1}. Parameter name, used as a prefix in the files of interest.
#' @param adj.pw character{1}. Output path. (default home folder).
#' @return NULL. It updates the GWAS result file with two more columns.
#'
#' @importFrom stats p.adjust
#'
#' @noRd
adjust = function(adj.par, adj.pw = normalizePath("~")){    #, adj.pw = normalizePath("~")) {
  # Input controls
  checkmate::assert_choice(x = adj.par, null.ok = T,
                           choices = c(colnames(environmental), colnames(phenotypical)))
  #checkmate::assert_character(x = adj.pw, any.missing = F, len = 1)

  #debug_msg("Starting adjust procedure \n")
  #debug_msg("Reading file \n")
  fls = list.files(pattern = paste0(adj.par, ".assoc"), path = paste0(adj.pw, "/output/"), full.names = T)
  a = read.delim(fls, stringsAsFactors = F)
  #debug_msg("Adjusting \n")
  a = cbind(a[,1:12], "BY" = p.adjust(a$p_wald, method = "BY"), "BH" = p.adjust(a$p_wald, method = "BH"))

  #write file with corrected pvals
  #debug_msg("Writing adjusted file \n")
  write.table(a, fls, sep = "\t", quote = F, row.names = F)

  #debug_msg("Adjust function completed successfully \n")
  return()
}
