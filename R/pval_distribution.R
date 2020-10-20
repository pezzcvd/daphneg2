#' pval_distribution
#'
#' it plots the histogram distribution corresponding to the requested parameter.
#'
#' @param h.par character{1}. Parameter name, used as a prefix in the files of interest.
#' @param h.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. It automatically produces a plot.
#' @export
#'
#' @examples
pval_distrubution = function(h.par, h.pw = normalizePath("~")){
  # Loading updated dataset
  load(paste0(h.pw, "/daphneg_backup_dataset/RData/complete_dataset.RData"))

  # Input controls
  checkmate::assert_character(x = h.par, any.missing = F, len = 1)

  # Setting the gwas result file path.
  fl = paste0(h.pw, "/daphneg_results/", h.par, "_dir/out_", h.par, ".assoc.txt")
  print(fl)

  # Reading file.
  fl = read.delim(fl, stringsAsFactors = F)

  ggplot2::ggplot(data = fl, ggplot2::aes(x = p_wald)) +
    ggplot2::geom_histogram(binwidth = 0.1) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::ggtitle(paste0("P-values distribution ", h.par)) +
    ggplot2::xlab("P-values") +
    ggplot2::ylab("Count")
  ggplot2::ggsave(paste0(h.pw, "/daphneg_results/", h.par, "_dir/", h.par, "_hist.png"))

  return()
}
