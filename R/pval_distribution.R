#' hist_distr
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
hist_distr = function(h.par, h.pw = normalizePath("~")){
  # Input controls
  checkmate::assert_character(x = h.par, any.missing = F, len = 1)

  # Setting the gwas result file path.
  fl = paste0(h.pw, "/daphneg_results/", h.par, "_dir/out_", h.par, ".assoc.txt")
  print(fl)
  #fl = paste0("results/gwas/", h.par, "_dir/output/out_", h.par, ".assoc.txt")

  # Reading file.
  fl = read.delim(fl, stringsAsFactors = F)
  #now_pval = list()
  #now_pval[[1]] = fl$p_wald
  #now_pval[[2]] = fl$BY
  #now_pval[[3]] = fl$BH

  #df = as.data.frame(cbind("nadj" = now_pval[[1]], "by" = now_pval[[2]], "bh" =now_pval[[3]]))

  #png(paste0(paste0("hist_", tit, ".png")))
  ggplot2::ggplot(data = fl, ggplot2::aes(x = p_wald)) +
    ggplot2::geom_histogram(binwidth = 0.1) +
    #+ geom_point(aes(y = op), pch = 1) + geom_line(aes(y = ep), col = "red") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::ggtitle(paste0("P-values distribution ", h.par)) +
    ggplot2::xlab("P-values") +
    ggplot2::ylab("Count")
  #dev.off()
  ggplot2::ggsave(paste0(h.pw, "/daphneg_results/", h.par, "_dir/", h.par, "_qq.png"))
  #ggsave(paste0("results/gwas/", h.par, "_dir/output/", h.par, "_hist.png"))
  return()
}
