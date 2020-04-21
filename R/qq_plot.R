#' qq_plot
#'
#' it plots the qq plot corresponding to the requested parameter.
#'
#' @param qq.par character{1}. Parameter name, used as a prefix in the files of interest.
#' @param qq.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. It automatically produces a plot.
#' @export
#'
#' @examples
qq_plot = function(qq.par, qq.pw = normalizePath("~")){
  # Loading updated dataset
  load(paste0(qq.pw, "/daphneg_backup_dataset/RData/complete_dataset.RData"))

  # Input controls
  checkmate::assert_character(x = qq.par, any.missing = F, len = 1)

  # Setting the gwas result file path.
  fl = paste0(qq.pw, "/daphneg_results/", qq.par, "_dir/out_", qq.par, ".assoc.txt")
  print(fl)
  #fl = paste0("results/gwas/", qq.par, "_dir/output/out_", qq.par, ".assoc.txt")
  # Reading file.
  fl = read.delim(fl, stringsAsFactors = F)

  # File content controls
  checkmate::assert_data_frame(x = fl, ncols = 14)

  tits = c("_nonadj", "_by", "_bh")
  now_pval = list()
  now_pval[[1]] = fl$p_wald
  now_pval[[2]] = fl$BY
  now_pval[[3]] = fl$BH
  now_lab = c("Non-corrected P-value", "BY-corrected P-value", "BH-corrected P-value")

  for (i in 1:3) {
    #qqplots
    tit = paste0(qq.par, tits[i])
    df = as.data.frame(cbind("ep" = -log10(seq(0,1, 1/(length(now_pval[[i]]) - 1))),
                             "op" = -log10(sort(now_pval[[i]]))))


    #png(paste0(paste0("qqPlot_", tit, ".png")))
    ggplot2::ggplot(data = df, ggplot2::aes(x = ep)) +
      ggplot2::geom_point(ggplot2::aes(y = op), pch = 1) +
      ggplot2::geom_line(ggplot2::aes(y = ep), col = "red") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::ggtitle(paste0("QQ_Plot ", qq.par, "\n ", now_lab[[i]])) +
      ggplot2::xlab("Theoretical quantiles (-Log10)") +
      ggplot2::ylab("Sample quantiles (-Log10)")
    #dev.off()
    #print(paste0(normalizePath("~"), "/assstufa_results/", qq.par, "_dir/", qq.par, tit, "_qq.png"))
    ggplot2::ggsave(paste0(qq.pw, "/daphneg_results/", qq.par, "_dir/", tit, "_qq.png"))
    #ggsave(paste0("results/gwas/", qq.par, "_dir/output/", tit, "_qq.png"))
  }
  return()
}
