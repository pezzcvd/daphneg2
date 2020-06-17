#' manhattan
#'
#' produces mnhattan plots
#'
#' @param mh.par character{1}. Parameter name, used as a prefix in the files of interest.
#' @param mh.pw character{1}. Output path. (default home folder).
#'
#' @return NULL. It automatically produces a plot.
#' @export
#' @importFrom  dplyr group_by summarise mutate select left_join arrange %>%
#'
#' @examples
manhattan = function(mh.par, mh.pw = normalizePath("~")) {
  # Loading updated dataset
  load(paste0(mh.pw, "/daphneg_backup_dataset/RData/complete_dataset.RData"))

  # Input controls
  checkmate::assert_character(x = mh.par, any.missing = F, len = 1)

  # Setting the gwas result file path.
  fl = paste0(mh.pw, "/daphneg_results/", mh.par, "_dir/out_", mh.par, ".assoc.txt")
  #fl = paste0("results/gwas/", mh.par, "_dir/output/out_", mh.par, ".assoc.txt")
  # Reading file.
  fl = read.delim(fl, stringsAsFactors = F)

  # File content controls
  #checkmate::assert_data_frame(x = fl, ncols = 14)

  # Parameters set up, we have three analysis (nonadj, by. bh)
  # and for each of them three thresholds.
  # For each analysis we produce one plot where three thresholds are included.
  cur_pval = 12:14
  tits = c("_nonadj", "_by", "_bh")
  thresh = list()
  thresh[[1]] = c(1e-5, 1e-6, 1e-7)
  thresh[[2]] = c(0.2, 0.1, 0.05)
  thresh[[3]] = c(0.1, 0.05, 0.01)

  for (i in 1:3) {
    gwasResults = fl[,c(1:3, cur_pval[i])]

    tit = paste0(mh.par, tits[i])
    print(tit)
    gwasResults = gwasResults[!is.na(gwasResults[,ncol(gwasResults)]),]
    colnames(gwasResults) = c("CHR", "SNP", "BP", "P")
    print(head(gwasResults))
    if (nrow(gwasResults) > 0) {
      don <- gwasResults %>%

        # Compute chromosome size
        group_by(CHR) %>%
        summarise(chr_len=max(BP)) %>%

        # Calculate cumulative position of each chromosome
        mutate(tot=cumsum(chr_len)-chr_len) %>%
        select(-chr_len) %>%

        # Add this info to the initial dataset
        left_join(gwasResults, ., by=c("CHR"="CHR")) %>%

        # Add a cumulative position of each SNP
        arrange(CHR, BP) %>%
        mutate( BPcum=BP+tot)

      #Then we need to prepare the X axis. Indeed we do not want to display the cumulative position of SNP in bp, but just show the chromosome name i$
      axisdf = don %>% dplyr::group_by(CHR) %>% dplyr::summarize(center=( max(BPcum) + min(BPcum) ) / 2 )

      #png(paste0("results/gwas/", mh.par, "_dir/output/manhattan_", tit[i], ".png"))
      #png("prova.png")
      ggplot2::ggplot(don, ggplot2::aes(x=BPcum, y=-log10(P))) + ggplot2::ggtitle(tit) +  ggplot2::labs(x = "Chromosome") +

        # Show all points
        ggplot2::geom_point(ggplot2::aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
        ggplot2::scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +

        #add title and axis
        ggplot2::geom_hline(yintercept = -log10(thresh[[i]][1]), col = "goldenrod", linetype = "dashed") +
        ggplot2::geom_hline(yintercept = -log10(thresh[[i]][2]), col = "orange", linetype = "longdash") +
        ggplot2::geom_hline(yintercept = -log10(thresh[[i]][3]), col = "red", linetype = "twodash") +
        # custom X axis:
        ggplot2::scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
        ggplot2::scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
        # Custom the theme:
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size=14, face="bold.italic"),
          legend.position="none",
          panel.border = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
      #dev.off()
      #ggsave(paste0("plots/manhattan/", tit, "_manhattan.png"))
      ggplot2::ggsave(paste0(mh.pw, "/daphneg_results/", mh.par, "_dir/", tit, "_manhattan.png"))
      #ggsave(paste0("results/gwas/", mh.par, "_dir/output/", tit, "_manhattan.png"))
      #dev.off()

    }
  }
  return()
}
