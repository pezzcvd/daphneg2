#' tagged_snpns
#'
#' @param tg.input
#' @param tg.pw
#' @return
#'
#' @noRd
tagged_snps = function(tg.input, tg.pw){
  tags = read.delim("/home/pejo/plink1-09/tests/test_tagged02.tags.list",
                    stringsAsFactors = F)
  print("load")
  assoc = read.delim(paste0(tg.pw, "/daphneg_results/", tg.input, "_dir/out_", tg.input, ".assoc.txt"),
                     stringsAsFactors = F)
  hits = assoc$rs[assoc$p_wald < 10e-5 & assoc$rs %in% tags$SNP]

  print("tagging")
  tagged = data.frame()
  for (i in 1:length(hits)) {
    now = hits[i]
    expd = assoc[assoc$rs == now,]
    if (tags$NTAG[tags$SNP == now] > 0) {
      expd = do.call("rbind", replicate(tags$NTAG[tags$SNP == now], expd, simplify = FALSE))
      #ids = strsplit(tags$TAGS[tags$SNP == now], "[|]")[[1]]
      expd$rs = strsplit(tags$TAGS[tags$SNP == now], "[|]")[[1]]
      tagged = rbind(tagged, expd)
    }
  }

  tg = c(rep(T, nrow(assoc)), rep(F, nrow(tagged)))

  tagged_assoc = rbind(assoc, tagged)
  tagged_assoc = cbind(tagged_assoc, tg)

  print("writing")
  write.table(tagged_assoc,
              paste0(tg.pw, "/daphneg_results/", tg.input, "_dir/out_", tg.input, ".assoc.tagged.txt"),
              sep = "\t", quote = F, row.names = F)
  return()
}
