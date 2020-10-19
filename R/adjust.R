#' adjust
#'
#' Internal function. It adds adjusted P-values to the result table. The function adjustment
#' is calulated with False Discovery Rate values, calculated with Benjamin-Hochberg and
#' Benjamin-Yetutiely methods.
#'
#' @param adj.par character{1}. Parameter name, used as a prefix in the files of interest.
#' @param adj.gen_annot character{1}- Genome annotation file path.
#' @param adj.pw character{1}. Output path. (default home folder).
#' @return NULL. It updates the GWAS result file with two more columns.
#'
#' @importFrom stats p.adjust
#' @importFrom rtracklayer import.gff3
#' @importFrom GenomicRanges GRanges findOverlaps
#' @importFrom S4Vectors subjectHits queryHits
#' @importFrom IRanges IRanges
#' @noRd
adjust = function(adj.par, adj.gen_annot, adj.pw = normalizePath("~")){    #, adj.pw = normalizePath("~")) {
  # Input controls
  #checkmate::assert_choice(x = adj.par, null.ok = T,
   #                        choices = c(colnames(environmental), colnames(phenotypical)))
  #checkmate::assert_character(x = adj.pw, any.missing = F, len = 1)

  #debug_msg("Starting adjust procedure \n")
  #debug_msg("Reading file \n")
  fls = list.files(pattern = paste0(adj.par, ".assoc"), path = paste0(adj.pw, "/output/"), full.names = T)

  annot = import.gff3(adj.gen_annot)
  annot = annot[annot$type == "gene",]


  for (c in 1:length(fls)) {
    # correct P-values
    a = read.delim(fls[c], stringsAsFactors = F)
    #debug_msg("Adjusting \n")
    a = cbind(a[,1:12], "BY" = p.adjust(a$p_wald, method = "BY"), "BH" = p.adjust(a$p_wald, method = "BH"))

    # add gene info
    ##########
    chr_tmp = sapply(1:nrow(a), function(x) paste0("Chr", a$chr[x]))
    hitsGR = GRanges(chr_tmp, IRanges(a$ps, a$ps))
    atgcode = rep("intergenic", length(hitsGR))
    symbcode = rep("intergenic", length(hitsGR))
    idx = unique(queryHits(findOverlaps(hitsGR, annot)))
    atgcode[idx] = annot$Name[subjectHits(findOverlaps(hitsGR, annot))[!duplicated(queryHits(findOverlaps(hitsGR, annot)))]]
    symbcode[idx] = annot$symbol[subjectHits(findOverlaps(hitsGR, annot))[!duplicated(queryHits(findOverlaps(hitsGR, annot)))]]
    a = cbind(a, "atg_genename" = atgcode, "gene_name" = symbcode)








    #write file with corrected pvals
    #debug_msg("Writing adjusted file \n")
    write.table(a, fls[c], sep = "\t", quote = F, row.names = F)

    #debug_msg("Adjust function completed successfully \n")
  }


  return()
}
