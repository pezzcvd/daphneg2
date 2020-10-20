#' gwas
#'
#' it performs a gwas analysis. It collects all the input files: phenotype, genotype and,
#' in case, covariate. Then through the software GEMMA it calculates the kinship matrix,
#' and it performs the assosiaction analysis.
#' Subsequently it adujsts the P-values of the associations, calculating FDRs and since the
#' genotype table is based on a pruned SNP list (meaning that the SNPs that are inherited
#' together with a target SNPs are filtered out) it eventually retrieves the lists of SNPs
#' in the same LD units with the significant SNPs in the result table.
#'
#' @param gemma.name character{1}. Gemma executable name. Version or system may vary.
#' @param gw.input character{1}. Parameter name, used as a prefix in the files of interest.
#' @param gw.cv integer{1}. Number of covariates. Can be 0, 1 or 2 (in this case is meant >1).
#' @param gw.snp_annot character{1}. Snp annotation file path.
#' @param gw.gen_annot character{1}. Genome annotation file path.
#' @param gw.cov character{1}. Covariate name, can be one specific covariate or "all". (defalut NULL)
#' @param gw.loco boolean(1). Leave one chromosome out approach. (default FALSE)-.
#' @param gw.pw character{1}. Output path. (default home folder).
#' @param gw.miss missingness
#' @param gw.maf minor allele frequency
#' @param gw.kinship precomputed kinship file name, default ""
#'
#' @return NULL. It automatically performs the whole GWAS analysis procedure.
#' @export
#' @import filesstrings
#'
#' @examples
gwas = function(gemma.name, gw.input, gw.cv, gw.snp_annot, gw.gen_annot, gw.cov = NULL, gw.loco = F,
                gw.miss = 0.05, gw.maf = 0.05, gw.kinship = "", gw.pw = normalizePath("~")) {

  # Loading updated dataset
  load(paste0(gw.pw, "/daphneg_backup_dataset/RData/complete_dataset.RData"))

  # Input controls
  checkmate::assert_string(x = gw.input)
  checkmate::assert_choice(gw.cv, choices = 0:2, null.ok = F)
  checkmate::assert_choice(x = gw.cov, null.ok = T,
                           choices = c("all", colnames(environmental), colnames(phenotypical)))
  checkmate::assert_string(x = gemma.name)
  checkmate::assert_string(x = gw.pw)

  # Setting up initial parameters.
  # Suffix depending on number of covariates.
  if (gw.cv == 0) {
    suf = ""
  }

  if (gw.cv == 1) {
    suf = paste0("_", gw.cov)
  }

  if (gw.cv == 2) {
    suf = "_all"
  }

  print(gw.cv)

  # Sets gemma
  gemman = gemma.name
  print(gemman)
  input_name = paste0(gw.input, suf)
  print(input_name)

  # Kinship
  if (gw.kinship == "") {
    kinship(input_name, gemman, gw.loco, gw.pw)
  }


  #GWAS
  gemma(input_name, gemman, gw.cv, gw.snp_annot, gw.miss, gw.maf, gw.kinship, gw.loco, gw.pw)

  # Adjusting P-values
  adjust(input_name, gw.gen_annot, gw.pw)

  # Managing output
  print("create res folder")
  # Files/folders management
  if (!dir.exists(paste0(gw.pw, "/daphneg_results"))){
    dir.create(paste0(gw.pw, "/daphneg_results"))
  }

  res_dir = paste0(gw.pw, "/daphneg_results")
  print(res_dir)

  if (gw.loco == T) {
    dir.create(paste0(res_dir, "/", input_name,"_loco_dir"))
    res_folder = paste0(res_dir, "/", input_name,"_loco_dir")
  } else {
    dir.create(paste0(res_dir, "/", input_name,"_dir"))
    res_folder = paste0(res_dir, "/", input_name,"_dir")
  }

  print("moving files")
  outfiles = list.files(path = paste0(gw.pw, "/output"), pattern = paste0(input_name), full.names = T)
  filesstrings::file.move(files = outfiles,
                          destinations = res_folder, overwrite = T)
  filesstrings::dir.remove(paste0(gw.pw, "/output"))

  outfiles = list.files(path = gw.pw, pattern = paste0(input_name), full.names = T)
  filesstrings::file.move(files = outfiles,
                          destinations = res_folder, overwrite = T)

  return()
}
