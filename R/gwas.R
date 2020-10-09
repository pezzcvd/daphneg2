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
#' @param gw.annot annotation
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
gwas = function(gemma.name, gw.input, gw.cv, gw.annot, gw.cov = NULL, gw.loco = F,
                gw.miss = 0.05, gw.maf = 0.05, gw.kinship = "", gw.pw = normalizePath("~")) {
  #debug_msg("Starting gwas function. \n")
  # Loading updated dataset
  #load(paste0(gw.pw, "/daphneg_backup_dataset/RData/complete_dataset.RData"))

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


  # Looks for the path of gemma executable.
  #gemman = list.files(path = gw.pw, pattern = gemma.name,
   #                  recursive = T, full.names = T)[1]
  gemman = gemma.name
  print(gemman)
  input_name = paste0(gw.input, suf)
  print(input_name)


  # Kinship
  if (gw.kinship == "") {
    kinship(input_name, gemman, gw.loco, gw.pw)
  }


  #GWAS
  gemma(input_name, gemman, gw.cv, gw.annot, gw.miss, gw.maf, gw.kinship, gw.loco, gw.pw)

#  if (gw.cv > 0) {
#    system(paste0(gemma, " -g ", gw.pw, "/geno_", input_name,
#                  " -p  ", gw.pw, "/pheno_", input_name,
#                  " -k  ", gw.pw, "/output/kinship_", input_name, ".cXX.txt",
#                  " -c  ", gw.pw, "/covar_", input_name,
#                  " -a  ", gw.annot,
#                  " -miss ", gw.miss, " -maf ", gw.maf,
#                  " -lmm 1 -o out_", input_name,
#                  " -outdir ", gw.pw,"/output"))

#  } else {
#    system(paste0(gemma, " -g  ", gw.pw, "/geno_", input_name,
#                  " -p  ", gw.pw, "/pheno_", input_name,
#                  " -k  ", gw.pw, "/output/kinship_", input_name, ".cXX.txt",
#                  " -a  ", gw.annot,
#                  " -miss ", gw.miss, " -maf ", gw.maf,
#                  " -lmm 1 -o out_", input_name,
#                  " -outdir ", gw.pw,"/output"))
#  }
  #debug_msg("GWAS analysis completed. \n")

  # Adjusting P-values
  adjust(input_name, gw.pw)
  #debug_msg("P-values adjusted \n")

  # Expanding results to tagged SNPs
  #tagged_snps(input_name, tag.file, gw.pw)

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

  #debug_msg("Results folder created \n")

  print("moving files")
  outfiles = list.files(path = paste0(gw.pw, "/output"), pattern = paste0(input_name), full.names = T)
  filesstrings::file.move(files = outfiles,
                          destinations = res_folder, overwrite = T)
  filesstrings::dir.remove(paste0(gw.pw, "/output"))

  outfiles = list.files(path = gw.pw, pattern = paste0(input_name), full.names = T)
  filesstrings::file.move(files = outfiles,
                          destinations = res_folder, overwrite = T)

  #filesstrings::file.move(files = paste0(gw.pw, "/pheno_", input_name),
  #                        destinations = paste0(res_dir, "/", input_name, "_dir"))
  #filesstrings::file.move(files = paste0(gw.pw, "/geno_", input_name),
  #                        destinations = paste0(res_dir, "/", input_name, "_dir"))

  #system(paste0("mv pheno_", input_name, " results/gwas/", input_name,"_dir"))
  #system(paste0("mv geno_", input_name, " results/gwas/", input_name,"_dir"))
#  if (gw.cv > 1) {
#    filesstrings::file.move(files = paste0(gw.pw, "/model_", input_name),
#                            destinations = paste0(res_dir, "/", input_name, "_dir"))
    #system(paste0("mv model_", input_name, " results/gwas/", input_name,"_dir"))
#  }
#  if (gw.cv > 0) {
#    filesstrings::file.move(files = paste0(gw.pw, "/covar_", input_name),
#                            destinations = paste0(res_dir, "/", input_name, "_dir"))
    #system(paste0("mv covar_", input_name, " results/gwas/", input_name,"_dir"))
#  }

  #out_files = list.files(path = gw.pw, pattern = "output", all.files = T, full.names = T)
  #filesstrings::file.move(files = out_files,
  #                        destinations = paste0(res_dir, "/", input_name, "_dir"))
  #system(paste0("mv output results/gwas/", input_name,"_dir"))
  #debug_msg("Moving all files in result folder \n")

  #debug_msg("gwas function completed successfully \n")
  return()
}
