#' add_param
#'
#' The function adds a new parameter to the dataset and performs a backup. It requires
#' two files: the first containing the actual new parameter data. Inside the file three
#' fields are mandatory: "accession_id", "accession_name", "phenotype_value". The second
#' file reports all the metadata information. (Note that environmental and phenotypical
#' parameters have different sets of metadata. They need to be stored with the correct
#' number of fields and in the correct order). Both the files are in .csv format.
#'
#' After the data file has been read it's fileterd so that only the ecotypes among our
#' selection of 1131 are retained; then the new data are saved in the exaustive
#' corresponding table (phenotypical or environmental).
#'
#' Finally the complete dataset is backed up and two sets of tables are printed: one tagged
#' by the current date, to keep track of history of modifications and a second set tagged as
#' "current" for the actual use and reference.
#'
#' @param ap.newparam Character. Indicates the filename (csv) of the new parameter data.
#' It has to contain at least three fields: accession_id, accession_name, phenotype_value
#' @param ap.metadata Character. Indicates the metadata filename (csv).
#' @param ap.mode Flag. F (default) for phenotypical parameter, T for environmental
#' @param ap.bk Character. Path to the backup folder. (default home folder).
#'
#' @return NULL. It automatically writes a backup.
#' @export
#' @importFrom utils read.csv
#' @import checkmate
#'
#' @examples
#'
#' # Example with environmental data
#' fl = system.file("extdata", "example_add_param.csv", package = "daphneg2")
#' mt = system.file("extdata", "example_add_param_meta.csv", package = "daphneg2")
#' add_param(fl, mt, TRUE)
#'
add_param = function(ap.newparam,ap.metadata, ap.mode = F, ap.bk = normalizePath("~")) {
  #debug_msg("Starting add function. \n")

  ### Checking type of analysis P or E
  # Set up of some default variables. (phenotyical analysis)
  old_table = phenotypical
  explain = phn_explain

  # Controls on ap.mode flag
  checkmate::assert_flag(x = ap.mode, na.ok = F, null.ok = F)

  # Change to values if flag is switched on. (environmental analysis)
  if (ap.mode) {
    old_table = environmental
    explain = env_explain
  }


  ## Controls on input parameters
  # Controls on ap.newparam as string
  checkmate::assert_character(ap.newparam, len = 1, pattern = ".csv")
  checkmate::assert_file(x = ap.newparam)
  # Controls on ap.metadata
  checkmate::assert_character(x = ap.metadata, len = 1, pattern = "_meta.csv")
  checkmate::assert_file(x = ap.metadata)

  # Sets the newparam name without the extension
  par_name = substr(ap.newparam, 1, nchar(ap.newparam) - 4)
  meta_name = substr(ap.metadata, 1, nchar(ap.metadata) - 9)

  # Control on par_name
  checkmate::assert_true(par_name == meta_name)
  checkmate::assert_false(x = par_name %in% colnames(old_table))

  # Read newparam file. From now on ap.newparam is a data.frame
  ap.newparam = utils::read.csv(ap.newparam, stringsAsFactors = F)

  # Controls on ap.newparam as a data.frame
  # It has to contain at least three fields: accession_id, accession_name, phenotype_value
  checkmate::assert_true(x = all(c("accession_id", "accession_name", "phenotype_value") %in%  colnames(ap.newparam)))

  # Read metadata file. from now on is a vector
  ap.metadata = as.character(read.csv(ap.metadata, header = F)[,1])

  # Controls on ap.metadata as a vector
  # It has to have the same length as the number of columns of the metadata table
  checkmate::assert_true(x = length(ap.metadata) == ncol(explain))

  ## Real thing
  # Filtering the new parameter
  # Removing unwanted accession_ids (accessions not present in core dataset)
  tmpv = ap.newparam[ap.newparam$accession_id %in% rownames(old_table), ]
  #debug_msg(paste0("Filter out accession IDs that are not in the core dataset.
  #                 Dimensions of the parameter of interest after first filtering
  #                 ", nrow(tmpv), " x ", ncol(tmpv), " \n"))

  # Removing unwanted accession_names
  tmpv = tmpv[tmpv$accession_name %in% ecotypes$name,]
  #debug_msg(paste0("Filter out accession names that are not in the core dataset.
  #                 Dimensions of the parameter of interest after second filtering
  #                 ", nrow(tmpv), " x ", ncol(tmpv), " \n"))

  # Removing duplicates
  if (sum(duplicated(tmpv$accession_id)) > 0) {
    tmpv = tmpv[-which(duplicated(tmpv$accession_id)),]
  }
  #debug_msg(paste0("Remove duplicates.
  #                 Dimensions of the parameter of interest after third filtering
  #                 ", nrow(tmpv), " x ", ncol(tmpv), " \n"))

  # Order lines in the same order of the core dataset
  tmpv = tmpv[order(tmpv$accession_id),]
  #debug_msg(paste0("Ordering the data according to their accession_id.
  #                 First lines: ", head(tmpv$accession_id), " \n"))



  # Setting up vector to add to the core dataset. The vector contains either the value corresponding to
  # the corresponding ecotype (present in the core dataset), or NA.
  res = ecotypes$ecotypeid %in% tmpv$accession_id
  res[res == T] = tmpv$phenotype_value
  res[res == F] = NA
  #debug_msg(paste0("New parameter vector set up. Dimensions: ", length(res), " \n"))
  #debug_msg(paste0("Reference table dimensions: ", nrow(old_table), " x ", ncol(old_table), " \n"))

  old_table = as.data.frame(cbind(old_table, res))
  #debug_msg("Core dataset updated \n")
  colnames(old_table) = c(colnames(old_table[1:(ncol(old_table)-1)]), par_name)
  #debug_msg("Column names updated \n")
  print(dim(old_table))

  # Update explain tables
  explain = rbind(explain, ap.metadata)


  # Print backups
  bk = set_backup(ap.bk)

  today = gsub(" ", "_", Sys.time(),fixed = T)
  today = gsub(":", "-", today,fixed = T)
  if(!ap.mode) {
    utils::write.csv(old_table, paste0(bk,"/phenotypical_", today, ".csv"), quote = F)
    utils::write.csv(old_table, paste0(bk,"/phenotypical_current.csv"), quote = F)
    utils::write.csv(explain, paste0(bk, "/phn_explain_", today, ".csv"), quote = F, row.names = F)
    utils::write.csv(explain, paste0(bk,"/phn_explain_current.csv"), quote = F, row.names = F)
  } else {
    utils::write.csv(old_table, paste0(bk,"/environmental_", today, ".csv"), quote = F)
    utils::write.csv(old_table, paste0(bk,"/environmental_current.csv"), quote = F)
    utils::write.csv(explain, paste0(bk, "/env_explain_", today, ".csv"), quote = F, row.names = F)
    utils::write.csv(explain, paste0(bk,"/env_explain_current.csv"), quote = F, row.names = F)
  }
  #debug_msg("Print updated tables in backup folder \n")
  print("written")

  # Deletes files older than one year
  #print(paste0("Deleted ",system("find  -type f -mtime +365 | wc -l "), " files older than one year"))
  #system("find  -type f -mtime +365 -delete")

  #debug_msg("Add function completed  successfully \n")
  return()
}
