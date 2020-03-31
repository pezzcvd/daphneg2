#' set_backup
#'
#' Creates the directory infrastructure that allows the user to mantain the dataset
#' up-to-date. The function creates a backup folder where all the parameter tables are
#' stored. This folder location can either be specified by the user or it is set to home
#' directory. Inside "daphneg_backup_dataset" the tables are saved in .csv format;tables are nested
#' here is created a second folder "RData" where a copy in .RData format is also stored
#' and this file is the one that is going to be used in the anlaysis.
#'
#' Set_backup() is also called by the function add_param() and synchro() in order
#' to make sure the system is always up-to-date before performing every other calculation.
#'
#' @param sb.dest Character. path where to create backup folder. (home directory as default)
#'
#' @return String of the backup folder. It automatically creates the infrastructure for dataset management.
#' It will be used by other functions.
#' @export
#' @importFrom utils write.csv
#'
#' @examples
#' # Example from default folder
#' set_backup()
#' # Example from custom folder
#' set_backup("/home/pejo/Documenti")
#'
set_backup = function(sb.dest = normalizePath("~")){
  mychar = unique(sapply(1:nchar(sb.dest), function(x) substr(sb.dest, x, x)))
  possibleChars = c(0:9, letters, LETTERS, "_", "-", "/", ".")
  checkmate::assert_true(all(mychar %in% possibleChars))

  # Set dest to daset and RData paths
  dest = paste0(sb.dest, "/daphneg_backup_dataset")
  RData = paste0(dest, "/RData")

  # Creates the directory if it doesn't exist
  if (!dir.exists(dest)) {
    print("Creates the backups folder")
    # Create destination folder
    dir.create(dest)
  }

  # If the directory is empty it populates it
  if (length(list.files(path = dest)) < 2) {
    print("Populate the backups folder")
    # Writes an initial set of *_current.csv versions of the core dataset
    write.csv(ecotypes, paste0(dest, "/ecotypes_current.csv"))
    write.csv(environmental, paste0(dest, "/environmental_current.csv"))
    write.csv(env_explain, paste0(dest, "/env_explain_current.csv"))
    write.csv(phenotypical, paste0(dest, "/phenotypical_current.csv"))
    write.csv(phn_explain, paste0(dest, "/phn_explain_current.csv"))
  }

  # Checks the existence of RData folder
  if (length(list.dirs(path = dest)) <= 1) {
    print("Create RData folder")
    # Create RData folder to stock current version of datasets
    dir.create(RData)
  }

  # Checks if RData folder is empty
  if (length(list.files(path = RData)) == 0) {
    print("Create RData files")
    save(ecotypes, environmental, env_explain, phenotypical, phn_explain, dest,
         file = paste0(RData, "/complete_dataset.RData"))
    #save(genotypes, file = paste0(dest, "/RData/prova_geno.RData"))
  }

  return(dest)
}
