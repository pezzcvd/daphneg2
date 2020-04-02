#' synchro
#'
#' It takes care that the system it's synchronized - all the parameters added to csv
#' files in the backup folder should be present also in the RData file.
#' Updates the data Rd files if a more recent backup is found.
#'
#' It's hypothically possible to syncrhonize the system with a version that is previous
#' to the current one. In this case the parameter s.version has to be specified with
#' the correct version the user wants, tagged with date and time in the format
#' yyyy-mm-dd_hh-mm-ss.
#'
#' @param s.mode Flag. F (default) for phenotypical parameter, T for environmental.
#' @param s.bk Character. Path to the backup folder. (default home folder).
#' @param s.version character. Tells which version should be saved in the .RData file ("current" default)
#'
#' @return NULL. It automatically updates RData files.
#' @export
#' @import utils
#'
#' @examples
#' # Example with phenotypical data
#' synchro()
#'
#' # Example with environmental data
#' synchro(s.mode = TRUE)
#'
#'
synchro = function(s.mode = T, s.bk = normalizePath("~"), s.version = "current"){
  #debug_msg("Starting synchro function \n")
  checkmate::assert_string(x = s.bk, null.ok = F)
  checkmate::assert_string(x = s.version, null.ok = F)

  bk = set_backup(s.bk)

  # Set initial parameters and tables depending if the analysis is environmental (s.mode = 1) or phenotypical
  # tmp and tmpx are the current versions, coming from the last run of add_param()
  # s.table and s.explain are the Rd tables in the dataset
  checkmate::assert_logical(x = s.mode, len = 1, null.ok = F)
  if (s.mode) {
    #debug_msg("Environmental. Define updated files. \n")
    tmp = read.csv(paste0(bk, "/environmental_", s.version, ".csv"), stringsAsFactors = F)
    tmpx = read.csv(paste0(bk, "/env_explain_", s.version, ".csv"), stringsAsFactors = F)
    s.table = environmental
    s.explain = env_explain
  } else {
    #debug_msg("Phenotypical. Define updated files. \n")
    tmp = read.csv(paste0(bk, "/phenotypical_", s.version, ".csv"), stringsAsFactors = F)
    tmpx = read.csv(paste0(bk, "/phn_explain_", s.version, ".csv"), stringsAsFactors = F)
    s.table = phenotypical
    s.explain = phn_explain
  }

  #debug_msg("Comparing RData with external files. \n")
  print(dim(tmp))
  print(dim(s.table))
  print(" ")
  print(dim(tmpx))
  print(dim(s.explain))

  # Comparison of dimensions of the tables
  # If they match the system is already synchronized
  if (nrow(tmp) == nrow(s.table) & nrow(tmpx) == nrow(s.explain)) {
    print("The core dataset is up to date \n")

  } else {
    # Otherwise the data tables are updated and saved
    print("The core dataset needs to be updated \n")
    if (s.mode) {
      environmental = tmp
      env_explain = tmpx
    } else {
      phenotypical = tmp
      phn_explain = tmpx
    }

    save(ecotypes, environmental, env_explain, phenotypical, phn_explain,
         file = paste0(bk, "/RData/complete_dataset.RData"))
    print("Updated")
  }

  #debug_msg("Synchro function has completed successfully \n")
  return()
}
