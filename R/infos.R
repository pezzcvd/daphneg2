#' infos
#'
#' The function belongs to the dataset management part. It searches a parameter of interest
#' (either phenotypical or environmental) against all the parameters already present in the
#' collection. If the parameter is there the function returns all the information and metadata
#' about this parameter. Otherwise it prints an error message and the user should first
#' use the add_param function and then try again.
#'
#' @param i.input Character. Parameter to print.
#' @param i.pw character{1}. Output path. (default home folder).
#'
#' @return Detailed information for the selected environmental/phenotypical parameter
#' @export
#' @import checkmate
#'
#' @examples
#' # Checking info for an environmental parameter.
#' infos("CO_Spring")
#'
#' # Checking info for a phenotypical parameter.
#' infos("LN16_87")
#'
#'
infos = function(i.input, i.pw = normalizePath("~")) {
  # Loading updated dataset
  load(paste0(i.pw, "/daphneg_backup_dataset/RData/complete_dataset.RData"))

  # Control on function input
  checkmate::assert_string(x = i.input)
  checkmate::assert_choice(x = i.input, choices = c(env_explain$ID, phn_explain$ID))

  # Sets ref to an empty data.frame
  ref = data.frame()
  # Checking if parameter is present in env_explain table
  # so if it is an environmental parameter
  if (i.input %in% env_explain$ID) {
    ref = env_explain
  }

  # Checking if parameter is present in phn_explain table
  # so if it is a phenotypical parameter
  if (i.input %in% phn_explain$ID) {
    ref = phn_explain
  }

  # Sets the right table to ref
  # Finds the index of the corresponding anetry in the file and displays it
  i.idx = which(ref$ID == i.input)

  return(ref[i.idx,])
}
