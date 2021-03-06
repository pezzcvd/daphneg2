% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_backup.R
\name{set_backup}
\alias{set_backup}
\title{set_backup}
\usage{
set_backup(sb.dest = normalizePath("~"))
}
\arguments{
\item{sb.dest}{Character. path where to create backup folder. (home directory as default)}
}
\value{
String of the backup folder. It automatically creates the infrastructure for dataset management.
It will be used by other functions.
}
\description{
Creates the directory infrastructure that allows the user to mantain the dataset
up-to-date. The function creates a backup folder where all the parameter tables are
stored. This folder location can either be specified by the user or it is set to home
directory. Inside "daphneg_backup_dataset" the tables are saved in .csv format;tables are nested
here is created a second folder "RData" where a copy in .RData format is also stored
and this file is the one that is going to be used in the anlaysis.
}
\details{
Set_backup() is also called by the function add_param() and synchro() in order
to make sure the system is always up-to-date before performing every other calculation.
}
\examples{
# Example from default folder
set_backup()
# Example from custom folder
set_backup("/home/pejo/Documenti")

}
