##' Input of urban temporalities
##'
##' Reads a file containing urban temporalities in table format and creates a data frame from it.
##' @param file the name of the file which the data are to be read from.
##' The first line (header) should contain column names.
##' Columns should be separated by a tabulation.
##' @return data frame
##' @author Timothee Flutre
##' @export
readTemporalities <- function(file){
  dat <- utils::read.table(file=file, header=TRUE, sep="\t")
  return(dat)
}
