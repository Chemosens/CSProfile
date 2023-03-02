#' @title Read an external file
#' @param path Delimited text file, xls or xlsx file path 
#' @param sep Decimal separator, if delimited text file
#' @return Dataframe
#' @export
readXlsxOrCsv=function(path, sep=";") {
  ext <- strsplit(basename(path), split="\\.")[[1]][-1]
  df <- NULL
  if (ext == "xls" | ext == "xlsx") {
    df<-openxlsx::read.xlsx(path)
  }
  if (ext == "txt" | ext == "csv") {
    df = utils::read.csv(path, header=TRUE, sep=sep)
  }
  return(df)
}