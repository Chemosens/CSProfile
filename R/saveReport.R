#' @title Save a list of outputs as an HTML report.
#' @param x List of outputs
#' @param fileName Name of the HTML file
#' @export
saveReport=function(x, fileName) {
  html=list()
  
  #htmltools::h1(title)  
  
  for (i in 1:length(x)) {
    for (j in 1:length(x[[i]])) {
      output=x[[i]][[j]]$output[[1]]
      # Enregistrement
      if ("ggplot" %in% class(output)) {
        html[[length(html)+1]]=gSavePlot(output, "html", "tmp")
      }
      
      if ("kableExtra" %in% class(output)) {
        html[[length(html)+1]]=htmltools::HTML(output)
      }
    }
  }
  
  d=dirname(fileName)
  dir.create(d)
  htmltools::save_html(html, fileName)
}