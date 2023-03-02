#' @title Plot the table of mrCa test per cell
#' @description Return a HTMl object.
#' @param res Result of mrCa test per cell
#' @param alpha TODO
#' @param choice  TODO
#' @param col.greater  TODO
#' @param col.lower  TODO
#' @param caption  TODO
#' @param format  TODO
#' @param fileName  TODO
#' @param ... Further parameters
#' @return a graph TODO

plot.mr.sig.cell=function (res, alpha = 0.075, choice = "percent.derived.cont", col.greater = "green", col.lower = "orange", caption="", format="", fileName="",...) 
{
  classe = class(res)
  if (classe != "list") {
    stop("res must be a list resulting from the execution of sensory.mr.sig.cell or mr.sig.cell")
  }
  taille = length(res)
  if (taille != 6) {
    stop("res must be a list resulting from the execution of sensory.mr.sig.cell or mr.sig.cell")
  }
  if (!choice %in% c("original.cont", "percent.cont", "null.cont", "p.value", "derived.cont", "percent.derived.cont")) {
    stop("choice is not valid")
  }
  ou.choice = which(names(res) == choice)
  plot.mat = res[[ou.choice]]
  mat.pval = res$p.value
  mat.exp = res$null.cont
  mat.obs = res$derived.cont
  plot.col = plot.mat
  for (i in 1:nrow(plot.col)) {
    for (j in 1:ncol(plot.col)) {
      if (mat.pval[i, j] <= alpha & mat.obs[i, j] > mat.exp[i, j]) {
        plot.col[i, j] = col.greater
      }
      else if (mat.pval[i, j] <= alpha & mat.obs[i, j] < mat.exp[i, j]) {
        plot.col[i, j] = col.lower
      }
      else {
        plot.col[i, j] = "white"
      }
    }
  }
  
  plot.col = apply(plot.col, 2, as.character)
  
  colSpec=list()
  for (j in 1:ncol(plot.mat)) {
    colSpec[[j]]=list(index=j+1, background=as.vector(plot.col[,j]))
  }
  plot.mat=cbind(row.names(plot.mat),plot.mat)
  colnames(plot.mat)[1]="descriptor"
  row.names(plot.mat)=NULL
  
  dfToFile(df=plot.mat, format=format, fileName=fileName, caption=caption, colSpec=colSpec)
}