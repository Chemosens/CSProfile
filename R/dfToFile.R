#' @title Transform a dataframe into a HTML table.
#' @description The dataframe can be styled and saved in an external file.
#' @param df A dataframe.
#' @param format The format of the file to be saved: "html", "png", "pdf" or "jpeg".
#' @param fileName The name of the file to be saved.
#' @param caption Caption for the html table.
#' @param colSpec A list of list used to format the table columns, with following elements: index, name, width, background, bold, italic, color, border_left, border_right#'
#' @return An object of class kable.
#' @importFrom CSUtils cleanString
dfToFile=function(df, format="", fileName="", caption="", colSpec=list()) {

  if (fileName == "") {
    fileName=stringr::str_replace_all(cleanString(stringr::str_replace(Sys.time(),"CET",""))," ","")
  }

  # Noms de colonnes
  if (length(colSpec)>0) {
    for (i in 1:length(colSpec)) {
      l=colSpec[[i]]
      if (is.null(l$name)==FALSE) {
        colnames(df)[l$index]=l$name
      }
    }
  }

  html=kableExtra::kable(df,format = "html", caption = caption)
  fhtml=kableExtra::kable_styling(html, "striped", position = "left", font_size = 12, full_width = F)

  for (i in 1:ncol(df)) {

    width="100"
    background="white"
    bold=FALSE
    italic=FALSE
    color="black"
    border_left=F
    border_right=F

    if (length(colSpec)>0) {
      # Elément de colSpec correspondant à la colonne
      listIndex=sapply(colSpec, function(x) {which(x$index == i)})
      w=which(listIndex>0)
      if (length(w) > 0 && w>0) {
        l=colSpec[[w]]

        if (is.null(l$width)==FALSE) {
          width=l$width
        } else {
          width=width
        }

        if (is.null(l$background)==FALSE) {
          background=l$background
        } else {
          background=background
        }

        if (is.null(l$bold)==FALSE) {
          bold=l$bold
        } else {
          bold=bold
        }

        if (is.null(l$italic)==FALSE) {
          italic=l$italic
        }

        if (is.null(l$color)==FALSE) {
          color=l$color
        } else {
          color=color
        }

        if (is.null(l$border_left)==FALSE) {
          border_left=l$border_left
        } else {
          border_left=border_left
        }

        if (is.null(l$border_right)==FALSE) {
          border_right=l$border_right
        } else{
          border_right=border_right
        }
      }
    }

    fhtml=kableExtra::column_spec(fhtml, i, width=width, bold = bold, italic=italic, color=color, background = background, border_left=border_left, border_right=border_right)
  }

  # if (print==TRUE) {
  #   print(fhtml)
  # }

  if (format !="") {
    kableExtra::save_kable(fhtml,paste(fileName,".",format,sep=""))
  }

  return (fhtml)
}


# TODO : autres format (PDF, image, etc.)