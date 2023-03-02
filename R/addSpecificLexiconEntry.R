#' @title Add a lexicon entry for an object of class fc
#' @description The lexicon entry is saved in the specific lexicon tab of the lexicon xlsx file.
#' @param fc An object of class "fc".
#' @param word A word (character) to add in the specific lexicon defined in fc.
#' @param lemma An empty string if the word has to be removed, a synonym if the word has to be grouped with another word, or a group based on the hierarchical clustering.
#' @return An object of class "fc".
#' @export
addSpecificLexiconEntry=function(fc, word, lemma) {
  df=fc$rawData
  # Recherche et remplacement
  newLemma=as.data.frame(cbind(word=word, lemma=lemma))
  df$cleanText = DataCombine::FindReplace(data = df, Var = "cleanText", replaceData = newLemma, from = "word", to = "lemma", exact = FALSE, vector = TRUE)
  df$cleanText = stringr::str_replace(df$cleanText, "_ ", "_")
  df$cleanText = paste(" ", df$cleanText, " ", sep = "")
  fc[["rawData"]] <- df
  
  # Enregistrement des modifications
  wb <- openxlsx::loadWorkbook(fc[["pathToLexicon"]])
  lastWordIndex=fc[["lastWordIndex"]]+1
  fc[["lastWordIndex"]]=lastWordIndex
  openxlsx::writeData(wb, sheet = fc[["specificWordsTabName"]], newLemma, startRow=lastWordIndex, colNames = F)
  openxlsx::saveWorkbook(wb,fc[["pathToLexicon"]],overwrite = T)
  
  # Infos
  fc=fcInfo(fc)
  
  return (fc)
}