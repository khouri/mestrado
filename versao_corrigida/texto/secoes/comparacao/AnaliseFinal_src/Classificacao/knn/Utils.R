#Biblioteca Utils

#Le tabela CSV
leArquivoCSV <- function(nomeArquivoCsv) {
  return(read.table(nomeArquivoCsv, header = TRUE, sep = ","))
}

#Retorna o caminho absoluto de um arquivo no diretorio local
getCaminhoAbsolutoArquivoLocal <- function(nomeArquivo) {
  
  diretorioLocal <- getwd()
  concatena <- paste(diretorioLocal,"/", nomeArquivo, sep="")
  return(concatena)
}

salvaArquivo <- function(path, parametros, matrizConfusao){
  
  sink(getCaminhoAbsolutoArquivoLocal("outputKNNClassifications.txt"), append=TRUE)
  print(paste("valor de k = ", parametros))
  print(matrizConfusao)
  print("--------------------------------------------------")
  sink()
}