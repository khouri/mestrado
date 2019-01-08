#!/usr/bin/python
#!python

from bs4 import BeautifulSoup
import urllib
import os
from os import listdir
from os.path import isfile, join

def leDiretorioAtual(mypath):
	onlyfiles = [ f for f in listdir(mypath) if isfile(join(mypath,f) ) ]
	return onlyfiles
pass

def pathSOAtual():
	return str(os.getcwd()) 
pass

def gravaArquivo(arquivo, string):

	stream = open(arquivo, 'a')
	stream.write("""{0}\n""".format(string))
	stream.close()
pass

def capturaLinkDosPDFs(path, i):

	caminhoDoArquivo = pathSOAtual() + "/" + str(i) + ".txt" 
	print(caminhoDoArquivo)
	soup = BeautifulSoup(open(path), "html")
	#Pega todos as tags <a>
	dadosPaginaFiltrado = soup.find_all("a")

	for i in range(1, len(dadosPaginaFiltrado)):
		
		#Filtra apenas tag <a> que contenham links de pdfs
		stringTmp = str(dadosPaginaFiltrado[i]).find("icon-pdf ng-scope")
		if stringTmp != -1:
			#Pega o link direto do PDF
			strTMP2 = str(dadosPaginaFiltrado[i]).split("href=")
			gravaArquivo(caminhoDoArquivo, strTMP2[1].split("id")[0].replace("amp;", ""))
		pass
	pass
pass


if __name__ == "__main__":
	
	pathAtual = pathSOAtual()
	arquivosDiretorioAtual = leDiretorioAtual(pathAtual)
	#print(arquivosDiretorioAtual)
	#print(pathAtual)
	
	for i in range(0,len(arquivosDiretorioAtual)):
		if ".html" in arquivosDiretorioAtual[i]:
			print(arquivosDiretorioAtual[i])	
			capturaLinkDosPDFs(arquivosDiretorioAtual[i], i)	
		pass
		#capturaLinkDosPDFs(string[i], i)			
	pass

pass

