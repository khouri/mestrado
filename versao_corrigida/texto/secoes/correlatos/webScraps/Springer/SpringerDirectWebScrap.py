#!/usr/bin/python
#!python

from bs4 import BeautifulSoup
import urllib
import os
from os import listdir
from os.path import isfile, join

def gravaArquivo(arquivo, string):

	stream = open(arquivo, 'a')
	stream.write("""{0}\n""".format(string))
	stream.close()
pass

def leDiretorioAtual(mypath):
	onlyfiles = [ f for f in listdir(mypath) if isfile(join(mypath,f)) ]
	return onlyfiles
pass

def pathSOAtual():
	return str(os.getcwd()) 
pass


def capturaLinkDosPDFs(path, i):

	caminhoDoArquivo = pathSOAtual() + "/" + str(i) + ".txt" 
	soup = BeautifulSoup(open(path), "html")

	#Pega todos as tags <a>
	#mydivs = soup.findAll("div", { "class" : "stylelistrow" })
	dadosPaginaFiltrado = soup.find_all("a")
	for j in range(1, len(dadosPaginaFiltrado)):
		
		stringTmp = str(dadosPaginaFiltrado[j]).find("class=webtrekk-track pdf-link")
		
		if stringTmp != -1:
			if dadosPaginaFiltrado[j].get('href') is not None:
				gravaArquivo(caminhoDoArquivo, dadosPaginaFiltrado[j].get('href').replace("view-source:", ""))
			pass

		pass
	pass
pass


if __name__ == "__main__":

	#string = leDiretorioAtual(pathSOAtual()+"/")

	variavel  = "1http:_link.springer.com_search?date-facet-mode=between&query=scientific+AND+workflow+AND+pipeline&showAll=false.html"
	capturaLinkDosPDFs(variavel, 0)		
	
	#for i in range(0, len(string) - 1):
	#	capturaLinkDosPDFs(string[i], i)		
	#pass

pass

