#!/usr/bin/python
#!python

from bs4 import BeautifulSoup
import urllib

def gravaArquivo(arquivo, string):

	stream = open(arquivo, 'a')
	stream.write("""{0}\n""".format(string))
	stream.close()
pass

def capturaLinkDosPDFs(path, i):

	caminhoDoArquivo = "/home/adilson/Desktop/scienceDirect/" + str(i) + ".txt" 
	soup = BeautifulSoup(open(path), "html")

	#Pega todos as tags <a>
	#mydivs = soup.findAll("div", { "class" : "stylelistrow" })
	dadosPaginaFiltrado = soup.find_all("a")
	for j in range(1, len(dadosPaginaFiltrado)):
		
		stringTmp = str(dadosPaginaFiltrado[j]).find("pii")
		stringTmp2 = str(dadosPaginaFiltrado[j]).find("main.pdf")
		
		if stringTmp != -1 and stringTmp2 != -1:
			if dadosPaginaFiltrado[j].get('href') is not None:
				gravaArquivo(caminhoDoArquivo, dadosPaginaFiltrado[j].get('href').replace("view-source:", ""))
			pass

		pass
	pass
pass


if __name__ == "__main__":

	string = ["/home/adilson/Desktop/scienceDirect/scienceDirect1.html", 
		  "/home/adilson/Desktop/scienceDirect/scienceDirect2.html"]

	for i in range(0, 2):
		capturaLinkDosPDFs(string[i], i)		
	pass

pass

