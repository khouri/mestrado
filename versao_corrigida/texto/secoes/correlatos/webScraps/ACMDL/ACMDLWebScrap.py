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

	caminhoDoArquivo = "/home/adilson/Desktop/ACMDL_webScrap/" + str(i) + ".txt" 
	soup = BeautifulSoup(open(path), "html")

	#Pega todos as tags <a>
	dadosPaginaFiltrado = soup.find_all("a")
	for j in range(1, len(dadosPaginaFiltrado)):

		#Filtra apenas tag <a> que contenham links de pdfs
		stringTmp = str(dadosPaginaFiltrado[j]).find("ft_gateway")

		if stringTmp != -1:
		#Pega o link direto do PDF
			strTMP2 = str(dadosPaginaFiltrado[j]).split("view-source:")
			if len(strTMP2) == 1:
				gravaArquivo(caminhoDoArquivo, strTMP2[0].split(" target")[0])			
			pass	
			if len(strTMP2) == 2:#tratamento padrao filtrar por gateway
				gravaArquivo(caminhoDoArquivo, strTMP2[1].split(">ft_gateway.cfm")[0])
			pass	
			
			#print(strTMP2[1])
			#print("porco")
			#print("\n")
			
			
			#print(vaiaaaa)
			#print("\n")
			#print(strTMP2[1].split(">ft_gateway.cfm")[0])
			#print("\n")
			#gravaArquivo(caminhoDoArquivo, strTMP2[1].split(">ft_gateway.cfm")[0])
		pass
	pass
pass


if __name__ == "__main__":

	string = [#"/home/adilson/Desktop/ACMDL_webScrap/1acmdl.html", 
		  #"/home/adilson/Desktop/ACMDL_webScrap/2acmdl.html", 
		  #"/home/adilson/Desktop/ACMDL_webScrap/3acmdl.html", 
		  #"/home/adilson/Desktop/ACMDL_webScrap/4acmld.html", 
		  #"/home/adilson/Desktop/ACMDL_webScrap/5acmdl.html", 
		  #"/home/adilson/Desktop/ACMDL_webScrap/6acmdl.html", 
		 # "/home/adilson/Desktop/ACMDL_webScrap/7acmdl.html", 
		 # "/home/adilson/Desktop/ACMDL_webScrap/8.html", 
		 # "/home/adilson/Desktop/ACMDL_webScrap/9acmdl.html", 
		#  "/home/adilson/Desktop/ACMDL_webScrap/10acmdl.html", 
		#  "/home/adilson/Desktop/ACMDL_webScrap/11acmdl.html", 
		#  "/home/adilson/Desktop/ACMDL_webScrap/12acmdl.html", 
		  "/home/adilson/Desktop/ACMDL_webScrap/13acmdl.html"]

#	for i in range(0, 1):
	capturaLinkDosPDFs(string[12], 12)		
#	pass

pass

