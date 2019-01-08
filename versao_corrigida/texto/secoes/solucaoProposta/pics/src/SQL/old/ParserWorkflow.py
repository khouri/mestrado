from bs4 import BeautifulSoup
from os import listdir
from os.path import isfile, join

#Pega todos os arquivos do diretorio
def converteVariavel(variavel):
	if variavel is None or variavel is [] or variavel == None or variavel == []:
		return ""
		
	else:
		return variavel[0]
pass

def converteVariavelUM(variavel):
	if  variavel is None or variavel is [] or variavel == None or variavel == [] or len(variavel) == 1:
		return ""
		
	else:
		return variavel[1].attrs["resource"]
pass

def criaSQL():

	mypathRDF = "./ArquivoWorkflowRDF/"
	mypathXML = "./ArquivoWorkflowXML/"
	mypathWorkflows = "./home/toasty/www.myexperiment.org/workflows/"
	arquivoStatistics = "statistics"
	saida = "workflowsinsert.sql"
	
	onlyfiles = [ f for f in listdir(mypathRDF) if isfile(join(mypathRDF,f)) ]
	onlyfilesXML = [ f for f in listdir(mypathXML) if isfile(join(mypathXML,f)) ]
	
	tmp1 = sorted(onlyfiles)
	tmp2 = sorted(onlyfilesXML)
	#i.replace(".rdf", "") #pega numero do diretorio

	arquivo = open(saida, 'a')
	for i, j in zip(tmp1, tmp2):

		soupRDF = BeautifulSoup(open(mypathRDF+i), "xml")
		soupXML = BeautifulSoup(open(mypathXML+j), "xml")
		
		if soupRDF.User != None or soupRDF.User is not None:
			variavelHomepage = converteVariavelUM(soupRDF.User.find_all("homepage"))
		else:
			variavelHomepage = ""

		variavelSGWC = converteVariavel(soupRDF.ContentType.title.contents)
		variavelMYME = converteVariavel(soupXML("mime-type")[0].contents)

		arquivo.write("""INSERT INTO sistema_gerenciador_workflows_cientificos (nomesgwc, mime_type, homepage, arquivo) VALUES('{0}' , '{1}' , '{2}' , '{3}');\n """.encode('utf-8').format(variavelSGWC, variavelMYME, variavelHomepage, i.replace(".rdf", "") ))
	
	pass
	arquivo.close();

pass

def testes():
	mypathRDF = "./ArquivoWorkflowRDF/1.rdf"
	mypathXML = "./ArquivoWorkflowXML/1.xml"

	saida = "contentType.txt"

	soupRDF = BeautifulSoup(open(mypathRDF), "xml")
	soupXML = BeautifulSoup(open(mypathXML), "xml")

	
	variavelHomepage = soupRDF.User.find_all("homepage")[1].attrs["resource"]
	variavelSGWC = soupRDF.ContentType.title.contents[0]
	variavelMYME = soupXML("mime-type")[0].contents[0]

	print("""INSERT INTO sistema_gerenciador_workflows_cientificos (nomesgwc, mime_type, homepage) VALUES('{0}' , '{1}' , '{2}');\n """.encode('utf-8').format(variavelSGWC, variavelMYME, variavelHomepage))

	print(variavelHomepage)
	print(variavelSGWC)
	print(variavelMYME)
pass


if __name__ == "__main__":

	#criaSQL()
	testes()
pass
