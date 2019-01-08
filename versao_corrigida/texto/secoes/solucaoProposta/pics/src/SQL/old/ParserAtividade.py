from bs4 import BeautifulSoup
from os import listdir
from os.path import isfile, join

def converteVariavelFilename(variavel):
	if type(variavel) == "NoneType" or variavel is None or variavel is [] or variavel == None or variavel == []:
		return ""
		
	else:
		if (type(variavel.title) == "NoneType" or type(variavel.title) is "NoneType" or variavel.title is 
None or variavel.title is [] or variavel.title == None or variavel.title == []):
			return ""
		else:
			return variavel.title.contents[0]#	return variavel.filename[0]		
pass

def converteVariavel(variavel):
	if type(variavel) == "NoneType" or variavel is None or variavel is [] or variavel == None or variavel == []:
		return ""
		
	else:
		if (type(variavel.processor) == "NoneType" or type(variavel.processor) is "NoneType" or variavel.processor is 
None or variavel.processor is [] or variavel.processor == None or variavel.processor == []):
			return ""
		else:
			return """{0}""".format(variavel.processor.contents[0])
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


	#arquivo = open(saida, 'a')
	for i, j in zip(tmp1, tmp2):
		
		soupRDF = BeautifulSoup(open(mypathRDF + i), "xml")
		soupXML = BeautifulSoup(open(mypathXML + j), "xml")	
		
		tipoDeSGWC = converteVariavelFilename(soupRDF.ContentType)
		if tipoDeSGWC == "Taverna 1" or tipoDeSGWC == "Taverna 2":
			print(tipoDeSGWC)
		
		mypathWorkflows = "/home/toasty/www.myexperiment.org/workflows/{0}/statistics".format(i.replace(".rdf", ""))
		soupStatistics = BeautifulSoup(open(mypathWorkflows))
		
		rating = ""
		versions = ""
		reviews = ""
		comments = ""
		citations = ""
		for ite in soupStatistics.select("p > a"):
				
			#print(ite.attrs)
			if ite.b is not None and ite.b != None and type(ite.b) != None and type(ite.b) is not None:
				if ite.b.string.strip() == "Rating:":
					rating = ite.text.split(":")[1].split("/")[0]

				if ite.b.string.strip() == "Versions:":
					versions = ite.text.split(":")[1]

				if ite.b.string.strip() == "Reviews:":
					reviews = ite.text.split(":")[1]

				if ite.b.string.strip() == "Comments:":
					comments = ite.text.split(":")[1]

				if ite.b.string.strip() == "Citations:":
					citations = ite.text.split(":")[1]
			
		pass
		print("myVersion {0}".format(versions))
	pass
pass

def testinho():
	string = "./pathways_and_gene_annotations_forqtl_region_290738.xml"	
	soupXMLTestinho = BeautifulSoup(open(string), "xml")
	
	print("#################################################################")	
	for ite in soupXMLTestinho.datalinks:
		
		if hasattr(ite, 'sink') and ite is not None and ite != None and ite.sink is not None and ite.sink != None and hasattr(ite, 'source')  and ite.source is not None and ite.source != None:
			
			print("""{0} -- {1} \n""".format(ite.sink, ite.source))		
	pass

	print("#################################################################")
pass

def testes():
	mypathRDF = "./ArquivoWorkflowRDF/4.rdf"
	mypathWorkflows = "/home/toasty/www.myexperiment.org/workflows/4/statistics"
	
	saida = "testes.txt"
	numWorkflow = "4"
	soupRDF = BeautifulSoup(open(mypathRDF), "xml")
	soupStatistics = BeautifulSoup(open(mypathWorkflows))

	print(soupRDF.ContentType.title.contents[0])
	print("Obtem dados de Workflows")
	rating = ""
	versions = ""
	reviews = ""
	comments = ""
	citations = ""
	for ite in soupStatistics.select("p > a"):
				
		#print(ite.attrs)
		if ite.b is not None and ite.b != None and type(ite.b) != None and type(ite.b) is not None:
			if ite.b.string.strip() == "Rating:":
				rating = ite.text.split(":")[1].split("/")[0]

			if ite.b.string.strip() == "Versions:":
				versions = ite.text.split(":")[1]

			if ite.b.string.strip() == "Reviews:":
				reviews = ite.text.split(":")[1]

			if ite.b.string.strip() == "Comments:":
				comments = ite.text.split(":")[1]

			if ite.b.string.strip() == "Citations:":
				citations = ite.text.split(":")[1]
					
	pass
	#print("{0}, {1}, {2}, {3}, {4}".format(rating, versions, reviews, comments, citations))
	print("{0}".format(versions))
pass


if __name__ == "__main__":

	#criaSQL()
	#testes()
	testinho()
pass


#INSERT INTO workflows(chave_sgwc, cod_workflow, rating, citacoes, visualizacoes, downloads) VALUES ( ?, ?, ?, ?, ?, ?);

