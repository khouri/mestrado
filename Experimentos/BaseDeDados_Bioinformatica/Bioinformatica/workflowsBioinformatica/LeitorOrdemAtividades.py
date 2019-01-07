#!/usr/bin/python
#!python

import os
from bs4 import BeautifulSoup
from os import listdir
from os.path import isfile, join
import POJOAtividades

def gravaArquivo(arquivo, tituloWorkflow, atividades, cod_workflow):

	stream = open(arquivo, 'a')
	stream.write("""{0}_{1}#{2}\n""".format(cod_workflow, tituloWorkflow, atividades.getLista()))
	stream.close()
pass

def geraWorkflowsFormatoTexto():

	#Diretorios para mineirar
	arrayDiretorios = [
				"1428", "1450", "1451", "1452", "1454", "1455", "1456", "1457", "1491", "1492", "1493",
				"1494", "158", "165", "167", "168",  "1689", "1697", "197", "198",  "199", "200", "201", "202",  "203", "204",
				"205", "206",  "207", "208", "209", "21",   "210", "211", "212", "213",  "215", "216", "217",  "218", "219",
				"22",  "220",  "221", "222", "224",  "225", "227", "228", "229",  "23",  "230", "231", "232",  "233", "235",
				"236", "237",  "238", "244", "245",  "251", "32", "368", "377",  "39",  "4",   "6",
				"737", "738",  "740", "90"
			]
		
	#Pega cada workflow e converte as atividades no meu formato
	for direc in arrayDiretorios:

		#Pega o diretorio
		caminho = """./{0}/download?version=1""".format(direc)
		soupStatistics = BeautifulSoup(open(caminho), "xml")
		
		#Cria a lista de atividades e suas sucessoras
		lst   = POJOAtividades.ListaDeAtividades()
		for link in soupStatistics.find_all("link"):

			nomeDaAtividade 	 = link["source"].split(":")[0]
			nomeDaAtividadeSucessora = [link["sink"].split(":")[0]]

			objetoAtividade = POJOAtividades.Atividade(nomeDaAtividade, nomeDaAtividadeSucessora)
			lst.insereAtividade(objetoAtividade)			
		pass

		#Pega o titulo do workflow
		tituloWorkflow =  """{0}{1}""".format(soupStatistics.workflowdescription["title"].replace(" ", "_"), ".t2flow")

		#Grava no arquivo texto.
		gravaArquivo("workflows_formato_adequeado.txt", tituloWorkflow, lst, direc)
	pass	
pass


if __name__ == "__main__":
	
	geraWorkflowsFormatoTexto()
pass

