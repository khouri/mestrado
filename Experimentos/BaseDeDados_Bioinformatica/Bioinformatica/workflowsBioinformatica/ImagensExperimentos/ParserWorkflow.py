#!/usr/bin/python
#!python

from os import listdir
from os.path import isfile, join
import wget

def criaSQL():

	#Caminhos usados
	diretorioRaiz = "./workflowsBioinformatica/"
	myLink = "http://www.myexperiment.org/workflows/{0}/versions/1/previews/full"
	saida = "links.txt"
	
	#Pega o nome dos diretorios
	onlyDirectory = listdir(diretorioRaiz)
	tmp1 = sorted(onlyDirectory)
	
	#download da imagem com o id do workflow.
	for i in tmp1:
		url = """http://www.myexperiment.org/workflows/{0}/versions/1/previews/full""".encode('utf-8').format(i)
		filename = wget.download(url, format(i))

	pass

pass

if __name__ == "__main__":

	criaSQL()
pass
