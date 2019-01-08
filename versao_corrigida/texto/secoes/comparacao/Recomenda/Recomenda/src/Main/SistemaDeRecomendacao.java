package Main;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Stack;


class ListaRecomendacao{
	public ArrayList<String> atividadesOrdenadas = new ArrayList<String>();
}

class SetentaETresListasDeRecomendacoes{

	public ListaRecomendacao[] listas = new ListaRecomendacao[73];

	public SetentaETresListasDeRecomendacoes(){
		for (int i = 0; i < listas.length; i++) {
			listas[i] = new ListaRecomendacao();
		}
	}
}

class DezSistemasRecomenda {

	public SetentaETresListasDeRecomendacoes [] DezCasos = new SetentaETresListasDeRecomendacoes[12];
	public String [] listaDeAtividadesRecomendadas = new String[73];

	public DezSistemasRecomenda(){
		for (int i = 0; i < DezCasos.length; i++) {
			DezCasos[i] = new SetentaETresListasDeRecomendacoes();
		}

		preencheAtividadesCarregadas();
	}

	public String retornaParaSalvar(){

		//		StringBuilder builderLindo

		return "";
	}

	public void preencheAtividadesCarregadas(){

		listaDeAtividadesRecomendadas[0]="object";
		listaDeAtividadesRecomendadas[1]="getfastauniprotentry";
		listaDeAtividadesRecomendadas[2]="getfastauniprotentry";
		listaDeAtividadesRecomendadas[3]="getfastauniprotentry";
		listaDeAtividadesRecomendadas[4]="sequencedatabase";
		listaDeAtividadesRecomendadas[5]="getfastauniprotentry";
		listaDeAtividadesRecomendadas[6]="sequencedatabase";
		listaDeAtividadesRecomendadas[7]="searchsimple";
		listaDeAtividadesRecomendadas[8]="getfastauniprotentry";
		listaDeAtividadesRecomendadas[9]="getfastauniprotentry";
		listaDeAtividadesRecomendadas[10]="sequencedatabase";
		listaDeAtividadesRecomendadas[11]="bioinformaticsprogram";
		listaDeAtividadesRecomendadas[12]="createfasta";
		listaDeAtividadesRecomendadas[13]="seqret";
		listaDeAtividadesRecomendadas[14]="rcsbprefix";
		listaDeAtividadesRecomendadas[15]="proteinid";
		listaDeAtividadesRecomendadas[16]="jobid";
		listaDeAtividadesRecomendadas[17]="jobid";
		listaDeAtividadesRecomendadas[18]="jobid";
		listaDeAtividadesRecomendadas[19]="jobid";
		listaDeAtividadesRecomendadas[20]="jobid";
		listaDeAtividadesRecomendadas[21]="askforemail";
		listaDeAtividadesRecomendadas[22]="jobid";
		listaDeAtividadesRecomendadas[23]="jobid";
		listaDeAtividadesRecomendadas[24]="jobid";
		listaDeAtividadesRecomendadas[25]="jobid";
		listaDeAtividadesRecomendadas[26]="jobid";
		listaDeAtividadesRecomendadas[27]="jobid";
		listaDeAtividadesRecomendadas[28]="jobid";
		listaDeAtividadesRecomendadas[29]="jobid";
		listaDeAtividadesRecomendadas[30]="database";
		listaDeAtividadesRecomendadas[31]="program";
		listaDeAtividadesRecomendadas[32]="jobid";
		listaDeAtividadesRecomendadas[33]="jobid";
		listaDeAtividadesRecomendadas[34]="jobid";
		listaDeAtividadesRecomendadas[35]="jobid";
		listaDeAtividadesRecomendadas[36]="seqret";
		listaDeAtividadesRecomendadas[37]="seqret";
		listaDeAtividadesRecomendadas[38]="seqret";
		listaDeAtividadesRecomendadas[39]="jobid";
		listaDeAtividadesRecomendadas[40]="jobid";
		listaDeAtividadesRecomendadas[41]="jobid";
		listaDeAtividadesRecomendadas[42]="blastfile";
		listaDeAtividadesRecomendadas[43]="jobid";
		listaDeAtividadesRecomendadas[44]="jobid";
		listaDeAtividadesRecomendadas[45]="jobid";
		listaDeAtividadesRecomendadas[46]="jobid";
		listaDeAtividadesRecomendadas[47]="fetchdata";
		listaDeAtividadesRecomendadas[48]="seqret";
		listaDeAtividadesRecomendadas[49]="fastastring";
		listaDeAtividadesRecomendadas[50]="seqret";
		listaDeAtividadesRecomendadas[51]="program";
		listaDeAtividadesRecomendadas[52]="jobid";
		listaDeAtividadesRecomendadas[53]="isdone";
		listaDeAtividadesRecomendadas[54]="seqret";
		listaDeAtividadesRecomendadas[55]="jobid";
		listaDeAtividadesRecomendadas[56]="sequence";
		listaDeAtividadesRecomendadas[57]="sequence";
		listaDeAtividadesRecomendadas[58]="idlist";
		listaDeAtividadesRecomendadas[59]="uniparcid";
		listaDeAtividadesRecomendadas[60]="jobid";
		listaDeAtividadesRecomendadas[61]="seqret";
		listaDeAtividadesRecomendadas[62]="query";
		listaDeAtividadesRecomendadas[63]="getproteinfasta";
		listaDeAtividadesRecomendadas[64]="id";
		listaDeAtividadesRecomendadas[65]="stproteinid";
		listaDeAtividadesRecomendadas[66]="sequence";
		listaDeAtividadesRecomendadas[67]="sequence";
		listaDeAtividadesRecomendadas[68]="getnucleotidefasta";
		listaDeAtividadesRecomendadas[69]="id";
		listaDeAtividadesRecomendadas[70]="flattenlist";
		listaDeAtividadesRecomendadas[71]="parametersxml";
		listaDeAtividadesRecomendadas[72]="searchsimple";

		return ;
	}

	public void geraDadosClassificadorComposto(String [][] workflows){
		int contador = 0;
		String [] tecnicas = new String[]{"CART_REG", "BINOMIAL_REG", "MARS_REG", 
				"NNET_REG", "SVM_REG", "CART_CLASS", 
				"NAIVE_BAYES_CLASS", "NNET_CLASS", "SVM_CLASS", "KNN_CLASS"};


		for(int i = 0; i < workflows.length; i++){//Itera pelas atividades dos workflows

			/*	//Para cada sistema de recomendacao pergunta a posicao
			int a = DezCasos.length;//atv recomendada
			int b = DezCasos[i].listas.length;//73 casos
			int c = DezCasos[i].listas[1].atividadesOrdenadas.size();//lista ordenada pela tecnica X
			System.out.println("Eita!");
			 */	
			for (int j = 0; j < DezCasos.length; j++) {//itera pelos 10 sistemas
				String atv_removida = workflows[i][1];//Pega o nome da removida

				//Pesquisa em cada lista
				//DezCasos[j].listas
			}

		}


		/*int [][] matriz = new int[73][10];
		for (int i = 0; i < workflows.length; i++) {//10 sistemas
			//tecnicas

			System.out.println(tecnicas[i]+"\t");
			int tmp122 = workflows.length;
			String teste = workflows[2][1];

			for (int j = 0; j < DezCasos[i].listas.length; j++) {//73 recomendacoes para cada um dos dez sistemas

				for (int k = 0; k < DezCasos[i].listas[j].atividadesOrdenadas.size(); k++) {//lista retornada por cada sistema em cada caso

					String pesquisada = listaDeAtividadesRecomendadas[j];
					String valorListaIterada = DezCasos[i].listas[j].atividadesOrdenadas.get(k);
					if(pesquisada.equals(valorListaIterada)){
						//System.out.println(pesquisada);
						//System.out.println(valorListaIterada);

						//System.out.print("valor de j "+j+"--"+k+", ");
						System.out.print(k+", ");
						System.out.println();
						break;
					}					
				}

				//System.out.println();
			}
			System.out.println();
		}
		String finaliza = "Done!";
		//System.out.println(finaliza);
		 */	}

	public void geraListaSimplesFinal(){

		int contador = 0;
		String [] tecnicas = new String[]{"CART_REG", "BINOMIAL_REG", "MARS_REG", 
				"NNET_REG", "SVM_REG", "CART_CLASS", 
				"NAIVE_BAYES_CLASS", "NNET_CLASS", "SVM_CLASS", "KNN_CLASS", "SVM_COMP", "ROTATION"};
		int [][] matriz = new int[73][12];
		for (int i = 0; i < DezCasos.length; i++) {//12 sistemas
			//tecnicas

			System.out.println(tecnicas[i]+"\t");
			for (int j = 0; j < DezCasos[i].listas.length; j++) {//73 recomendacoes para cada um dos dez sistemas

				//String pesquisaPosicao = listaDeAtividadesRecomendadas[j];
				//String posicao = DezCasos[i].listas[j].atividadesOrdenadas

				//					contains(pesquisaPosicao);

				int tmp = DezCasos[i].listas[j].atividadesOrdenadas.size();
				//int tmp =  DezCasos[i].listas[j].atividadesOrdenadas.size();
				for (int k = 0; k < DezCasos[i].listas[j].atividadesOrdenadas.size(); k++) {//lista retornada por cada sistema em cada caso

					String pesquisada = listaDeAtividadesRecomendadas[j];
					String valorListaIterada = DezCasos[i].listas[j].atividadesOrdenadas.get(k);
					if(pesquisada.equals(valorListaIterada)){
						//System.out.println(pesquisada);
						//System.out.println(valorListaIterada);

						//System.out.print("valor de j "+j+"--"+k+", ");
						System.out.print(k+", ");
						System.out.println();
						break;
					}					
				}

				//System.out.println();
			}
			System.out.println();
		}
		String finaliza = "Done!";
		//System.out.println(finaliza);

	}
	
	public void pesquisaAtividade(String pesquisada,  ArrayList<String> atividadesOrdenadas){
		
		int posicao = -1;
		for (int i = 0; i < atividadesOrdenadas.size(); i++) {
			if(pesquisada.equals(atividadesOrdenadas.get(i))){
				posicao = i;
				System.out.print(i+", ");
				System.out.println();
			}
		}
		
		return ;
	}
	
	
	
	public void geraListaSimplesFinalComposta(String [][] workflows){

		int contador = 0;
		String [] tecnicas = new String[]{"CART_REG", "BINOMIAL_REG", "MARS_REG", 
				"NNET_REG", "SVM_REG", "CART_CLASS", 
				"NAIVE_BAYES_CLASS", "NNET_CLASS", "SVM_CLASS", "KNN_CLASS"};
		int [][] matriz = new int[73][10];
		for (int i = 0; i < DezCasos.length; i++) {//10 sistemas
			//tecnicas

			System.out.println(tecnicas[i]+"\t");
			for (int j = 0; j < DezCasos[i].listas.length; j++) {//73 recomendacoes para cada um dos dez sistemas

				//String pesquisaPosicao = listaDeAtividadesRecomendadas[j];
				//String posicao = DezCasos[i].listas[j].atividadesOrdenadas

				//					contains(pesquisaPosicao);

				//int tmp =  DezCasos[i].listas[j].atividadesOrdenadas.size();
				for (int k = 0; k < DezCasos[i].listas[j].atividadesOrdenadas.size(); k++) {//lista retornada por cada sistema em cada caso

					String pesquisada = listaDeAtividadesRecomendadas[j];
					String valorListaIterada = DezCasos[i].listas[j].atividadesOrdenadas.get(k);
					if(pesquisada.equals(valorListaIterada)){
						//System.out.println(pesquisada);
						//System.out.println(valorListaIterada);

						//System.out.print("valor de j "+j+"--"+k+", ");
						System.out.print(k+", ");
						System.out.println();
						break;
					}					
				}

				//System.out.println();
			}
			System.out.println();
		}
		String finaliza = "Done!";
		//System.out.println(finaliza);

	}


}

class LinhaAnalise {
	/*
	 * Class, ativ_recomendada, frequencia, cod_ontologia, 
	 * CART_REGRESSAO_RESULTADO, BINOMIAL_REGRESSAO_RESULTADO, MARS_REGRESSAO_RESULTADO, 
	 * NNET_REGRESSAO_RESULTADO, SVM_REGRESSAO_RESULTADO, CART_CLASSIFICACAO_RESULTADO, 
	 * NAIVE_BAYES_CLASSIFICACAO, NNET_CLASSIFICACAO_RESULTADO, SVM_CLASSIFICACAO_RESULTADO, 
	 * KNN_CLASSIFICACAO_RESULTADO
	 * 
	 * */
	int classeReal;
	String nomeAtividadeRecomendada;
	int frequencia;
	int cod_ontologia;//Isso aqui eh um ArrayList composto que ja esta pronto!
	int CART_REGRESSAO_RESULTADO; 
	int BINOMIAL_REGRESSAO_RESULTADO; 
	int MARS_REGRESSAO_RESULTADO; 
	int NNET_REGRESSAO_RESULTADO; 
	int SVM_REGRESSAO_RESULTADO; 
	int CART_CLASSIFICACAO_RESULTADO; 
	int NAIVE_BAYES_CLASSIFICACAO; 
	int NNET_CLASSIFICACAO_RESULTADO; 
	int SVM_CLASSIFICACAO_RESULTADO;
	int KNN_CLASSIFICACAO_RESULTADO;
	int SVM_COMPOSTO;
	int ROTATION_FOREST;


	public void printObject(){
		System.out.println("  classe real: "+this.classeReal+
				" nome atividade: "+this.nomeAtividadeRecomendada+
				" frequencia: "+this.frequencia+
				" cod_ontologia: "+this.cod_ontologia+
				" CART_REGRESSAO_RESULTADO: "+this.CART_REGRESSAO_RESULTADO+
				" BINOMIAL_REGRESSAO_RESULTADO: "+this.BINOMIAL_REGRESSAO_RESULTADO+
				" MARS_REGRESSAO_RESULTADO: "+this.MARS_REGRESSAO_RESULTADO+
				" NNET_REGRESSAO_RESULTADO: "+this.NNET_REGRESSAO_RESULTADO+
				" SVM_REGRESSAO_RESULTADO: "+this.SVM_REGRESSAO_RESULTADO+
				" CART_CLASSIFICACAO_RESULTADO: "+this.CART_CLASSIFICACAO_RESULTADO+
				" NAIVE_BAYES_CLASSIFICACAO: "+this.NAIVE_BAYES_CLASSIFICACAO+
				" NNET_CLASSIFICACAO_RESULTADO: "+this.NNET_CLASSIFICACAO_RESULTADO+
				" SVM_CLASSIFICACAO_RESULTADO: "+this.SVM_CLASSIFICACAO_RESULTADO+
				" KNN_CLASSIFICACAO_RESULTADO: "+this.KNN_CLASSIFICACAO_RESULTADO+
				"SVM_COMPOSTO: "+this.SVM_COMPOSTO+
				"ROTATION_FOREST: "+this.ROTATION_FOREST);
	}
}

class Grupo118Analise {
	LinhaAnalise analise[] = new LinhaAnalise[8615];
	DezSistemasRecomenda finalList = new DezSistemasRecomenda();

	public Grupo118Analise() {
		//Inicializa o array
		for (int i = 0; i < analise.length; i++) {
			analise[i] = new LinhaAnalise();
		}
	}

	public void liberaMemoria(){
		analise = null;
		analise = new LinhaAnalise[8615];
	}

	public void criaArquivoParaClassificadorComposto(ListaDeAtividadesAnotadas ListaAnotacoes, String [][] workflows){

		int contador = 0;
		ArrayList<LinhaAnalise> arrayDeCopiaPAraAnalise = new ArrayList<LinhaAnalise>();
		for (int casoDeRecomendacao = 1; casoDeRecomendacao < analise.length; 
				casoDeRecomendacao = casoDeRecomendacao + 1) {//Deveria ter 73 rodadas

			LinhaAnalise nova = new LinhaAnalise();
			nova.classeReal = analise[casoDeRecomendacao].classeReal;
			nova.nomeAtividadeRecomendada = analise[casoDeRecomendacao].nomeAtividadeRecomendada;
			nova.frequencia = analise[casoDeRecomendacao].frequencia;
			nova.cod_ontologia = analise[casoDeRecomendacao].cod_ontologia;
			nova.CART_REGRESSAO_RESULTADO = analise[casoDeRecomendacao].CART_REGRESSAO_RESULTADO; 
			nova.BINOMIAL_REGRESSAO_RESULTADO = analise[casoDeRecomendacao].BINOMIAL_REGRESSAO_RESULTADO; 
			nova.MARS_REGRESSAO_RESULTADO = analise[casoDeRecomendacao].MARS_REGRESSAO_RESULTADO; 
			nova.NNET_REGRESSAO_RESULTADO = analise[casoDeRecomendacao].NNET_REGRESSAO_RESULTADO; 
			nova.SVM_REGRESSAO_RESULTADO = analise[casoDeRecomendacao].SVM_REGRESSAO_RESULTADO; 
			nova.CART_CLASSIFICACAO_RESULTADO = analise[casoDeRecomendacao].CART_CLASSIFICACAO_RESULTADO; 
			nova.NAIVE_BAYES_CLASSIFICACAO = analise[casoDeRecomendacao].NAIVE_BAYES_CLASSIFICACAO; 
			nova.NNET_CLASSIFICACAO_RESULTADO = analise[casoDeRecomendacao].NNET_CLASSIFICACAO_RESULTADO; 
			nova.SVM_CLASSIFICACAO_RESULTADO = analise[casoDeRecomendacao].SVM_CLASSIFICACAO_RESULTADO;
			nova.KNN_CLASSIFICACAO_RESULTADO = analise[casoDeRecomendacao].KNN_CLASSIFICACAO_RESULTADO;
			nova.SVM_COMPOSTO = analise[casoDeRecomendacao].SVM_COMPOSTO;
			nova.ROTATION_FOREST = analise[casoDeRecomendacao].ROTATION_FOREST;
			arrayDeCopiaPAraAnalise.add(nova);

		}

		ordenaTudoRecomendacoes(arrayDeCopiaPAraAnalise, ListaAnotacoes, contador, workflows);
		finalList.geraDadosClassificadorComposto(workflows);
	}

	@SuppressWarnings("unused")
	private void ordenaTudoRecomendacoes(ArrayList<LinhaAnalise> arrayCom118, 
			final ListaDeAtividadesAnotadas ListaAnotacoes, int rodada,String [][] workflows){

		//pILHAS fALSE
		Stack<LinhaAnalise> pilhaAnaliseCARTREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseBINREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseMARSREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNETREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseCARTCLASS = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNAIVE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNET = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVM = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseKNN = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMCOMP = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseROTATION = new Stack<LinhaAnalise>();

		//Pilhas true
		Stack<LinhaAnalise> pilhaAnaliseCARTREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseBINREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseMARSREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNETREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseCARTCLASSTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNAIVETRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNETTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseKNNTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMCOMPTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseROTATIONTRUE = new Stack<LinhaAnalise>();

		ArrayList<LinhaAnalise> arrayDeCopiaPAraAnalise = clonaArrayLists(arrayCom118);

		//Cada 118 representam um caso dos experimentos
		for (int i = 0; i < arrayCom118.size(); i++) {//Itera em todas as recomendacoes, removendo as false

			
		
			
			//Cassos FALSE
			//Para cada caso devo acrescentar uma Lista completa de atividades ordenadas no final.
			if(arrayDeCopiaPAraAnalise.get(i).CART_REGRESSAO_RESULTADO == 0){//add na pilha

				pilhaAnaliseCARTREG.push(arrayDeCopiaPAraAnalise.get(i));			
			}

			if(arrayDeCopiaPAraAnalise.get(i).BINOMIAL_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseBINREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).MARS_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseMARSREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseNNETREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseSVMREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).CART_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseCARTCLASS.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NAIVE_BAYES_CLASSIFICACAO == 0){//add na pilha
				pilhaAnaliseNAIVE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseNNET.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseSVM.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).KNN_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseKNN.push(arrayDeCopiaPAraAnalise.get(i));

			}
			
			if(arrayDeCopiaPAraAnalise.get(i).SVM_COMPOSTO == 0){//add na pilha

				pilhaAnaliseSVMCOMP.push(arrayDeCopiaPAraAnalise.get(i));			
			}
			if(arrayDeCopiaPAraAnalise.get(i).ROTATION_FOREST == 0){//add na pilha

				pilhaAnaliseROTATION.push(arrayDeCopiaPAraAnalise.get(i));			
			}
			

			//Casos TRUE
			if(arrayDeCopiaPAraAnalise.get(i).CART_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseCARTREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));				
			}

			if(arrayDeCopiaPAraAnalise.get(i).BINOMIAL_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseBINREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).MARS_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseMARSREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseNNETREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseSVMREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).CART_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseCARTCLASSTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NAIVE_BAYES_CLASSIFICACAO == 1){//add na pilha
				pilhaAnaliseNAIVETRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseNNETTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseSVMTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).KNN_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseKNNTRUE.push(arrayDeCopiaPAraAnalise.get(i));				
			}
			
			if(arrayDeCopiaPAraAnalise.get(i).SVM_COMPOSTO == 1){//add na pilha
				pilhaAnaliseSVMCOMPTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).ROTATION_FOREST == 1){//add na pilha
				pilhaAnaliseROTATIONTRUE.push(arrayDeCopiaPAraAnalise.get(i));				
			}

		}

		System.out.println("DOIne!");

		//InsereTUDOObjetoListaFinal(pilhaAnaliseCARTREGTRUE, pilhaAnaliseCARTREG, ListaAnotacoes, rodada, 0, workflows);
		InsereObjetoListaFinal(pilhaAnaliseCARTREGTRUE, pilhaAnaliseCARTREG, ListaAnotacoes, rodada, 0);
		InsereObjetoListaFinal(pilhaAnaliseBINREGTRUE, pilhaAnaliseBINREG, ListaAnotacoes, rodada, 1);
		InsereObjetoListaFinal(pilhaAnaliseMARSREGTRUE, pilhaAnaliseMARSREG, ListaAnotacoes, rodada, 2);
		InsereObjetoListaFinal(pilhaAnaliseNNETREGTRUE, pilhaAnaliseNNETREG, ListaAnotacoes, rodada, 3);
		InsereObjetoListaFinal(pilhaAnaliseSVMREGTRUE, pilhaAnaliseSVMREG, ListaAnotacoes, rodada,4);
		InsereObjetoListaFinal(pilhaAnaliseCARTCLASSTRUE, pilhaAnaliseCARTCLASS, ListaAnotacoes, rodada,5);
		InsereObjetoListaFinal(pilhaAnaliseNAIVETRUE, pilhaAnaliseNAIVE, ListaAnotacoes, rodada,6);
		InsereObjetoListaFinal(pilhaAnaliseNNETTRUE, pilhaAnaliseNNET, ListaAnotacoes, rodada,7);
		InsereObjetoListaFinal(pilhaAnaliseSVMTRUE, pilhaAnaliseSVM, ListaAnotacoes, rodada,8) ;
		InsereObjetoListaFinal(pilhaAnaliseKNNTRUE, pilhaAnaliseKNN, ListaAnotacoes, rodada,9);
		InsereObjetoListaFinal(pilhaAnaliseSVMCOMPTRUE, pilhaAnaliseSVMCOMP, ListaAnotacoes, rodada,10) ;
		InsereObjetoListaFinal(pilhaAnaliseROTATIONTRUE, pilhaAnaliseROTATION, ListaAnotacoes, rodada,11);
		
		

	}
	public void InsereTUDOObjetoListaFinal(Stack<LinhaAnalise> pilhaAnaliseCARTREGTRUE, 
			Stack<LinhaAnalise> pilhaAnaliseCARTREG,
			final ListaDeAtividadesAnotadas ListaAnotacoes, 
			int rodada, int numeroLista, String [][]workflows){

		ArrayList<LinhaAnalise> ordenarCARTREGTRUE = new ArrayList<LinhaAnalise>();

		//Agora vamos ordenar cada uma das dez pilhas TRUE j < pilhaAnaliseCARTREGTRUE.size()
		for (int j = 0; !pilhaAnaliseCARTREGTRUE.isEmpty(); j++) {
			ordenarCARTREGTRUE.add(pilhaAnaliseCARTREGTRUE.pop());
		}

		//Ordena alfabeticamente.
		Collections.sort(ordenarCARTREGTRUE, new Comparator<LinhaAnalise>() {

			@Override
			public int compare(LinhaAnalise  linha1, LinhaAnalise  linha2)
			{

				String atv1 = linha1.nomeAtividadeRecomendada;
				String atv2 = linha2.nomeAtividadeRecomendada;

				//Pesquisa o numero de ontologias que possui
				int tamanhoLista01 = ListaAnotacoes.contemAnotacaoTamanhoDaLista(atv1);
				int tamanhoLista02 = ListaAnotacoes.contemAnotacaoTamanhoDaLista(atv2);

				if(tamanhoLista01 > tamanhoLista02) {
					return 1;
				}
				else if(tamanhoLista01 < tamanhoLista02){
					return -1;
				}
				else { 
					//Pega a frequencia
					int freq01 = linha1.frequencia;
					int freq02 = linha2.frequencia;

					if(freq01 > freq02){
						return 1;
					}

					if(freq01 < freq02){
						return -1;
					}	
					else{
						return 0;
					}					
				}
			}
		});

		//Add os Falses no fim da lista
		for (int j = 0; !pilhaAnaliseCARTREG.isEmpty() ; j++) {
			ordenarCARTREGTRUE.add(ordenarCARTREGTRUE.size(), pilhaAnaliseCARTREG.pop());
		}

		//insere as atividades finais
		for (int i = 0; i < ListaAnotacoes.listaAtividadesAnotadas.size(); i++) {

			LinhaAnalise nova = new LinhaAnalise();			
			nova.classeReal = 0;
			nova.nomeAtividadeRecomendada = ListaAnotacoes.listaAtividadesAnotadas.get(i).nome;
			nova.frequencia = 0;
			nova.cod_ontologia = 0;
			nova.CART_REGRESSAO_RESULTADO = 0; 
			nova.BINOMIAL_REGRESSAO_RESULTADO = 0; 
			nova.MARS_REGRESSAO_RESULTADO = 0; 
			nova.NNET_REGRESSAO_RESULTADO = 0; 
			nova.SVM_REGRESSAO_RESULTADO = 0; 
			nova.CART_CLASSIFICACAO_RESULTADO = 0; 
			nova.NAIVE_BAYES_CLASSIFICACAO = 0; 
			nova.NNET_CLASSIFICACAO_RESULTADO = 0; 
			nova.SVM_CLASSIFICACAO_RESULTADO = 0;
			nova.KNN_CLASSIFICACAO_RESULTADO = 0;

			//insere no fim da lista
			ordenarCARTREGTRUE.add(ordenarCARTREGTRUE.size(),nova);
		}

		//System.out.println(ordenarCARTREGTRUE.size());

		//Insere no objeto Fodao! Aqui vai ser a diferenca!!!
		int tmp = ordenarCARTREGTRUE.size();
		ArrayList<String> testinho = new ArrayList<String>();
		for (int i = 0; i < ordenarCARTREGTRUE.size(); i++) {
			testinho.add(ordenarCARTREGTRUE.get(i).nomeAtividadeRecomendada);
		
		}
		
		for (int i = 0; i < testinho.size(); i++) {
	//		System.out.println("i: "+i+"--"+testinho.get(i));
		
		}
		//System.out.println(workflows.length);//soma 4 no que vier
		
		for (int i = 0; i < workflows.length; i++) {
			//for (int j = 4; j < workflows[i].length; j++) {
			//	System.out.println(i+"--"+workflows[i][1]);//cada uma procuro na lista testinho a posicao :) busca linear mesmo pra termnar logo
				//System.out.println(workflows[i][4+numeroLista]+", ");
			//}
		//	System.out.println();
			
			String pesquisada = workflows[i][1];
			for (int j = 0; j < testinho.size(); j++) {
				if(testinho.get(j).equals(pesquisada)){
					System.out.println(j);
					break;
				}
			}
			
		}
		
		
		
		//Pesquisa em cada workflow as atividades e cria uma matriz que será salva em memoria
		
		//System.out.println();

		//System.out.println();

	}
	
	public static int removeDuplicates(ArrayList<String> strings) {

	    int size = strings.size();
	    int duplicates = 0;

	    // not using a method in the check also speeds up the execution
	    // also i must be less that size-1 so that j doesn't
	    // throw IndexOutOfBoundsException
	    for (int i = 0; i < size - 1; i++) {
	        // start from the next item after strings[i]
	        // since the ones before are checked
	        for (int j = i + 1; j < size; j++) {
	            // no need for if ( i == j ) here
	            if (!strings.get(j).equals(strings.get(i)))
	                continue;
	            duplicates++;
	            strings.remove(j);
	            // decrease j because the array got re-indexed
	            j--;
	            // decrease the size of the array
	            size--;
	        } // for j
	    } // for i

	    return duplicates;

	}
	
	public void efetuaRecomendacaoTotal(ListaDeAtividadesAnotadas ListaAnotacoes, String [][] workflows){

		int contador = 0;
		//		for (int i = 1; i < analise.length; i = i + 118) { estava com 119 para um teste pontual
		for (int casoDeRecomendacao = 1; 
				casoDeRecomendacao < analise.length; 
				casoDeRecomendacao = casoDeRecomendacao + 118) {//Deveria ter 73 rodadas



			ArrayList<LinhaAnalise> arrayDeCopiaPAraAnalise = new ArrayList<LinhaAnalise>();
			for (int j = casoDeRecomendacao; j < (casoDeRecomendacao+118); j++) {
				//analise[j].printObject();
				//System.out.println(" contador: " + contador + " -- valor de i: "+i+"\t valor de j: "+j);

				LinhaAnalise nova = new LinhaAnalise();
				nova.classeReal = analise[j].classeReal;
				nova.nomeAtividadeRecomendada = analise[j].nomeAtividadeRecomendada;
				nova.frequencia = analise[j].frequencia;
				nova.cod_ontologia = analise[j].cod_ontologia;
				nova.CART_REGRESSAO_RESULTADO = analise[j].CART_REGRESSAO_RESULTADO; 
				nova.BINOMIAL_REGRESSAO_RESULTADO = analise[j].BINOMIAL_REGRESSAO_RESULTADO; 
				nova.MARS_REGRESSAO_RESULTADO = analise[j].MARS_REGRESSAO_RESULTADO; 
				nova.NNET_REGRESSAO_RESULTADO = analise[j].NNET_REGRESSAO_RESULTADO; 
				nova.SVM_REGRESSAO_RESULTADO = analise[j].SVM_REGRESSAO_RESULTADO; 
				nova.CART_CLASSIFICACAO_RESULTADO = analise[j].CART_CLASSIFICACAO_RESULTADO; 
				nova.NAIVE_BAYES_CLASSIFICACAO = analise[j].NAIVE_BAYES_CLASSIFICACAO; 
				nova.NNET_CLASSIFICACAO_RESULTADO = analise[j].NNET_CLASSIFICACAO_RESULTADO; 
				nova.SVM_CLASSIFICACAO_RESULTADO = analise[j].SVM_CLASSIFICACAO_RESULTADO;
				nova.KNN_CLASSIFICACAO_RESULTADO = analise[j].KNN_CLASSIFICACAO_RESULTADO;
				arrayDeCopiaPAraAnalise.add(nova);

				//vai add e cria um arraylist aqui

			}

			//System.out.println(contador);
			//Passa para o ordenador
			ordena118Recomendacoes(arrayDeCopiaPAraAnalise, ListaAnotacoes, contador);

			//Zera o objeto
			arrayDeCopiaPAraAnalise = null;
			contador = contador + 1;
			//contador = 0;
			//System.out.println("-----------------------------------------------------");
		}

		finalList.geraListaSimplesFinalComposta(workflows);


	}

	public void efetuaRecomendacao(ListaDeAtividadesAnotadas ListaAnotacoes){

		int contador = 0;
		//		for (int i = 1; i < analise.length; i = i + 118) { estava com 119 para um teste pontual
		for (int casoDeRecomendacao = 1; 
				casoDeRecomendacao < analise.length; 
				casoDeRecomendacao = casoDeRecomendacao + 118) {//Deveria ter 73 rodadas



			ArrayList<LinhaAnalise> arrayDeCopiaPAraAnalise = new ArrayList<LinhaAnalise>();
			for (int j = casoDeRecomendacao; j < (casoDeRecomendacao+118); j++) {
				//analise[j].printObject();
				//System.out.println(" contador: " + contador + " -- valor de i: "+i+"\t valor de j: "+j);

				LinhaAnalise nova = new LinhaAnalise();
				nova.classeReal = analise[j].classeReal;
				nova.nomeAtividadeRecomendada = analise[j].nomeAtividadeRecomendada;
				nova.frequencia = analise[j].frequencia;
				nova.cod_ontologia = analise[j].cod_ontologia;
				nova.CART_REGRESSAO_RESULTADO = analise[j].CART_REGRESSAO_RESULTADO; 
				nova.BINOMIAL_REGRESSAO_RESULTADO = analise[j].BINOMIAL_REGRESSAO_RESULTADO; 
				nova.MARS_REGRESSAO_RESULTADO = analise[j].MARS_REGRESSAO_RESULTADO; 
				nova.NNET_REGRESSAO_RESULTADO = analise[j].NNET_REGRESSAO_RESULTADO; 
				nova.SVM_REGRESSAO_RESULTADO = analise[j].SVM_REGRESSAO_RESULTADO; 
				nova.CART_CLASSIFICACAO_RESULTADO = analise[j].CART_CLASSIFICACAO_RESULTADO; 
				nova.NAIVE_BAYES_CLASSIFICACAO = analise[j].NAIVE_BAYES_CLASSIFICACAO; 
				nova.NNET_CLASSIFICACAO_RESULTADO = analise[j].NNET_CLASSIFICACAO_RESULTADO; 
				nova.SVM_CLASSIFICACAO_RESULTADO = analise[j].SVM_CLASSIFICACAO_RESULTADO;
				nova.KNN_CLASSIFICACAO_RESULTADO = analise[j].KNN_CLASSIFICACAO_RESULTADO;
				nova.SVM_COMPOSTO = analise[j].SVM_COMPOSTO;
				nova.ROTATION_FOREST = analise[j].ROTATION_FOREST;
				arrayDeCopiaPAraAnalise.add(nova);

				//vai add e cria um arraylist aqui

			}

			//System.out.println(contador);
			//Passa para o ordenador
			ordena118Recomendacoes(arrayDeCopiaPAraAnalise, ListaAnotacoes, contador);

			//Zera o objeto
			arrayDeCopiaPAraAnalise = null;
			contador = contador + 1;
			//contador = 0;
			//System.out.println("-----------------------------------------------------");
		}

		finalList.geraListaSimplesFinal();


	}

	@SuppressWarnings("unused")
	private void ordena118Recomendacoes(ArrayList<LinhaAnalise> arrayCom118, 
			final ListaDeAtividadesAnotadas ListaAnotacoes, int rodada){

		//pILHAS fALSE
		Stack<LinhaAnalise> pilhaAnaliseCARTREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseBINREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseMARSREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNETREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMREG = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseCARTCLASS = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNAIVE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNET = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVM = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseKNN = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMCOMP = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseROTATION = new Stack<LinhaAnalise>();
		
		//Pilhas true
		Stack<LinhaAnalise> pilhaAnaliseCARTREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseBINREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseMARSREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNETREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMREGTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseCARTCLASSTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNAIVETRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseNNETTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseKNNTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseSVMCOMPTRUE = new Stack<LinhaAnalise>();
		Stack<LinhaAnalise> pilhaAnaliseROTATIONTRUE = new Stack<LinhaAnalise>();

		ArrayList<LinhaAnalise> arrayDeCopiaPAraAnalise = clonaArrayLists(arrayCom118);

		//Cada 118 representam um caso dos experimentos
		for (int i = 0; i < arrayCom118.size(); i++) {//Itera em todas as recomendacoes, removendo as false

			//Cassos FALSE
			//Para cada caso devo acrescentar uma Lista completa de atividades ordenadas no final.
			if(arrayDeCopiaPAraAnalise.get(i).CART_REGRESSAO_RESULTADO == 0){//add na pilha

				pilhaAnaliseCARTREG.push(arrayDeCopiaPAraAnalise.get(i));			
			}

			if(arrayDeCopiaPAraAnalise.get(i).BINOMIAL_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseBINREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).MARS_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseMARSREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseNNETREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_REGRESSAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseSVMREG.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).CART_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseCARTCLASS.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NAIVE_BAYES_CLASSIFICACAO == 0){//add na pilha
				pilhaAnaliseNAIVE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseNNET.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseSVM.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).KNN_CLASSIFICACAO_RESULTADO == 0){//add na pilha
				pilhaAnaliseKNN.push(arrayDeCopiaPAraAnalise.get(i));

			}
			
			if(arrayDeCopiaPAraAnalise.get(i).SVM_COMPOSTO == 0){//add na pilha

				pilhaAnaliseSVMCOMP.push(arrayDeCopiaPAraAnalise.get(i));			
			}
			if(arrayDeCopiaPAraAnalise.get(i).ROTATION_FOREST == 0){//add na pilha

				pilhaAnaliseROTATION.push(arrayDeCopiaPAraAnalise.get(i));			
			}
			

			//Casos TRUE
			if(arrayDeCopiaPAraAnalise.get(i).CART_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseCARTREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));				
			}

			if(arrayDeCopiaPAraAnalise.get(i).BINOMIAL_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseBINREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).MARS_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseMARSREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseNNETREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_REGRESSAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseSVMREGTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).CART_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseCARTCLASSTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NAIVE_BAYES_CLASSIFICACAO == 1){//add na pilha
				pilhaAnaliseNAIVETRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).NNET_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseNNETTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).SVM_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseSVMTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).KNN_CLASSIFICACAO_RESULTADO == 1){//add na pilha
				pilhaAnaliseKNNTRUE.push(arrayDeCopiaPAraAnalise.get(i));				
			}
			
			if(arrayDeCopiaPAraAnalise.get(i).SVM_COMPOSTO == 1){//add na pilha
				pilhaAnaliseSVMCOMPTRUE.push(arrayDeCopiaPAraAnalise.get(i));

			}
			if(arrayDeCopiaPAraAnalise.get(i).ROTATION_FOREST == 1){//add na pilha
				pilhaAnaliseROTATIONTRUE.push(arrayDeCopiaPAraAnalise.get(i));				
			}

		}
	
		

		InsereObjetoListaFinal(pilhaAnaliseCARTREGTRUE, pilhaAnaliseCARTREG, ListaAnotacoes, rodada, 0);
		InsereObjetoListaFinal(pilhaAnaliseBINREGTRUE, pilhaAnaliseBINREG, ListaAnotacoes, rodada, 1);
		InsereObjetoListaFinal(pilhaAnaliseMARSREGTRUE, pilhaAnaliseMARSREG, ListaAnotacoes, rodada, 2);
		InsereObjetoListaFinal(pilhaAnaliseNNETREGTRUE, pilhaAnaliseNNETREG, ListaAnotacoes, rodada, 3);
		InsereObjetoListaFinal(pilhaAnaliseSVMREGTRUE, pilhaAnaliseSVMREG, ListaAnotacoes, rodada,4);
		InsereObjetoListaFinal(pilhaAnaliseCARTCLASSTRUE, pilhaAnaliseCARTCLASS, ListaAnotacoes, rodada,5);
		InsereObjetoListaFinal(pilhaAnaliseNAIVETRUE, pilhaAnaliseNAIVE, ListaAnotacoes, rodada,6);
		InsereObjetoListaFinal(pilhaAnaliseNNETTRUE, pilhaAnaliseNNET, ListaAnotacoes, rodada,7);
		InsereObjetoListaFinal(pilhaAnaliseSVMTRUE, pilhaAnaliseSVM, ListaAnotacoes, rodada,8) ;
		InsereObjetoListaFinal(pilhaAnaliseKNNTRUE, pilhaAnaliseKNN, ListaAnotacoes, rodada,9);
		InsereObjetoListaFinal(pilhaAnaliseSVMCOMPTRUE, pilhaAnaliseSVMCOMP, ListaAnotacoes, rodada,10) ;
		InsereObjetoListaFinal(pilhaAnaliseROTATIONTRUE, pilhaAnaliseROTATION, ListaAnotacoes, rodada,11);

	}

	public void InsereObjetoListaFinal(Stack<LinhaAnalise> pilhaAnaliseCARTREGTRUE, 
			Stack<LinhaAnalise> pilhaAnaliseCARTREG,
			final ListaDeAtividadesAnotadas ListaAnotacoes, 
			int rodada, int numeroLista){

		ArrayList<LinhaAnalise> ordenarCARTREGTRUE = new ArrayList<LinhaAnalise>();

		//Agora vamos ordenar cada uma das dez pilhas TRUE j < pilhaAnaliseCARTREGTRUE.size()
		for (int j = 0; !pilhaAnaliseCARTREGTRUE.isEmpty(); j++) {
			ordenarCARTREGTRUE.add(pilhaAnaliseCARTREGTRUE.pop());
		}

		//Ordena alfabeticamente.
		Collections.sort(ordenarCARTREGTRUE, new Comparator<LinhaAnalise>() {

			@Override
			public int compare(LinhaAnalise  linha1, LinhaAnalise  linha2)
			{

				String atv1 = linha1.nomeAtividadeRecomendada;
				String atv2 = linha2.nomeAtividadeRecomendada;

				//Pesquisa o numero de ontologias que possui
				int tamanhoLista01 = ListaAnotacoes.contemAnotacaoTamanhoDaLista(atv1);
				int tamanhoLista02 = ListaAnotacoes.contemAnotacaoTamanhoDaLista(atv2);

				if(tamanhoLista01 > tamanhoLista02) {
					return 1;
				}
				else if(tamanhoLista01 < tamanhoLista02){
					return -1;
				}
				else { 
					//Pega a frequencia
					int freq01 = linha1.frequencia;
					int freq02 = linha2.frequencia;

					if(freq01 > freq02){
						return 1;
					}

					if(freq01 < freq02){
						return -1;
					}	
					else{
						return 0;
					}					
				}
			}
		});

		//Add os Falses no fim da lista
		for (int j = 0; !pilhaAnaliseCARTREG.isEmpty() ; j++) {
			ordenarCARTREGTRUE.add(ordenarCARTREGTRUE.size(), pilhaAnaliseCARTREG.pop());
		}

		//insere as atividades finais contanto que elas nao tenham repeticoes!!, verifica primeiro e roda depois
		for (int i = 0; i < ListaAnotacoes.listaAtividadesAnotadas.size(); i++) {

			String atvListinha = ListaAnotacoes.listaAtividadesAnotadas.get(i).nome;
			if(ordenarCARTREGTRUE.contains(atvListinha) == false){ 
				LinhaAnalise nova = new LinhaAnalise();			
				nova.classeReal = 0;
				nova.nomeAtividadeRecomendada = ListaAnotacoes.listaAtividadesAnotadas.get(i).nome;
				nova.frequencia = 0;
				nova.cod_ontologia = 0;
				nova.CART_REGRESSAO_RESULTADO = 0; 
				nova.BINOMIAL_REGRESSAO_RESULTADO = 0; 
				nova.MARS_REGRESSAO_RESULTADO = 0; 
				nova.NNET_REGRESSAO_RESULTADO = 0; 
				nova.SVM_REGRESSAO_RESULTADO = 0; 
				nova.CART_CLASSIFICACAO_RESULTADO = 0; 
				nova.NAIVE_BAYES_CLASSIFICACAO = 0; 
				nova.NNET_CLASSIFICACAO_RESULTADO = 0; 
				nova.SVM_CLASSIFICACAO_RESULTADO = 0;
				nova.KNN_CLASSIFICACAO_RESULTADO = 0;
				nova.SVM_COMPOSTO = 0;
				nova.ROTATION_FOREST = 0;

				//insere no fim da lista
				ordenarCARTREGTRUE.add(ordenarCARTREGTRUE.size(),nova);
			}
			
		}

		//System.out.println(ordenarCARTREGTRUE.size());

		//Insere no objeto Fodao! Aqui vai ser a diferenca!!!
		int tmp = ordenarCARTREGTRUE.size();
		for (int i = 0; i < ordenarCARTREGTRUE.size(); i++) {
			//finalList[numeroLista] //(ordenarCARTREGTRUE.get(i).nomeAtividadeRecomendada);
			//Object ooo = 
			//System.out.println(rodada);
			finalList.DezCasos[numeroLista].listas[rodada].atividadesOrdenadas.add(ordenarCARTREGTRUE.get(i).nomeAtividadeRecomendada);
			//System.out.println(ordenarCARTREGTRUE.get(i).nomeAtividadeRecomendada);//i+"--"+

		}
		//System.out.println();

		//System.out.println();

	}




	private ArrayList<LinhaAnalise> clonaArrayLists(ArrayList<LinhaAnalise> arrayCom118){

		ArrayList<LinhaAnalise> retorno = new  ArrayList<LinhaAnalise>();
		for (int i = 0; i < arrayCom118.size(); i++) {

			//Cria um objeto novo
			LinhaAnalise nova = new LinhaAnalise();
			nova.classeReal = arrayCom118.get(i).classeReal;
			nova.nomeAtividadeRecomendada = arrayCom118.get(i).nomeAtividadeRecomendada;
			nova.frequencia = arrayCom118.get(i).frequencia;
			nova.cod_ontologia = arrayCom118.get(i).cod_ontologia;
			nova.CART_REGRESSAO_RESULTADO = arrayCom118.get(i).CART_REGRESSAO_RESULTADO; 
			nova.BINOMIAL_REGRESSAO_RESULTADO = arrayCom118.get(i).BINOMIAL_REGRESSAO_RESULTADO; 
			nova.MARS_REGRESSAO_RESULTADO = arrayCom118.get(i).MARS_REGRESSAO_RESULTADO; 
			nova.NNET_REGRESSAO_RESULTADO = arrayCom118.get(i).NNET_REGRESSAO_RESULTADO; 
			nova.SVM_REGRESSAO_RESULTADO = arrayCom118.get(i).SVM_REGRESSAO_RESULTADO; 
			nova.CART_CLASSIFICACAO_RESULTADO = arrayCom118.get(i).CART_CLASSIFICACAO_RESULTADO; 
			nova.NAIVE_BAYES_CLASSIFICACAO = arrayCom118.get(i).NAIVE_BAYES_CLASSIFICACAO; 
			nova.NNET_CLASSIFICACAO_RESULTADO = arrayCom118.get(i).NNET_CLASSIFICACAO_RESULTADO; 
			nova.SVM_CLASSIFICACAO_RESULTADO = arrayCom118.get(i).SVM_CLASSIFICACAO_RESULTADO;
			nova.KNN_CLASSIFICACAO_RESULTADO = arrayCom118.get(i).KNN_CLASSIFICACAO_RESULTADO;
			retorno.add(nova);
		}	

		return retorno;
	}


}


public class SistemaDeRecomendacao {

	private int tryParseInt(String converter){

		int retorno = -1;
		try {
			retorno = Integer.parseInt(converter);
		} catch (Exception e) {
			retorno = -1;
			//System.err.println(converter);
			//System.err.println("AIAIAIAIAIAI");
		}

		return retorno;
	}

	public Grupo118Analise geraDadosClassificadorComposto(String [][] workflows, ListaDeAtividadesAnotadas ListaAnotacoes){		

		//Preenche o objeto recomendador
		Grupo118Analise grpAna = new Grupo118Analise();
		for (int i = 1; i < workflows.length; i++) {

			grpAna.analise[i].classeReal = tryParseInt(workflows[i][0]);
			grpAna.analise[i].nomeAtividadeRecomendada = workflows[i][1];
			grpAna.analise[i].frequencia = tryParseInt(workflows[i][2]);
			grpAna.analise[i].cod_ontologia = tryParseInt(workflows[i][3]);
			grpAna.analise[i].CART_REGRESSAO_RESULTADO = tryParseInt(workflows[i][4]); 
			grpAna.analise[i].BINOMIAL_REGRESSAO_RESULTADO = tryParseInt(workflows[i][5]); 
			grpAna.analise[i].MARS_REGRESSAO_RESULTADO = tryParseInt(workflows[i][6]); 
			grpAna.analise[i].NNET_REGRESSAO_RESULTADO = tryParseInt(workflows[i][7]); 
			grpAna.analise[i].SVM_REGRESSAO_RESULTADO = tryParseInt(workflows[i][8]); 
			grpAna.analise[i].CART_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][9]); 
			grpAna.analise[i].NAIVE_BAYES_CLASSIFICACAO = tryParseInt(workflows[i][10]); 
			grpAna.analise[i].NNET_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][11]); 
			grpAna.analise[i].SVM_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][12]);
			grpAna.analise[i].KNN_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][13]);

		}

		//Cria alguma estrutura ordenada de acordo com os critérios
		grpAna.criaArquivoParaClassificadorComposto(ListaAnotacoes, workflows);

		//imprimeListaAtividades(ListaAnotacoes);
		//imprimeWfs(workflows);



		return grpAna;
	}


	/*
	 * Percorre a matriz 'workflows' e agrupa a cada 118 linhas
	 *Ordena essas 118 e junta com a lista de atividades ordenadas.
	 * 
	 * */
	public Grupo118Analise recomenda(String [][] workflows, ListaDeAtividadesAnotadas ListaAnotacoes){		

		//Preenche o objeto recomendador
		Grupo118Analise grpAna = new Grupo118Analise();
		for (int i = 1; i < workflows.length; i++) {

			grpAna.analise[i].classeReal = tryParseInt(workflows[i][0]);
			grpAna.analise[i].nomeAtividadeRecomendada = workflows[i][1];
			grpAna.analise[i].frequencia = tryParseInt(workflows[i][2]);
			grpAna.analise[i].cod_ontologia = tryParseInt(workflows[i][3]);
			grpAna.analise[i].CART_REGRESSAO_RESULTADO = tryParseInt(workflows[i][4]); 
			grpAna.analise[i].BINOMIAL_REGRESSAO_RESULTADO = tryParseInt(workflows[i][5]); 
			grpAna.analise[i].MARS_REGRESSAO_RESULTADO = tryParseInt(workflows[i][6]); 
			grpAna.analise[i].NNET_REGRESSAO_RESULTADO = tryParseInt(workflows[i][7]); 
			grpAna.analise[i].SVM_REGRESSAO_RESULTADO = tryParseInt(workflows[i][8]); 
			grpAna.analise[i].CART_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][9]); 
			grpAna.analise[i].NAIVE_BAYES_CLASSIFICACAO = tryParseInt(workflows[i][10]); 
			grpAna.analise[i].NNET_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][11]); 
			grpAna.analise[i].SVM_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][12]);
			grpAna.analise[i].KNN_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][13]);
			grpAna.analise[i].SVM_COMPOSTO = tryParseInt(workflows[i][14]);
			grpAna.analise[i].ROTATION_FOREST = tryParseInt(workflows[i][15]);
			
		}

		//Cria alguma estrutura ordenada de acordo com os critérios
		grpAna.efetuaRecomendacao(ListaAnotacoes);

		//imprimeListaAtividades(ListaAnotacoes);
		//imprimeWfs(workflows);



		return grpAna;
	}
	public Grupo118Analise recomendaTotal(String [][] workflows, ListaDeAtividadesAnotadas ListaAnotacoes){		

		//Preenche o objeto recomendador
		Grupo118Analise grpAna = new Grupo118Analise();
		for (int i = 1; i < workflows.length; i++) {

			grpAna.analise[i].classeReal = tryParseInt(workflows[i][0]);
			grpAna.analise[i].nomeAtividadeRecomendada = workflows[i][1];
			grpAna.analise[i].frequencia = tryParseInt(workflows[i][2]);
			grpAna.analise[i].cod_ontologia = tryParseInt(workflows[i][3]);
			grpAna.analise[i].CART_REGRESSAO_RESULTADO = tryParseInt(workflows[i][4]); 
			grpAna.analise[i].BINOMIAL_REGRESSAO_RESULTADO = tryParseInt(workflows[i][5]); 
			grpAna.analise[i].MARS_REGRESSAO_RESULTADO = tryParseInt(workflows[i][6]); 
			grpAna.analise[i].NNET_REGRESSAO_RESULTADO = tryParseInt(workflows[i][7]); 
			grpAna.analise[i].SVM_REGRESSAO_RESULTADO = tryParseInt(workflows[i][8]); 
			grpAna.analise[i].CART_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][9]); 
			grpAna.analise[i].NAIVE_BAYES_CLASSIFICACAO = tryParseInt(workflows[i][10]); 
			grpAna.analise[i].NNET_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][11]); 
			grpAna.analise[i].SVM_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][12]);
			grpAna.analise[i].KNN_CLASSIFICACAO_RESULTADO = tryParseInt(workflows[i][13]);

		}

		//Cria alguma estrutura ordenada de acordo com os critérios
		grpAna.efetuaRecomendacaoTotal(ListaAnotacoes, workflows);

		//imprimeListaAtividades(ListaAnotacoes);
		//imprimeWfs(workflows);



		return grpAna;
	}



	/*Para ajudar a testar*/
	@SuppressWarnings("unused")
	private void imprimeListaAtividades(ListaDeAtividadesAnotadas ListaAnotacoes){

		//Itera nas atividades.
		for (int i = 0; i < ListaAnotacoes.listaAtividadesAnotadas.size(); i++) {
			System.out.print(ListaAnotacoes.listaAtividadesAnotadas.get(i).nome+": ");

			StringBuilder builder = new StringBuilder();

			//Itera nas anotacoes de cada atividade.
			for (int j = 0; j < ListaAnotacoes.listaAtividadesAnotadas.get(i).anotacoes.size(); j++) {
				builder.append(ListaAnotacoes.listaAtividadesAnotadas.get(i).anotacoes.get(j)+", ");					
			}

			System.out.print(builder.toString().subSequence(0, builder.toString().length()-2));
			System.out.println();
		}		
	}

	@SuppressWarnings("unused")
	private void imprimeWfs(String [][] workflows){

		for (int i = 0; i < workflows.length-8000; i++) {

			for (int j = 0; j < workflows[i].length; j++) {
				System.out.print(workflows[i][j]+", ");
				//System.out.println("valor de j: "+j);
			}

			System.out.println();
		}



	}

}