package Main;

import java.util.ArrayList;

class ElementoOntologico {

	private String nome;
	private ArrayList<Integer> codOntologico;
	private int profundidade;
	
	public String getNome() {
		return nome;
	}	

	public int getProfundidade() {
		return profundidade;
	}

	public ElementoOntologico(String nome, ArrayList<Integer> codOntologico, int profundidade) {
		
		this.nome			=	nome;
		this.profundidade	=	profundidade;
		this.codOntologico	=	new ArrayList<Integer>();

		//Laco para inserir valores no arraylist
		for (int i = 0; i < codOntologico.size(); i++) {
			this.codOntologico.add(codOntologico.get(i));
		}
	}
}


class ArvoreOntologica {

	private ArrayList<ElementoOntologico> arvoreDeOntologias = new ArrayList<ElementoOntologico>();
	
	public void addElemento(String nome, ArrayList<Integer> codOntologico, int profundidade){
		
		ElementoOntologico novoElemento = new ElementoOntologico(nome, codOntologico, profundidade);
		arvoreDeOntologias.add(novoElemento);		
	}
	
	public int retornaProfundidadeElemento(String nome){
		
		int profundidadeElemento = -1;
		for (ElementoOntologico elementoOntologico: arvoreDeOntologias){
			
			if(elementoOntologico.equals(nome)){
				profundidadeElemento = elementoOntologico.getProfundidade();
				break;
			}
		}
		
		return profundidadeElemento;
	}
	
}


public class Ontologia {

	private ArvoreOntologica arvoreOntologica = null;

	public Ontologia() {

		this.preencheOntologia();
	}
	
	private void preencheOntologia() {
		/*
		//Aqui vou precisar de um arrayList
		arvoreOntologica = new ArvoreOntologica();
		arvoreOntologica.addElemento("Thing", 1, 1);
		arvoreOntologica.addElemento("Experiment", 2, 2);
		arvoreOntologica.addElemento("Alignment", 3, 3);
		arvoreOntologica.addElemento("MSA", 4, 4);
		arvoreOntologica.addElemento("Clustal", 4, 4);
		arvoreOntologica.addElemento("Kalign", 4, 4);
		arvoreOntologica.addElemento("Mafft", 4, 4);
		arvoreOntologica.addElemento("MPSearch", 4, 4);
		arvoreOntologica.addElemento("Muscle", 4, 4);
		arvoreOntologica.addElemento("TCoffe", 4, 4);
		arvoreOntologica.addElemento("PairWise", 4, 3);
		arvoreOntologica.addElemento("BLAST", 4, 4);
		arvoreOntologica.addElemento("DaliLite", 4, 4);
		arvoreOntologica.addElemento("FAST", 4, 4);
		arvoreOntologica.addElemento("SmithWaterman", 4, 4);
		arvoreOntologica.addElemento("AnnotationOfGenes", 4, 3);
		arvoreOntologica.addElemento("BinaryInteraction", 4, 3);
		arvoreOntologica.addElemento("ComparisonOfSequences", 3, 3);
		arvoreOntologica.addElemento("FamiliesClassification", 3, 3);
		arvoreOntologica.addElemento("MappingSequences", 3, 3);
		arvoreOntologica.addElemento("PredictionStructures", 3, 3);
		arvoreOntologica.addElemento("SnapDragons", 4, 4);
		arvoreOntologica.addElemento("SearchStringsComposed", 3, 3);
		*/
	}


}