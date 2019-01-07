/*

--Selects com usuarios
SELECT * FROM usuarios 

--junta com workflows favoritos
INNER JOIN favoritos
ON favoritos.cod_usuario = usuarios.cod

--junta com os grupos
INNER JOIN usuario_grupos
ON usuario_grupos.cod_usuario = usuarios.cod

INNER JOIN grupos
ON grupos.cod_grupo = usuario_grupos.cod_grupo

--junta com amigos
INNER JOIN amigos
ON amigos.cod_usuario = usuarios.cod;

*/

/*

SELECT * FROM workflows

INNER JOIN sistema_gerenciador_workflows_cientificos as sgwc 
ON sgwc.arquivo = workflows.chave_sgwc 

INNER JOIN workflows_tags
ON workflows_tags.cod_workflow = workflows.cod_workflow

INNER JOIN tags
ON tags.cod_tag = workflows_tags.cod_tag

--WHERE workflows.chave_sgwc  IN (1,2)

;


﻿select * from usuarios;
select * from usuario_grupos;
select * from usuarios_favoritos;
select * from workflows  ;
select * from workflows_tags ;
select * from sistema_gerenciador_workflows_cientificos ;
select * from tags ;
select * from favoritos  ;
select * from amigos  ;
select * from grupos;

*/


-- vou usar apenas Taverna1 e Taverna2
/*
select
	wf.cod_workflow,
	wf.nome,
	tags.nome_tag,
	tags.cod_tag
	
from workflows as wf 

 -- pega as relacoes com tags
 inner join workflows_tags as wf_tags
 on wf.cod_workflow  = wf_tags.cod_workflow

 -- pega apenas tags de bioinformatica
 inner join tags as tags
 on wf_tags.cod_tag = tags.cod_tag
 
where chave_sgwc in (1, 2) and wf.cod_workflow in (

						   --contem no nome algum termo que remete a bioinformatica
						   30, 119, 75, 74, 115, 379, 80, 116, 72, 31, 
						   154, 81, 113, 117, 801, 124, 821, 804, 3349, 
						   1097, 805, 802, 28, 1428, 1846, 162, 931, 3397, 
						   16, 154, 119, 30, 81, 80, 

						   --contem a tag bioinformatica
						   168, 737, 738, 32, 217, 231, 251, 218, 197, 167, 
						   230, 219, 741, 23, 22, 200, 220, 229, 201, 158, 
						   221, 222, 1456, 202, 228, 162, 223, 740, 227, 250, 
						   1455, 203, 1428, 165, 224, 225, 204, 205, 206, 245, 
						   207, 208, 4, 397, 2083, 209, 244, 1491, 1988, 1452, 
						   39, 2876, 1492, 198, 199, 1454, 1451, 2969, 21, 210, 
						   1705, 1493, 211, 512, 52, 238, 1450, 212, 1494, 2120, 
						   1457, 380, 237, 2121, 236, 235, 377, 1697, 90, 2127, 
						   1696, 213, 368, 2139, 214, 215, 1689, 233, 1688, 6, 
						   216, 232, 109, 1688, 1981, 198, 197, 1491, 1452, 1492, 1451, 
						   --contem BLAST						   
						   1450, 733, 1493, 90, 1494, 71,  );

*/

--Retorna apenas BLAST
select
	distinct wf.cod_workflow ,
	wf.nome
	--tags.nome_tag,
	--tags.cod_tag
	
from workflows as wf 

 -- pega as relacoes com tags
 inner join workflows_tags as wf_tags
 on wf.cod_workflow  = wf_tags.cod_workflow

 -- pega apenas tags de bioinformatica
 inner join tags as tags
 on wf_tags.cod_tag = tags.cod_tag
 
where chave_sgwc in (1, 2) and tags.nome_tag in ('BLAST', 'bioinformatics');



--SELECT id, cod_workflow, nome_atividade, nome_atividade_seguinte
--FROM atividades;





--Pesquisas sobre bioinformatica


/*Numero de atividades*/
select
	distinct atv.nome_atividade,
	wf.cod_workflow,
	wf.nome
	
from workflows as wf 

 -- pega as tags
 inner join workflows_tags as wf_tags
 on wf.cod_workflow  = wf_tags.cod_workflow

 -- pega apenas tags de bioinformatica
 inner join tags as tags
 on wf_tags.cod_tag = tags.cod_tag

 -- pega as atividades
 inner join atividades as atv
 on atv.cod_workflow = wf.cod_workflow

 -- 1 Taverna 1, 2 Vistrails
where chave_sgwc in ( 1 ) and tags.nome_tag in ('bioinformatics')
 --tags.nome_tag like '%mining%';
order by atv.nome_atividade;


/*Numero de workflows*/
select
	distinct wf.cod_workflow ,
	wf.nome
	
from workflows as wf 

 -- pega as relacoes com tags
 inner join workflows_tags as wf_tags
 on wf.cod_workflow  = wf_tags.cod_workflow

 -- pega apenas tags de bioinformatica
 inner join tags as tags
 on wf_tags.cod_tag = tags.cod_tag

 
where chave_sgwc in (1, 2) and tags.nome_tag in ('bioinformatics');
 --tags.nome_tag like '%mining%';




--Pesquisas sobre text mining

/*Numero de atividades*/
select
	distinct atv.nome_atividade,
	wf.cod_workflow ,
	wf.nome
	
from workflows as wf 

 -- pega as relacoes com tags
 inner join workflows_tags as wf_tags
 on wf.cod_workflow  = wf_tags.cod_workflow

 -- pega apenas tags de bioinformatica
 inner join tags as tags
 on wf_tags.cod_tag = tags.cod_tag

 -- pega as atividades relacionadas
 inner join atividades as atv
 on atv.cod_workflow = wf.cod_workflow
 
where chave_sgwc in (1, 2) and tags.nome_tag like '%mining%';


/*Numero de workflows*/
select
	distinct wf.cod_workflow ,
	wf.nome
	
from workflows as wf 

 -- pega as relacoes com tags
 inner join workflows_tags as wf_tags
 on wf.cod_workflow  = wf_tags.cod_workflow

 -- pega apenas tags de bioinformatica
 inner join tags as tags
 on wf_tags.cod_tag = tags.cod_tag

 
where chave_sgwc in (1, 2) and tags.nome_tag like '%mining%';



/*Tags sobre o assunto X*/
select distinct tags.nome_tag from tags as tags where tags.nome_tag like '%mining%';

