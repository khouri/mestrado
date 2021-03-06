#Biblioteca Utils


#impressao padronizada
imprimeLinhaPontilhada <- function(){
  print("-----------------------------------------------------------------------------------------------")
}

#Le tabela CSV
leArquivoCSV <- function(nomeArquivoCsv) {
  return(read.table(nomeArquivoCsv, header = TRUE, sep = ","))
}

#Retorna o caminho absoluto de um arquivo no diretorio local
getCaminhoAbsolutoArquivoLocal <- function(nomeArquivo) {
  
  diretorioLocal <- getwd()
  concatena <- paste(diretorioLocal,"/", nomeArquivo, sep="")
  return(concatena)
}

salvaArquivo <- function(path, parametros, matrizConfusao){
  
  sink(getCaminhoAbsolutoArquivoLocal(path), append=TRUE)
  print(parametros)
  #print(limiar)  
  print(matrizConfusao)
  print("--------------------------------------------------")
  print("--------------------------------------------------")
  sink()
}


pausa <- function()
{
  cat ("Press [enter] to continue")
  line <- scan(n=1)
}


ActivitiesFake <-list(
  list("getfastauniprotentry",6,list(1450,1451,1452,1491,1492))
  ,list("runfasta",2,list(199,200,213))
  ,list("sequence",6,list(4,39,235,236,1689,1697))
  ,list("runclustalw2",5,list(203,206,207,210,213))
  ,list("bytetostring",4,list(4,39,1689,1697))
  ,list("contentlist",15,list(197,198,201,202,203,204,205,207,210,210,212,213,216,218,229,230,230,206,207,208,210,211,216,217,219,220,221,222,224,244))
  ,list("seqret",9,list(158,165,214,215,216,227,229,231,232,245))
  ,list("ddbjaccession",5,list(1454,1455,1456,1457,1493,1494))
  ,list("bioinformaticsparameter",6,list(1451,1452,1455,1456,1491,1493,1450,1451,1454,1455,1491,1492,1493,1494,1452,1456))
  ,list("parametersxml",5,list(223,231,232,245,740,1689,1697))
  ,list("msa",2,list(210,213))
  ,list("unpackoutput",5,list(206,207,210,220,233))
  ,list("searchdatabase",10,list(197,198,201,202,208,210,210,218,220,229,230,231))
  ,list("blastprogram",7,list(197,198,201,202,210,229,230,231))
  ,list("runinterproscan",7,list(4,204,205,212,216,229,230))
  ,list("sequenceorid",24,list(197,198,199,200,201,202,204,205,208,210,211,212,213,214,215,216,218,220,227,229,230,231,232,244,245))
  ,list("email",9,list(210,211,213,216,244,1689,1697))
  ,list("idlist",6,list(209,210,213,230,235,237))
  ,list("unpackxmlresult",11,list(199,200,201,202,204,205,208,212,213,216,229,230))
  ,list("searchsimple",7,list(90,368,1450,1454,1457,1492,1494))
  ,list("contentslist",14,list(197,198,201,202,203,204,205,207,210,210,212,213,216,218,229,230,230,206,207,208,210,211,216,217,219,220,221,222,224,244))
  ,list("getoutput",5,list(206,207,210,220,233))
  ,list("getids",5,list(199,200,201,202,208,213,229))
  ,list("getuniquehomolog",1,list(158))
  ,list("getfastaddbjentry",5,list(1454,1455,1456,1457,1493,1494))
  ,list("checkstatus",29,list(4,39,197,198,199,200,201,202,203,204,205,206,207,208,210,211,212,213,216,217,218,219,220,221,222,224,229,230,233,244))
  ,list("runblastpgp",1,list(208))
  ,list("unpacktextresult",12,list(199,200,201,202,204,205,208,212))
  ,list("sequencedatabase",8,list(1492,1494))
  ,list("jobparams",26,list(197,198,201,202,203,204,205,206,207,208,210,211,212,213,216,217,218,219,220,221,222,224,229,230,233,244))
  ,list("runncbiblast",3,list(39,201,202,229))
  ,list("gettextresult",16,list(197,198,199,200,201,202,204,205,208,210,211,212,213,216,218,229,230,230))
  ,list("runphobius",3,list(211,216))
  ,list("getalignment",5,list(217,219,221,222,244))
  ,list("fetchbatch",6,list(209,210,213,230,235,237,397))
  ,list("tmap",4,list(213,215,216))
  ,list("jobid",29,list(197,198,199,200,201,202,203,204,205,206,207,208,210,211,212,213,216,217,218,219,220,221,222,224,229,230,233,244,1689,1697))
  ,list("xpathfromtext",2,list(231,232,245))
  ,list("bioinformaticsprogram",8,list(1450,1451,1454,1455,1491,1492,1493,1494,1452,1456))
  ,list("uniprotaccession",6,list(1450,1451,1452,1491,1492))
  ,list("sequenceoridorgi",2,list(231,232,245))
  ,list("fetchdata",8,list(214,215,216,224,225,227,229,231,232,245))
  ,list("database",10,list(21,23,32,90,199,200,209,210,213,230,368,1689))
  ,list("runwublast",4,list(197,198,210,230))
  ,list("ebiwublast",2,list(198,230))
  ,list("formatlistfordbfetch",6,list(209,210,213,230,235,237))
  ,list("inputdata",26,list(197,198,199,200,201,202,203,204,205,206,207,208,210,210,211,212,213,216,217,218,219,220,221,222,224,229,230,230,244))
  ,list("ebiinterproscan",3,list(205,229,230))
  ,list("issequence",7,list(214,215,216,227,229,231,232,245))
  ,list("emailaddress",25,list(201,202,203,204,205,206,207,208,210,212,213,216,217,218,219,220,221,222,224,229,230,233))
  ,list("isdone",29,list(4,197,198,199,200,201,202,203,204,205,206,207,208,210,210,211,212,213,213,216,217,218,219,220,221,222,224,229,230,230,231,233,244))
  ,list("unpackalignment",5,list(217,219,221,222,244))
  ,list("program",6,list(21,23,32,90,368,1689))
  ,list("blastddbj",3,list(21,23,32))
  ,list("flattenlist",3,list(231,232,245,738,231,232,245,738,231,232,245,231,232,245,231,232,245))
  ,list("runcensor",1,list(244))
  ,list("getxmlresult",14,list(197,198,199,200,201,202,204,205,208,210,212,213,216,229,230,230))
  ,list("sss",2,list(210,213))
  ,list("sequences",8,list(203,207,210,213,217,219,221,222))
)

#ActivitiesFake

AllActivities <- list(
  "addangle"
  ,"addprefixtoid"
  ,"addsuffix"
  ,"askforemail"
  ,"askforsequence"
  ,"beanshellscriptinghost"
  ,"bget"
  ,"bgetin"
  ,"bioinformaticsparameter"
  ,"bioinformaticsprogram"
  ,"bioinformaticsprograms"
  ,"biologicalsequencedatabase"
  ,"blast"
  ,"blastdatabase"
  ,"blastddbj"
  ,"blastfile"
  ,"blastfilecomparer"
  ,"blastp"
  ,"blastprogram"
  ,"blastsimplifier"
  ,"boxshade"
  ,"buildfastaseq"
  ,"buildinterpromatchesuniparcurl"
  ,"buildqblastgeturl"
  ,"buildqblastputurl"
  ,"bytetostring"
  ,"chainid"
  ,"checkforsrserror"
  ,"checkforsuccess"
  ,"checkstatus"
  ,"checkstatusworkflow"
  ,"chromosomefilter"
  ,"clustalwalignment"
  ,"clustalwphylogenetictree"
  ,"compression"
  ,"concatenatetwostrings"
  ,"contentlist"
  ,"contentslist"
  ,"contentxml"
  ,"convertlistofkeggidsinto1elementlistoflists"
  ,"createfasta"
  ,"database"
  ,"databases"
  ,"ddbjaccession"
  ,"decodebase64tobyte"
  ,"decodetextresult"
  ,"decodexmlresult"
  ,"download"
  ,"downloadin"
  ,"downloadout"
  ,"ebidbfetchfetchbatch"
  ,"ebidbfetchuniparc"
  ,"ebifasta"
  ,"ebifetchinterpromatchesuniparc"
  ,"ebiinterproscan"
  ,"ebiinterproscantmhmmsignalp"
  ,"ebincbiblast"
  ,"ebiphobius"
  ,"ebipicrsequencetoid"
  ,"ebiwublast"
  ,"email"
  ,"emailaddress"
  ,"emma"
  ,"extractaccver"
  ,"extractdes"
  ,"extractentries"
  ,"extractposition"
  ,"extractqblastjobid"
  ,"extractqblaststatus"
  ,"extractseq"
  ,"genscan"
  ,"genscansplitter"
  ,"getalignment"
  ,"getalignmentresult"
  ,"getdownloadurl"
  ,"getdownloadurlin"
  ,"getdownloadurlout"
  ,"getdragonsimpleannotatedimages"
  ,"getentryfromsrs"
  ,"getfastaddbjentry"
  ,"getfastafromgi"
  ,"getfastauniprotentry"
  ,"getguidetreeresult"
  ,"gethitidlist"
  ,"gethssequence"
  ,"getids"
  ,"getimagefromurl"
  ,"getjpegfromannotatedimage"
  ,"getmaskedsequence"
  ,"getmmsequence"
  ,"getnjtreeresult"
  ,"getnucleotidefasta"
  ,"getnucleotidegbseqxml"
  ,"getnucleotideinsdseqxml"
  ,"getnucleotidetinyseqxml"
  ,"getoperationstatus"
  ,"getoperationstatusin"
  ,"getoperationstatusout"
  ,"getorf"
  ,"getoutput"
  ,"getoutputresult"
  ,"getpathwaysbycompounds"
  ,"getphyliptreeresult"
  ,"getproteinfasta"
  ,"getproteingbseqxml"
  ,"getproteininsdseqxml"
  ,"getproteintinyseqxml"
  ,"getresult"
  ,"getrnsequence"
  ,"getstatus"
  ,"gettable"
  ,"gettextresult"
  ,"gettrna"
  ,"getuniquehomolog"
  ,"getupiforsequence"
  ,"getxmlresult"
  ,"gi"
  ,"ginumber"
  ,"gioption"
  ,"hsapiensgeneensembl"
  ,"id"
  ,"idlist"
  ,"idtype"
  ,"idunico"
  ,"inputdata"
  ,"inputdatalist"
  ,"inputlisttext"
  ,"inputlisttextin"
  ,"inputlisttextout"
  ,"inputparameters"
  ,"isdone"
  ,"isgi"
  ,"isrunning"
  ,"issequence"
  ,"isstructure"
  ,"job"
  ,"jobid"
  ,"jobparams"
  ,"jobstatus"
  ,"jobstatuspoll"
  ,"keyin"
  ,"makedatabaselist"
  ,"makefastaprogramlist"
  ,"makeprogramlist"
  ,"markpathwaybyobjects"
  ,"mergegff"
  ,"mergestringlisttostring"
  ,"minimumorflength"
  ,"moleculetype"
  ,"mpsrchprogram"
  ,"msa"
  ,"mustang"
  ,"namespace"
  ,"ncbidatabase"
  ,"ncbiesearch"
  ,"ndproteinid"
  ,"ndproteinstructure"
  ,"nucid"
  ,"nucleotideorftranslation"
  ,"parametersxml"
  ,"params"
  ,"paramsxml"
  ,"parsemobydatajpegimage"
  ,"parsemobydatasimpleannotatedjpegimage"
  ,"pdbformatcoordinates"
  ,"pdbid"
  ,"phylogenetictree"
  ,"picrinputparams"
  ,"plot"
  ,"poll"
  ,"program"
  ,"proteinid"
  ,"proteinsequences"
  ,"qblastget"
  ,"qblastjobid"
  ,"qblastput"
  ,"query"
  ,"queryseq"
  ,"querysequence"
  ,"rcsbprefix"
  ,"rcsbsuffix"
  ,"regex"
  ,"removeduplicatestrings"
  ,"removefirstcharacter"
  ,"repeatlibrary"
  ,"repeatmasker"
  ,"requestxml"
  ,"run"
  ,"runblastpgp"
  ,"runcensor"
  ,"runclustalw"
  ,"rundalilite"
  ,"runefetchms"
  ,"runesummary"
  ,"runfasta"
  ,"runinterproscan"
  ,"runkalign"
  ,"runmafft"
  ,"runmaxsprout"
  ,"runmpsrch"
  ,"runmuscle"
  ,"runncbiblast"
  ,"runphobius"
  ,"runscanps"
  ,"runtcoffee"
  ,"runwublast"
  ,"searchdatabase"
  ,"searchmode"
  ,"searchparallel"
  ,"searchparam"
  ,"searchsimple"
  ,"selectdatabase"
  ,"selectfastaprogram"
  ,"selectprogram"
  ,"seqret"
  ,"sequence"
  ,"sequencealignment"
  ,"sequencedatabase"
  ,"sequenceorid"
  ,"sequenceoridorgi"
  ,"sequenceorigin"
  ,"sequences"
  ,"sequencetype"
  ,"speciesfilter"
  ,"spitpicrresult"
  ,"splitbynewline"
  ,"splitintosequences"
  ,"splitstringintolist"
  ,"splitstringintostringlistbyregularexpression"
  ,"sss"
  ,"sssprogram"
  ,"statuscheck"
  ,"stproteinid"
  ,"stproteinstructure"
  ,"stringconstant"
  ,"structure"
  ,"structureorid"
  ,"swiss"
  ,"uniparcid"
  ,"uniprotaccession"
  ,"unpackalignment"
  ,"unpackalignmentresult"
  ,"unpackguidetreeresult"
  ,"unpackmaskedsequence"
  ,"unpacknjtree"
  ,"unpackoutput"
  ,"unpackoutputresult"
  ,"unpackphyliptree"
  ,"unpackresult"
  ,"unpacktable"
  ,"unpacktextoutput"
  ,"unpacktextresult"
  ,"unpackxmlresult"
  ,"unwrappicrresult"
  ,"unwrapresult"
  ,"usestructure"
  ,"wsarrayofdataxml"
  ,"xpathfromtext"
  ,"Fastajobparams"
  ,"Fastaprogram"
  ,"Fastastring"
  ,"Fastastringtofastalist"
  ,"Fetchbatch"
  ,"Fetchcompoundimage"
  ,"Fetchcompoundinfo"
  ,"Fetchdata"
  ,"Fetchhits"
  ,"Fetchpage"
  ,"Fetchsequences"
  ,"Findbinaryinteractions"
  ,"Flattenimagelist"
  ,"Flattenlist"
  ,"Format"
  ,"Formatasgff"
  ,"Formatlistfordbfetch"
  ,"object"
  ,"Terms"
  ,"Tmap"
  ,"Tmapsingleseq"
  ,"Translationtable"
  
)


