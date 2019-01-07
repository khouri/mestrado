library(arules)


#arquivo <- file.choose()
#print(arquivo) 
#dados <- read.csv("/home/toasty/Desktop/workflowsBioinformatica.csv", header=TRUE)
#[1] "/home/toasty/Desktop/workflowsBioinformatica.csv"
  
rules <- apriori(read.csv("/home/toasty/Desktop/workflowsBioinformatica.csv", header=TRUE), 
                 parameter = list(minlen = 1, maxlen = 3, supp = 0.05, conf = 0.05, target = "rules"))



#print(summary(rules))
#as(rules, "data.frame");


write.csv(file="/home/toasty/Desktop/sup005_conf005_regrasApriori.csv", x=as(rules, "data.frame"))

closeAllConnections()#Limpa memoria
rm(list=ls())
gc()  