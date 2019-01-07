library(ggplot2)

df2 <- data.frame(status = factor(c("incluídos",
                                    "excluídos", 
                                    "duplicados")),
                  
                  time = factor(c("Science Direct",  
                                  "Science Direct",   
                                  "Science Direct"), 
                                
                                levels=c("Science Direct")),
                  total_bill = c(15,  
                                 130, 
                                 0))

# Very basic bar graph
r <- ggplot(data=df2, aes(x=time, y=total_bill, fill=status)) +
  theme(text = element_text(size=20))+
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#FFFF33", "#FF0033", "#00FF33")
                    
                    #      + xlab("New x label")
                    #       + ylab("New y label")
                    
  )
r <- r + xlab("Classificação dos artigos")
r <- r + ylab("Número de artigos")
r <- r + scale_y_continuous(breaks=c(0, 15, 130), 
                            labels=c("0","15", "130"))


#385, 401
#Salva grafico
ggsave(filename="/home/toasty/Dropbox/MestradoAdilson/2semestre2014/Qualificacao/texto/ScriptR/ScienceDirect.png", dpi = 600)
#print(r)



#     Documentacao sobre graficos em ggplot2
#     http://docs.ggplot2.org/current/ggsave.html
#     http://stackoverflow.com/questions/13297995/changing-font-size-and-direction-of-axes-text-in-ggplot2
#     https://rstudio-pubs-static.s3.amazonaws.com/3364_d1a578f521174152b46b19d0c83cbe7e.html
#     http://www.cookbook-r.com/Graphs/Axes_%28ggplot2%29/