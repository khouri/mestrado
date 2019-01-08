library(ggplot2)

df2 <- data.frame(status = factor(c("incluídos",
                                    "excluídos", 
                                    "duplicados")),
                  
                  time = factor(c("IEEE",  
                                  "IEEE",   
                                  "IEEE"), 
                                
                                levels=c("IEEE")),
                  total_bill = c(20,  
                                 349, 
                                 38))

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
r <- r + scale_y_continuous(breaks=c(0, 20, 38, 349), 
                            labels=c("0","20", "38", "349"))

ggsave(filename="/home/toasty/Dropbox/MestradoAdilson/2semestre2014/Qualificacao/texto/ScriptR/IEEEPLOT.png", dpi = 600)
