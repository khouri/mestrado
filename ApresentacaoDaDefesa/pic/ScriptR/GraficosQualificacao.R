# Load the ggplot2 library
library(ggplot2)

df2 <- data.frame(status = factor(c("incluídos", "incluídos", "incluídos",
                                    "excluídos", "excluídos", "excluídos",
                                    "duplicados", "duplicados", "duplicados" )),

                  time = factor(c("IEEE", "ACMDL", "Science Direct", 
                                  "IEEE", "ACMDL", "Science Direct",  
                                  "IEEE", "ACMDL", "Science Direct" ), 

                  levels=c("IEEE","ACMDL", "Science Direct")),
                  total_bill = c(21, 13, 16, 
                                 348, 190, 129, 
                                 38, 38, 0 ))

# Very basic bar graph
r <- ggplot(data=df2, aes(x=time, y=total_bill, fill=status)) + 
            geom_bar(stat="identity", position=position_dodge(), colour="black") +
            scale_fill_manual(values=c("#FFFF33", "#FF0033", "#00FF33")
                              
                              #      + xlab("New x label")
                      #       + ylab("New y label")
                             
           )
r <- r + xlab("Base de dados")
r <- r + ylab("Número de artigos")
r <- r + scale_y_continuous(breaks=c(13, 16, 8, 21, 38, 0, 348, 190, 129), 
                           labels=c("13", "16", "8", "21", "38", "0", "348", "190", "129"))

print(r)