# Title     : Draw Evolotuion of RL Critic Soft Max
# Objective : Draw Evolotuion of RL Critic Soft Max
# Created by: vmchura
# Created on: 3/13/20
library(ggplot2)

drawEvolutionRL <- function(a,b,title,xlabel,ylabel,result){
  df <- data.frame(x = a, y = b)


  p <- ggplot(df, aes(x, y)) + geom_smooth(span = 100)

  h3 <- p  + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + theme(plot.title = element_text(hjust = 0.5))
  h3
  ggsave(result)

}

