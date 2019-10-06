# Title     : drawAndSaveHist2D
# Objective : Draw Histogram 2D and save it as file.png
# Created by: vmchura
# Created on: 10/6/19


drawAndSaveHist2D <- function(a,b,title,xlabel,ylabel,result){
    df <- data.frame(x = a, y = b)


    p <- ggplot(df, aes(x,y))
    h3 <- p + stat_bin2d() + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + theme(plot.title = element_text(hjust = 0.5))
    h3
    ggsave(result)

}