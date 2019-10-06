library(ggplot2)

drawAndSaveHist <- function(a,title,xlabel,minX,maxX,binwidth,result){
    qplot(a,
    geom="histogram",
    binwidth = binwidth,
    main = title,
    xlab = xlabel,
    fill=I("blue"),
    col=I("red"),
    alpha=I(.2),
    xlim=c(minX,maxX))  + theme(plot.title = element_text(hjust = 0.5))
    ggsave(result)

}

