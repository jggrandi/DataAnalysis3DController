#commands:

`1.playerLog2_test` = `1.playerLog2_test`[-c(21), ] #remove linha

p2 <- ggplot(`1.playerLog2_test`,aes(ymin=0,xmax=164)) # arruma os eixos

p22 <- p2 + geom_rect(aes(xmin = Start, xmax = Finish, ymax = 1, fill = Action,colour = factor(Action))) # monta o gráfico

`1.playerLog2_test` = `1.playerLog2_test`[,c(1,2,5,4,3)] # reordena as colunas

colnames(`1.playerLog2_test`) <- c("Task","Action","Duration","Finish","Start") # renomeia as colunas

`1.playerLog2_test`$Start = `1.playerLog2_test`$V4- `1.playerLog2_test`$V3 # operações entre as colunas

library(cowplot) # para plotar gráficos 


# função que pega a legenda para ser usada pelos 
> get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p1) # pega a legenda do plot1
p1 <- p1 + theme(legend.position="none") # remove a legenda do plot1

p22 <- p2 + geom_rect(aes(xmin = Start, xmax = Finish, ymax = 1, fill = Action)) + theme(legend.position="none") # cria o segundo gráfico sem legenda

grid.arrange(p1,legend,p22,ncol=2,nrow=2,layout_matrix = rbind(c(1,2),c(3,2)),widths = c(2.5,0.2),heights=c(2.7,2.7)) # coloca gráficos um embaixo do outro com a legenda centralizada na direita