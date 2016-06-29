#install.packages("ggplot2")
#install.packages("DataCombine")
#install.packages("plotly")
#install.packages("cowplot")

require(plotly)
require(ggplot2)
require(DataCombine)
require(cowplot)
require(grid)

com = read.csv("~/Desktop/Comm.csv")

items<-as.factor(unique(com$ASItemNo))

for (i in 1:length(items)){
  td<-com[com$ASItemNo==items[i],]
  stdcom<-(td$CommodityPrice-mean(td$CommodityPrice))/sd(td$CommodityPrice)
  com$stdcom[com$ASItemNo==items[i]]<-stdcom
  
  stditm<-(td$ItemPrice-mean(td$ItemPrice))/sd(td$ItemPrice)
  com$stitm[com$ASItemNo==items[i]]<-stditm
}

com$MonthDate<-as.Date(com$MonthDate, "%m/%d/%Y")

com<-slide(data=com, Var = 'stdcom', NewVar = 'stdcom1', GroupVar = 'ASItemNo', slideBy = -1)

com<-slide(data=com, Var = 'stdcom1', NewVar = 'stdcom2', GroupVar = 'ASItemNo', slideBy = -1)

com<-slide(data=com, Var = 'stdcom2', NewVar = 'stdcom3', GroupVar = 'ASItemNo', slideBy = -1)

for (j in 1:length(items)){
  td<-com[com$ASItemNo==items[j],]
  cor0<-cor(td$stdcom,td$stitm,use="pairwise.complete.obs")
  com$cor0[com$ASItemNo==items[j]]<-cor0
  
  cor1<-cor(td$stdcom1,td$stitm,use="pairwise.complete.obs")
  com$cor1[com$ASItemNo==items[j]]<-cor1
  
  cor2<-cor(td$stdcom2,td$stitm,use="pairwise.complete.obs")
  com$cor2[com$ASItemNo==items[j]]<-cor2
  
  cor3<-cor(td$stdcom3,td$stitm,use="pairwise.complete.obs")
  com$cor3[com$ASItemNo==items[j]]<-cor3
}

for (k in 1:length(items)){
  
tmpdf<-com[com$ASItemNo==items[k],]

g0<-ggplot() + 
  geom_line(data = tmpdf, aes(y = stdcom, x = MonthDate, color = "Commodity Price")) +geom_line(data = tmpdf, aes(y = stitm, x = MonthDate, color = "Item Price",text = paste("R2 value:",round(cor0*100,2),"%")))  +
  ylab('Price') + xlab('MonthDate')+ggtitle("Plot of Item Price vs Commodity Price")

g1<-ggplot() + 
  geom_line(data = tmpdf, aes(y = stdcom1, x = MonthDate, color = "Commodity Price")) +geom_line(data = tmpdf, aes(y = stitm, x = MonthDate, color = "Item Price",text = paste("R2 value:",round(cor1*100,2),"%")))  +
  ylab('Price') + xlab('MonthDate')+ggtitle("Plot of Item Price vs Commodity Price lagged by 1 month")

g2<-ggplot() + 
  geom_line(data = tmpdf, aes(y = stdcom2, x = MonthDate, color = "Commodity Price")) +geom_line(data = tmpdf, aes(y = stitm, x = MonthDate, color = "Item Price",text = paste("R2 value:",round(cor2*100,2),"%")))  +
  ylab('Price') + xlab('MonthDate')+ggtitle("Plot of Item Price vs Commodity Price lagged by 2 months")

g3<-ggplot() + 
  geom_line(data = tmpdf, aes(y = stdcom3, x = MonthDate, color = "Commodity Price")) +geom_line(data = tmpdf, aes(y = stitm, x = MonthDate, color = "Item Price",text = paste("R2 value:",round(cor3*100,2),"%")))  +
  ylab('Price') + xlab('MonthDate')+ggtitle("Plot of Item Price vs Commodity Price lagged by 3 months")

print(plot_grid(g0,g1,g2,g3, align='h'))
}


