#install.packages("ggplot2")
#install.packages("DataCombine")
#install.packages("plotly")

require(plotly)
require(ggplot2)
require(DataCombine)

com<-read.csv(file.choose())

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
    td<-com[com$ASItemNo==items[i],]
    cor0<-cor(td$stdcom,td$stitm,use="pairwise.complete.obs")
    com$cor0[com$ASItemNo==items[i]]<-cor0
    
    cor1<-cor(td$stdcom1,td$stitm,use="pairwise.complete.obs")
    com$cor1[com$ASItemNo==items[i]]<-cor1
    
    cor2<-cor(td$stdcom2,td$stitm,use="pairwise.complete.obs")
    com$cor2[com$ASItemNo==items[i]]<-cor2
    
    cor3<-cor(td$stdcom3,td$stitm,use="pairwise.complete.obs")
    com$cor3[com$ASItemNo==items[i]]<-cor3
  }
  
ggplot() + 
  geom_line(data = com, aes(y = stdcom, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  +
  ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price")

ggplot() + 
  geom_line(data = com, aes(y = stdcom1, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  +
  ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price (Lagged by 1 month)")


ggplot() + 
  geom_line(data = com, aes(y = stdcom2, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  +
  ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price (Lagged by 2 months)")

ggplot() + 
  geom_line(data = com, aes(y = stdcom3, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  +
  ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price (Lagged by 3 months)")
