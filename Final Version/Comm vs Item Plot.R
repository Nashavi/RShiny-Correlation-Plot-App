#install.packages("ggplot2")
#install.packages("DataCombine")

  require(ggplot2)
  require(DataCombine)

  com<-read.csv(file.choose())

#Convert monthdate to date and the correct format
  com$MonthDate<-as.Date(com$MonthDate, "%m/%d/%Y")

  #Actual Commodity Price vs actual item Price 
ggplot() + 
  geom_line(data = com, aes(y = CommodityPrice, x = MonthDate, color = "Commodity Price")) +
  geom_line(data = com, aes(y = ItemPrice, x = MonthDate, color = "Item Price"))  +  ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo,scales = "free")+ggtitle("Actual Commodity Prices vs Actual Item Price")
  
items<-as.factor(unique(com$ASItemNo))

# Create normalized values of CP and IP
for (i in 1:length(items)){
  td<-com[com$ASItemNo==items[i],]
  stdcom<-(td$CommodityPrice-mean(td$CommodityPrice))/sd(td$CommodityPrice)
  com$stdcom[com$ASItemNo==items[i]]<-stdcom
  
  stditm<-(td$ItemPrice-mean(td$ItemPrice))/sd(td$ItemPrice)
  com$stitm[com$ASItemNo==items[i]]<-stditm
}


# Slide the commodity prices by 1,2,3 months
com<-slide(data=com, Var = 'stdcom', NewVar = 'stdcom1', GroupVar = 'ASItemNo', slideBy = -1)

com<-slide(data=com, Var = 'stdcom1', NewVar = 'stdcom2', GroupVar = 'ASItemNo', slideBy = -1)

com<-slide(data=com, Var = 'stdcom2', NewVar = 'stdcom3', GroupVar = 'ASItemNo', slideBy = -1)



#Calculate normal and lagged correlations for each AS Item No
  cor0<-cor1<-cor2<-cor3<-NULL
  for (j in 1:length(items)){
    td<-com[com$ASItemNo==items[j],]
    cor0[j]<-cor(td$stdcom,td$stitm,use="pairwise.complete.obs")
    com$cor0[com$ASItemNo==items[j]]<-cor0[j]
    
    cor1[j]<-cor(td$stdcom1,td$stitm,use="pairwise.complete.obs")
    com$cor1[com$ASItemNo==items[j]]<-cor1[j]
    
    cor2[j]<-cor(td$stdcom2,td$stitm,use="pairwise.complete.obs")
    com$cor2[com$ASItemNo==items[j]]<-cor2[j]
    
    cor3[j]<-cor(td$stdcom3,td$stitm,use="pairwise.complete.obs")
    com$cor3[com$ASItemNo==items[j]]<-cor3[j]
  }

  

#Correlation Data Frame 
  cordf<-data.frame(items,round(cor0,2),round(cor1,2),round(cor2,2),round(cor3,2))
  names(cordf)<-c("AS Item No","R Sqr","R Sqr 1 Lag","R Sqr 2 Lag","R Sqr 3 Lag")

  
  
# Plot std values - No lag plot
  ggplot() + 
    geom_line(data = com, aes(y = stdcom, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  +  ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price")
  
  
# Plot std values - 1 month lag plot  
  ggplot() + 
    geom_line(data = com, aes(y = stdcom1, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  + ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price (Lag by 1 month)")
  
  
# Plot std values - 2 months lag plot    
  ggplot() + 
    geom_line(data = com, aes(y = stdcom2, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  + ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price (Lag by 2 months)")

  
# Plot std values - 3 months lag plot    
  ggplot() + 
    geom_line(data = com, aes(y = stdcom3, x = MonthDate, color = "Commodity Price")) +geom_line(data = com, aes(y = stitm, x = MonthDate, color = "Item Price"))  + ylab('Price') + xlab('MonthDate')+facet_wrap(~ASItemNo)+ggtitle("Plot of Item Price vs Commodity Price (Lag by 3 months)")
  
  
#Regression of commodity Price as a factor of item Price  
  ggplot(data = com, aes(y = ItemPrice, x = CommodityPrice)) + geom_line(color="red")+geom_smooth(data = com, aes(y = ItemPrice, x = CommodityPrice)) + xlab("Commodity Price")+ylab("Item Price")+facet_wrap(~ASItemNo,scales = "free")+ggtitle("Reg of Item Price on Commodity Price")
  
