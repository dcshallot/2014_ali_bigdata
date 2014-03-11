

setwd("D:/github/2014_ali_bigdata")

odata <- read.csv("t_alibaba_data.csv")
names(odata)
odata$type <- factor( odata$type, levels=c(0,1,2,3), labels=c("click","buy","store" ,"cart") )

odata$visit_datetime <- gsub( "月", "-" ,  odata$visit_datetime )
odata$visit_datetime <- gsub( "日", "" ,  odata$visit_datetime  )
odata$visit_datetime <- as.Date(  odata$visit_datetime , "%m-%d" )

summary(odata)

sqldf(" select count(distinct(user_id)) from odata")
sqldf(" select count(distinct(brand_id)) from odata")
sqldf(" select count(distinct(visit_datetime)) from odata")

#0311 buy vs click vs store vs cart
library(reshape2)
active <- dcast( odata, user_id~type, fun=length)
pairs( ~ buy + click + store +cart, data=active)

cor( active[,-1])
cor.test( active$buy, active$click)

summary( active$buy[active$buy!=0] )
boxplot( active$buy[active$buy!=0] )

active <- within( active, {
  user_g[ buy ==0 ] <- 0
  user_g[ buy >0 & buy <= 3] <- 1
  user_g[ buy >3 & buy <= 7] <- 2
  user_g[ buy >7 & buy <= 12] <- 3
  user_g[ buy >12 ] <- 4 })
active$user_g <- factor( active$user_g , labels= c("0","1-3","3-7","7-12","12+"))

barplot( table(active$user_g), ylim=c(0,300) , main= "grouped buyer" , 
          xlab=" items buy", ylab="n of user")
grid()

boxplot( active$click ~ active$user_g, xlab="buyer group" , ylab="times of click")
boxplot( active$store ~ active$user_g, xlab="buyer group" , ylab="times of store")
boxplot( active$cart ~ active$user_g, xlab="buyer group" , ylab="times of cart")
###

fit <- aov( buy ~ click* cart , data= active)
summary(fit)


fit <- lm( buy ~ click + click* cart, data= active)
summary(fit)






library(arules)

dcast(test, user_id )

paste(test[,2] , collapse = ",")

??transpos

rules <- apriori( Groceries, parameter = list( support =0.01, confidence=0.2))
inspect( sort(rules, by = "support") [1:6]) #按支持度查看钱六条规则
inspect( sort(rules, by = "confidence")[1:6]) #按置信度

sub.rules <- subset( rules, subset = rhs %in% "whole milk" & lift > 1.2) #规则筛选，rhs=rigth hand side

sets <- eclat( Groceries, parameter = list(support = 0.05, maxlen =10)) #eclat算法求频繁项

itemFrequencyPlot( Groceries, support = 0.05, cex.names = 0.8) # 画图

df.rules <- as(rules, "data.frame") #规则导出
