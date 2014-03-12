
#data import and clean
setwd("D:/github/2014_ali_bigdata_git")

odata <- read.csv("t_alibaba_data.csv")
names(odata)
odata$type <- factor( odata$type, levels=c(0,1,2,3), labels=c("click","buy","store" ,"cart") )

odata$visit_datetime <- gsub( "ÔÂ", "-" ,  odata$visit_datetime )
odata$visit_datetime <- gsub( "ÈÕ", "" ,  odata$visit_datetime  )
odata$visit_datetime <- as.Date(  odata$visit_datetime , "%m-%d" )

summary(odata)

library(sqldf)
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

fit <- aov( buy ~ click* cart , data= active)
summary(fit)
fit <- lm( buy ~ click + click* cart, data= active)
summary(fit)


#0312 what a buy act looks like
library(sqldf)
act1b <- sqldf( "select user_id, brand_id, visit_datetime as bdate from odata where type='buy'
                group by user_id, brand_id having min(visit_datetime) ")
aa <- sqldf(" select user_id, brand_id, min(visit_datetime) as fdate, type as fact 
                from odata group by user_id, brand_id")
act1c <- sqldf( " select a.* , b.fdate, b.fact from act1b as a left join aa as b 
                on a.user_id=b.user_id and a.brand_id=b.brand_id")

act1d <- sqldf( " select a.*, count( b.type) as clikcs 
                from act1c as a left join odata as b 
                on a.user_id=b.user_id and a.brand_id=b.brand_id and a.bdate>=b.visit_datetime and b.type='click'
                group by a.user_id, a.brand_id")

act1e <- sqldf( " select a.*, count( b.type) as stores 
                from act1d as a left join odata as b 
                on a.user_id=b.user_id and a.brand_id=b.brand_id and a.bdate>=b.visit_datetime and b.type='store'
                group by a.user_id, a.brand_id")

act1f <- sqldf( " select a.*, count( b.type) as carts 
                from act1e as a left join odata as b 
                on a.user_id=b.user_id and a.brand_id=b.brand_id and a.bdate>=b.visit_datetime and b.type='cart'
                group by a.user_id, a.brand_id")

act1f$bdate <- as.Date(act1f$bdate,  "1970-01-01" )
act1f$fdate <- as.Date(act1f$fdate,  "1970-01-01" )
summary(act1f)

act2a <- act1f[ act1f$bdate > "2014-04-20" , ] # get out data may incomplete for date reason

act2a$duration <- as.numeric( act2a$bdate- act2a$fdate)

summary( act2a$duration )
hist( act2a$duration,  breaks=50 ,
      xlab="days between look and buy")

