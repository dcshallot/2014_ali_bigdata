


setwd("D:/github/2014_ali_bigdata")

odata <- read.csv("t_alibaba_data.csv")
names(odata)

summary(odata)


library(sqldf)

sqldf(" select count(distinct(user_id)) from odata")
sqldf(" select count(distinct(brand_id)) from odata")
sqldf(" select count(distinct(visit_datetime)) from odata")


