# hre<-read.csv("D:/My Stuff/Hospital Readmission/SingHealth.csv")
# names(hre)
# hre<-hre[,-c(1,4,5,12,13,14,19,26:34,37:45)]
# names(hre)
# hre<-hre[,-c(19,20,21,23,24)]
# write.csv(hre,file="D:/My Stuff/Hospital Readmission/hre1.csv")

library(plyr)
library(arules)
hre<-read.csv("D:/My Stuff/Hospital Readmission/hre1.csv")
hre[hre=="NULL"] <- NA
hre[hre==""] <- NA


txn = read.transactions(file="D:/My Stuff/Hospital Readmission/hre1.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1,quote = "");
basket_rules <- apriori(txn,parameter = list(sup = 0.5, conf = 0.5,target="rules"));

library(tm)

if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}

df_basket <- as(basket_rules,"data.frame")
View(df_basket)

#inspect(basket_rules)
library(zoom)
library(plyr)
library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))




