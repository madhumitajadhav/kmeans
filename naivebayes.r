hre<-read.csv("D:/My Stuff/Hospital Readmission/SingHealth.csv")
names(hre)
hre<-hre[,-c(1,4,5,8,9,12,13,14,17,19,22,24,26:35,37:46,48:49)]
names(hre)

library(e1071)

model <- naiveBayes(hre$Add_Status ~ ., data = hre)
predict(model, hre[1:10,-1])
predict(model, hre[1:10,-1], type = "raw")

pred <- predict(model, hre[,-1])
table(pred, hre$Add_Status)
