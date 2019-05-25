

##########Pre-processing:


rm(list=ls())

library(plyr)
# set folder directory
file_path="C:/Users/Administrator/Desktop/assi1/avocado/"
data=read.csv(paste(file_path,"avocado.csv",sep=""))
head(data[,1:10])
ls()
set.seed(110)
dim(data)
data1=data.frame(data)
#########
data1=data1[,-1]
head(data1)
summary(data1)
data1$month=substr(data1$Date,6,7)
data1$day=substr(data1$Date,9,10)
data1=arrange(data1,day)
data1=arrange(data1,month)
data1=arrange(data1,year)
data1$day=as.numeric(data1$day)
data1$month=as.numeric(data1$month)
index_conventional=which(data1$type=="conventional")
length(index_conventional)
data1=data1[index_conventional,]
index_Midsouth=which(data1$region=="Midsouth")
length(index_Midsouth)
data1=data1[index_Midsouth,]
dim(data1)
data1=data1[,-11]#drop type
data1=data1[,-12]#drop region
na_flag = apply(is.na(data1), 2, sum)
na_flag

##########splitting :


x=data.frame(data1[,3:13])
y=data1[,2]
t=0.6*(dim(data1)[1])
v=0.8*(dim(data1)[1])

training_x=x[1:ceiling(t),]
validation_x=x[(ceiling(t)+1):ceiling(v),]
test_x=x[(ceiling(v)+1):dim(data1)[1],]

dim(training_x)
dim(validation_x)
dim(test_x)

training_y=y[1:ceiling(t)]
validation_y=y[(ceiling(t)+1):ceiling(v)]
test_y=y[(ceiling(v)+1):dim(data1)[1]]
length(training_y)
length(validation_y)
length(test_y)

summary(test_y)



###############svm

##########svm
library(e1071)
set.seed(123)


svm_model_1_1=svm(training_x,training_y,kernel = "radial",gamma=1)
svm_predict_1_1=predict(svm_model_1_1,validation_x)
RMSE_svm_1_1= sqrt(mean((validation_y-svm_predict_1_1)^2))
RMSE_svm_1_1


par(mfrow = c(2,2))
plot(1:dim(validation_x)[1],svm_predict_1_1,type="l",ylim=c(0.9,1.73),main="gamma=1")
lines(validation_y,col="orange")

############
svm_model_1_2=svm(training_x,training_y,kernel = "radial",gamma = 10)
svm_predict_1_2=predict(svm_model_1_2,validation_x)
RMSE_svm_1_2= sqrt(mean((validation_y-svm_predict_1_2)^2))
RMSE_svm_1_2


plot(1:dim(validation_x)[1],svm_predict_1_2,type="l",ylim=c(0.9,1.73),main="gamma=10")
lines(validation_y,col="orange")

###########

svm_model_1_3=svm(training_x,training_y,kernel = "radial",gamma=0.1)
svm_predict_1_3=predict(svm_model_1_3,validation_x)
RMSE_svm_1_3= sqrt(mean((validation_y-svm_predict_1_3)^2))
RMSE_svm_1_3


plot(1:dim(validation_x)[1],svm_predict_1_3,type="l",ylim=c(0.9,1.73),main="gamma=0.1")
lines(validation_y,col="orange")

svm_model_1_4=svm(training_x,training_y,kernel = "radial",gamma=0.01)
svm_predict_1_4=predict(svm_model_1_4,validation_x)
RMSE_svm_1_4= sqrt(mean((validation_y-svm_predict_1_4)^2))
RMSE_svm_1_4


plot(1:dim(validation_x)[1],svm_predict_1_4,type="l",ylim=c(0.9,1.73),main="gamma=0.01")
lines(validation_y,col="orange")


svm_predict=predict(svm_model_1_4,test_x)

MSE_svm= mean((test_y-svm_predict)^2)
MSE_svm

RMSE_svm= sqrt(mean((test_y-svm_predict)^2))
RMSE_svm

MAPE_svm= mean(abs(100*(test_y-svm_predict)/test_y))
MAPE_svm

par(mfrow = c(1,2))
plot(1:dim(test_x)[1],svm_predict,type="l",ylim=c(0.9,1.73),main="svm predictive and true value")
lines(test_y,col="orange")
plot(1:dim(test_x)[1],test_y-svm_predict,type="l",main="svm error")

###########linear regression
#########LR
library("corrplot")
a=cor(d)
d=data1[,-1]
d= as.matrix(log(d))

lm_model=lm(training_y~.,training_x)
lm_model
lm_predict=predict(lm_model,validation_x)
summary(lm_model)
#drop two column which have NA
lm_model=lm(training_y~Total.Volume+ X4046 + X4225+ X4770 +
              Small.Bags + Large.Bags + year + month + day ,training_x)
lm_model
lm_predict_1=predict(lm_model,validation_x)
summary(lm_model)

RMSE_lm_1= sqrt(mean((validation_y-lm_predict_1)^2))
RMSE_lm_1

plot(1:dim(validation_x)[1],lm_predict_1,type="l",ylim=c(1.1,1.82),main="lm")
lines(validation_y,col="orange")

lm_predict=predict(lm_model,test_x)
summary(lm_predict)


#R_squar=(sum((lm_predict-mean(test_y))^2))/(sum((test_y-mean(test_y))^2))
#R_square
MSE_lm= mean((test_y-lm_predict)^2)
MSE_lm

RMSE_lm= sqrt(mean((test_y-lm_predict)^2))
RMSE_lm

MAPE_lm= mean(abs(100*(test_y-lm_predict)/test_y))
MAPE_lm

par(mfrow = c(1,2))
plot(1:dim(test_x)[1],lm_predict,type="l",ylim=c(0.9,1.91),main="lm predictive and true value")
lines(test_y,col="orange")
plot(1:dim(test_x)[1],test_y-lm_predict,type="l",main="lm error")
par(mfrow=c(1,1))
plot(1:169,y,type="l")
lines(137:169,lm_predict,type="l",col="red")
lines(103:136,lm_predict_1,type="l",col="blue")


############random forest
##############rf
##########random forest
library(randomForest)
rf_model_1=randomForest(training_x,training_y,mtry=1)
rf_predict_1=predict(rf_model_1,validation_x)
summary(rf_predict_1)
RMSE_rf_1= sqrt(mean((validation_y-rf_predict_1)^2))
RMSE_rf_1
par(mfrow = c(3,2))
plot(1:dim(validation_x)[1],rf_predict_1,type="l",ylim=c(0.9,1.5),main="mtry=1")
lines(validation_y,col="orange")
###########2
rf_model_2=randomForest(training_x,training_y,mtry=2)
rf_predict_2=predict(rf_model_2,validation_x)
summary(rf_predict_2)
RMSE_rf_2= sqrt(mean((validation_y-rf_predict_2)^2))
RMSE_rf_2
plot(1:dim(validation_x)[1],rf_predict_2,type="l",ylim=c(0.9,1.5),main="mtry=2")
lines(validation_y,col="orange")
#############3
rf_model_3=randomForest(training_x,training_y,mtry=3)
rf_predict_3=predict(rf_model_3,validation_x)
summary(rf_predict_3)
RMSE_rf_3= sqrt(mean((validation_y-rf_predict_3)^2))
RMSE_rf_3
plot(1:dim(validation_x)[1],rf_predict_3,type="l",ylim=c(0.9,1.5),main="mtry=3")
lines(validation_y,col="orange")
#############4
rf_model_4=randomForest(training_x,training_y,mtry=4)
rf_predict_4=predict(rf_model_4,validation_x)
summary(rf_predict_4)
RMSE_rf_4= sqrt(mean((validation_y-rf_predict_4)^2))
RMSE_rf_4
plot(1:dim(validation_x)[1],rf_predict_4,type="l",ylim=c(0.9,1.5),main="mtry=4")
lines(validation_y,col="orange")

rf_model_5=randomForest(training_x,training_y,mtry=5)
rf_predict_5=predict(rf_model_5,validation_x)
summary(rf_predict_1)
RMSE_rf_5= sqrt(mean((validation_y-rf_predict_1)^2))
RMSE_rf_5
plot(1:dim(validation_x)[1],rf_predict_5,type="l",ylim=c(0.9,1.5),main="mtry=5")
lines(validation_y,col="orange")


rf_model_6=randomForest(training_x,training_y,mtry=6)
rf_predict_6=predict(rf_model_6,validation_x)
summary(rf_predict_6)
RMSE_rf_6= sqrt(mean((validation_y-rf_predict_6)^2))
RMSE_rf_6
plot(1:dim(validation_x)[1],rf_predict_6,type="l",ylim=c(0.9,1.5),main="mtry=6")
lines(validation_y,col="orange")



##########
rf_predict=predict(rf_model_6,test_x)

MSE_rf= mean((test_y-rf_predict)^2)
MSE_rf

RMSE_rf= sqrt(mean((test_y-rf_predict)^2))
RMSE_rf

MAPE_rf= mean(abs(100*(test_y-rf_predict)/test_y))
MAPE_rf
summary(rf_predict)
par(mfrow = c(1,2))
plot(1:dim(test_x)[1],rf_predict,type="l",ylim=c(1.0,1.82),main="rf predictive and true value")
lines(test_y,col="orange")
plot(1:dim(test_x)[1],test_y-rf_predict,type="l",main="rf error")

#########final figure 
par(mfrow = c(1,1))
plot(1:dim(test_x)[1],test_y,type="l")
lines(rf_predict,col="red")
lines(lm_predict,col="blue")
lines(svm_predict,col="green")
