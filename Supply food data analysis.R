Fat_Supply_Quantity_Data = read.csv("C:/Users/ANJLEEN TIRKEY/Documents/Interim Semester 2022-2023/NASSCOM2001/Class Activity/Fat_Supply_Quantity_Data.csv")

head(Fat_Supply_Quantity_Data)
str(Fat_Supply_Quantity_Data)

sum(is.na(Fat_Supply_Quantity_Data))
Fat_Supply_Quantity_Data[is.na(Fat_Supply_Quantity_Data)] = 0

head(Fat_Supply_Quantity)

nrow(Fat_Supply_Quantity)
ncol(Fat_Supply_Quantity)

#building training and testing model
set.seed(1234)
ind<-sample(2, nrow(Fat_Supply_Quantity_Data),replace=T,prob=c(0.8,0.2))
train<-Fat_Supply_Quantity_Data[ind==1,]
test<-Fat_Supply_Quantity_Data[ind==2,]
nrow(train)
nrow(test)

#model building
Model1=lm(Recovered~Alcoholic.Beverages+Animal.Products+Animal.fats+Aquatic.Products..Other+Meat+Milk...Excluding.Butter+Vegetal.Products+Vegetable.Oils,data=train) #Model1
Model1

Model2=lm(Recovered~Alcoholic.Beverages+Animal.fats+Vegetable.Oils,data=train)
Model2

Model3=lm(Recovered~Animal.fats, data=train)
Model3

#model summary
summary(Model1)
summary(Model2)
summary(Model3)

#coefficient of models(slope and intercept)
coeff=coefficients(Model1)
coeff

coeff=coefficients(Model2)
coeff

coeff=coefficients(Model3)
coeff

#correlation between dependent and independent variables in all models
cor_Model1 = cor(train$Recovered,train$Alcoholic.Beverages+train$Animal.Products+train$Animal.fats+train$Aquatic.Products..Other+train$Meat+train$Milk...Excluding.Butter+train$Vegetal.Products+train$Vegetal.Products+train$Vegetable.Oils)
cor_Model1
cor_Model2 = cor(train$Recovered,train$Alcoholic.Beverages+train$Animal.fats+train$Vegetable.Oils)
cor_Model2
cor_Model3 = cor(train$Recovered,train$Animal.fats)
cor_Model3

#predicting using training model
Model1_Predict=predict(Model1,train)
Model1_Predict
Model2_Predict=predict(Model2,train)
Model2_Predict
Model3_Predict=predict(Model3,train)
Model3_Predict

#predicting using testing model
Model1_Predict_test=predict(Model1,test)
Model1_Predict_test
Model2_Predict_test=predict(Model2,test)
Model2_Predict_test
Model3_Predict_test=predict(Model3,test)
Model3_Predict_test

#residuals in models
Model1_residuals=residuals(Model1)
Model1_residuals
Model2_residuals=residuals(Model2)
Model2_residuals
Model3_residuals=residuals(Model3)
Model3_residuals

library(ie2misc)
library(Metrics)

#plotting residual vs predicted plot
plot(Model1_residuals~Model1_Predict,main="Residual VS Predicted Plot",col="red")
plot(Model2_residuals~Model2_Predict,main="Residual VS Predicted Plot",col="purple")
plot(Model3_residuals~Model3_Predict,main="Residual VS Predicted Plot",col="pink")

#mse and rmse for model1
mse(train$Recovered,Model1_Predict)
mse(test$Recovered,Model1_Predict_test)
rmse(train$Recovered,Model1_Predict)
rmse(test$Recovered,Model1_Predict_test)

#mse and rmse for model2
mse(train$Recovered,Model2_Predict)
mse(test$Recovered,Model2_Predict_test)
rmse(train$Recovered,Model2_Predict)
rmse(test$Recovered,Model2_Predict_test)

#mse and rmse for model3
mse(train$Recovered,Model3_Predict)
mse(test$Recovered,Model3_Predict_test)
rmse(train$Recovered,Model3_Predict)
rmse(test$Recovered,Model3_Predict_test)

#AIC and BIC
AIC(Model1)
AIC(Model2)
AIC(Model3)

BIC(Model1)
BIC(Model2)
BIC(Model3)

#mae and mape for model1
mae(train$Recovered,Model1_Predict)
mae(test$Recovered,Model1_Predict_test)
mape(train$Recovered,Model1_Predict)
mape(test$Recovered,Model1_Predict_test)

#mae and mape for model2
mae(train$Recovered,Model2_Predict)
mae(test$Recovered,Model2_Predict_test)
mape(train$Recovered,Model2_Predict)
mape(test$Recovered,Model2_Predict_test)

#mae and mape for model3
mae(train$Recovered,Model3_Predict)
mae(test$Recovered,Model3_Predict_test)
mape(train$Recovered,Model3_Predict)
mape(test$Recovered,Model3_Predict_test)

library(ggplot2)

#scatter plot
ggplot(Fat_Supply_Quantity_Data,aes(Animal.fats,Recovered))+geom_point()+geom_smooth()
ggplot(Fat_Supply_Quantity_Data,aes(Alcoholic.Beverages+Animal.fats+Vegetable.Oils,Recovered))+geom_point()+geom_smooth()
ggplot(Fat_Supply_Quantity_Data,aes(Alcoholic.Beverages+Animal.Products+Animal.fats+Aquatic.Products..Other+Meat+Milk...Excluding.Butter+Vegetal.Products+Vegetable.Oils,Recovered))+geom_point()+geom_smooth()

#boxplot
boxplot(Fat_Supply_Quantity_Data$Animal.fats,xlab="Animal.fats",col = "#DC143C")
boxplot(Fat_Supply_Quantity_Data$Alcoholic.Beverages,xlab="Alcoholic.Beverages",col="#00FA9A")
boxplot(Fat_Supply_Quantity_Data$Animal.Products,xlab="Animal.Products",col="#20B2AA")
boxplot(Fat_Supply_Quantity_Data$Aquatic.Products..Other,xlab="Aquatic.Products..Other",col="#8A2BE2")
boxplot(Fat_Supply_Quantity_Data$Meat,xlab="Meat",col="#4B0082")
boxplot(Fat_Supply_Quantity_Data$Milk...Excluding.Butter,xlab="Milk...Excluding.Butter",col="#C71585")
boxplot(Fat_Supply_Quantity_Data$Vegetal.Products,xlab="Vegetal.Products",col="#B0C4DE")
boxplot(Fat_Supply_Quantity_Data$Vegetable.Oils,xlab="Vegetable.Oils",col="#BC8F8F")
