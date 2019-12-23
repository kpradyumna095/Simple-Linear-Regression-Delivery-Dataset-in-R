library(readr)
dt_st <-read_csv("E:/data sci/excelr/Assignments/Simple Linear Regression/delivery_time.csv")

View(dt_st) #view dataset

summary(dt_st) #summary of dataset/EDA

#Scatterplot of input Vs otput
plot(dt_st$`Sorting Time`, dt_st$`Delivery Time`)  # plot(X,Y)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#attached dataset
attach(dt_st)

#Correlation between output to input
cor(`Sorting Time`, `Delivery Time`) #cor(x,y)
#from value of correlation coe.(r) we can say that moderate correlation between o/p & i/p

# Simple Linear Regression model
reg <- lm(`Delivery Time`~ `Sorting Time`) # lm(Y ~ X)

#Summary of regression model
summary(reg)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is less than 0.80 so we can say that model is underfit (moderately good).
#We can write eq. as DT=6.5227+1.6490(ST)

#check fitted values(predicted)
reg$fitted.values
reg$residuals

#but we have to check with predicted values
pred <- predict(reg)

#Check for error associated with each obs.
reg$residuals
sum(reg$residuals)

#check for mean of sum of errors is equal to 0.
mean(reg$residuals)
hist(reg$residuals) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(reg$residuals^2)/nrow(dt_st))  #RMSE
sqrt(mean(reg$residuals^2))

#interval for 5% of confidence
confint(reg,level=0.95)

predict(reg,interval="predict")

#visualising model
library(ggplot2)

ggplot(data = dt_st, aes(x = `Sorting Time`, y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`, y=pred))

#Inferences-
#From all above  value or correlatio coe.r is 0.82 which is moderateely acceptable ,
# function is linear in nature i.e. lm(DT ~ ST), 
# Coe. are significant and coe.of Determination value (R^2) is 0.682 which is also moderately acceptable
# mean of errors is 4.758099e-17 which is almost 0 ans errors are almost normally distributed.
# RMSE value is 2.79165 which is nearest to lower range value of delivery time
# so as model underfits,we need to go for transformation



# Logarithamic Model

# x = log(Sorting Time); y = Delivery Time

#Scatterplot of input Vs output
plot(log(`Sorting Time`), `Delivery Time`)   # plot(log(X),Y)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor(log(`Sorting Time`), `Delivery Time`)

# Simple Linear Regression model-log transform
reg_log <- lm(`Delivery Time` ~ log(`Sorting Time`))   # lm(Y ~ log(X)

#Summary of regression model
summary(reg_log)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is less than 0.80 so we can say that model is moderate and improved than previous
#We can write eq. as DT=1.160+9.043(log(ST))

#check fitted values(predicted)
reg_log$fitted.values
reg_log$residuals

#but we have to check with predicted values
predict(reg_log)

#Check for error associated with each obs.
reg_log$residuals
sum(reg_log$residuals)

#check for mean of sum of errors is equal to 0.
mean(reg_log$residuals)
hist(reg_log$residuals) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(reg_log$residuals^2)/nrow(dt_st))  #RMSE
sqrt(mean(reg_log$residuals^2))

#interval for 5% of confidence
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

#visualising model
library(ggplot2)

ggplot(data = dt_st, aes(x = log(`Sorting Time`), y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=log(`Sorting Time`), y=pred))

#Inferences-
#From all above  value or correlation coe.(r) is 0.83 which is moderateely acceptable improved than previuos ,
# function is linear in nature i.e. lm(DT ~ log(ST)), 
# Coe. are significant and coe.of Determination value (R^2) is 0.695 which is also moderately acceptable and improved than previous.
# mean of errors is -1.863589e-16 which is almost 0 ans errors are almost normally distributed.
# RMSE value is 2.73 decreased slightly than previous model which is nearest to lower range value of delivery time
# so as model underfits,we need to go for another transformation to improve (r^2) value.




# Exponential Model

# x = Sorting Time and y = log(Delivery Time)

#Scatterplot of input Vs output
plot(`Sorting Time`, log(`Delivery Time`)) #plot(x,log(y))
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor(`Sorting Time`, log(`Delivery Time`)) #cor(x,log(y))
#from value of correlation coe.(r) we can say that very good correlation between o/p & i/p

# Simple Linear Regression model-exp transform
reg_exp <- lm(log(`Delivery Time`) ~ `Sorting Time` )  #lm(log(Y) ~ X)

#Summary of regression model
summary(reg_exp)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is slight less than 0.80 best fit so moderate value  so we can say that model is somewhat bestfit as of now
#We can write eq. as log(DT)=2.12+0.1055(ST)

#check fitted values(predicted)
reg_exp$fitted.values
reg_exp$residuals

#but we have to check with predicted values
predict(reg_exp)

#convert exp values to normal
logdt <- predict(reg_exp)
dt <- exp(logdt)

#Check for error associated with each obs.
error = dt_st$`Delivery Time` - dt
error
sum(error)

#check for mean of sum of errors is equal to 0.
mean(error)
hist(error) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(error^2)/nrow(dt_st))  #RMSE
sqrt(mean(error^2))

#interval for 5% of confidence
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

#visualising model
library(ggplot2)

ggplot(data = dt_st, aes(x = `Sorting Time`, y = log(`Delivery Time`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`, y=pred))



#Inferences-
#From all above  value or correlation coe.(r) is 0.843 which is good and improved than previuos ,
# function is linear in nature i.e. lm(log(DT) ~ ST), 
# Coe. are significant and coe.of Determination value (R^2) is 0.7109 which is also moderately acceptable and improved than previous.
# mean of errors is 0.1981094 which is almost 0 ans errors are normally distributed.
# RMSE value is 2.94025 increased slightly than previous model which is nearest to lower range value of delivery time
# so as model underfits,we need to go for another transformation to improve (r^2) value.




# Polynomial model with 2 degree (quadratic model)

# x = Sorting Time and y = `Sorting Time`+ I(`Sorting Time`*`Sorting Time`)

#Scatterplot of input Vs output
plot(`Sorting Time`, `Delivery Time`)
plot(`Sorting Time`*`Sorting Time`, `Delivery Time`)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor(`Sorting Time`, `Delivery Time`)
cor(`Sorting Time`*`Sorting Time`,(`Delivery Time`))
#from value of correlation coe.(r) we can say that moderately acceptable correlation between o/p & i/p


# Simple Linear Regression model-polynomial with 2nd degree transform# lm(Y ~ X + I(X*X))

reg2degree <- lm((`Delivery Time`) ~ `Sorting Time` + I(`Delivery Time`*`Delivery Time`))

#Summary of regression model
summary(reg2degree)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is very good as it is more than 0.80 so we can say that model is bestfit as of now
#We can write eq. as DT=7.47+0.31*ST+I(0.23*ST*ST)

#check fitted values(predicted)
reg2degree$fitted.values
reg2degree$residuals

#but we have to check with predicted values
predict(reg2degree)

#Check for error associated with each obs.
reg2degree$residuals
sum(reg2degree$residuals)

#check for mean of sum of errors is equal to 0.
mean(reg2degree$residuals)
hist(reg2degree$residuals) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum((reg2degree$residuals)^2)/nrow(dt_st))  #RMSE
sqrt(mean(reg2degree$residuals^2))

#interval for 5% of confidence
confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization

ggplot(data = dt_st, aes(x = `Sorting Time` + I((`Sorting Time`)^2), y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`+I(`Sorting Time`^2), y=pred))



#Inferences-
#From all above  value or correlation coe.(r) is 0.82 & 0.79 resp as polynomial which is moderate and decreased than previuos ,
# function is linear in nature i.e. lm((DT) ~ ST+I(ST*ST), 
# Coe. are significant and coe.of Determination value (R^2) is 0.97 which is very good and improved than previous.
# mean of errors is -5.266125e-18 which is almost 0 ans errors are normally distributed.
# RMSE value is  0.7902562 increased slightly than previous model which is nearest to lower range value of delivery time
# so as model best fits,we need to go with this transformation although correlation coe.(r) value is slightly less than best fit.


