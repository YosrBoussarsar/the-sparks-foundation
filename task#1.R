# Import data
stdata <- read.csv("C:/Users/lenovo/Documents/data.csv",  sep=",")
stdata

# Linear Regression and Plot
summary(stdata)
model <- lm(Scores ~ Hours, data= stdata)
beta1 <- coef(model)[[1]]
beta2 <- coef(model)[[2]]
summary(model)
plot(stdata$Hours,stdata$Scores,xlab="Nb. hours studied",ylab="Score obtained",main="Students' scores vs. hours studied",col="blue")
abline(beta1,beta2,col="blue")

# Predict scores for the new hours studied values
predicted_scores <- predict(model)
predicteddata <- data.frame(stdata$Hours,predicted_scores)
colnames(predicteddata)[which(colnames(predicteddata) == "stdata.Hours")] <- "Hours"
predicteddata
plot(predicteddata$Hours, predicteddata$predicted_scores, xlab = "Nb. hours studied", ylab = "Predicted scores", main = "Predicted Scores",col="red")

# Predict score for the value of hours studied (9.25)
hr <- data.frame(Hours = 9.25)
predict(model, newdata = hr)

# Root Mean Squared Error (RMSE) to measure the average deviation of the predicted values from the actual values
rmse <- sqrt(mean((predict(model) - stdata$Scores)^2))
print(rmse)

# Mean Absolute Error (MAE) to measure the avg absolute difference between the predicted values and the actual values.
mae <- mean(abs(predict(model) - stdata$Scores))
print(mae)
