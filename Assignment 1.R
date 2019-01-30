# Read in Data:
ms.data <- read.csv("ManSalary.csv")
names(ms.data)

# Question A:
# Is there are relationship between these two variables?
plot(ms.data$Salary~ms.data$Experience, main = "Model")

# Question B:
fit <- lm(Salary~Experience, data = ms.data)
summary(fit)

# Question C Diagnostic Checks
plot(fit$res~fit$fitted, main = "Diagnostic Check Model 1")
plot(fit$res~ms.data$Experience, main = "Diagnostic Check Model 2")
hist(fit$res, main = "Diagnostic Check Model Histogram",
     col = c("blue"))
qqnorm(fit$res)
shapiro.test(fit$res)

# Question D: Test hypothesis that the slope of the regression line equal 0 vs
# the alternative that the slope of the regression line not equal 0
# regression model
confint(fit)
summary(fit)


# Question E: What is the value of R-Squared?
summary(fit)
0.7869 * 100

# Question F:
newdata <- ms.data$Salary
pred <- predict(fit, ms.data, interval = "prediction")
# Leaving it as "prediction" will automatically set to 95% confidence.
pred
