# Question A: Read in Data:
gas.data <- read.csv("gasconsumption.csv")
names(gas.data)
str(gas.data)
summary(gas.data)

# Plot Scatter Plots of GPM vs all predictors:
plot(gas.data$GPM~., data = gas.data, main = "Plot 1, GPM vs All Predictors")

# Question 2B: Create Multiple Regression Model:
fit <- lm(GPM~ WT + DIS + NC, data = gas.data)
summary(fit)

# Question 2C:
summary(fit)

# Question 2D: Diagnostic Check for Model:
plot(fit$res~fit$fitted, main = "Diagnostic Check Plot 1")
plot(fit$res~gas.data$WT, main = "Diagnostic Check Plot 2")
plot(fit$res~gas.data$DIS, main = "Diagnostic Check Plot 3")
plot(fit$res~gas.data$NC, main = "Diagnostic Check Plot 4")
hist(fit$res, main = "Diagnostic Check Model Histogram",
     col = c("red"))
qqnorm(fit$res)

# Question 2E:
shapiro.test(fit$res)
fit2 <- lm(GPM~)

# Question 2F:
fit2 <- lm(GPM~ WT + DIS, data = gas.data)
summary(fit2)
plot(fit2$res~fit2$fitted, main = "Diagnostic Check Plot 2")
qqnorm(fit2$res)
plot(fit2$res~gas.data$WT, main = "Diagnostic Check Plot 2")
plot(fit2$res~gas.data$DIS, main = "Diagnostic Check Plot 3")
hist(fit2$res, main = "Diagnostic Check Model Histogram 2",
     col = c("green"))
shapiro.test(fit2$res)

