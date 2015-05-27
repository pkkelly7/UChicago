library(tidyr)
library(dplyr)
library(ggplot2)
library(tseries) # for adf.test() fn
library(usdm) # for vif() fn


#bring in dataset

data_1 <- read.csv("~/R/UChicago/Time_Series/hollywood_movies.csv")

# 1. Plot the independent variables X2, X3 X4 together in a single plot - what do you conclude in terms of relationship between them?
library(scatterplot3d)
scatterplot3d(data_1$X2,data_1$X3,data_1$X4, main="X2 by X3 by X4")

# 2. Scatter plot among the variables
plot(data_1$X2,data_1$X3, main="X2 by X3")
plot(data_1$X2,data_1$X4, main="X2 by X4")
plot(data_1$X3,data_1$X4, main="X3 by X4")

# 3. ADF of the independent variables
library(tseries)
adf.test(data_1$X2)
adf.test(data_1$X3)
adf.test(data_1$X4)

# 4. Regression output - R2 and any other metric you want to mention
lm1 <- lm(X1 ~ X2 + X3 + X4, data=data_1)
summ_lm1 <- summary(lm1)
summ_lm1

# 5. Regression coefficients
summ_lm1$coefficients[,1]

# 6. p-value of the coefficients
summ_lm1$coefficients[,4]

# 7. What can you comment about multicollinearity?
library(car)
vif(lm1)

# 8. plot the ACF of the residuals
res <- residuals(lm1)
plot(res, main="residuals plot")
acf(res, main="ACF of residuals")
