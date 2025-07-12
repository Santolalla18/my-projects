# Importing libraries 

library("PerformanceAnalytics")
library("dplyr")
library("xts")
library("ggplot2")

my_data <- read.csv("C:/Users/15612/OneDrive/Documents/New Project CAPM in R.csv", header = TRUE)
head(my_data)

# Make correction for observed data format

my_data$observation_date <- as.Date(my_data$observation_date, format = "%m/%d/%Y")
sum(is.na(my_data$observation_date))

# Overwriting our dataframe for simplicity purposes

my_data <- xts(my_data[,3:4], order.by = as.Date(my_data[,1]))

head(my_data)

# Evaluating Jensen's alpha and beta coefficient with simple linear regression

lm_my_data <- lm(my_data$Asset.Return~my_data$Market.Return)
summary(lm_my_data)

# Intercept corresponds to Jensen's alpha(expected excess return) to market beta

# Using CAPM function for Linear Model Validation 

CAPM.beta(Ra = my_data$Asset.Return, Rb = my_data$Market.Return) 

# Covariance Matrix for quantifying correlation between asset and market return 

cov(my_data$Asset.Return,my_data$Market.Return)/var(my_data$Market.Return)

CAPM.alpha(Ra = my_data$Asset.Return, Rb = my_data$Market.Return)

# We obtained same values as for Linear Model 

# Plotting without Excess Return

regression_plot <- data.frame(Market = as.numeric(my_data$Market), 
Asset = as.numeric(my_data$Asset.Return))

ggplot(regression_plot, aes(x = Market, y = Asset)) + geom_point(color = "steelblue", alpha = 0.6) + geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dotted") + labs(title = "CAPM: Asset vs Market Return", x = "Market Return", y = "PFE Return") + theme_minimal()

# Computing CAPM with Excess Return 
