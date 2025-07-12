
# 1. Install necessary packages (if necessary)
if (!require("readxl")) install.packages("readxl")
if (!require("lmtest")) install.packages("lmtest")
if (!require("tseries")) install.packages("tseries")
if (!require("stargazer")) install.packages("stargazer")
# 2. Load libraries
library(readxl)
library(lmtest)
library(tseries)
library(vars)
library(tidyverse)
library(stargazer)
# 3. Read the data from Excel
data <- read.csv("C:/Users/15612/OneDrive/Documents/hOUSING_DIEBOLD_EXTENDED.csv")  # Replace 'your_file.xlsx' with your file

head(data)
str(data)

# Creating VAR model 
data_ts <- ts(data[, c("STARTS", "COMPS", "UNEMP")], start = c(1968, 1), frequency = 12)
data_sub <- window(data_ts, end = c(1991, 12))
plot(data_sub, main = "Time Series Variables")
plot(data_ts, main = "Time Series Variables") 

# 1. Lag selection
lag_selection <- VARselect(data_sub, lag.max = 8, type = "const")
optimal_lag <- lag_selection$selection["SC(n)"]
cat("Optimal lag (BIC):", optimal_lag, "\n")
# 2. Estimation of optimal lag.
# 3. Look at the output to check that all inverted AR roots are less than 1
var_model <- VAR(data_sub, p = optimal_lag, type = "const")
summary(var_model)
# 4. Test for residual correlation Portmanteu's test
serial.test(var_model, lags.pt = 16, type = "PT.asymptotic")
# 5. Forecast for a 12 month horizon
var_forecast <- predict(var_model, n.ahead = 24)
print(var_forecast)
plot(var_forecast)

# Granger Causality tests
causality(var_model, cause = "STARTS")
causality(var_model, cause = "COMPS")
causality(var_model, cause = "UNEMP")



