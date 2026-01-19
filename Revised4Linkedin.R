# Load libraries
library(readxl)
library(tseries)
library(forecast)
library(rugarch)
library(tidyverse)

# 2. Data Preparation
Data <- read_excel("Gold1.xlsx")
Data$DATE <- as.Date(Data$DATE)
Data$VALUE <- as.numeric(Data$VALUE)
last_date <- max(Data$DATE)

# 3. Calculate Log Returns
# diff(log(x)) is standard for daily financial time series
gold_ret <- diff(log(Data$VALUE))
gold_ret <- na.omit(gold_ret)

# 4. Rectified GARCH Specification
# ARMA(2,2) solves the autocorrelation (Ljung-Box) error
# eGARCH solves the volatility asymmetry (Sign Bias) error
# distribution = "std" solves the "fat tail" (Kurtosis) error
spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(5, 5), include.mean = TRUE), # Increased to 5 to clear the Ljung-Box serial correlation
  distribution.model = "std"
)

# 5. Model Fitting
fit_garch <- ugarchfit(spec, gold_ret)
plot(fit_garch)

# --- Check the 'Weighted Ljung-Box Test' in the output below ---
# Success = p-values > 0.05
print(fit_garch)

# 6. 10-Year Simulation (Projecting into 2026)
h_10yrs <- 10 * 365 # 10 years of daily steps
n_sims  <- 2000    # Number of random paths
sim     <- ugarchsim(fit_garch, n.sim = h_10yrs, m.sim = n_sims)

# 7. Convert Returns back to Price Paths
# We take the last price from Nov 2016 and apply simulated returns
P0 <- tail(Data$VALUE, 1)
sim_ret <- fitted(sim)
price_paths <- apply(sim_ret, 2, function(x) P0 * exp(cumsum(x)))

# 8. Create Forecast Dataframe
# Using a daily sequence from the last date in 2016
price_forecast <- data.frame(
  Date   = seq(from = last_date + 1, by = "day", length.out = h_10yrs),
  Median = apply(price_paths, 1, median),
  Lo_80 = apply(price_paths, 1, quantile, 0.10),
  Hi_80 = apply(price_paths, 1, quantile, 0.90),
  Lo_95 = apply(price_paths, 1, quantile, 0.025),
  Hi_95 = apply(price_paths, 1, quantile, 0.975)
)

# 9. Final Clean Visualization
ggplot(price_forecast, aes(Date)) +
  geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95), fill = "gold", alpha = 0.15) +
  geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80), fill = "grey60", alpha = 0.15) +
  geom_line(aes(y = Median), color = "darkgoldenrod", size = 0.8) +
  labs( title = "Gold Price Projection: 2016 - 2026",
        subtitle = "Simulated using eGARCH(1,1) with ARMA(5,5) Mean Model",
        y = "Price (USD)",
        x = "Year") +
  theme_classic()


