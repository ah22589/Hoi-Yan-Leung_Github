# 1.Loading the time series------------------------------------------------

setwd("C:/Users/rache/OneDrive/Desktop/Year 4/Sem 2 - Time series/Coursework1_221117152")
gb_exercise <- read.csv("GB_exercise_searchHistory.csv")
gb_exercise <- ts(gb_exercise$exercise, start = c(2011, 1), frequency = 12)

exercise_lm <- lm(gb_exercise ~ time(gb_exercise))
plot(gb_exercise, main = "Web search volumne for exercise in GB", type = "l")
abline(exercise_lm, col = "red")
legend("topleft", legend=c("Original ts", "Linear reg"), fill=c("black", "red"))

# 2.decompose gb_exercise------------------------------------------------
exercise_decom <- decompose(gb_exercise, type = "additive")
plot(exercise_decom$trend, main = "Trend from the decomposed time series")
plot(exercise_decom$figure, main = "Seasonality from the decomposed time series", type = "l")
plot(exercise_decom$random, main = "Resdiuals from the decomposed time series")

#testing properties of residuals
exercise_decom_resid <- zoo::na.trim(exercise_decom$random)

#adf test
#H0: Time series is not stationary
tseries::adf.test(exercise_decom_resid) #p-value = 0.01 < 0.05
#kpss test
#H0: Time series is stationary
feasts::unitroot_kpss(exercise_decom_resid) #p-value = 0.1 > 0.5
#Ljung box test
#H0: Time series is white noise
Box.test(exercise_decom_resid, type = "Ljung-Box")

exercise_subset_resid <- zoo::na.trim(exercise_decom$random)
tseries::adf.test(exercise_subset_resid)
Box.test(exercise_subset_resid, type = "Ljung-Box")

# Reapply decompose on post covid time series
exercise_subset <- window(gb_exercise, start = c(2022,1))
exercise_subset_decom <- decompose(exercise_subset, type = "additive")
plot(exercise_subset_decom)
plot(exercise_subset_decom$random, main = "Residuals for post-covid model")

exercise_subset_resid <- zoo::na.trim(exercise_decom$random)
tseries::adf.test(exercise_subset_resid)
Box.test(exercise_subset_resid, type = "Ljung-Box")

# 3.Apply prophet on the time series------------------------------------------------

library(prophet)
library(zoo)
library(ggplot2)

ds <- as.yearmon(time(gb_exercise))
y <- gb_exercise
exercise_df <- data.frame(ds, y)

prophet_m <- prophet(exercise_df, weekly.seasonality = FALSE,daily.seasonality = FALSE) 
#disable weekly and daily seasonality as time series data is expressed in months

prophet_time <- make_future_dataframe(prophet_m, periods = 60, freq = "month")
# 5 years is equivalent to 60 periods

prophet_pred <- predict(prophet_m, prophet_time)
# make 5 years of predictions using the Prophet model

plot(prophet_m, prophet_pred, xlab = "Time", ylab = "Search volume") +
  ggtitle("Time series modelled using Prophet")

# Apply prophet to pre-covid time series

exercise_subset <- window(gb_exercise, end = c(2019,12))
#subsets the time series for pre-2020 data

ds <- as.yearmon(time(exercise_subset))
y <- exercise_subset
exercise_df <- data.frame(ds, y)
prophet_model <- prophet(exercise_df, weekly.seasonality = FALSE,daily.seasonality = FALSE)
prophet_time <- make_future_dataframe(prophet_model, periods = 60, freq = "month")
prophet_pred <- predict(prophet_model, prophet_time)
plot(prophet_model, prophet_pred, xlab = "Time", ylab = "Search volume") +
  ggtitle("Pre-Covid subset modelled using Prophet")


# Apply prophet to post-covid time series

exercise_subset <- window(gb_exercise, start = c(2022,1))
#subsets the time series for post-2022 data

ds <- as.yearmon(time(exercise_subset))
y <- exercise_subset
exercise_df <- data.frame(ds, y)
prophet_model <- prophet(exercise_df, weekly.seasonality = FALSE,daily.seasonality = FALSE)
prophet_time <- make_future_dataframe(prophet_model, periods = 60, freq = "month")
prophet_pred <- predict(prophet_model, prophet_time)
plot(prophet_model, prophet_pred, xlab = "Time", ylab = "Search volume") +
  ggtitle("Post-Covid subset modelled using Prophet")

# 4.Alternative method using ARIMA------------------------------------------------

library(forecast)
exercise_subset <- window(gb_exercise, start = c(2022,1))
forecast_arima <- forecast::forecast(exercise_subset, h = 60)
time_arima <- seq(from = 2026, by = 1/12, length.out = 60)
plot(forecast_arima, main = "ARIMA forecast")

# 5. Actual vs forecast

gb_exercise_26 <- c(54, 57, 47) 
#Data points extracted directly from google trends instead of loading a csv file
gb_exercise_26 <- ts(gb_exercise_26, start = c(2026,1), frequency = 12)
gb_exercise_26 <- ts(c(gb_exercise, gb_exercise_26), start = c(2011,1), frequency = 12)
plot(gb_exercise_26, main = "Web search volumne for exercise in GB", type = "l")

p_filtered <- prophet_pred[prophet_pred$ds <= as.Date("2026-03-01"), ]
gb_exercise_26_filtered <- window(gb_exercise_26, start = c(2022,01))
p_2026 <- data.frame(Time=as.Date(gb_exercise_26_filtered), ts= gb_exercise_26_filtered, p=p_filtered$yhat)

plot(p_2026$Time, p_2026$ts, type = "l", main = "2026 Web search volumne for exercise in GB")
lines(p_2026$Time, p_2026$p, col = "red")
legend("topleft", legend = c("Actual ts", "Prophet pred"), fill = c("black", "red") ,cex = 0.75)

plot(forecast_arima, main = "ARIMA forecast")
lines(gb_exercise_26, col="red")
lines(gb_exercise)
legend("topleft", legend=c("2011-2025 ts", "2026 ts", "ARIMA"), fill=c("black", "red", "deepskyblue"), cex = 0.6)