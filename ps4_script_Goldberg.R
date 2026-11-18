library(tidyverse)
library(fpp3)
library(BVAR)

#2

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

liquor |>
autoplot(sales) +
  labs(title = "Monthly US liquor sales", y = "$ million")

liquor |>
  gg_season(log(sales)) +
  labs(title = "Seasonal plot: Monthly US liquor sales", y = "$ million")
# There is a trend of increasing sales over time, and there is a seasonal pattern with peaks in December and July and troughs in January. The seasonality does grow with the trend through time. Logs here get rid of the growing of seasonal swings related to time. 

liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()
# The Arima model that was chosen was a Arima(1,0,2)(2,1,1). It chose a 0 difference and then a 1 difference for the seasonal term. The ETS used a ETS(A,A,A) model. The three parameters are being added together.

liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)
# The results tell us that p value is statistically significant and that we can reject the null hypothesis of white noise.

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))


liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))
nrow(liquor_cv)
glimpse(liquor_cv)
system.time({
  test_snaive <- liquor_cv |>
    model(snaive = SNAIVE(sales))
})
system.time({
  test_ets <- liquor_cv |>
    model(ets = ETS(log(sales)))
})

system.time({
  test_arima <- liquor_cv |>
    model(arima = ARIMA(log(sales)))
})

nrow(liquor_cv)
n_distinct(liquor_cv$.id)

liquor |>
  summarise(
    observations = n(),
    start = min(Month),
    end = max(Month)
  )
saveRDS(test_ets, "results/test_ets.rds")
list.files("results")
liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")
glimpse(liquor_cv_fit)
report(liquor_cv_fit)
liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")


getwd()
liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")

glimpse(liquor_cv_fit)
# The number of forecast origins is 120.
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)
liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |> knitr:: kable(digits = 2)
# Here we are forecasting one month ahead. The arima model has the lowest RMSE and MAE and forecasts the best. The two metrics agree that the arima model is the best.

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)
# DM stat for arima vs ets is -2.9062. The p-value is 0.004042. The DM stat for arima vs snaive is -10.755. The p-value is 2.2e-16. Both p-values are less than 0.05, so we reject the null hypothesis that the two models have equal forecast accuracy. If I were the liquor store chain I would use the Arima model.