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
