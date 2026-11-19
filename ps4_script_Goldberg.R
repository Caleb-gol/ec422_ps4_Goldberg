library(tidyverse)
library(fpp3)
library(BVAR)

#PART 2

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
#PART 3

liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))|> knitr::kable(digits = 2)
# Combo does slightly worse than arima itself. It still does better than ets.
cor(liquor_errs$arima, liquor_errs$ets)
# Using correlation between arima and ets, we can see a relatively high correlation of 0.87. Using this and the DM test, we can see the reason why the combo does not beat out the arima is because the two models are highly correlated. The combo does not add much new information to the arima model.

#PART 4
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]
#We will make 116 forecasts. for each variable. The first forecast is for the first quarter of 1990 and the last is for the 4th quarter of 2019.

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)
# The VAR 1 forecasts better in all eight variables besides inflation, but it's still very close. The smaller model forecasts better because it contains less parameters. The VAR 4 contains more noise that is not useful in forecasting the data.

bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")


set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)

## readRDS("results/var_results.rds") |>
##  knitr::kable(digits = 3))

# The BVAR 4 with lambda = 0.3 does better than the OLS VAR 4. Using the priors helps improve the forecast accuracy even though they have the same variables and lags.The BVAR 4 with lambda = 0.03 does better in some variables compared to the VAR 1. It does better in inv, inflation, and gs10. VAR 1 does better in all others. When we turn lambda to 0.05, our coefficients become slightly worse other than one. This tells us that having less shrinkage doesn't always provide better coefficients. Using the RMSE of the winner here would not be a good forecasting strategy as the weights of the variables are not equal and the errors vary by variables.

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

e_var4 <- actual[, "infl"] - fc_var4[, 5]
e_bvar <- actual[, "infl"] - fc_bvar_03[, 5]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

# The DM test for the GDP variable has a p value of 0.03. The results are statistically significant and we can reject the null hypothesis that the two models have equal forecast accuracy for GDP. The DM test for the inflation variable has a p value of 0.1489. The results are not statistically significant and we cannot reject the null hypothesis that the two models have equal forecast accuracy for inflation.This again supports the idea that using shrinkage depends on the variable.

# PART 5

md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,                      # level
         "2" = d(x),                   # first difference
         "3" = d(d(x)),                # second difference
         "4" = log(x),                 # log
         "5" = 100 * d(log(x)),        # growth rate
         "6" = 100 * d(d(log(x))),     # change in the growth rate
         "7" = d(x / dplyr::lag(x) - 1))
}

fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

fm <- fm[, colSums(is.na(fm)) == 0]

dim(fm)

tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]

# INDPRO is put into a growth rate. PAYEMS is also put into a growth rate. CPIAUCSL is put into a change in growth rate. FEDFUNDS is put into a first difference. Industrial production is put into a growth rate because it is a level series. The fed funs is first differenced to make it a stationary series.

Z <- fm |> select(-date) |> as.matrix()

pc <- prcomp(scale(Z))

summary(pc)$importance[, 1:5]
# The share of the variance is about 15% for the first component.

F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()
sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]
#The series names are not currently available on FRED, the series appear to be related to the industrial production besides some occasional outliers.
