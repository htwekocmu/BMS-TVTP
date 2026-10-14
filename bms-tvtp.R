# ---------------------------
# PART 1: DOWNLOAD, PROCESS, AND SAVE DATA
# ---------------------------
rm(list = ls())
library(quantmod)
library(dplyr)
library(zoo)
library(lubridate)
library(forecast)

# ---------------------------
# 1. Download Base Oil, FX, and Volatility Data
# ---------------------------
cat("📥 Downloading base financial data: Brent Oil, DXY, VIX...\n")
getSymbols(c("BZ=F", "DX-Y.NYB", "^VIX"), src = "yahoo", from = "2018-01-01", auto.assign = TRUE)

brent_price <- Cl(`BZ=F`)
dxy_price <- Cl(`DX-Y.NYB`)
vix <- Cl(`VIX`)

brent_df <- data.frame(date = as.Date(index(brent_price)), price = as.numeric(coredata(brent_price)))
dxy_df <- data.frame(date = as.Date(index(dxy_price)), dxy_raw = as.numeric(coredata(dxy_price)))
vix_df <- data.frame(date = as.Date(index(vix)), vix_raw = as.numeric(coredata(vix)))

oil_df_daily_raw <- brent_df %>%
  inner_join(dxy_df, by = "date") %>%
  inner_join(vix_df, by = "date") %>%
  arrange(date)

cat("📊 Aligned daily rows (before cleaning):", nrow(oil_df_daily_raw), "\n")

clean_ts <- function(x, dates) {
  if (length(x) < 3) return(x)
  z <- zoo(x, order.by = dates)
  as.numeric(forecast::tsclean(z))
}

oil_df_daily_raw <- oil_df_daily_raw %>%
  mutate(
    price = clean_ts(price, date),
    dxy_raw = clean_ts(dxy_raw, date),
    vix_raw = clean_ts(vix_raw, date)
  ) %>%
  na.omit()

oil_df_daily_cleaned <- oil_df_daily_raw %>%
  mutate(
    log_price = log(price),
    return = c(NA, diff(log_price)),
    dxy_log = log(dxy_raw),
    dxy_return = c(NA, diff(dxy_log)),
    vix_lag1 = lag(vix_raw, 1)
  ) %>%
  na.omit()

cat("✅ Daily base data cleaned. Rows:", nrow(oil_df_daily_cleaned), "\n")

# ---------------------------
# 1.5 Download Additional Covariates
# ---------------------------
cat("📥 Downloading additional macro & financial data...\n")
additional_syms <- c("SPY", "UNG", "TIP", "IEI")
getSymbols(additional_syms, src = "yahoo", from = "2018-01-01", auto.assign = TRUE)

sp500_df <- data.frame(date = as.Date(index(Cl(SPY))), sp500_price = as.numeric(coredata(Cl(SPY))))
natgas_df <- data.frame(date = as.Date(index(Cl(UNG))), natgas_price = as.numeric(coredata(Cl(UNG))))
tip_df <- data.frame(date = as.Date(index(Cl(TIP))), tip_price = as.numeric(coredata(Cl(TIP))))
treasury_df <- data.frame(date = as.Date(index(Cl(IEI))), treasury_price = as.numeric(coredata(Cl(IEI))))



oil_df_daily_enhanced <- oil_df_daily_cleaned %>%
  left_join(sp500_df, by = "date") %>%
  left_join(natgas_df, by = "date") %>%
  left_join(tip_df, by = "date") %>%
  left_join(treasury_df, by = "date") %>%
  arrange(date)

clean_col <- function(x, dates) {
  if (all(is.na(x)) || length(unique(na.omit(x))) < 2) return(x)
  z <- zoo(x, order.by = dates)
  as.numeric(forecast::tsclean(z))
}

for (col in c("sp500_price", "natgas_price", "tip_price", "treasury_price")) {
  if (col %in% names(oil_df_daily_enhanced)) {
    before <- sum(is.na(oil_df_daily_enhanced[[col]]))
    oil_df_daily_enhanced[[col]] <- clean_col(oil_df_daily_enhanced[[col]], oil_df_daily_enhanced$date)
    after <- sum(is.na(oil_df_daily_enhanced[[col]]))
    cat("🧹 Cleaned", col, "- NAs:", before, "->", after, "\n")
  }
}

oil_df_daily_final <- oil_df_daily_enhanced %>%
  mutate(
    bei_proxy = log(tip_price / treasury_price),
    dxy_real = dxy_raw / exp(3 * bei_proxy),
    sp500_return = c(NA, diff(log(sp500_price))),
    natgas_return = c(NA, diff(log(natgas_price))),
    oil_natgas_ratio = log(price / natgas_price)
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>%
  na.omit()

cat("✅ Final daily data rows (after enhancements):", nrow(oil_df_daily_final), "\n")

# ---------------------------
# Define Geopolitical Events
# ---------------------------
conflict_periods_raw <- list(
  list(start = as.Date("2018-05-08"), end = as.Date("2018-12-31"), name = "US Reimposes Iran Sanctions"),
  list(start = as.Date("2018-07-06"), end = as.Date("2019-12-15"), name = "U.S.–China Trade War"),
  list(start = as.Date("2019-06-10"), end = as.Date("2019-07-15"), name = "Gulf Tanker Attacks"),
  list(start = as.Date("2019-09-14"), end = as.Date("2019-10-31"), name = "Saudi Aramco Drone Strike"),
  list(start = as.Date("2020-01-03"), end = as.Date("2020-01-10"), name = "US–Iran Tensions (Soleimani)"),
  list(start = as.Date("2020-03-06"), end = as.Date("2020-04-15"), name = "Oil Price War (Russia–Saudi)"),
  list(start = as.Date("2021-07-29"), end = as.Date("2021-08-31"), name = "Gulf Oil Facility Disruptions"),
  list(start = as.Date("2022-02-24"), end = as.Date("2022-12-31"), name = "Russia–Ukraine War"),
  list(start = as.Date("2022-09-26"), end = as.Date("2022-10-15"), name = "Nord Stream Pipeline Sabotage"),
  list(start = as.Date("2023-04-01"), end = as.Date("2023-05-15"), name = "Red Sea Shipping Disruptions Begin"),
  list(start = as.Date("2023-10-07"), end = as.Date("2023-12-31"), name = "Israel–Hamas War"),
  list(start = as.Date("2024-01-10"), end = as.Date("2024-02-28"), name = "Houthi Attacks Escalate in Red Sea"),
  list(start = as.Date("2024-04-15"), end = as.Date("2024-05-15"), name = "Iran–Israel Retaliation"),
  list(start = as.Date("2024-07-01"), end = as.Date("2024-09-30"), name = "OPEC+ Production Cut Disputes"),
  list(start = as.Date("2024-11-01"), end = as.Date("2025-02-28"), name = "Red Sea Maritime Escalations II"),
  list(start = as.Date("2025-03-01"), end = as.Date("2025-06-30"), name = "Ukraine Counteroffensive 2025"),
  list(start = as.Date("2025-07-15"), end = as.Date("2025-10-10"), name = "Strait of Hormuz Tensions Renewed"),
  list(start = as.Date("2022-08-01"), end = as.Date("2023-09-30"), name = "U.S.–China Tech/Taiwan Tensions"),
  list(start = as.Date("2025-10-10"), end = as.Date("2025-10-10"), name = "U.S.–China Trade War Escalation")
)


decay_rate <- 4  # weeks

# ---------------------------
# 1a. Aggregate to Weekly
# ---------------------------
cat("📅 Aggregating to weekly data (Friday-ending weeks)...\n")
get_week_ending_friday <- function(dates) {
  fridays <- dates + days(5 - wday(dates, week_start = 1))
  df_temp <- data.frame(date = dates, week_ref = floor_date(dates, "week"))
  df_agg <- df_temp %>%
    group_by(week_ref) %>%
    summarise(week_end_friday = max(date), .groups = "drop")
  df_temp %>% left_join(df_agg, by = "week_ref") %>% pull(week_end_friday)
}

oil_df_weekly_initial <- oil_df_daily_final %>%
  mutate(week_end_friday = get_week_ending_friday(date)) %>%
  group_by(week_end_friday) %>%
  summarise(
    price = last(price),
    log_price = last(log_price),
    return = sum(return, na.rm = TRUE),
    dxy = last(dxy_raw),
    dxy_return = sum(dxy_return, na.rm = TRUE),
    vix = last(vix_raw),
    sp500_return = sum(sp500_return, na.rm = TRUE),
    natgas_return = sum(natgas_return, na.rm = TRUE),
    dxy_real = last(dxy_real),
    bei_proxy = last(bei_proxy),
    oil_natgas_ratio = last(oil_natgas_ratio),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  arrange(week_end_friday)

clean_weekly <- function(x, dates) {
  if (length(x) < 3) return(x)
  z <- zoo(x, order.by = dates)
  as.numeric(forecast::tsclean(z))
}

oil_df_weekly_cleaned <- oil_df_weekly_initial %>%
  mutate(
    price = clean_weekly(price, week_end_friday),
    return = clean_weekly(return, week_end_friday),
    dxy = clean_weekly(dxy, week_end_friday),
    dxy_return = clean_weekly(dxy_return, week_end_friday),
    vix = clean_weekly(vix, week_end_friday),
    sp500_return = clean_weekly(sp500_return, week_end_friday),
    natgas_return = clean_weekly(natgas_return, week_end_friday),
    dxy_real = clean_weekly(dxy_real, week_end_friday),
    bei_proxy = clean_weekly(bei_proxy, week_end_friday),
    oil_natgas_ratio = clean_weekly(oil_natgas_ratio, week_end_friday),
    log_price = log(price)
  ) %>%
  na.omit()

# Add conflict intensity with decay
oil_df_weekly_cleaned$conflict_intensity <- 0
for (event in conflict_periods_raw) {
  in_window <- oil_df_weekly_cleaned$week_end_friday >= event$start & 
    oil_df_weekly_cleaned$week_end_friday <= event$end
  oil_df_weekly_cleaned$conflict_intensity[in_window] <- 1
}

for (event in conflict_periods_raw) {
  weeks_since_start <- as.numeric((oil_df_weekly_cleaned$week_end_friday - event$start) / 7)
  intensity <- ifelse(
    oil_df_weekly_cleaned$week_end_friday >= event$start & oil_df_weekly_cleaned$week_end_friday <= event$end,
    exp(-weeks_since_start / decay_rate),
    0
  )
  oil_df_weekly_cleaned$conflict_intensity <- oil_df_weekly_cleaned$conflict_intensity + intensity
}
oil_df_weekly_cleaned$conflict_intensity <- pmin(pmax(oil_df_weekly_cleaned$conflict_intensity, 0), 1)

# Final weekly data with lags
oil_df_weekly_final <- oil_df_weekly_cleaned %>%
  arrange(week_end_friday) %>%
  mutate(
    lag_log_price = lag(log_price, 1),
    dxy_return_lag1 = lag(dxy_return, 1),
    vix_lag1 = lag(vix, 1),
    sp500_return_lag1 = lag(sp500_return, 1),
    natgas_return_lag1 = lag(natgas_return, 1),
    bei_proxy_lag1 = lag(bei_proxy, 1)
  ) %>%
  na.omit()

cat("✅ Final weekly data rows:", nrow(oil_df_weekly_final), "\n")

# ---------------------------
# SAVE FOR PART 2
# ---------------------------
write.csv(oil_df_weekly_final, "brent_oil_data_for_modeling.csv", row.names = FALSE)
cat("💾 Data saved to 'brent_oil_data_for_modeling.csv'\n")




# ---------------------------
# PART 2: LOAD SAVED DATA AND FIT MODEL
# ---------------------------
rm(list = ls())

# Load libraries
library(cmdstanr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(posterior)
library(loo)

# ---------------------------
# LOAD PREPROCESSED DATA
# ---------------------------
if (!file.exists("brent_oil_data_for_modeling.csv")) {
  stop("❌ 'brent_oil_data_for_modeling.csv' not found. Run Part 1 first.")
}

oil_df_weekly_final <- read.csv("brent_oil_data_for_modeling.csv", stringsAsFactors = FALSE)
oil_df_weekly_final$week_end_friday <- as.Date(oil_df_weekly_final$week_end_friday)

# Verify required columns exist
required_cols <- c("return", "sp500_return_lag1","natgas_return_lag1","dxy_return_lag1","vix_lag1", "bei_proxy_lag1", "conflict_intensity", "week_end_friday")
missing <- setdiff(required_cols, names(oil_df_weekly_final))
if (length(missing) > 0) {
  stop("❌ Missing columns in CSV: ", paste(missing, collapse = ", "))
}

cat("✅ Loaded weekly data. Rows:", nrow(oil_df_weekly_final), "\n")

# ---------------------------
# DEFINE CONFLICT PERIODS (for plotting)
# ---------------------------
conflict_periods_raw <- list(
  list(start = as.Date("2018-05-08"), end = as.Date("2018-12-31"), name = "US Reimposes Iran Sanctions"),
  list(start = as.Date("2018-07-06"), end = as.Date("2019-12-15"), name = "U.S.–China Trade War"),
  list(start = as.Date("2019-06-10"), end = as.Date("2019-07-15"), name = "Gulf Tanker Attacks"),
  list(start = as.Date("2019-09-14"), end = as.Date("2019-10-31"), name = "Saudi Aramco Drone Strike"),
  list(start = as.Date("2020-01-03"), end = as.Date("2020-01-10"), name = "US–Iran Tensions (Soleimani)"),
  list(start = as.Date("2020-03-06"), end = as.Date("2020-04-15"), name = "Oil Price War (Russia–Saudi)"),
  list(start = as.Date("2021-07-29"), end = as.Date("2021-08-31"), name = "Gulf Oil Facility Disruptions"),
  list(start = as.Date("2022-02-24"), end = as.Date("2022-12-31"), name = "Russia–Ukraine War"),
  list(start = as.Date("2022-09-26"), end = as.Date("2022-10-15"), name = "Nord Stream Pipeline Sabotage"),
  list(start = as.Date("2023-04-01"), end = as.Date("2023-05-15"), name = "Red Sea Shipping Disruptions Begin"),
  list(start = as.Date("2023-10-07"), end = as.Date("2023-12-31"), name = "Israel–Hamas War"),
  list(start = as.Date("2024-01-10"), end = as.Date("2024-02-28"), name = "Houthi Attacks Escalate in Red Sea"),
  list(start = as.Date("2024-04-15"), end = as.Date("2024-05-15"), name = "Iran–Israel Retaliation"),
  list(start = as.Date("2024-07-01"), end = as.Date("2024-09-30"), name = "OPEC+ Production Cut Disputes"),
  list(start = as.Date("2024-11-01"), end = as.Date("2025-02-28"), name = "Red Sea Maritime Escalations II"),
  list(start = as.Date("2025-03-01"), end = as.Date("2025-06-30"), name = "Ukraine Counteroffensive 2025"),
  list(start = as.Date("2025-07-15"), end = as.Date("2025-10-10"), name = "Strait of Hormuz Tensions Renewed"),
  list(start = as.Date("2022-08-01"), end = as.Date("2023-09-30"), name = "U.S.–China Tech/Taiwan Tensions"),
  list(start = as.Date("2025-10-10"), end = as.Date("2025-10-10"), name = "U.S.–China Trade War Escalation")
)


# ---------------------------
# BUILD DESIGN MATRICES (AS SPECIFIED)
# ---------------------------
obs_vars <- list(
  Intercept = 1,
  sp500_return_lag1 = oil_df_weekly_final$sp500_return_lag1,
  natgas_return_lag1 = oil_df_weekly_final$natgas_return_lag1,
  dxy_return_lag1 = oil_df_weekly_final$dxy_return_lag1,
  VIX_Lag1 = oil_df_weekly_final$vix_lag1,
  bei_proxy_lag1 = oil_df_weekly_final$bei_proxy_lag1,
  conflict_intensity = oil_df_weekly_final$conflict_intensity
)

trans_vars <- list(
  Intercept = 1,
  ConflictIntensity = oil_df_weekly_final$conflict_intensity
)

X_obs_raw_df <- as.data.frame(obs_vars)
X_trans_raw_df <- as.data.frame(trans_vars)

# Combine and remove NAs
data_for_stan <- c(
  list(y = oil_df_weekly_final$return),
  X_obs_raw_df,
  X_trans_raw_df
) %>%
  as.data.frame() %>%
  na.omit()

if (nrow(data_for_stan) == 0) stop("❌ No complete cases after NA removal.")

y <- data_for_stan$y
X_obs_final <- as.matrix(data_for_stan[names(obs_vars)])
X_trans_final <- as.matrix(data_for_stan[names(trans_vars)])

colnames(X_obs_final) <- names(obs_vars)
colnames(X_trans_final) <- names(trans_vars)

T <- length(y)
K_obs <- ncol(X_obs_final)
K_trans <- ncol(X_trans_final)
vol_estimate <- max(sd(y, na.rm = TRUE), 1e-6)
rate_value <- 2 / vol_estimate

stan_data <- list(
  T = T,
  K_obs = K_obs,
  K_trans = K_trans,
  X_obs = X_obs_final,
  X_trans = X_trans_final,
  y = y,
  mu_0 = 0,
  sigma_0 = 1,
  shape = 2,
  rate = rate_value
)

cat("✅ Design matrices ready. T =", T, "\n")

# ---------------------------
# COMPILE & FIT STAN MODEL
# ---------------------------
if (!file.exists("C:\\Users\\User\\Desktop\\1msm_model_tvtp_optimized.stan")) {
  stop("❌ Stan model file '1:\\Users\\User\\Desktop\\1msm_model_tvtp_optimized.stan not found.")
}

cat("🛠️ Compiling Stan model...\n")
mod <- cmdstan_model("C:\\Users\\User\\Desktop\\1msm_model_tvtp_optimized.stan", force_recompile = TRUE)

cat("🏃 Fitting model (test run)...\n")

fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_sampling = 2000,
  iter_warmup = 1000,
  seed = 123,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 0
)

fit$save_object("fit_msm_tvtp_weekly.rds")
cat("💾 Model fit saved to 'fit_msm_tvtp_weekly.rds'\n")
print(fit)
# ---------------------------
# POSTERIOR ANALYSIS
# ---------------------------
draws <- fit$draws()
draws_array <- as.array(draws)
available_vars <- dimnames(draws_array)[[3]]

# Extract z
z_vars <- available_vars[grepl("^z\\[", available_vars)]
if (length(z_vars) == 0) stop("❌ 'z' not found.")
z_draws <- as.array(fit$draws(variables = z_vars))
prob_regime1 <- apply(z_draws == 1, 3, mean)

# Align dates
complete_rows <- !is.na(rowMeans(data_for_stan[, -1]))
dates_used <- oil_df_weekly_final$week_end_friday[complete_rows]
if (length(prob_regime1) != length(dates_used)) {
  len <- min(length(prob_regime1), length(dates_used))
  prob_regime1 <- tail(prob_regime1, len)
  dates_used <- tail(dates_used, len)
}

# ---------------------------
# CREATE transition_analysis_df CORRECTLY FROM Gamma_saved
# ---------------------------
# ---------------------------
# CORRECT transition_analysis_df CREATION
# ---------------------------
cat("🔄 Creating transition_analysis_df with REGIME-SPECIFIC durations...\n")

# Extract posterior mean of delta coefficients
delta_mean <- matrix(0, nrow = 2, ncol = K_trans)
for (i in 1:2) {
  for (j in 1:K_trans) {
    param_name <- paste0("delta[", i, ",", j, "]")
    delta_mean[i, j] <- fit$summary(variables = param_name)$mean
  }
}

stay1 <- plogis(X_trans_final %*% delta_mean[1, ])  # P(1→1)
stay2 <- plogis(X_trans_final %*% delta_mean[2, ])  # P(2→2)

# Compute expected durations
duration1 <- 1 / pmax(1e-6, 1 - stay1)
duration2 <- 1 / pmax(1e-6, 1 - stay2)

# Create transition_analysis_df
transition_analysis_df <- data.frame(
  date = dates_used,
  expected_duration_1 = duration1,
  expected_duration_2 = duration2
)
cat("✅ transition_analysis_df created. Durations are now REGIME-SPECIFIC.\n")

print(transition_analysis_df)

# ---------------------------
# EXTRACT Gamma_saved FOR AVERAGE TRANSITION MATRIX
# ---------------------------
cat("🔄 Extracting Gamma_saved for average transition matrix...\n")
gamma_draws <- fit$draws("Gamma_saved")
gamma_array <- as.array(gamma_draws)
iter <- dim(gamma_array)[1]; chains <- dim(gamma_array)[2]; num_cols <- dim(gamma_array)[3]
T_check <- num_cols / 4
if (T_check != as.integer(T_check)) stop("Gamma dimension error")
T_check <- as.integer(T_check)

# Reshape to [iter, chains, T, 2, 2]
gamma_5d <- array(NA, dim = c(iter, chains, T_check, 2, 2))
for (i in 1:iter) {
  for (c in 1:chains) {
    flat <- gamma_array[i, c, ]
    mat_2d <- matrix(flat, nrow = T_check, ncol = 4, byrow = TRUE)
    for (t in 1:T_check) {
      gamma_5d[i, c, t, , ] <- matrix(mat_2d[t, ], nrow = 2, ncol = 2, byrow = TRUE)
    }
  }
}

# Compute posterior median transition matrices [T, 2, 2]
gamma_posterior <- apply(gamma_5d, c(3,4,5), median)
cat("✅ gamma_posterior created with dimensions:", dim(gamma_posterior), "\n")
print(gamma_posterior)
# ---------------------------
# PLOTS
# ---------------------------
conflict_df <- data.frame(
  start_date = as.Date(sapply(conflict_periods_raw, `[[`, "start")),
  end_date = as.Date(sapply(conflict_periods_raw, `[[`, "end"))
)

# Plot 1: Returns + Regimes
plot_data <- data.frame(
  date = dates_used,
  return = data_for_stan$y[complete_rows] * 100,
  prob_regime1 = prob_regime1,
  prob_regime2 = 1 - prob_regime1
)
p1 <- ggplot(plot_data, aes(x = date)) +
  geom_rect(data = conflict_df, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  geom_col(aes(y = return), fill = "gray20", alpha = 0.7) +
  geom_line(aes(y = prob_regime1 * 100), color = "blue", size = 1) +
  geom_line(aes(y = prob_regime2 * 100), color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(name = "Return (%)", sec.axis = sec_axis(~., name = "Regime Prob (%)")) +
  scale_x_date(name = "Date", date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(title = "Brent Oil: Returns, Regimes & Conflicts") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("oil_tvtp_analysis.png", p1, width = 14, height = 8, dpi = 300)
print(p1)
# Plot 2: Durations
duration_df <- data.frame(date = dates_used, Regime1 = duration1, Regime2 = duration2) %>%
  pivot_longer(cols = c(Regime1, Regime2), names_to = "Regime", values_to = "Duration") %>%
  mutate(Regime = ifelse(Regime == "Regime1", "Regime 1", "Regime 2"))

p2 <- ggplot(duration_df, aes(x = date, y = Duration, color = Regime)) +
  geom_rect(data = conflict_df, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(size = 1) +
  scale_y_continuous("Expected Duration (weeks)", limits = c(0, 52)) +
  scale_x_date("Date", date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Regime 1" = "blue", "Regime 2" = "red")) +
  labs(title = "Time-Varying Expected Regime Duration") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("duration_tvtp.png", p2, width = 14, height = 6, dpi = 300)
print(p2)
print(oil_df_weekly_final)
# ---------------------------
# SAVE ALL RESULTS
# ---------------------------
write.csv(oil_df_weekly_final, "oil_weekly_data_with_conflicts.csv", row.names = FALSE)
write.csv(data.frame(date = dates_used, prob_regime_1 = prob_regime1, prob_regime_2 = 1 - prob_regime1, oil_return = data_for_stan$y[complete_rows]), "smoothed_regime_probabilities.csv", row.names = FALSE)
write.csv(apply(gamma_posterior, c(2,3), mean), "transition_matrix_avg.csv")
write.csv(duration_df %>% pivot_wider(names_from = Regime, values_from = Duration), "duration_time_varying.csv", row.names = FALSE)

# Coefficient table with sigma parameters
all_params <- variables(draws)
beta_names <- all_params[grepl("^beta\\[\\d+,\\d+\\]$", all_params)]
delta_names <- all_params[grepl("^delta\\[\\d+,\\d+\\]$", all_params)]
sigma_names <- all_params[grepl("^sigma\\[\\d+\\]$", all_params)]  # Add sigma parameter names

beta_labels <- colnames(X_obs_final)
delta_labels <- colnames(X_trans_final)
sigma_labels <- paste0("Regime_", 1:2)  # Assuming 2 regimes

beta_labeled <- unlist(lapply(beta_labels, function(name) c(paste0(name, "_Reg1"), paste0(name, "_Reg2"))))
delta_labeled <- unlist(lapply(delta_labels, function(name) c(paste0(name, "_Delta_Reg1_Stay"), paste0(name, "_Delta_Reg2_Stay"))))
sigma_labeled <- paste0("sigma_", sigma_labels)

get_summary <- function(draws, param_names, labels, equation) {
  if (length(param_names) == 0) return(NULL)
  draws_subset <- subset_draws(draws, variable = param_names)
  draws_df <- as_draws_df(draws_subset)
  long <- draws_df %>%
    pivot_longer(everything(), names_to = "param", values_to = "value") %>%
    mutate(Coefficient = labels[match(sub("\\..*", "", param), param_names)]) %>%
    filter(!is.na(Coefficient)) %>%
    group_by(Coefficient) %>%
    summarise(
      Equation = equation,
      Mean = round(mean(value), 4),
      SD = round(sd(value), 4), 
      Q025 = round(quantile(value, 0.025), 4),
      Q975 = round(quantile(value, 0.975), 4),
      Rhat = round(rhat(value), 4),
      ESS_bulk = round(ess_bulk(value), 0),
      ESS_tail = round(ess_tail(value), 0),
      .groups = "drop"
    )
}

full_sum <- bind_rows(
  get_summary(draws, beta_names, beta_labeled, "Observation"),
  get_summary(draws, delta_names, delta_labeled, "Transition"),
  get_summary(draws, sigma_names, sigma_labeled, "Volatility")  # Add sigma summary
)
write.csv(full_sum, "coefficient_table_auto_named.csv", row.names = FALSE)
print(full_sum)
# ---------------------------
# EVENT ANALYSIS WITH DURATIONS (FIXED)
# ---------------------------
event_dates <- as.Date(c(
  "2018-05-08", "2019-06-10", "2019-09-14", "2020-01-03", "2020-03-06", 
  "2020-03-18", "2021-02-14", "2021-07-29", "2022-02-24", "2022-03-08", 
  "2022-09-26", "2023-04-01", "2023-10-07", "2023-10-19", "2024-01-10", 
  "2024-04-15", "2024-06-02", "2025-03-01", "2025-07-15", "2025-10-10"
))

# Names of geopolitical events
event_names <- c(
  "US Reimposes Iran Sanctions",
  "Gulf Tanker Attacks", 
  "Saudi Aramco Drone Strike",
  "US–Iran Tensions (Soleimani)",
  "Oil Price War (Russia–Saudi)",
  "Global Oil Demand Collapse (COVID-19)",
  "Texas Winter Storm",
  "Gulf Oil Facility Disruptions",
  "Russia–Ukraine War",
  "US/EU Ban Russian Oil",
  "Nord Stream Pipeline Sabotage",
  "Red Sea Shipping Disruptions Begin",
  "Israel–Hamas War",
  "OPEC+ Deepens Production Cuts",
  "Houthi Attacks Escalate in Red Sea",
  "Iran–Israel Retaliation",
  "Gulf of Guinea Piracy Surge",
  "Ukraine Counteroffensive 2025",
  "Strait of Hormuz Tensions Renewed",
  "U.S.–China Trade War Escalation"
)

get_closest_index <- function(target, date_vec) which.min(abs(as.numeric(date_vec - target)))
indices <- sapply(event_dates, get_closest_index, date_vec = dates_used)
indices <- pmax(1, pmin(indices, length(prob_regime1), nrow(transition_analysis_df)))

event_probs_durations <- data.frame(
  Event = event_names,
  Event_Date = format(event_dates, "%Y-%m-%d"),
  Matched_Date = format(dates_used[indices], "%Y-%m-%d"),
  P_Regime_1 = round(prob_regime1[indices], 4),
  P_Regime_2 = round(1 - prob_regime1[indices], 4),
  Duration_Regime_1_weeks = round(transition_analysis_df$expected_duration_1[indices], 2),
  Duration_Regime_2_weeks = round(transition_analysis_df$expected_duration_2[indices], 2)
)

write.csv(event_probs_durations, "regime_probs_and_durations_at_events.csv", row.names = FALSE)
print(event_probs_durations, row.names = FALSE)

cat("🎉 ALL RESULTS SAVED:\n")
cat("  • oil_weekly_data_with_conflicts.csv\n")
cat("  • smoothed_regime_probabilities.csv\n")
cat("  • transition_matrix_avg.csv\n")
cat("  • duration_time_varying.csv\n")
cat("  • coefficient_table_auto_named.csv\n")
cat("  • regime_probs_and_durations_at_events.csv\n")
cat("  • oil_tvtp_analysis.png\n")
cat("  • duration_tvtp.png\n")





# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Ensure necessary objects exist
if (!exists("full_sum") || !exists("prob_regime1") || !exists("dates_used") || !exists("conflict_periods_raw") || !exists("fit")) {
  stop("❌ Required objects (full_sum, prob_regime1, dates_used, conflict_periods_raw, fit) not found.")
}

# 1. Extract observation equation coefficients (beta) from full_sum
beta_summary <- full_sum[full_sum$Equation == "Observation", ]

# 2. Create a mapping between coefficient names and Covariate/Regime
beta_summary <- beta_summary %>%
  mutate(
    Covariate = case_when(
      grepl("Intercept", Coefficient) ~ "Intercept",
      grepl("sp500_return_lag1", Coefficient)~"sp500_return_lag1",
      grepl("natgas_return_lag1", Coefficient)~"natgas_return_lag1",
      grepl("dxy_return_lag1", Coefficient)~"dxy_return_lag1",
      grepl("conflict_intensity", Coefficient)~"conflict_intensity",
      grepl("VIX_Lag1", Coefficient) ~ "VIX_Lag1",
      grepl("bei_proxy_lag1", Coefficient) ~ "bei_proxy_lag1",
      TRUE ~ "Other" # In case other covariates are present
    ),
    Regime = case_when(
      grepl("_Reg1", Coefficient) ~ "Regime 1",
      grepl("_Reg2", Coefficient) ~ "Regime 2",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(Covariate != "Other") # Keep only the specified covariates

# 3. Get the posterior mean (or median) values for these coefficients
# Using Mean from your full_sum table
beta_means <- beta_summary %>% select(Covariate, Regime, Mean)

# 4. Create a long data frame for the time-varying contributions
# We have T time points (from prob_regime1/2 and dates_used)
T <- length(prob_regime1)
prob_regime2 <- 1 - prob_regime1

# Create a base data frame with all dates
time_varying_contributions <- data.frame(date = dates_used)

# Loop through each unique covariate to calculate its regime-specific contributions
for (cov in unique(beta_means$Covariate)) {
  # Get the SCALAR coefficient values for Regime 1 and Regime 2 for this covariate
  # Use pull() to extract the numeric value from the tibble
  beta_reg1_val <- beta_means %>% 
    filter(Covariate == cov, Regime == "Regime 1") %>% 
    pull(Mean)
  beta_reg2_val <- beta_means %>% 
    filter(Covariate == cov, Regime == "Regime 2") %>% 
    pull(Mean)
  
  # Calculate the time-varying contribution: Coefficient_Value * P(Regime)
  # This shows how much this regime's coefficient contributes to the expected value at time t
  time_varying_contributions[[paste0(cov, "_Reg1_Contribution")]] <- prob_regime1 * beta_reg1_val
  time_varying_contributions[[paste0(cov, "_Reg2_Contribution")]] <- prob_regime2 * beta_reg2_val
}

# 5. Reshape the data frame into long format for ggplot
# Select only the contribution columns along with the date
contrib_cols <- grep("_Contribution$", names(time_varying_contributions), value = TRUE)
time_varying_long <- time_varying_contributions %>%
  select(date, all_of(contrib_cols)) %>%
  pivot_longer(
    cols = -date,
    names_to = "Component",
    values_to = "Contribution"
  )

# 6. Parse the Component name to get Covariate and Regime
time_varying_long <- time_varying_long %>%
  mutate(
    Covariate = case_when(
      grepl("Intercept", Component) ~ "Intercept",
      grepl("sp500_return_lag1", Component)~"sp500_return_lag1",
      grepl("natgas_return_lag1", Component)~"natgas_return_lag1",
      grepl("dxy_return_lag1", Component)~"dxy_return_lag1",
      grepl("conflict_intensity", Component)~"conflict_intensity",
      grepl("VIX_Lag1", Component) ~ "VIX_Lag1",
      grepl("bei_proxy_lag1", Component) ~ "bei_proxy_lag1",
      TRUE ~ NA_character_
    ),
    Regime = case_when(
      grepl("Reg1", Component) ~ "Regime 1",
      grepl("Reg2", Component) ~ "Regime 2",
      TRUE ~ NA_character_
    )
  ) %>%
  # Create a combined identifier for legend/faceting if needed
  mutate(
    Covariate_Regime = paste(Covariate, Regime)
  )

# 7. Prepare conflict periods for shading
conflict_df <- data.frame(
  start_date = as.Date(sapply(conflict_periods_raw, `[[`, "start")),
  end_date = as.Date(sapply(conflict_periods_raw, `[[`, "end"))
)

# 8. Create the plot
p_regime_contributions <- ggplot(time_varying_long, aes(x = date, y = Contribution, color = Regime)) +
  # Add shaded rectangles for conflict periods
  geom_rect(data = conflict_df,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  # Add lines for each regime's contribution
  geom_line(size = 1) +
  # Facet by Covariate to get separate panels
  facet_wrap(~ Covariate, scales = "free_y", ncol = 1) +
  # Add a horizontal line at y=0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels and title
  labs(
    title = "Time-Varying Contribution of Regime-Specific Coefficients",
    subtitle = "Contribution = (Regime Coefficient Value) * P(Regime | Data)",
    x = "Date",
    y = "Contribution to Expected Value",
    color = "Regime"
  ) +
  # Color scale for Regime
  scale_color_manual(values = c("Regime 1" = "blue", "Regime 2" = "red")) +
  # Theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# 9. Print and save the plot
print(p_regime_contributions)
ggsave("time_varying_regime_coeff_contributions.png", p_regime_contributions, width = 12, height = 10, dpi = 300)
cat("✅ Time-varying regime-specific coefficient contribution plot saved.\n")




# --- Extract regime-specific sigmas (posterior means or medians) ---
# Try the common naming: sigma[1], sigma[2]
sig_vars <- c("sigma[1]", "sigma[2]")

# If your model used a different name, auto-detect first match that looks like sigma
if (!all(sig_vars %in% fit$metadata()$model_params)) {
  all_vars <- dimnames(as.array(fit$draws()))[[3]]
  detected <- grep("^sigma(\\[\\d+\\])?$", all_vars, value = TRUE)
  if (length(detected) >= 2) {
    sig_vars <- detected[1:2]
  } else {
    stop("❌ Could not find sigma parameters in the fit. Available vars include: ",
         paste(head(all_vars, 50), collapse = ", "))
  }
}

# Posterior means (use $median if you prefer medians)
sig_sum <- fit$summary(variables = sig_vars)

sigma_reg1 <- sig_sum$mean[ sig_sum$variable == sig_vars[1] ]
sigma_reg2 <- sig_sum$mean[ sig_sum$variable == sig_vars[2] ]

cat("σ estimates -> Regime 1:", round(sigma_reg1, 6),
    " Regime 2:", round(sigma_reg2, 6), "\n")

# Calculate prob_regime2 from prob_regime1
prob_regime2 <- 1 - prob_regime1

time_varying_volatility <- data.frame(
  date = dates_used,
  regime_1_volatility = prob_regime1 * sigma_reg1,
  regime_2_volatility = prob_regime2 * sigma_reg2,
  overall_volatility  = prob_regime1 * sigma_reg1 + prob_regime2 * sigma_reg2
)

print(head(time_varying_volatility))

# Create the time-varying volatility dataframe with separate regime volatilities
time_varying_volatility <- data.frame(
  date = dates_used,
  regime_1_volatility = prob_regime1 * sigma_reg1,
  regime_2_volatility = prob_regime2 * sigma_reg2,
  overall_volatility = prob_regime1 * sigma_reg1 + prob_regime2 * sigma_reg2
)

# Explicitly set the column names
colnames(time_varying_volatility) <- c("date", "regime_1_volatility", "regime_2_volatility", "overall_volatility")

# Check if the dataframe was created properly
print("Columns in time_varying_volatility:")
print(colnames(time_varying_volatility))
print("First few rows:")
head(time_varying_volatility)

# Reshape the data to long format for plotting both regime-specific volatilities
volatility_long <- time_varying_volatility %>%
  select(date, regime_1_volatility, regime_2_volatility) %>%
  pivot_longer(
    cols = c(regime_1_volatility, regime_2_volatility),
    names_to = "Regime",
    values_to = "Volatility"
  ) %>%
  mutate(
    Regime = case_when(
      Regime == "regime_1_volatility" ~ "Regime 1",
      Regime == "regime_2_volatility" ~ "Regime 2",
      TRUE ~ NA_character_
    )
  )

# Prepare conflict periods for shading
conflict_df <- data.frame(
  start_date = as.Date(sapply(conflict_periods_raw, `[[`, "start")),
  end_date = as.Date(sapply(conflict_periods_raw, `[[`, "end"))
)

# Create plot for time-varying regime-specific volatilities
p_regime_volatility <- ggplot(volatility_long, aes(x = date, y = Volatility, color = Regime)) +
  # Add shaded rectangles for conflict periods
  geom_rect(data = conflict_df,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  # Add lines for each regime's volatility contribution
  geom_line(size = 1) +
  # Add horizontal line at y=0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels and title
  labs(
    title = "Time-Varying Regime-Specific Volatilities",
    subtitle = "Volatility = P(Regime) × σ_Regime",
    x = "Date",
    y = "Volatility Contribution",
    color = "Regime"
  ) +
  # Color scale for Regime
  scale_color_manual(values = c("Regime 1" = "blue", "Regime 2" = "red")) +
  # Theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Print and save
print(p_regime_volatility)
ggsave("time_varying_regime_specific_volatility.png", p_regime_volatility, width = 12, height = 6, dpi = 300)
cat("✅ Time-varying regime-specific volatility plot saved.\n")

# Create plot for overall time-varying volatility
p_overall_volatility <- ggplot(time_varying_volatility, aes(x = date, y = overall_volatility)) +
  # Add shaded rectangles for conflict periods
  geom_rect(data = conflict_df,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  # Add line for overall volatility
  geom_line(size = 1, color = "darkgreen") +
  # Add horizontal line at y=0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels and title
  labs(
    title = "Time-Varying Overall Volatility",
    subtitle = "Overall Volatility = P(Regime 1) × σ₁ + P(Regime 2) × σ₂",
    x = "Date",
    y = "Overall Volatility"
  ) +
  # Theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print and save
print(p_overall_volatility)
ggsave("time_varying_overall_volatility.png", p_overall_volatility, width = 12, height = 6, dpi = 300)
cat("✅ Time-varying overall volatility plot saved.\n")

# Create combined plot showing both regime contributions and overall
p_combined_volatility <- ggplot() +
  # Add regime-specific contributions (dashed lines)
  geom_line(data = volatility_long, 
            aes(x = date, y = Volatility, color = Regime, linetype = "Dashed"),
            size = 0.8) +
  # Add overall volatility (solid line)
  geom_line(data = time_varying_volatility,
            aes(x = date, y = overall_volatility),
            color = "black", size = 1.2, linetype = "solid") +
  # Add shaded rectangles for conflict periods
  geom_rect(data = conflict_df,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  # Add horizontal line at y=0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels and title
  labs(
    title = "Time-Varying Volatility: Regime Contributions vs Overall",
    subtitle = "Dashed: Regime contributions; Solid: Overall volatility",
    x = "Date",
    y = "Volatility",
    color = "Regime Contribution",
    linetype = "Line Type"
  ) +
  # Color scale for Regime
  scale_color_manual(values = c("Regime 1" = "blue", "Regime 2" = "red")) +
  # Linetype scale
  scale_linetype_manual(values = c("Dashed" = "dashed", "solid" = "solid"), guide = "none") +
  # Theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Print and save
print(p_combined_volatility)
ggsave("time_varying_volatility_combined.png", p_combined_volatility, width = 12, height = 8, dpi = 300)
cat("✅ Combined time-varying volatility plot saved.\n")




# ---------------------------
# TRANSITION PROBABILITY MATRIX (time-varying + averages)
# ---------------------------
cat("🧮 Computing transition probability matrices...\n")

# 1) Time-varying transition matrices from the posterior-mean deltas
#    (these align with X_trans_final rows used in the model)
p11 <- plogis(X_trans_final %*% delta_mean[1, ])  # P(1→1)
p22 <- plogis(X_trans_final %*% delta_mean[2, ])  # P(2→2)
p12 <- 1 - p11                                   # P(1→2)
p21 <- 1 - p22                                   # P(2→1)

Gamma_t_from_delta <- array(NA_real_, dim = c(T, 2, 2))
Gamma_t_from_delta[, 1, 1] <- as.numeric(p11)
Gamma_t_from_delta[, 1, 2] <- as.numeric(p12)
Gamma_t_from_delta[, 2, 1] <- as.numeric(p21)
Gamma_t_from_delta[, 2, 2] <- as.numeric(p22)

# Align dates for X_trans_final rows
dates_for_trans <- dates_used
if (length(dates_for_trans) != T) {
  # fallback alignment (shouldn't trigger, but safe)
  len <- min(length(dates_for_trans), T)
  dates_for_trans <- tail(dates_for_trans, len)
  Gamma_t_from_delta <- Gamma_t_from_delta[(T-len+1):T, , ]
}

# Save time-varying transition probabilities as a flat table
trans_df <- data.frame(
  date = dates_for_trans,
  p11 = Gamma_t_from_delta[,1,1],
  p12 = Gamma_t_from_delta[,1,2],
  p21 = Gamma_t_from_delta[,2,1],
  p22 = Gamma_t_from_delta[,2,2]
)
write.csv(trans_df, "transition_matrix_timevarying.csv", row.names = FALSE)
cat("💾 Saved time-varying transitions to 'transition_matrix_timevarying.csv'\n")

# 2) Overall (posterior-mean) matrix from deltas
Gamma_mean_from_delta <- matrix(c(mean(trans_df$p11),
                                  mean(trans_df$p12),
                                  mean(trans_df$p21),
                                  mean(trans_df$p22)),
                                nrow = 2, byrow = TRUE)
dimnames(Gamma_mean_from_delta) <- list(
  from = c("Regime1","Regime2"),
  to   = c("Regime1","Regime2")
)
cat("📌 Overall mean transition matrix (from deltas):\n")
print(round(Gamma_mean_from_delta, 4))

# Also save it
write.csv(Gamma_mean_from_delta, "transition_matrix_overall_from_deltas.csv", row.names = TRUE)
cat("💾 Saved 'transition_matrix_overall_from_deltas.csv'\n")

# 3) Optional: Overall matrix from Gamma_saved (you already computed gamma_posterior)
#    We take the mean over time of the posterior-median Gamma_t
Gamma_mean_from_saved <- apply(gamma_posterior, c(2,3), mean)
dimnames(Gamma_mean_from_saved) <- list(from = c("Regime1","Regime2"),
                                        to   = c("Regime1","Regime2"))
cat("📌 Overall mean transition matrix (from Gamma_saved):\n")
print(round(Gamma_mean_from_saved, 4))
write.csv(Gamma_mean_from_saved, "transition_matrix_overall_from_saved.csv", row.names = TRUE)
cat("💾 Saved 'transition_matrix_overall_from_saved.csv'\n")

# 4) Optional: Steady-state (stationary) distribution for both overall matrices
stationary <- function(G) {
  # Solve π' G = π', sum(π)=1
  A <- t(G) - diag(2)
  A <- rbind(A, c(1,1))
  b <- c(0,0,1)
  as.numeric(qr.solve(A, b))
}
pi_delta <- stationary(Gamma_mean_from_delta)
pi_saved <- stationary(Gamma_mean_from_saved)

cat("🔁 Steady-state (from deltas):  Regime1 =", round(pi_delta[1], 4),
    ", Regime2 =", round(pi_delta[2], 4), "\n")
cat("🔁 Steady-state (from saved):   Regime1 =", round(pi_saved[1], 4),
    ", Regime2 =", round(pi_saved[2], 4), "\n")

# 5) Optional quick plot of time-varying stay probabilities
suppressPackageStartupMessages(require(ggplot2))
stay_df <- data.frame(
  date = dates_for_trans,
  P11 = trans_df$p11,
  P22 = trans_df$p22
) |>
  tidyr::pivot_longer(-date, names_to = "Transition", values_to = "Probability")

p_stay <- ggplot(stay_df, aes(date, Probability, color = Transition)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Time-Varying Stay Probabilities", x = "Date", y = "Probability") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("transition_stay_probabilities.png", p_stay, width = 10, height = 5, dpi = 300)
cat("🖼️ Saved 'transition_stay_probabilities.png'\n")



