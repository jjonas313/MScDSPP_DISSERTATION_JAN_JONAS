### preparing aggregated datasets

# load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(kableExtra)

# Calculate yield spreads with Germany as the benchmark
yield_spreads <- EMU_yields %>%
  mutate(across(-date, ~ . - Germany, .names = "spread_{col}")) %>%
  select(date, starts_with("spread_"))

# Rename columns to match the country names (remove "spread_" prefix)
names(yield_spreads) <- gsub("spread_", "", names(yield_spreads))

yield_spreads <- yield_spreads %>% 
  select(-c("Germany"))

# remove estonia due to large amount of missing values (all missing until 2020)
yield_spreads <- yield_spreads %>% 
  select(-c("Estonia", "Lithuania"))

# Add event dates:
uncert_prop <- read.csv("uncertain_proportion.csv")

uncert_prop$date <- as.Date(uncert_prop$date, format = "%Y-%m-%d")

event_dates <- uncert_prop$date

yield_spreads <- yield_spreads %>% 
  mutate(event = ifelse(date %in% event_dates, 1, 0))

# forward fill missing values

yield_spreads <- yield_spreads %>%
  mutate(across(.cols = -c(date, event), ~ na.locf(., na.rm = FALSE)))

# remove first row with NAs
yield_spreads <- yield_spreads[-1,]

#### Germany US yield spread

US_DE_yieldspread <- german_bunds %>% 
  full_join(us_treasury, by = "date") %>% 
  select(date, bunds_yield, us_yield, everything())

# remove rows before 2015-01-01
US_DE_yieldspread <- US_DE_yieldspread %>% 
  filter(date >= "2015-01-01") %>% 
  filter(date <= "2024-05-31")

# calculate yield spread
US_DE_yieldspread <- US_DE_yieldspread %>% 
  mutate(yield_spread_US_DE = bunds_yield - us_yield)

US_DE_yieldspread <- as_tibble(US_DE_yieldspread)

View(US_DE_yieldspread)

## Interpretation: narrowing yield spread maens flight to US, risk of German bonds increases

#### QUARTERLY DATA

quart_list <- list(current_account, govt_deficit, govt_debt, gdp, debt_securities_issued_quarterly)

# merge all quarterly datasets

quarterly_data <- reduce(quart_list, full_join, by = c("date", "country"))

quarterly_data <- as_tibble(quarterly_data)

View(quarterly_data)

quarterly_data <- quarterly_data %>%
  filter(country != "Croatia")


vars_to_fill <- names(quarterly_data[,3:ncol(quarterly_data)])


quarterly_data <- quarterly_data %>%
  group_by(country) %>%
  mutate(across(all_of(vars_to_fill), ~ifelse(date == "2024-Q2" & is.na(.), 
                                              lag(., n = 1), .)))
View(quarterly_data)

## change to monthly frequencies

monthly_quarterly <- quarterly_data %>%
  # Separate year and quarter into two columns
  separate(date, into = c("year", "quarter"), sep = "-Q") %>%
  # Create the corresponding months for each quarter
  mutate(month = case_when(
    quarter == "1" ~ list(c("01", "02", "03")),
    quarter == "2" ~ list(c("04", "05", "06")),
    quarter == "3" ~ list(c("07", "08", "09")),
    quarter == "4" ~ list(c("10", "11", "12"))
  )) %>%
  # Expand the data frame to include each month for each quarter
  unnest(month) %>%
  # Create a new date column in YYYY-MM format
  mutate(date = paste(year, month, sep = "-"))

monthly_quarterly <- monthly_quarterly %>% 
  select(-c("year", "quarter", "month"))

monthly_quarterly$date <- as.Date(paste0(monthly_quarterly$date, "-01"))

# remove rows that are not in the range of 2014-12-01 to 2024-06-01
monthly_quarterly <- monthly_quarterly %>% 
  filter(date >= "2014-12-01")

View(monthly_quarterly)

#### Monthly data

month_list <- list(hicp_data, indust_production, reer_data, unemployment_rate)

monthly_data <- reduce(month_list, full_join, by = c("date", "country"))

monthly_data <- as_tibble(monthly_data)

# combine with monthly_quarterly

monthly_data <- monthly_data %>% 
  full_join(monthly_quarterly, by = c("date", "country"))

# remove rows with estonia in country column
monthly_data <- monthly_data %>% 
  filter(country != "Estonia") %>%
  filter(country != "Lithuania") %>%
  filter(country != "Germany") 

monthly_data$year_month <- format(monthly_data$date, "%Y-%m")

# Lag monthly values as values are reported at the end of month so for the respective month we want the value from previous month

columns_to_lag <- setdiff(names(monthly_data), c("date", "country", "year_month"))

monthly_data_lagged <- monthly_data %>%
  arrange(country, date) %>%
  group_by(country) %>%             
  mutate(across(
    .cols = all_of(columns_to_lag), 
    .fns = ~ lag(.x, n = 1)         
  )) %>%
  ungroup()

monthly_data_lagged <- monthly_data_lagged %>%
  select(-c(expenditure_total_million_euro, expenditure_total_percent_gdp, revenue_total_million_euro, revenue_total_percent_egdp,
            deficit_total, `gdp_current_prices(million_euro)`))

monthly_data_lagged <- rename(monthly_data_lagged, c("hicp" = "hicp(2015=100)",
                                                      "gdp" = "gdp_price_index(2015=100)"))

monthly_data_lagged <- rename(monthly_data_lagged, "Country" = "country")

monthly_data_lagged$debt_securities <- as.numeric(monthly_data_lagged$debt_securities)
monthly_data_lagged$short_term_debt_securities <- as.numeric(monthly_data_lagged$short_term_debt_securities)
monthly_data_lagged$long_term_debt_securities <- as.numeric(monthly_data_lagged$long_term_debt_securities)

### Daily data

daily_list <- list(v2tx_data, vix_data, ovx_data, brent_data, ciss_data)

daily_data <- reduce(daily_list, full_join, by = "date")

daily_data <- daily_data %>% 
  filter(date >= "2015-01-01") %>% 
  filter(date <= "2024-05-31")

daily_data[, -c(1)] <- lapply(daily_data[, -c(1)], as.numeric)

daily_data <- daily_data %>% 
  full_join(US_DE_yieldspread, by = "date")

daily_data <- daily_data %>% 
  mutate(event = ifelse(date %in% event_dates, 1, 0))

daily_data <- daily_data %>%
  mutate(across(.cols = -c(date, event), ~ na.locf(., na.rm = FALSE)))


setDT(daily_data)

daily_data[, date := as.Date(date)]

setorder(daily_data, date)

daily_data[, row_id := .I]

daily_data[, event := as.integer(event)]

# Initialize event window columns with zeros
daily_data[, `:=`(
  ev_window_1 = 0L,
  ev_window_2 = 0L,
  ev_window_5 = 0L
)]

# Get the row_ids where 'event' == 1
event_rows <- daily_data[event == 1, row_id]

max_row_id <- nrow(daily_data)

# Function to create event window indicators based on row_ids
create_event_window <- function(data, event_rows, window_size, col_name) {
  window_row_ids <- unique(unlist(
    lapply(event_rows, function(x) {
      window_ids <- (x - window_size):(x + window_size)
      window_ids[window_ids >= 1 & window_ids <= max_row_id]
    })
  ))
  
  data[window_row_ids, (col_name) := 1L]
}

# Apply function
create_event_window(daily_data, event_rows, 1, "ev_window_1")
create_event_window(daily_data, event_rows, 2, "ev_window_2")
create_event_window(daily_data, event_rows, 5, "ev_window_5")



# Remove 'event_id' from daily_data if it exists
if ("event_id" %in% names(daily_data)) {
  daily_data[, event_id := NULL]
}

# Create a data.table of event dates with unique event IDs
ev_dates <- daily_data[event == 1, .(ev_date = date)]
ev_dates[, event_id := .I]

# Adjusted function to create event window dates
create_event_window_dates <- function(ev_dates, window_size) {
  event_windows <- ev_dates[, {
    # Ensure event_date and event_id are scalar within each group
    ed <- ev_date[1]
    eid <- event_id[1]
    date_seq <- seq(ed - window_size, ed + window_size, by = "day")
    .(date = date_seq, event_id_temp = eid)
  }, by = event_id]
  return(event_windows)
}

# Generate event window dates for each window size
event_windows_1 <- create_event_window_dates(ev_dates, window_size = 1)
event_windows_2 <- create_event_window_dates(ev_dates, window_size = 2)
event_windows_5 <- create_event_window_dates(ev_dates, window_size = 5)

# Merge event windows with daily_data
# Since 'event_id_temp' is not in 'daily_data', there will be no duplication
daily_data_1 <- merge(daily_data, event_windows_1, by = "date", allow.cartesian = TRUE)
daily_data_2 <- merge(daily_data, event_windows_2, by = "date", allow.cartesian = TRUE)
daily_data_5 <- merge(daily_data, event_windows_5, by = "date", allow.cartesian = TRUE)

# Identify variables to average (adjust as necessary)
variables_to_average <- setdiff(names(daily_data), c("date", "event", "ev_window_1", "ev_window_2", "ev_window_5", "row_id"))

# Compute averages for each dataset
averages_1 <- daily_data_1[, lapply(.SD, mean, na.rm = TRUE), by = event_id_temp, .SDcols = variables_to_average]
averages_2 <- daily_data_2[, lapply(.SD, mean, na.rm = TRUE), by = event_id_temp, .SDcols = variables_to_average]
averages_5 <- daily_data_5[, lapply(.SD, mean, na.rm = TRUE), by = event_id_temp, .SDcols = variables_to_average]

# Merge event dates into averages to include 'event_date' and 'event_id'
averages_1 <- merge(averages_1, ev_dates, by.x = "event_id_temp", by.y = "event_id")
averages_2 <- merge(averages_2, ev_dates, by.x = "event_id_temp", by.y = "event_id")
averages_5 <- merge(averages_5, ev_dates, by.x = "event_id_temp", by.y = "event_id")

# Clean up and rename columns
averages_1[, event_id_temp := NULL]
averages_2[, event_id_temp := NULL]
averages_5[, event_id_temp := NULL]

# Assign to desired dataset names
daily_data_1 <- averages_1
daily_data_2 <- averages_2
daily_data_5 <- averages_5

daily_data_1 <- daily_data_1 %>%
  select(-c(bunds_yield, us_yield)) %>% 
  rename(date = ev_date)

daily_data_2 <- daily_data_2 %>%
  select(-c(bunds_yield, us_yield)) %>% 
  rename(date = ev_date)

daily_data_5 <- daily_data_5 %>%
  select(-c(bunds_yield, us_yield)) %>% 
  rename(date = ev_date)

View(daily_data_1)
View(daily_data_2)
View(daily_data_5)


# Plots and graphs

yield_data_plot <- EMU_yields %>%
  select(-c(Estonia)) 

yield_data_plot <- yield_data_plot[-1,]

EMU_yields_long <- yield_data_plot %>%
  pivot_longer(cols = -date,     # Keep the Date column, pivot the rest
               names_to = "Country",
               values_to = "Yield")

plot_10yr_yields <- ggplot(data = EMU_yields_long, 
                           aes(x = date, y = Yield, group = Country, color = Country)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "9 month") +
  labs(x = "Date", y = "10-Year government bond yields", 
       title = "Figure 2: 10-year government bond yields") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        plot.title = element_text(size = 20, face = "bold")) +
  facet_wrap(~Country, scales = "free_y", ncol = 4)

ggsave("plot_10yr_yields.png", plot_10yr_yields, width = 1000, height = 550, units = "px", dpi = 100)


### create table with variables, descriptions, and source

variables <- data.frame(Variable = c("yield_spreads", "proportion_uncertain", "hicp", "industrial_production_growth", "reer", "unemp_rate", "current_account_balance_million_euro",
                                       "deficit_percent_gdp", "debt_total_percent_gdp", "debt_securities", "short_term_debt_securities",
                                       "v2tx", "vix", "ovx", "brent_price", "ciss", "yield_spread_US_D"),
                        Description = c("Yield spreads calculated using Eurostat's Maastricht Treaty EMU convergence criterion series. These represent bond yields for EMU countries with maturities around 10 years. German yields used as benchmark.",
                                          "Proportion of uncertain words in the ECB Monetary Policy Accounts",
                                          "Harmonized Index of Consumer Prices (HICP) - Index 2015 = 100",
                                          "Industrial production growth - percentage change on previous period",
                                          "Real effective exchange rate (REER) - deflated by consumer price index (CPI)",
                                          "Unemployment rate - percentage of total labor force",
                                          "Current account balance - in million euros",
                                          "General government deficit as a percentage of GDP - caluclated using Expenditure and Revenue data",
                                          "General government debt as a percentage of GDP",
                                          "General government debt securities issued - Liabilities, transactions (flow of debt securities)",
                                          "General government short-term debt securities - Liabilities, transactions (flow of debt securities)",
                                          "V2TX volatility index",
                                          "VIX volatility index",
                                          "CBOE Crude Oil volatility index",
                                          "Crude Oil Prices: Brent - Europe",
                                          "Composite Indicator of Systemic Stress (CISS)",
                                          "Yield spread between 10-year German government bonds and 10-year US Treasury bonds"),
                        Source = c("Eurostat", 
                                   "ECB", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "Eurostat", 
                                   "STOXX", 
                                   "FRED", 
                                   "FRED", 
                                   "FRED",
                                   "ECB Data Portal",
                                   "FRED/Bundesbank"))


View(variables)

url = c("https://ec.europa.eu/eurostat/databrowser/view/irt_lt_mcby_d__custom_13035888/default/table",
       "https://www.ecb.europa.eu/press/accounts/html/index.en.html",
       "https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_midx__custom_12843359/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/sts_inpr_m__custom_12843409/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/ert_eff_ic_m__custom_12843423/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/une_rt_m__custom_12843439/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/bop_c6_q__custom_12843390/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/gov_10q_ggnfa__custom_12843330/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/gov_10q_ggdebt__custom_12843232/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/gov_10q_ggfa__custom_12843307/default/table", 
       "https://ec.europa.eu/eurostat/databrowser/view/gov_10q_ggfa__custom_12843307/default/table", 
       "https://www.stoxx.com/data-index-details?symbol=V2TX", 
       "https://fred.stlouisfed.org/series/VIXCLS", 
       "https://fred.stlouisfed.org/series/OVXCLS", 
       "https://fred.stlouisfed.org/series/DCOILBRENTEU",
       "https://data.ecb.europa.eu/data/datasets/CISS/CISS.D.U2.Z0Z.4F.EC.SS_CIN.IDX",
       "https://fred.stlouisfed.org/series/DGS10")


variables %>%
  kbl(caption = '<div style="text-align:left; font-size:18px; font-weight:bold; color:#000000;">
  Table B: Variable descriptions and links
</div>', booktabs = T, align = "c") %>%
  kable_styling(bootstrap_options = "striped") %>%
  column_spec(3, link = url) %>% 
  save_kable("table_variable_descriptions.html")



