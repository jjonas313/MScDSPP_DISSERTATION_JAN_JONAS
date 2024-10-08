### EVENT STUDY

# test with eventstudies package
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(grid) 
library(devtools)
library(lmtest)
library(sandwich)
library(plm)
library(webshot2)

devtools::install_github("nipfpmf/eventstudies", ref="master")
library(eventstudies)

library(data.table)
library(xts)
library(zoo)

yield_spreads_data <- setDT(yield_spreads)


str(yield_spreads_data)

View(yield_spreads_data)

country_names <- names(yield_spreads_data)[3:18]

yield_spreads_data_long <- melt(
  yield_spreads_data,
  id.vars = c("date", "event", "Euro_area"),
  measure.vars = country_names,
  variable.name = "Country",
  value.name = "yield_spread"
)

setorder(yield_spreads_data_long, Country, date)

View(yield_spreads_data_long)

yield_spreads_data_long[, spread_change := c(NA, diff(yield_spread)), by = Country]
yield_spreads_data_long <- yield_spreads_data_long[!is.na(spread_change)]

# Create a data.table for events
events_df <- data.table(
  when = event_dates,
  name = "Event"
)

events_df[, when := as.Date(when)]

# Get all available trading dates
trading_dates <- sort(unique(yield_spreads_data_long$date))

# Function to adjust event dates to the previous available trading date
adjust_event_dates <- function(event_dates, trading_dates) {
  adjusted_dates <- sapply(event_dates, function(date) {
    while (!(date %in% trading_dates) && date >= min(trading_dates)) {
      date <- date - 1  # Move to the previous day
    }
    return(date)
  })
  return(as.Date(adjusted_dates))
}

# Adjust event dates
events_df[, adjusted_when := adjust_event_dates(when, trading_dates)]

# Update 'when' with adjusted dates
events_df[, when := adjusted_when]
events_df[, adjusted_when := NULL]

# Function to get N trading days before a given date
get_previous_trading_days <- function(date, trading_dates, n) {
  idx <- which(trading_dates < date)
  if (length(idx) >= n) {
    return(trading_dates[idx[(length(idx) - n + 1):length(idx)]])
  } else {
    return(trading_dates[idx])
  }
}

# Function to get N trading days after a given date
get_next_trading_days <- function(date, trading_dates, n) {
  idx <- which(trading_dates > date)
  if (length(idx) >= n) {
    return(trading_dates[idx[1:n]])
  } else {
    return(trading_dates[idx])
  }
}

# Number of trading days before and after the event, uncomment depending on wanted event window

# [-5, +5]
n_pre_event <- 5
n_post_event <- 5

# # [-1, +1]
# n_pre_event <- 1
# n_post_event <- 1

# # [-2, +2]
# n_pre_event <- 2
# n_post_event <- 2

# Create a list to store event window dates for each event
event_windows <- lapply(events_df$when, function(event_date) {
  pre_event_dates <- get_previous_trading_days(event_date, trading_dates, n_pre_event)
  post_event_dates <- get_next_trading_days(event_date, trading_dates, n_post_event)
  
  # Combine pre-event, event, and post-event dates
  event_window_dates <- c(pre_event_dates, event_date, post_event_dates)
  
  # Ensure dates are sorted and unique
  event_window_dates <- sort(unique(event_window_dates))
  
  return(event_window_dates)
})

# Assign event dates as names to the list
names(event_windows) <- as.character(events_df$when)


# Create a list of zoo objects for each country's returns
returns_zoo_list <- lapply(country_names, function(country) {
  country_data <- yield_spreads_data_long[Country == country]
  
  # Create a zoo object with a data frame to ensure it has at least one column
  zoo_df <- zoo(
    x = data.frame(spread_change = country_data$spread_change),
    order.by = country_data$date
  )
  
  return(zoo_df)
})
names(returns_zoo_list) <- country_names

# create sample average return
yield_spreads_data[, spread_euro_change := c(NA, diff(Euro_area))]

# Create a zoo object for the market returns
spread_avg_returns_zoo <- zoo(
  x = data.frame(spread_euro_change = yield_spreads_data$spread_euro_change),
  order.by = yield_spreads_data$date
)

# Initialize a list to store abnormal returns for all events
all_abnormal_returns <- list()

# Loop over each event date
for (i in seq_along(events_df$when)) {
  event_date <- events_df$when[i]
  event_window_dates <- event_windows[[as.character(event_date)]]
  
  cat("Processing event date:", as.character(event_date), "\n")
  
  # Initialize a list to store abnormal returns per country for the current event
  event_abnormal_returns <- list()
  
  # Loop over each country
  for (country in country_names) {
    cat("  Processing country:", country, "\n")
    
    # Extract country returns and market returns
    country_returns <- returns_zoo_list[[country]]
    country_dates <- index(country_returns)
    
    # Ensure the event window dates are available in the data
    available_event_dates <- event_window_dates[event_window_dates %in% country_dates]
    
    # Check if sufficient data is available in the event window
    if (length(available_event_dates) < (n_pre_event + n_post_event + 1)) {
      cat("    Insufficient data in event window for", country, "\n")
      next
    }
    
    # Define the estimation window dates (previous 20 days due to data availability)
    estimation_dates <- tail(trading_dates[trading_dates < min(available_event_dates)], 20)
    
    # Extract estimation data
    estimation_data <- merge.zoo(
      country_returns[estimation_dates],
      spread_avg_returns_zoo[estimation_dates],
      all = FALSE
    )
    colnames(estimation_data) <- c("CountryReturn", "MarketReturn")
    
    # Remove non-finite values
    estimation_data_clean <- estimation_data[is.finite(estimation_data$CountryReturn) & is.finite(estimation_data$MarketReturn), ]
    
    # Check if there is enough data after cleaning
    if (nrow(estimation_data_clean) < 20) {
      cat("    Insufficient data after cleaning for", country, "on event date", as.character(event_date), "\n")
      next
    }
    
    # Fit the model using cleaned data
    model <- lm(CountryReturn ~ MarketReturn, data = as.data.frame(estimation_data_clean))
    
    # Extract event window data
    event_data <- merge.zoo(
      country_returns[available_event_dates],
      spread_avg_returns_zoo[available_event_dates],
      all = FALSE
    )
    colnames(event_data) <- c("CountryReturn", "MarketReturn")
    
    # Remove non-finite values from event_data
    event_data_clean <- event_data[is.finite(event_data$CountryReturn) & is.finite(event_data$MarketReturn), ]
    
    if (nrow(event_data_clean) == 0) {
      cat("    No valid event data for", country, "on event date", as.character(event_date), "\n")
      next
    }
    
    # Calculate expected returns
    expected_returns <- predict(model, newdata = as.data.frame(event_data_clean))
    
    # Calculate abnormal returns
    abnormal_returns <- event_data_clean$CountryReturn - expected_returns
    
    # Store abnormal returns with event date
    event_abnormal_returns[[country]] <- data.table(
      date = index(event_data_clean),
      Country = country,
      EventDate = event_date,
      AbnormalReturn = as.numeric(abnormal_returns)
    )
  }
  
  # Combine abnormal returns for all countries for the current event
  if (length(event_abnormal_returns) > 0) {
    all_abnormal_returns[[as.character(event_date)]] <- rbindlist(event_abnormal_returns)
  } else {
    cat("No abnormal returns calculated for event date:", as.character(event_date), "\n")
  }
}

# Combine all abnormal returns into one data.table
if (length(all_abnormal_returns) > 0) {
  abnormal_returns_all_events <- rbindlist(all_abnormal_returns)
} else {
  abnormal_returns_all_events <- data.table()
}

# for event windows [-1, +1] and [-2, +2], uncomment depending on the event window

# # [-1, +1]
# if (length(all_abnormal_returns) > 0) {
#   abnormal_returns_all_events_1 <- rbindlist(all_abnormal_returns)
# } else {
#   abnormal_returns_all_events_1 <- data.table()
# }

# # [-2, +2]
# if (length(all_abnormal_returns) > 0) {
#   abnormal_returns_all_events_2 <- rbindlist(all_abnormal_returns)
# } else {
#   abnormal_returns_all_events_2 <- data.table()
# }


# Check the result
head(abnormal_returns_all_events)

# Get unique list of countries and event dates to check if correct
unique_countries <- unique(abnormal_returns_all_events$Country)
unique_events <- unique(abnormal_returns_all_events$EventDate)

# results for event window [-5, +5]

#Calculate CARs for Each Country and Event 
car_data <- abnormal_returns_all_events[, .(
  CAR = sum(AbnormalReturn, na.rm = TRUE)
), by = .(Country, EventDate)]

#Calculate AARs for Each Country and Event
aar_data <- abnormal_returns_all_events[, .(
  AAR = mean(AbnormalReturn, na.rm = TRUE)
), by = .(Country, EventDate)]

#Calculate CAARs for each event
caar_data_event <- car_data[, .(
  CAAR = mean(CAR, na.rm = TRUE)
), by = EventDate]

#Calculate CAARs for each country
caar_data_country <- car_data[, .(
  CAAR = mean(CAR, na.rm = TRUE)
), by = Country]


# Define a function to compute clustered standard errors and confidence intervals for each country to adjust for correlation between countries
clustered_se_t_test_country <- function(data, cluster_var) {
  model <- lm(CAR ~ 1, data = data) # Regression with no predictors for CAR
  # Clustered standard errors
  clustered_se <- coeftest(model, vcov = vcovCL, cluster = data[[cluster_var]])
  
  # Calculate 95% confidence interval
  mean_CAR <- mean(data$CAR, na.rm = TRUE)
  sd_CAR <- sd(data$CAR, na.rm = TRUE)
  n <- nrow(data)
  t_critical <- qt(0.975, df = n - 1) # 95% confidence level
  ci_lower <- mean_CAR - t_critical * (sd_CAR / sqrt(n))
  ci_upper <- mean_CAR + t_critical * (sd_CAR / sqrt(n))
  
  return(list(t_stat = clustered_se[1, "t value"], p_value = clustered_se[1, "Pr(>|t|)"], ci_lower = ci_lower, ci_upper = ci_upper))
}

# Group the data by Country and apply the clustered standard error and CI calculation
car_stats_country_clustered <- car_data %>%
  group_by(Country) %>%
  do({
    test_results <- clustered_se_t_test_country(., cluster_var = "EventDate")
    
    data.frame(
      Country = .$Country[1],
      mean_CAR = mean(.$CAR, na.rm = TRUE),
      sd_CAR = sd(.$CAR, na.rm = TRUE),
      t_stat = test_results$t_stat,
      p_value = test_results$p_value,
      ci_lower = test_results$ci_lower,
      ci_upper = test_results$ci_upper
    )
  })

# Test statistics: 

# Perform t-tests per event
event_t_tests <- car_data[, {
  t_result <- t.test(CAR, mu = 0, alternative = "two.sided", conf.level = 0.95)
  list(
    MeanCAR = mean(CAR, na.rm = TRUE),
    StdDevCAR = sd(CAR, na.rm = TRUE),
    t_statistic = t_result$statistic,
    p_value = t_result$p.value,
    conf_lower = t_result$conf.int[1],
    conf_upper = t_result$conf.int[2]
  )
}, by = EventDate]


### repeat all code above for event windows [-1, +1] and [-2, +2]

# results for event window [-1, +1] -- we just change n_pre_event and n_post_event to be 1 and then run the same code


#Calculate CARs for Each Country and Event 
car_data_1 <- abnormal_returns_all_events_1[, .(
  CAR = sum(AbnormalReturn, na.rm = TRUE)
), by = .(Country, EventDate)]

#Calculate AARs for Each Country and Event
aar_data_1 <- abnormal_returns_all_events_1[, .(
  AAR = mean(AbnormalReturn, na.rm = TRUE)
), by = .(Country, EventDate)]

#Calculate CAARs for each event
caar_data_event_1 <- car_data_1[, .(
  CAAR = mean(CAR, na.rm = TRUE)
), by = EventDate]

#Calculate CAARs for each country
caar_data_country_1 <- car_data_1[, .(
  CAAR = mean(CAR, na.rm = TRUE)
), by = Country]


# Define a function to compute clustered standard errors and confidence intervals for each country to adjust for correlation between countries
clustered_se_t_test_country_1 <- function(data, cluster_var) {
  model <- lm(CAR ~ 1, data = data) # Regression with no predictors for CAR
  # Clustered standard errors
  clustered_se <- coeftest(model, vcov = vcovCL, cluster = data[[cluster_var]])
  
  # Calculate 95% confidence interval
  mean_CAR <- mean(data$CAR, na.rm = TRUE)
  sd_CAR <- sd(data$CAR, na.rm = TRUE)
  n <- nrow(data)
  t_critical <- qt(0.975, df = n - 1) # 95% confidence level
  ci_lower <- mean_CAR - t_critical * (sd_CAR / sqrt(n))
  ci_upper <- mean_CAR + t_critical * (sd_CAR / sqrt(n))
  
  return(list(t_stat = clustered_se[1, "t value"], p_value = clustered_se[1, "Pr(>|t|)"], ci_lower = ci_lower, ci_upper = ci_upper))
}

# Group the data by Country and apply the clustered standard error and CI calculation
car_stats_country_clustered_1 <- car_data_1 %>%
  group_by(Country) %>%
  do({
    test_results <- clustered_se_t_test_country(., cluster_var = "EventDate")
    
    data.frame(
      Country = .$Country[1],
      mean_CAR = mean(.$CAR, na.rm = TRUE),
      sd_CAR = sd(.$CAR, na.rm = TRUE),
      t_stat = test_results$t_stat,
      p_value = test_results$p_value,
      ci_lower = test_results$ci_lower,
      ci_upper = test_results$ci_upper
    )
  })

# Test statistics: 

# Perform t-tests per event
event_t_tests_1 <- car_data_1[, {
  t_result <- t.test(CAR, mu = 0, alternative = "two.sided", conf.level = 0.95)
  list(
    MeanCAR = mean(CAR, na.rm = TRUE),
    StdDevCAR = sd(CAR, na.rm = TRUE),
    t_statistic = t_result$statistic,
    p_value = t_result$p.value,
    conf_lower = t_result$conf.int[1],
    conf_upper = t_result$conf.int[2]
  )
}, by = EventDate]


# results for event window [-2, +2] -- we just change n_pre_event and n_post_event to be 1 and then run the same code

#Calculate CARs for Each Country and Event 
car_data_2 <- abnormal_returns_all_events_2[, .(
  CAR = sum(AbnormalReturn, na.rm = TRUE)
), by = .(Country, EventDate)]

#Calculate AARs for Each Country and Event
aar_data_2 <- abnormal_returns_all_events_2[, .(
  AAR = mean(AbnormalReturn, na.rm = TRUE)
), by = .(Country, EventDate)]

#Calculate CAARs for each event
caar_data_event_2 <- car_data_2[, .(
  CAAR = mean(CAR, na.rm = TRUE)
), by = EventDate]


#Calculate CAARs for each country
caar_data_country_2 <- car_data_2[, .(
  CAAR = mean(CAR, na.rm = TRUE)
), by = Country]



# Define a function to compute clustered standard errors and confidence intervals for each country to adjust for correlation between countries
clustered_se_t_test_country_2 <- function(data, cluster_var) {
  model <- lm(CAR ~ 1, data = data) # Regression with no predictors for CAR
  # Clustered standard errors
  clustered_se <- coeftest(model, vcov = vcovCL, cluster = data[[cluster_var]])
  
  # Calculate 95% confidence interval
  mean_CAR <- mean(data$CAR, na.rm = TRUE)
  sd_CAR <- sd(data$CAR, na.rm = TRUE)
  n <- nrow(data)
  t_critical <- qt(0.975, df = n - 1) # 95% confidence level
  ci_lower <- mean_CAR - t_critical * (sd_CAR / sqrt(n))
  ci_upper <- mean_CAR + t_critical * (sd_CAR / sqrt(n))
  
  return(list(t_stat = clustered_se[1, "t value"], p_value = clustered_se[1, "Pr(>|t|)"], ci_lower = ci_lower, ci_upper = ci_upper))
}

# Group the data by Country and apply the clustered standard error and CI calculation
car_stats_country_clustered_2 <- car_data_2 %>%
  group_by(Country) %>%
  do({
    test_results <- clustered_se_t_test_country(., cluster_var = "EventDate")
    
    data.frame(
      Country = .$Country[1],
      mean_CAR = mean(.$CAR, na.rm = TRUE),
      sd_CAR = sd(.$CAR, na.rm = TRUE),
      t_stat = test_results$t_stat,
      p_value = test_results$p_value,
      ci_lower = test_results$ci_lower,
      ci_upper = test_results$ci_upper
    )
  })

# Test statistics: 

# Perform t-tests per event
event_t_tests_2 <- car_data_2[, {
  t_result <- t.test(CAR, mu = 0, alternative = "two.sided", conf.level = 0.95)
  list(
    MeanCAR = mean(CAR, na.rm = TRUE),
    StdDevCAR = sd(CAR, na.rm = TRUE),
    t_statistic = t_result$statistic,
    p_value = t_result$p.value,
    conf_lower = t_result$conf.int[1],
    conf_upper = t_result$conf.int[2]
  )
}, by = EventDate]


# prepare CAR data for panel data analysis
car_data <- car_data %>%
  mutate(date = as.Date(EventDate, format = "%Y-%m-%d")) 

car_data$year_month <- format(car_data$date, "%Y-%m")


car_data_1 <- car_data_1 %>%
  mutate(date = as.Date(EventDate, format = "%Y-%m-%d"))

car_data_1$year_month <- format(car_data_1$date, "%Y-%m")


car_data_2 <- car_data_2 %>%
  mutate(date = as.Date(EventDate, format = "%Y-%m-%d"))

car_data_2$year_month <- format(car_data_2$date, "%Y-%m")



### Tables and graphs for event study results:

# Load necessary libraries

### Figure 1: Uncertainty in ECB Monetary Policy Accounts
uncertainty_plot <- ggplot(uncert_prop, aes(x = date, y = proportion_uncertain)) +
  geom_line() +
  labs(
    title = "Figure 1: Uncertainty in ECB Monetary Policy Accounts",
    x = "",
    y = "Proportion of Uncertain Sentences"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

ggsave("figure1_unceratainty_prop.png", uncertainty_plot, width = 839, height = 418, units = "px", dpi = 100)


### Table 2: Average CARs per country for each event window
# Create a table by combining the three datasets
car_table <- data.frame(
  Country = car_stats_country_clustered$Country,
  `[-5,+5]` = paste0(round(car_stats_country_clustered$mean_CAR, 4), " (", round(car_stats_country_clustered$p_value, 4), ")"),
  `[-1,+1]` = paste0(round(car_stats_country_clustered_1$mean_CAR, 4), " (", round(car_stats_country_clustered_1$p_value, 4), ")"),
  `[-2,+2]` = paste0(round(car_stats_country_clustered_2$mean_CAR, 4), " (", round(car_stats_country_clustered_2$p_value, 4), ")")
)

colnames(car_table) <- c("Country", "[-5,+5]", "[-1,+1]", "[-2,+2]")

View(car_table)

# Add a title to the table and print it using knitr for a nicely formatted output
car_table %>%
  kbl(caption = '<div style="text-align:left; font-size:18px; font-weight:bold; color:#000000;">
        Table 2: Average CARs per Country
      </div>', align = 'c') %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  add_header_above(c(" " = 1, "Event Window" = 3)) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(general = "CAR values are displayed with p-values in parentheses. Statistically significant values are shown in bold.", 
           general_title = "Note: ", 
           footnote_as_chunk = TRUE)
# exported as png



### Figure 2: Average CARs per country for each event window
# Plot average CARs for each country
CAR_plot <- ggplot(car_stats_country_clustered, aes(x = reorder(Country, -mean_CAR), y = mean_CAR)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "[-5,+5]",
       x = "Country",
       y = "Average CAR")


CAR_plot_1 <- ggplot(car_stats_country_clustered_1, aes(x = reorder(Country, -mean_CAR), y = mean_CAR)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "[-1,+1]",
       x = "Country",
       y = "Average CAR")


CAR_plot_2 <- ggplot(car_stats_country_clustered_2, aes(x = reorder(Country, -mean_CAR), y = mean_CAR)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "[-2,+2]",
       x = "Country",
       y = "Average CAR")

combined_plot_country <- grid.arrange(
  CAR_plot, CAR_plot_1, CAR_plot_2, 
  ncol = 1, 
  top = textGrob("Figure 3: Average CARs per Country", gp = gpar(fontsize = 16, fontface = "bold"), just = "right") 
)

# Save the plot as a PNG file
ggsave("event_combined_plot_country.png", combined_plot_country, width = 750, height = 850, units = "px", dpi = 97)


### Table 3: Average CARs across countries per event
# Filter the events with p-values < 0.05 for each dataset
significant_events_1 <- event_t_tests %>% filter(p_value < 0.05)
significant_events_2 <- event_t_tests_1 %>% filter(p_value < 0.05)
significant_events_3 <- event_t_tests_2 %>% filter(p_value < 0.05)

# Combine the three datasets by EventDate using a full join to retain all significant events
combined_events <- full_join(significant_events_1, significant_events_2, by = "EventDate", suffix = c("_1", "_2")) %>%
  full_join(significant_events_3, by = "EventDate") %>%
  select(EventDate, MeanCAR_1 = MeanCAR_1, MeanCAR_2 = MeanCAR_2, MeanCAR_3 = MeanCAR, 
         p_value_1 = p_value_1, p_value_2 = p_value_2, p_value_3 = p_value)

# Create a table with MeanCAR values and p-values in parentheses
combined_table <- combined_events %>%
  mutate(`Event Window [-5,+5]` = ifelse(!is.na(MeanCAR_1), paste0(round(MeanCAR_1, 4), " (", round(p_value_1, 4), ")"), ""),
         `Event Window [-1,+1]` = ifelse(!is.na(MeanCAR_2), paste0(round(MeanCAR_2, 4), " (", round(p_value_2, 4), ")"), ""),
         `Event Window [-2,+2]` = ifelse(!is.na(MeanCAR_3), paste0(round(MeanCAR_3, 4), " (", round(p_value_3, 4), ")"), "")) %>%
  select(EventDate, `Event Window [-5,+5]`, `Event Window [-1,+1]`, `Event Window [-2,+2]`)

colnames(combined_table) <- c("Event Date", "[-5,+5]", "[-1,+1]", "[-2,+2]")

# order by event date
combined_table <- combined_table[order(combined_table$`Event Date`),]

# Generate table
kable(combined_table, caption = '<div style="text-align:left; font-size:18px; font-weight:bold; color:#000000;">
        Table 3: Significant CARs per Event
      </div>', align = 'c', format = "html") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  footnote(general = "Note: CAR values are displayed with p-values in parentheses. CAR values 
  are averages across all countries per event", 
           general_title = "Note: ", 
           footnote_as_chunk = TRUE)
# exported as png

combined_events_full <- full_join(event_t_tests, event_t_tests_1, by = "EventDate", suffix = c("_1", "_2")) %>%
  full_join(event_t_tests_2, by = "EventDate") %>%
  select(EventDate, MeanCAR_1 = MeanCAR_1, MeanCAR_2 = MeanCAR_2, MeanCAR_3 = MeanCAR, 
         p_value_1 = p_value_1, p_value_2 = p_value_2, p_value_3 = p_value)

combined_events_full <- combined_events_full %>%
  mutate(`Event Window [-5,+5]` = ifelse(!is.na(MeanCAR_1), paste0(round(MeanCAR_1, 4), " (", round(p_value_1, 4), ")"), ""),
         `Event Window [-1,+1]` = ifelse(!is.na(MeanCAR_2), paste0(round(MeanCAR_2, 4), " (", round(p_value_2, 4), ")"), ""),
         `Event Window [-2,+2]` = ifelse(!is.na(MeanCAR_3), paste0(round(MeanCAR_3, 4), " (", round(p_value_3, 4), ")"), "")) %>%
  select(EventDate, `Event Window [-5,+5]`, `Event Window [-1,+1]`, `Event Window [-2,+2]`)

colnames(combined_events_full) <- c("Event Date", "[-5,+5]", "[-1,+1]", "[-2,+2]")

combined_events_full <- combined_events_full[order(combined_events_full$`Event Date`),]

combined_events_full_1 <- combined_events_full %>% 
  filter(`Event Date` < "2019-10-10")

combined_events_full_2 <- combined_events_full %>%
  filter(`Event Date` >= "2019-10-10")


kable(combined_events_full_1, caption = '<div style="text-align:left; font-size:18px; font-weight:bold; color:#000000;">
        Table C: Average CARs per Event across Countries
      </div>', align = 'c', format = "html") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  footnote(general = "Note: CAR values are displayed with p-values in parentheses. CAR values 
  are averaged across all countries per event", 
           general_title = "Note: ", 
           footnote_as_chunk = TRUE) %>%
  save_kable("combined_events_full_1.png")

kable(combined_events_full_2, caption = '<div style="text-align:left; font-size:18px; font-weight:bold; color:#000000;">
        Table C - Continued
      </div>', align = 'c', format = "html") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  footnote(general = "Note: CAR values are displayed with p-values in parentheses. CAR values 
  are averaged across all countries per event", 
           general_title = "Note: ", 
           footnote_as_chunk = TRUE) %>%
  save_kable("combined_events_full_2.png")


### Figure 3: Average CARs across countries per event

create_event_plot <- function(data, event_window_title) {
  data %>%
    mutate(Significant = p_value < 0.05) %>%
    ggplot(aes(x = EventDate, y = MeanCAR, fill = Significant)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("black", "red")) +  # Highlight significant bars in red
    theme_minimal() +
    labs(title = event_window_title,
         x = "",
         y = "Average CAR") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability 
          legend.position = "none")  # Rotate x-axis labels for better readability
}

event_plot <- create_event_plot(event_t_tests, "[-5,+5]")
event_plot_1 <- create_event_plot(event_t_tests_1, "[-1,+1]")
event_plot_2 <- create_event_plot(event_t_tests_2, "[-2,+2]")

combined_plot_event <- grid.arrange(
  event_plot, event_plot_1, event_plot_2, 
  ncol = 1,  # Arrange the plots vertically in a single column
  top = textGrob("Figure 4: Average CARs per Event", gp = gpar(fontsize = 16, fontface = "bold"), just = "right") 
)

ggsave("combined_plot_event.png", combined_plot_event, width = 800, height = 800, units = "px", dpi = 110)


