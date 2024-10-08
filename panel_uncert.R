### Panel data for measuring effect of uncertainty

library(tidyverse)
library(plm)
library(stargazer)
library(lmtest)
library(car)
library(corrplot)

options(scipen = 999)

final_data_5 <- car_data %>%
  left_join(monthly_data_lagged, by = c("Country", "year_month")) 

View(final_data_5)

final_data_5 <- final_data_5 %>% 
  select(-c(date.y, EventDate)) 

final_data_5 <- rename(final_data_5, date = date.x)

uncert_prop <- uncert_prop %>% 
  select(c(date, proportion_uncertain))

final_data_5 <- final_data_5 %>% 
  left_join(uncert_prop, by = "date") %>% 
  select(-year_month)
 
# merge with daily data
final_data_5 <- merge(final_data_5, daily_data_5, by = "date", all.x = TRUE)

# round all numeric values to 4 decimal places
final_data_5 <- as.data.frame(final_data_5)

final_data_5[, -c(1, 2)] <- round(final_data_5[, -c(1,2)], 4)

View(final_data_5)

control_names <- names(final_data_5)[-c(1,2,3,18)]

# remove highly correlated variables
final_data_5 <- final_data_5 %>% 
  select(-c(long_term_debt_securities, consolidated_gross_debt_percent_gdp, long_term_debt_percent_gdp, short_term_debt_percent_gdp))


# add indicator variable for Covid-19 pandemic
final_data_5$covid <- ifelse(final_data_5$date >= "2020-03-01" & final_data_5$date <= "2021-10-31", 1, 0)

# add indicator variable for invasion of Ukraine
final_data_5$ukr <- ifelse(final_data_5$date >= "2021-11-01" & final_data_5$date <= "2023-11-01", 1, 0)


# repeat for event window [-1, +1]

final_data_1 <- car_data_1 %>%
  left_join(monthly_data_lagged, by = c("Country", "year_month")) 

View(final_data_1)

final_data_1 <- final_data_1 %>% 
  select(-c(date.y, EventDate)) 

final_data_1 <- rename(final_data_1, date = date.x)

uncert_prop <- uncert_prop %>% 
  select(c(date, proportion_uncertain))

final_data_1 <- final_data_1 %>% 
  left_join(uncert_prop, by = "date") %>% 
  select(-year_month)

# merge with daily data
final_data_1 <- merge(final_data_1, daily_data_1, by = "date", all.x = TRUE)

# round all numeric values to 4 decimal places
final_data_1 <- as.data.frame(final_data_1)

final_data_1[, -c(1, 2)] <- round(final_data_1[, -c(1,2)], 4)

View(final_data_1)

# remove highly correlated variables
final_data_1 <- final_data_1 %>% 
  select(-c(long_term_debt_securities, consolidated_gross_debt_percent_gdp, long_term_debt_percent_gdp, short_term_debt_percent_gdp))

# add indicator variable for Covid-19 pandemic
final_data_1$covid <- ifelse(final_data_1$date >= "2020-03-01" & final_data_1$date <= "2021-10-31", 1, 0)

# add indicator variable for invasion of Ukraine
final_data_1$ukr <- ifelse(final_data_1$date >= "2021-11-01" & final_data_1$date <= "2023-11-01", 1, 0)


# repeat for event window [-2, +2]
final_data_2 <- car_data_2 %>%
  left_join(monthly_data_lagged, by = c("Country", "year_month")) 

View(final_data_2)

final_data_2 <- final_data_2 %>% 
  select(-c(date.y, EventDate)) 

final_data_2 <- rename(final_data_2, date = date.x)

uncert_prop <- uncert_prop %>% 
  select(c(date, proportion_uncertain))

final_data_2 <- final_data_2 %>% 
  left_join(uncert_prop, by = "date") %>% 
  select(-year_month)

# merge with daily data
final_data_2 <- merge(final_data_2, daily_data_2, by = "date", all.x = TRUE)

# round all numeric values to 4 decimal places
final_data_2 <- as.data.frame(final_data_2)

final_data_2[, -c(1, 2)] <- round(final_data_2[, -c(1,2)], 4)

View(final_data_2)

# remove highly correlated varibales 
final_data_2 <- final_data_2 %>% 
  select(-c(long_term_debt_securities, consolidated_gross_debt_percent_gdp, long_term_debt_percent_gdp, short_term_debt_percent_gdp))

# add indicator variable for Covid-19 pandemic
final_data_2$covid <- ifelse(final_data_2$date >= "2020-03-01" & final_data_2$date <= "2021-10-31", 1, 0)

# add indicator variable for invasion of Ukraine
final_data_2$ukr <- ifelse(final_data_2$date >= "2021-11-01" & final_data_2$date <= "2023-11-01", 1, 0)
# create formula for control variables

control_names_final <- names(final_data_5)[-c(1,2,3,14)]

control_formula <- paste(control_names_final, collapse = " + ")

model_formula <- as.formula(paste("CAR ~ proportion_uncertain +", control_formula))

# we check vif values for multicollinearity to see if any variables should be ommited


vif_values <- vif(lm(model_formula, data = final_data_5))
print(vif_values)

# significant vif values for hicp and gdp
# check correlation of the two variables
cor_hicp_gdp <- cor(final_data_5$hicp, final_data_5$gdp)
print(cor_hicp_gdp)
# correlation of 0.95 so we remove gdp from the model as hicp is more informative

# remove "gdp" from control_names_final 
control_names_final <- control_names_final[-8]

control_formula_final <- paste(control_names_final, collapse = " + ")

model_formula_final <- as.formula(paste("CAR ~ proportion_uncertain +", control_formula_final)) 


#### Fixed effects model with country fixed effects


# format names of variables for stargazer tables


# PLM:

# PLM_5 full
plm_CAR_5 <- plm(model_formula_final, data = final_data_5, 
                 index = c("Country"), model = "within")

# bp test for heteroskedasticity
bp_test <- bptest(plm_CAR_5, studentize = FALSE)
print(bp_test) # pavalue significant so heteroskedacity present

#  Breusch-Godfrey Test for autocorrelation
bg_test <- bgtest(plm_CAR_5, order = 1)
print(bg_test) # p value significant so autocorrelation present 

bg_test_lag2 <- bgtest(plm_CAR_5, order = 2)
print(bg_test_lag2) # p value significant so autocorrelation present

# cd test for cross-sectional dependence
cd_test <- pcdtest(plm_CAR_5, test = "cd")

print(cd_test) # p value not significant so no cross-sectional dependence detected

# due to detected heteroskedasticity and autocorrelation use cluster robust standard errors
plm_CAR_5$vcov <- vcovHC(plm_CAR_5, method = "white1", type = "HC1", cluster = c("group", "time"))

# PLM_1 full
plm_CAR_1 <- plm(model_formula_final, data = final_data_1, 
             index = c("Country"), model = "within")

plm_CAR_1$vcov <- vcovHC(plm_CAR_1,  method = "white1", type = "HC1", cluster = c("group", "time"))

# PLM_2 full
plm_CAR_2 <- plm(model_formula_final, data = final_data_2, 
                 index = c("Country"), model = "within")

plm_CAR_2$vcov <- vcovHC(plm_CAR_2,  method = "white1", type = "HC1", cluster = c("group", "time"))

stargazer(plm_CAR_1, plm_CAR_2, plm_CAR_5, type = "html", title = "Table 4: Fixed Effects Model with Country Fixed Effects", 
          column.labels = c("[-1,+1]", "[-2,+2]", "[-5,+5]"),
          model.numbers = FALSE, 
          single.row = TRUE, 
          align = TRUE,
          omit = c("covid", "ukr"),
          covariate.labels = c("Proportion of Uncertain Sentences", "HICP", 
                               "Industrial Production Growth", "Real Effective Exchange Rate", 
                               "Unemployment Rate", "Current Account Balance", "Government Deficit (% of GDP)",
                               "Total Debt (% of GDP)", "Total Debt Securities Issued", "Short Term Debt Securities Issued",
                               "STOXX50 Volatility Index", "VIX Index", "Crude Oil Volatility Index", "Crude Oil Prices: Brent - Europe", 
                               "CISS", "US-Germany Yield Spread"),
          notes = "Table presents results of Country Fixed Effects Estimation using cluster robust standard errors to account for serial correlation and heteroscedasticity. Significance Levels: *p<0.1; **p<0.05; ***p<0.01", 
          notes.append = FALSE, notes.align = "l",
          out = "table4.html")


### models for covid-19 and ukraine invasion

## Covid-19
final_data_5_cvd <- final_data_5 %>% 
  filter(covid == 1)

final_data_1_cvd <- final_data_1 %>% 
  filter(covid == 1)

final_data_2_cvd <- final_data_2 %>% 
  filter(covid == 1)

plm_CAR_cvd_5 <- plm(model_formula_final, data = final_data_5_cvd, 
                     index = c("Country"), model = "within")

plm_CAR_cvd_5$vcov <- vcovHC(plm_CAR_cvd_5,  method = "arellano", type = "HC1", cluster = c("group", "time"))


plm_CAR_cvd_1 <- plm(model_formula_final, data = final_data_1_cvd, 
                 index = c("Country"), model = "within")

plm_CAR_cvd_1$vcov <- vcovHC(plm_CAR_cvd_1,  method = "arellano", type = "HC1", cluster = c("group", "time"))


plm_CAR_cvd_2 <- plm(model_formula_final, data = final_data_2_cvd, 
                 index = c("Country"), model = "within")

plm_CAR_cvd_2$vcov <- vcovHC(plm_CAR_cvd_2,  method = "arellano", type = "HC1", cluster = c("group", "time"))

stargazer(plm_CAR_cvd_1, plm_CAR_cvd_2, plm_CAR_cvd_5, type = "html", title = "Table 5: Fixed Effects Model with Country Fixed Effects for Covid-19", 
          column.labels = c("[-1,+1]", "[-2,+2]", "[-5,+5]"),
          model.numbers = FALSE, 
          single.row = TRUE, 
          align = TRUE,
          omit = c("covid", "ukr"),
          covariate.labels = c("Proportion of Uncertain Sentences", "HICP", 
                               "Industrial Production Growth", "Real Effective Exchange Rate", 
                               "Unemployment Rate", "Current Account Balance", "Government Deficit (% of GDP)",
                               "Total Debt (% of GDP)", "Total Debt Securities Issued", "Short Term Debt Securities Issued",
                               "STOXX50 Volatility Index", "VIX Index", "Crude Oil Volatility Index", "Crude Oil Prices: Brent - Europe", 
                               "CISS", "US-Germany Yield Spread"),
          notes = "Table presents results of Country Fixed Effects Estimation using cluster robust standard errors to account for serial correlation and heteroscedasticity. Significance Levels: *p<0.1; **p<0.05; ***p<0.01", 
          notes.append = FALSE, notes.align = "l",
          out = "table5.html")


## Ukraine Invasion
final_data_5_ukr <- final_data_5 %>% 
  filter(ukr == 1)

final_data_1_ukr <- final_data_1 %>% 
  filter(ukr == 1)

final_data_2_ukr <- final_data_2 %>% 
  filter(ukr == 1)

plm_CAR_ukr_5 <- plm(model_formula_final, data = final_data_5_ukr, 
                     index = c("Country"), model = "within")

plm_CAR_ukr_5$vcov <- vcovHC(plm_CAR_ukr_5,  method = "arellano", type = "HC1", cluster = c("group", "time"))


plm_CAR_ukr_1 <- plm(model_formula_final, data = final_data_1_ukr, 
                     index = c("Country"), model = "within")

plm_CAR_ukr_1$vcov <- vcovHC(plm_CAR_ukr_1,  method = "arellano", type = "HC1", cluster = c("group", "time"))


plm_CAR_ukr_2<- plm(model_formula_final, data = final_data_2_ukr, 
                     index = c("Country"), model = "within")

plm_CAR_ukr_2$vcov <- vcovHC(plm_CAR_ukr_2,  method = "arellano", type = "HC1", cluster = c("group", "time"))

stargazer(plm_CAR_ukr_1, plm_CAR_ukr_2, plm_CAR_ukr_5, type = "html", title = "Table 6: Fixed Effects Model with Country Fixed Effects for the Ukraine Invasion", 
          column.labels = c("[-1,+1]", "[-2,+2]", "[-5,+5]"),
          model.numbers = FALSE, 
          single.row = TRUE, 
          align = TRUE,
          omit = c("covid", "ukr"),
          covariate.labels = c("Proportion of Uncertain Sentences", "HICP", 
                               "Industrial Production Growth", "Real Effective Exchange Rate", 
                               "Unemployment Rate", "Current Account Balance", "Government Deficit (% of GDP)",
                               "Total Debt (% of GDP)", "Total Debt Securities Issued", "Short Term Debt Securities Issued",
                               "STOXX50 Volatility Index", "VIX Index", "Crude Oil Volatility Index", "Crude Oil Prices: Brent - Europe", 
                               "CISS", "US-Germany Yield Spread"),
          notes = "Table presents results of Country Fixed Effects Estimation using cluster robust standard errors to account for serial correlation and heteroscedasticity. Significance Levels: *p<0.1; **p<0.05; ***p<0.01", 
          notes.append = FALSE, notes.align = "l", 
          out = "table6.html")
