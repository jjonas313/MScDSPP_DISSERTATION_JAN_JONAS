### Preparing and cleaning data

# load required libraries
library(tidyverse)
library(readxl)

### EMU Convergence Criteria Data -- 10 year government bonds yields

# load xlsx file
EMU_yields <- read_xlsx("10yr_govt_bond_yield.xlsx", sheet = "Sheet 1", skip = 7)


EMU_yields <- EMU_yields[-c(1),]


# change name of first column to Date
names(EMU_yields)[1] <- "date"
names(EMU_yields)[2] <- "Euro_area"

EMU_yields$date <- as.Date(EMU_yields$date, format = "%Y-%m-%d")

# remove empty or unnecessary rows and columns
EMU_yields <- EMU_yields[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39, 41)]
EMU_yields <- EMU_yields[-c(2459:2463),]

# change type of columns to numeric except for date
EMU_yields <- EMU_yields %>% mutate(across(where(is.character), as.numeric))


#### 10 year German Bunds Yield

german_bunds <- read.csv("10year_german_bunds.csv")

german_bunds <- german_bunds[-c(1:8),]

colnames(german_bunds) <- c("date", "bunds_yield", "missing")

german_bunds$date <- as.Date(german_bunds$date, format = "%Y-%m-%d")

# remove rows with "." in column bunds_yield
german_bunds <- german_bunds[german_bunds$bunds_yield != ".",]

german_bunds <- german_bunds[,-3]

german_bunds$bunds_yield <- as.numeric(german_bunds$bunds_yield)

View(german_bunds)

#### 10 year US Treasury yield

us_treasury <- read.csv("10year_US_treasury.csv")

colnames(us_treasury) <- c("date", "us_yield")

us_treasury$date <- as.Date(us_treasury$date, format = "%Y-%m-%d")

us_treasury <- us_treasury[us_treasury$us_yield != ".",]

us_treasury$us_yield <- as.numeric(us_treasury$us_yield)

View(us_treasury)


#### Debt Securities Issued -- Liquidity proxy, decide between quarterly and monthly

## quarterly -- source Eurostat
debt_securities_issued_quarterly <- read_xlsx("eurostat_debt_issues_quarterly.xlsx", sheet = "Sheet 1", skip = 11) 

debt_securities_issued_quarterly <- debt_securities_issued_quarterly[-c(1,2,725:729), -c(4,6,8)]

colnames(debt_securities_issued_quarterly) <- c("date", "country", "debt_securities", "short_term_debt_securities", "long_term_debt_securities")

View(debt_securities_issued_quarterly)

#### GDP --- Quarterly

gdp <- read_xlsx("gdp.xlsx", sheet = "Sheet 1", skip = 8)
 
gdp <- gdp[-c(1,782:786),-c(4,6)]

colnames(gdp) <- c("country", "date", "gdp_current_prices(million_euro)", "gdp_price_index(2015=100)")

View(gdp)


#### Government debt as percetnage of GDP -- Quarterly

govt_debt <- read_xlsx("debt_percent_gdp_quarterly.xlsx", sheet = "Sheet 1", skip = 8)

govt_debt <- govt_debt[-c(1,724:728), -c(4,6,8,10)]

colnames(govt_debt) <- c("country", "date", "debt_total_percent_gdp", "short_term_debt_percent_gdp", "long_term_debt_percent_gdp", "consolidated_gross_debt_percent_gdp")

View(govt_debt)

#### Government Deficit -- QUARTERLY

govt_deficit <- read_xlsx("govt_deficit.xlsx", sheet = "Sheet 1", skip = 8)

govt_deficit <- govt_deficit[-c(1,2,725:729), -c(4,6,8,10)]

colnames(govt_deficit) <- c("date", "country", "expenditure_total_million_euro", "expenditure_total_percent_gdp", "revenue_total_million_euro", "revenue_total_percent_egdp")

# apart from date and country, change all columns to numeric
govt_deficit$expenditure_total_million_euro <- as.numeric(govt_deficit$expenditure_total_million_euro)
govt_deficit$expenditure_total_percent_gdp <- as.numeric(govt_deficit$expenditure_total_percent_gdp)
govt_deficit$revenue_total_million_euro <- as.numeric(govt_deficit$revenue_total_million_euro)
govt_deficit$revenue_total_percent_egdp <- as.numeric(govt_deficit$revenue_total_percent_egdp)

govt_deficit$deficit_total <- govt_deficit$expenditure_total_million_euro - govt_deficit$revenue_total_million_euro
govt_deficit$deficit_percent_gdp <- govt_deficit$expenditure_total_percent_gdp - govt_deficit$revenue_total_percent_egdp

View(govt_deficit)

#### Current account balance -- quarterly

current_account <- read_xlsx("current_account_balance.xlsx", sheet = "Sheet 1", skip = 11)

current_account <- current_account[-c(1,724:726),]

colnames(current_account) <- c("country", "date", "current_account_balance_million_euro")

View(current_account) 

#### HICP -- Monthly

hicp_data <- read_xlsx("hicp.xlsx", sheet = "Sheet 1", skip = 7)

hicp_data <- hicp_data[-c(1,2187:2189),]

colnames(hicp_data) <- c("date", "country", "hicp(2015=100)")

# place holder -1 for monthly data to be able to change it to date type
hicp_data$date <- as.Date(paste0(hicp_data$date, "-01"))

View(hicp_data)

#### industrial production -- percentage growth

indust_production <- read_xlsx("indust_production.xlsx", sheet = "Sheet 1", skip = 9)

indust_production <- indust_production[-c(1,2187:2191), -4]

colnames(indust_production) <- c("date", "country", "industrial_production_growth")

# place holder -1 for monthly data to be able to change it to date type
indust_production$date <- as.Date(paste0(indust_production$date, "-01"))

View(indust_production)

#### Real Effective Exchange Rate -- (deflator: consumer price index - 42 trading partners - industrial countries )

reer_data <- read_xlsx("real_effective_exchange_rate.xlsx", sheet = "Sheet 1", skip = 7)

reer_data <- reer_data[-c(1,2187:2189),]

colnames(reer_data) <- c("country", "date", "reer")

reer_data$date <- as.Date(paste0(reer_data$date, "-01"))

View(reer_data)

#### Unemployment rate -- Monthly

unemployment_rate <- read_xlsx("unemp_rate.xlsx", sheet = "Sheet 1", skip = 9)

unemployment_rate <- unemployment_rate[-c(1,2187:2192),-4]

colnames(unemployment_rate) <- c("country", "date", "unemp_rate")

unemployment_rate$date <- as.Date(paste0(unemployment_rate$date, "-01"))

View(unemployment_rate)

#### V2TX -- STOXX50 volatility index

v2tx_data <- read_xlsx("V2TX_Historical_Data.xlsx")

v2tx_data <- v2tx_data %>% select(Date, Indexvalue) %>% rename(date = Date, v2tx = Indexvalue)

v2tx_data$date <- as.Date(v2tx_data$date, format = "%d.%m.%Y")

v2tx_data <- v2tx_data %>% 
  filter(date >= "2015-01-01" & date <= "2024-05-31")

View(v2tx_data)

#### VIX - CBOE Volatility Index

vix_data <- read.csv("VIXCLS.csv")

colnames(vix_data) <- c("date", "vix")

vix_data$date <- as.Date(vix_data$date, format = "%Y-%m-%d")

# replace . with NA 

View(vix_data)

### deal with missing values



#### Yield Curve --- Daily

yield_curve <- read_xlsx("yield_curve.xlsx", sheet = "Sheet 1", skip = 9)

yield_curve <- yield_curve[-c(1,2393:2395),]

colnames(yield_curve) <- c("date", "yield_3m", "yield_1y", "yield_2y", "yield_5y", "yield_10y", "yield_30y")

yield_curve$date <- as.Date(yield_curve$date, format = "%Y-%m-%d")

View(yield_curve)


#### CBOE Crude Oil ETF Volatility Index 

ovx_data <- read.csv("OVXCLS.csv") 

colnames(ovx_data) <- c("date", "ovx")

ovx_data$date <- as.Date(ovx_data$date, format = "%Y-%m-%d")

View(ovx_data)



####  Crude Oil Prices: Brent - Europe 

brent_data <- read.csv("OILBRENTEU.csv")

colnames(brent_data) <- c("date", "brent_price")

brent_data$date <- as.Date(brent_data$date, format = "%Y-%m-%d")

View(brent_data)

#### CISS -- Composite Indicator of Systemic Stress

ciss_data <- read.csv("CISS.csv")

View(ciss_data)

ciss_data <- ciss_data %>% select(-2)

colnames(ciss_data) <- c("date", "ciss")

ciss_data$date <- as.Date(ciss_data$date, format = "%Y-%m-%d")

ciss_data <- ciss_data %>%
  filter(date >= "2015-01-01" & date <= "2024-05-31")
