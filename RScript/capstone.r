# Tanner Norton: Capstone Project

# Load libraries
pacman::p_load(devtools,tidyverse, readxl, stringr, stringi,blscrapeR, forcats, skimr, rio, lubridate, riem, 
               dygraphs,epitools, tidyquant, quantmod, timetk, DT, scales, USAboundaries, USAboundariesData,
               ggrepel,sf, maps, geofacet, ggplot2, maptools, buildings, rnaturalearth, rvest, leaflet, 
               prettydoc, magrittr, cdlTools, htmltools, viridis, RColorBrewer, riem, weathermetrics, here,
               lmtest, sandwich, nlme, stargazer, car)



# Do countries with a higher percentage of women in parliament result in lower military spending as a percentage of total government expenditures?


## Example code for using gather to tidy the data ##
# mil_spend <- read_csv("mil_spend.csv") %>% 
#   gather("Year", "mil_spending", 5:34) %>% 
#   filter(Year > 1999)
# 
# 
# mil_spend <- read_excel("master_capstone.xls", sheet = 1, skip = 3) %>% 
#   gather("Year", "mil_spending", 5:34) %>% 
#   filter(Year == "2012")
# 
# arms_imp <- read_excel("master_capstone.xls", sheet = 2, skip = 3) %>% 
#   gather("Year", "arms_imports", 5:34) %>% 
#   filter(Year == "2012")
# 
# wom_par <- read_excel("master_capstone.xls", sheet = 3, skip = 3) %>% 
#   gather("Year", "women_parliament", 5:34) %>% 
#   filter(Year == "2012")
# 
# arm_pop <- read_excel("master_capstone.xls", sheet = 4, skip = 3) %>% 
#   gather("Year", "military_population", 5:34) %>% 
#   filter(Year == "2012")
# 
# tax_rev <- read_excel("master_capstone.xls", sheet = 5, skip = 3) %>% 
#   gather("Year", "tax_gdp", 5:35) %>% 
#   filter(Year == "2012")
# 
# mil_gdp <- read_excel("master_capstone.xls", sheet = 6, skip = 3) %>% 
#   gather("Year", "military_gdp", 5:34) %>% 
#   filter(Year == "2012")
# 
# 
# # Here are test results
# gender <- lm(mil_gdp$mil_spending ~ wom_par$women_parliament + arm_pop$military_population + tax_rev$tax_gdp)
# summary(gender)
# 
# 
# # Export data into CSV to combine in master sheet
# # Import metadata which has the income levels of each country
# 
# meta <- read_excel("master_capstone.xls", sheet = 9)
# 
# 
# 
# mil_spend <- read_excel("master_capstone.xls", sheet = 1, skip = 3) %>% 
#   gather("Year", "mil_spending", 5:34) %>% 
#   select(-"Indicator Code", -"Indicator Name")
# 
# arms_imp <- read_excel("master_capstone.xls", sheet = 2, skip = 3) %>% 
#   gather("Year", "arms_imports", 5:34) %>% 
#   select(-"Indicator Code", -"Indicator Name")
# 
# wom_par <- read_excel("master_capstone.xls", sheet = 3, skip = 3) %>% 
#   gather("Year", "women_parliament", 5:34) %>%
#   select(-"Indicator Code", -"Indicator Name") 
# 
# arm_pop <- read_excel("master_capstone.xls", sheet = 4, skip = 3) %>% 
#   gather("Year", "military_population", 5:34) %>% 
#   select(-"Indicator Code", -"Indicator Name")
# 
# tax_rev <- read_excel("master_capstone.xls", sheet = 5, skip = 3) %>% 
#   gather("Year", "tax_gdp", 5:35) %>% 
#   select(-"Indicator Code", -"Indicator Name")
# 
# battle <- read_excel("master_capstone.xls", sheet = 6, skip = 3) %>% 
#   gather("Year", "battle_deaths", 5:34) %>% 
#   select(-"Indicator Code", -"Indicator Name")
# 
# 
# all_data <- wom_par %>%  left_join(arms_imp) %>%  left_join(tax_rev) %>% left_join(arm_pop) %>% 
#   left_join(mil_spend) %>% left_join(meta) %>% 
#   select(-"SpecialNotes", -"TableName") %>% 
#   filter(!is.na(Region))
# 
# # write_csv(all_data, "all_data.csv")
# 
# 
# all_data <- read_csv("all_data.csv") %>% 
#   filter(Year > 1999)
# 
# government <- read_excel("government.xls") %>% 
#   select("countryname","ifs","year","system","gov1rlc") %>% 
#   rename("Country Code" = "ifs", "Year" = "year") %>% 
#   filter(Year > 1999) %>% 
#   filter(gov1rlc != "-999",
#          gov1rlc != "0") 



# No null values from 2000 on

# full_dat <- dat %>% 
#  left_join(standard_all) %>% 
#  select(`Country Name`, `Country Code`, Region, IncomeGroup, Year, mil_spending, women_parliament, arms_imports, #log_arms, tax_gdp, military_population, gov1rlc, undp_hdi, ciri_wopol, gle_rgdpc, bci_bci) %>% 
#  rename("HDI" = "undp_hdi", "RGDP" = "gle_rgdpc", "Wom_right" = "ciri_wopol",
#         "Corrup" = "bci_bci")

# write_csv(full_dat, "full_dat.csv")


# full data unbalanced regression model
# standard_unbalanced <- read_csv("standard_all.csv") %>% 
#   select(ccode, cname, `Country Code`, Year, wdi_expmilge, wdi_wip, wdi_armimp, wdi_afp, gle_rgdpc) %>% 
#   drop_na()
# 
# attach(standard_unbalanced)
# standard_lm <- lm(wdi_expmilge ~ wdi_wip + wdi_armimp + wdi_afp + log(gle_rgdpc))
# summary(standard_lm)
# 
# 
# # full data unbalanced regression model
# full_dat <- read_csv("full_dat.csv") %>% 
#   mutate(per_milspend = (.01*mil_spending),
#          per_womenpar = (.01*women_parliament),
#          per_milpop = (.01*military_population))
# 
# attach(full_dat)
# ols.unbalanced <- lm(per_milspend ~ per_womenpar + per_milpop + log_arms + gov1rlc + IncomeGroup )
# summary(ols.unbalanced)
# 







# full data unbalanced regression model
full_dat <- read_csv("full_dat.csv") %>% 
  mutate(per_milspend = (.01*mil_spending),
         per_womenpar = (.01*women_parliament),
         per_milpop = (.01*military_population))

attach(full_dat)
ols.unbalanced <- lm(mil_spending ~ women_parliament + military_population + log_arms + gov1rlc + IncomeGroup )
summary(ols.unbalanced)



# Tests for diseases
# Check for autocorrelation
dwtest(ols.unbalanced)  # No autocorrelation close b/c of DW close to 2

# Check for heteroskedasticity
bptest(ols.unbalanced)        # test is significant meaning that we assume heteroskedasticity

# Check for multicollinearity
vif(ols.unbalanced)  # 




# Fixes for Heteroskedasticy
rbs_unbalanced <- coeftest(ols.unbalanced,vcov. = vcovHC(ols.unbalanced, type="HC1"))   # Robust standard error


attach(full_dat)
gls.unbalanced <- gls(mil_spending ~ women_parliament + military_population + log_arms + gov1rlc + IncomeGroup  )
summary(gls.unbalanced)


# Export regression results unbalanced
stargazer(gls.unbalanced, type="html", dep.var.labels= "Military Spending % of Total Government Spending",
          covariate.labels=c("Women in Parliament %","Military Population %","Log of Arms Imports",
                             "Majority party Left", "Majority party Right", "Low Income", "Low Middle Income", "Upper Middle Income"), title = "Model 2: GLS unbalanced" ,out="unbalanced_gls.htm")



## Visualizations for unbalanced data
ggplot(full_dat, aes(x = as.factor(Year), y = women_parliament)) +
  geom_boxplot()


ggplot(full_dat, aes(x = gov1rlc , y = mil_spending)) +
  geom_boxplot()


# Create summary  stats table

library(table1)
label(full_dat$`Country Name`) <- "Country Name"
label(full_dat$Year) <- "Year"
label(full_dat$mil_spending) <- "Military Spending % Govt Spending"
label(full_dat$log_arms) <- "Log of Arms Imports"
label(full_dat$IncomeGroup) <- "Income Group"
label(full_dat$women_parliament) <- "Women in Parliament %"
label(full_dat$gov1rlc) <- "Political Spectrum"
label(full_dat$military_population) <- "Population in Military %"

table1(~ mil_spending + women_parliament + military_population + log_arms + gov1rlc + IncomeGroup , data = full_dat, caption = "Unbalanced Summary Statistics")   

table1(~ mil_spending + women_parliament + military_population + log_arms + gov1rlc | IncomeGroup,data = full_dat, caption = "Model 2: Unbalanced Summary Statistics")  

















# Balanced regression model
balanced <- read_csv("dat2.csv") 

attach(balanced)
ols.balanced <- lm(mil_spending ~ women_parliament + military_population + log_arms + gov1rlc + IncomeGroup )
summary(ols.balanced)


# Tests for diseases

# Check for autocorrelation
dwtest(ols.balanced)  # No autocorrelation close b/c of DW close to 2


# Check for heteroskedasticity
bptest(ols.balanced)        # test is insignificant meaning that we assume homoskedasticity


# Check for multicollinearity
vif(ols.balanced)  # No multicollinearity







# Fixes for Heteroskedasticy
rbs_balanced <- coeftest(ols.balanced,vcov. = vcovHC(ols.balanced, type="HC1"))   # Robust standard error

gls.balanced <- gls(mil_spending ~ women_parliament + military_population + log_arms + gov1rlc + IncomeGroup)
summary(gls.balanced)


# Export regression results balanced
stargazer(gls.balanced, type="html", dep.var.labels= "Military Spending % of Total Government Spending",
          covariate.labels=c("Women in Parliament %","Military Population %","Log of Arms Imports",
                             "Majority party Left", "Majority party Right", "Low Middle Income", "Upper Middle Income"), title = "Model 1: GLS balanced" ,out="balanced_gls.htm")





## Visualizations for balanced data
ggplot(balanced, aes(x = as.factor(Year), y = women_parliament)) +
  geom_boxplot()


# Create summary  stats table

library(table1)
label(balanced$`Country Name`) <- "Country Name"
label(balanced$Year) <- "Year"
label(balanced$mil_spending) <- "Military Spending % Govt Spending"
label(balanced$women_parliament) <- "Women in Parliament %"
label(balanced$log_arms) <- "Log of Arms imports"
label(balanced$tax_gdp) <- "Tax Revenue % GDP"
label(balanced$military_population) <- "Population in Military %"
label(balanced$gov1rlc) <- "Political Spectrum"
label(balanced$IncomeGroup) <- "Income Group"

table1(~ mil_spending + women_parliament + military_population + log_arms + gov1rlc + IncomeGroup | Year, data = balanced, caption = "Model 1: Balanced Summary Statistics")









# Tests for diseases

# Check for autocorrelation
dwtest(all)  # No autocorrelation close b/c of DW close to 2


# Check for heteroskedasticity
bptest(ols.balanced)        # test is insignificant meaning that we assume homoskedasticity


# Check for multicollinearity
vif(all)  # No multicollinearity
plot(lm1$residuals, type="b")
qqPlot(lm1)


# Create the Residuals and normality plots
par(mfrow=c(1,2))
plot(lm1, which=1:2, pch=16)


# Export regression results
stargazer(gls.unbalanced, type="html", dep.var.labels= "Military Spending % of Total Government Spending",
          covariate.labels=c("Women in Parliament %","Military Population %","Log of Arms Imports",
                             "Majority party Left", "Majority party Right", "Low Income", "Low Middle Income", "Upper Middle Income"), out="unbalanced_gls.htm")













# Caculating a one percent increase in women in parliament impact on US military spending

# in 2018 US government spent $4.1 trillion

# in 2018 military expenditures made up 9.014% of general govt. spending

# 4.1 trillion * .09014 = 369.574 billion dollars

# My model predicts a 1% increase in female legislatures decreases military spending by -.054 to 8.958%

#   4100000000000*.09014 =  3.69574e+11
# 4100000000000*.08958 = 3.67278e+11

# The difference is 2.296 billion dollars






# 2000 military spend = $311 Billion   beta = -.101, my beta = -.054 (.01) = -.00054













# 
# 
# # Need to make the years column the same data type
# all_data$Year <- as.character(all_data$Year)
# 
# # unbalanced_dat <- all_data %>% 
# filter(Year < 2013,
#        mil_spending != is.na(mil_spending),
#        women_parliament != is.na(women_parliament)) %>% 
#   left_join(government) %>% 
#   mutate(log_arms = log(arms_imports))
# 
# write_csv(unbalanced_dat, "unbalanced_dat.csv")
# 
# 
# 
# unbalanced_dat <- read_csv("unbalanced_dat.csv")
# 
# # This is a really good model if I am able to use unbalance data
# attach(unbalanced_dat)
# lm_unbalanced <- lm(mil_spending ~ women_parliament + military_population + tax_gdp + log_arms + gov1rlc)
# summary(lm_unbalanced)
# 
# 
# # No null values but missing countries between years
# dat <- read_csv("dat.csv") %>% 
#   filter(Year > 1999)
# 
# dat$Year <- as.character(dat$Year)
# 
# attach(dat)
# lm_balanced <- lm(mil_spending ~ women_parliament + military_population + tax_gdp + log_arms + gov1rlc)
# summary(lm_balanced)
# 
# 
# 
# filter_data <- read_csv("filter_data.csv")
# 
# filter_data <- na.omit(all_data) 
# 
# # write_csv(filter_data, "filter_data.csv")
# 
# # Try to figure out what years would be best to use for the regression
# 
# obs_data <- read_csv("filter_data.csv") %>% 
#   group_by(Year) %>% 
#   summarise(obs = n())
# 
# 
# years <- filter_data %>% 
#   filter(Year %in% c("2002","2007", "2012","2017"))
# 
# attach(filter_data)
# # Here are test results
# gender <- lm(mil_spending ~ women_parliament + military_population + tax_gdp + arms_imports + IncomeGroup)
# summary(gender)
# 
# 
# # This data contains info about the government from specific years
# government <- read_excel("government.xls") %>% 
#   select("countryname","ifs","year","system","gov1rlc") %>% 
#   rename("Country Code" = "ifs", "Year" = "year")
# 
# 
# # Need to make the years column the same data type
# years$Year <- as.character(years$Year)
# join <- years %>% 
#   left_join(government)
# 
# attach(join)
# attach(all_dat)
# # Here are test results
# gender <- lm(mil_spending ~ women_parliament + military_population + tax_gdp + arms_imports + gov1rlc)
# summary(gender)
# 
# 
# 
# 
# # Join the filterd data to the government data 
# 
# filter_data$Year <- as.character(filter_data$Year)
# 
# dat <- filter_data %>% 
#   left_join(government) %>% 
#   na.omit() %>% 
#   filter(gov1rlc != "-999",
#          gov1rlc != "0") %>% 
#   select(-"countryname") 
# 
# # write_csv(dat, "dat.csv")
# 
# 
# 
# dat3 <- read_csv("dat3.csv") 
# 
# 
# 
# # Lagging the variable gave better results
# attach(dat3)
# lm3 <- lm(mil_spending ~ lag(women_parliament) + military_population + tax_gdp + log(arms_imports) + gov1rlc + War )
# summary(lm3)
# 
# 
# ggplot(dat3, aes(x = gov1rlc, y = mil_spending)) +
#   geom_boxplot()
# 
# 
# # This data is a full panel data from 2002, 2006, 2012
# dat2 <- read_csv("dat2.csv") %>% 
#   left_join(dat)
# 
# dat2 <- read_csv("dat2.csv")
# 
# # I should not lag variables when I have spaced out years
# attach(dat2)
# lm2 <- lm(mil_spending ~ women_parliament + military_population + tax_gdp + arms_imports + gov1rlc)
# summary(lm2)
# 
# 
# 
# 
# 
# 
# # Use basic data set to see if I can get more information
# basic <- read_csv("basic.csv") %>% 
#   filter(year %in% c("2002","2012"),
#          chga_demo == 1,
#          !ccodealp %in% c("DOM","GHA","MKD")) %>% 
#   select(ccode, cname, year, ccodealp, wdi_wip, wdi_expmil, chga_demo, bicc_gmi, wdi_armimp, wdi_afp)
# 
# 
# 
# 
# attach(basic)
# lm_basic <- lm(wdi_expmil ~ wdi_wip + wdi_afp  + log(wdi_armimp) + log(bicc_gmi))
# summary(lm_basic)
# 
# 
# 
# # Use standard data set to see if I can get more information
# standard <- read_csv("standard.csv") %>% 
#   filter(year %in% c("2006","2012")) %>% 
#   select(ccode, cname, year, ccodealp, wdi_wip, wdi_expmilge, wdi_expmil, chga_demo, bicc_gmi, wdi_armimp, wdi_afp)
# 
# 
# attach(standard)
# lm_standard <- lm(wdi_expmilge ~ wdi_wip + wdi_afp + chga_demo + wdi_armimp + log(bicc_gmi))
# summary(lm_standard)
# 












country <- full_dat %>% group_by(`Country Code`) %>% summarise(number = n())













