#Joscha Hügle 
#
#Do Chinese aid flows invluence OECD aid allocation patterns?  

  

library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(countrycode)


# 1. Indipendent Variable and Dependent Variable -----------------------------------------------------------

# 1.1 China Aid Data ------------------------------------------------------

china <- read_excel ("data/GlobalChineseOfficialFinanceDataset.xlsx")

#Subsetting China Aid Data 
china <- china[ , c("project_id", "recommended_for_research", "year", "donor", "recipient_oecd_name", "flow_class", "usd_current", "crs_sector_name", "year_uncertain", "recipient_region")]
china <- china[china$recommended_for_research == T, ]
china <- china[china$flow_class == "ODA-like", ]
china <- china[china$year_uncertain == F, ]

#Aggregate Committments and adding country codes 
china <- china %>% 
  group_by(year, donor, recipient_oecd_name, recipient_region) %>% 
  summarize(usd_current = sum(usd_current, na.rm = T)) %>% 
  arrange(recipient_oecd_name, year) %>% 
  filter(!str_detect(recipient_oecd_name, "(regional|\\;)")) %>% 
  ungroup %>% 
  mutate(recipient_oecd_name = recipient_oecd_name %>% str_replace("Rep\\.", "Republic"),
         recipient_code = countrycode(recipient_oecd_name, "country.name", "iso3c")) %>% 
  select(year, recipient_code, chn_aid = usd_current)


# 1.2 ODA DAC Comittment-----------------------------------

odacom <- read.csv("data/DACTotal.csv")
odacom <- odacom [ ,c("Year", "Donor", "Recipient", "Value")]

odacom <- odacom %>% 
  mutate(recipient_code = Recipient %>% countrycode("country.name", "iso3c", warn = F),
         donor_code = Donor %>% countrycode("country.name", "iso3c", warn = F),
         donor_code = ifelse(Donor == "DAC Countries, Total", "DAC", donor_code))
         
#deleting regional aidflows 
odacom <- odacom [!is.na(odacom$donor_code) & !is.na(odacom$recipient_code),]



# 2. Controls -----------------------------------      

# 2.1 GDP Per Capita-----------------------------------

GDP <- read_csv("data/GDP.csv", skip = 3)

#relevant indicators
GDP <- GDP[c("Country Code", 2000:2014)] %>% 
  gather("year", "gdp", `2000`:`2014`) %>% 
  mutate(year = as.numeric(year))



# 2.2 Population-----------------------------------

#load Worldbank Dataset population 
pop <- read_csv("data/population.csv", skip = 3)
#relevant indicators
pop <- pop[c("Country Code", 2000:2014)]  %>% 
  gather("year", "pop", `2000`:`2014`) %>% 
  mutate(year = as.numeric(year))



# 2.3 openess to trade-----------------------------------

#load Worldbank Dataset Trade

openess <- read_csv("data/Trade.csv", skip = 3)

openess <- openess[c("Country Code", 2000:2014)] %>% 
  gather("year", "openness", `2000`:`2014`) %>% 
  mutate(year = as.numeric(year))

# 2.4 Member UNSC-----------------------------------

unsc <- read_excel("data/UNSCdata.xls")      

unsc <- unsc %>%
  select(year, code, unsc)





# 3. Join data ------------------------------------------------------------

odacom <- odacom %>% 
  left_join(GDP, by = c("recipient_code" = "Country Code", "Year" = "year")) %>% 
  left_join(pop, by = c("recipient_code" = "Country Code", "Year" = "year")) %>% 
  left_join(openess, by = c("recipient_code" = "Country Code", "Year" = "year")) %>% 
  left_join(china, by = c("recipient_code" = "recipient_code", "Year" = "year")) %>% 
  left_join(unsc, by = c(c("recipient_code" = "code", "Year" = "year")))



# 4. Exploring Data-----------------------------------


#Installing the necessary libraries for analysis 

install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")


#Loading these libraries

library(funModeling) 
library(tidyverse) 
library(Hmisc)

#HEAD

head(odacom)


#Tail 

tail(odacom)


str(odacom)

#Data Size 


dim(odacom)


colnames(odacom)

rownames(odacom)


#Analysing the Summary statistics for numeric  and value counts for categorical variables 

Summary_statistics <- summary(odacom)

#Applying the basic EDA process to understand the nature of data

df_status(odacom)

freq(odacom)

#Look at the right bottom, these were some necessary visualizations to make

plot_num(odacom)


data_prof=profiling_num(odacom)

#Analysing Numerical and Categorical

library(Hmisc)
describe(odacom)



#Removing the missing values in the data set

odacom <- na.omit(odacom)

dim(odacom)

library(dplyr)


#Correlations Table

correlation <- select(odacom, Value, openness, pop, gdp, Year, chn_aid)


cor(correlation)

#We have filtered the data for the most important countries having highest contributions

top_countries <- odacom %>% filter(Donor == 'United States' | Donor  == 'Germany' | Donor  == 'United Kingdom' | Donor  == 'Japan')

dim(top_countries)



# 5. Analyses-----------------------------------


linear_model <- lm(Value ~ ., data = odacom)
linear_model_stats <- summary(linear_model)

# Plotting the linear models here, keeping hitting enter and wait for the result to appear in the plot section.


plot(linear_model)


# Switching to OLS Regressions


install.packages("plm", dependencies = TRUE)

install.packages("plm", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))

library(plm)

df = distinct(odacom, Year,Donor, .keep_all= TRUE)

#POOLED OLS REGRESSION TO ESTIMATE THE PARAMETERS AND IDENTIFY THE CORRELATION 

pool <- plm(Value ~ chn_aid, data = df , model = "pooling")
OLS_regression_table <- summary(pool)
OLS_regression_table

#BETWEEN OLS - IT CONSIDERS THE TIME BY TAKING AN AVERAGE ON ALL OF THE CROSS SECTIONAL DATA ON THE SAME CATEGORY SO AS TO CONSIDER TIME 
# SERIES AS WELL.

pool2 <- plm(Value ~ chn_aid, data = df, model = "between")
Between_regression_table <- summary(pool2)
Between_regression_table


