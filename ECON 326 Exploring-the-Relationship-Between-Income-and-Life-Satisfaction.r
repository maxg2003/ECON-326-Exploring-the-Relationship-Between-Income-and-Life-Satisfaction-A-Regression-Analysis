#load packages

library(tidyverse)
library(haven)
library(dplyr)
installed.packages("stargazer")
library(stargazer)
library(lmtest)
install.packages("sandwich")
library(sandwich)


#read and load data
data <- read_dta('CCHS_Annual_2017_2018_curated_trimmed.dta') %>%
    select('incdghh', 'dhhgage', 'GEN_005', 'EHG2DVR3', 'dhhgms', 'GEN_010','GEN_015')
#another way to filter our data 
#filteredData <- data[,c('incdghh','dhhgage', 'GEN_005', 'EHG2DVR3', 'dhhgms', 'GEN_010','GEN_015')]


#rename data for easier access
data <- data %>%
    rename(household_income = incdghh) %>%
    rename(age = dhhgage) %>%
    rename(health = GEN_005) %>%
    rename(education = EHG2DVR3) %>%
    rename(marital_status = dhhgms) %>%
    rename(satisfaction = GEN_010) %>%
    rename(mental_health = GEN_015)

data <- data %>%

#education
    mutate(education = case_when(
            education == 1 ~ "Less than high school",
            education == 2 ~ "High school",
            education == 3 ~ "Post-secondary or University",
            TRUE ~ "NA" 
        )) %>%
    filter(education != "NA")%>%

#income
    mutate(income = case_when(
            household_income == 1 ~ 10000,
            household_income == 2 ~ 30000,
            household_income == 3 ~ 50000,
            household_income == 4 ~ 70000,
            household_income == 5 ~ 90000,
            TRUE ~ 0 
        )) %>%
    filter(household_income != 0)%>%

#Marital status
    mutate(marital_status = case_when(
             marital_status == 1 ~ "married",
             marital_status == 2 ~ "common-law",
             marital_status == 3 ~ "divorced",
             marital_status == 4 ~ "single",
             TRUE ~ "NA"
         )) %>%
    filter(marital_status != "NA") %>%
    mutate(marital_status = as_factor(marital_status)) %>%

#age
    mutate(age = case_when(
            age == 13 ~ 67,
            age == 16 ~ 82,
            age == 1 ~ 13,
            age == 2 ~ 16,
            age == 3 ~ 18.5,
            age == 4 ~ 22,
            age == 5 ~ 27,
            age == 6 ~ 32,
            age == 7 ~ 37,
            age == 8 ~ 42,
            age == 9 ~ 47,
            age == 10 ~ 52,
            age == 11 ~ 57,
            age == 12 ~ 62,
            age == 14 ~ 72,
            age == 15 ~ 77,
            TRUE ~ 0)) %>%
    filter(age != 0) %>%

#health
    mutate(health = case_when(
                health == 1 ~ 5,
                health == 2 ~ 4,
                health == 3 ~ 3,
                health == 4 ~ 2,
                health == 5 ~ 1,
                TRUE ~ 0
            )) %>%
    filter(health != 0) %>%

#satisfaction 
    mutate(satisfaction = case_when(
            satisfaction == 1 ~ 1,
            satisfaction == 2 ~ 2,
            satisfaction == 3 ~ 3,
            satisfaction == 4 ~ 4,
            satisfaction == 5 ~ 5,
            satisfaction == 6 ~ 6,
            satisfaction == 7 ~ 7,
            satisfaction == 8 ~ 8,
            satisfaction == 9 ~ 9,
            satisfaction == 10 ~ 10,
            TRUE ~ 0)) %>%
    filter(satisfaction != 0) %>%

#Mental health
    mutate(mental_health = case_when(
            mental_health == 1 ~ 5,
            mental_health == 2 ~ 4,
            mental_health == 3 ~ 3,
            mental_health == 4 ~ 2,
            mental_health == 5 ~ 1,
            TRUE ~ 0)) %>%
    filter(mental_health != 0) %>%

#Household income 
    mutate(household_income = as_factor(household_income))


glimpse(data)


#replace representitive answer values in the data with real data values

#we'll start with dhhgage

age_results <- data %>% 
    summarize(variable = "Age", mean = mean(age), sd = sd(age), max = max(age), min = min(age)) 
    
age_results

income_results <- data %>%
    summarize(variable = "Income", mean = mean(income), sd = sd(income), max = max(income), min = min(income))

income_results

satisfaction_results <- data %>%
    summarize(variable = "Satisfaction", mean = mean(satisfaction), sd = sd(satisfaction), max = max(satisfaction), min = min(satisfaction))

satisfaction_results

health_results <- data %>% 
    summarize(variable = "Health", mean = mean(health), sd = sd(health), max = max(health), min = min(health)) 

health_results


# creating dummies for marital_status
married_or_common_law <- ifelse(data$marital_status == "married" | data$marital_status == "common-law", 1, 0)

divorced <- ifelse(data$marital_status == "divorced", 1, 0)

marital_status_dummy <- data.frame(married_or_common_law, divorced)

# calculating mean, sd, max, min for the three dummies
married_results <- marital_status_dummy %>% 
    summarize(variable = "Married or Common-Law", mean = mean(married_or_common_law), sd = sd(married_or_common_law), max = max(married_or_common_law), min = min(married_or_common_law))

divorced_results <- marital_status_dummy %>% 
    summarize(variable = "Divorced", mean = mean(divorced), sd = sd(divorced), max = max(divorced), min = min(divorced)) 

married_results
divorced_results

# creating dummies for education with three levels --> two dummies
less_than_high_school <- ifelse(data$education == "Less than high school", 1, 0)
university <- ifelse(data$education == "Post-secondary or University", 1, 0)

education_dummy <- data.frame(less_than_high_school, university)

# calculating mean, sd, max, min for the two dummies
less_than_high_school_results <- education_dummy %>% 
    summarize(variable = "Less than high school", mean = mean(less_than_high_school), sd = sd(less_than_high_school), max = max(less_than_high_school), min = min(less_than_high_school))

university_results <- education_dummy %>% 
    summarize(variable = "University", mean = mean(university), sd = sd(university), max = max(university), min = min(university))

less_than_high_school_results
university_results

# use stargazer to create a linear regression table
low_income <- median(data$income) * 0.5
low_income


data <- data %>%
    mutate(adjusted_income = 100 *((data$income / low_income) - 1))


reg1 <- lm(satisfaction ~ health + adjusted_income, data = data)

reg2 <- lm(satisfaction ~ health + adjusted_income + married_or_common_law, data = data)

reg2.5 <- lm(satisfaction ~ health + adjusted_income + married_or_common_law + age, data = data)

reg3 <- lm(satisfaction ~ health + adjusted_income + married_or_common_law + age + university, data = data)


stargazer(reg1, reg2, reg2.5, reg3, type = "text",
         covariate.labels = c("Health", "Income", "Married or Common-Law", "Age", "University"),
          dep.var.labels = c("Satisfaction"))


# testing heteroskedasticity ~ Breusch-Pagan test
bptest(reg1)
bptest(reg2)
bptest(reg2.5)
bptest(reg3)

# correcting for heteroskedasticity ~ robust standard errors

se1 <- sqrt(diag(vcovHC(reg1, type = "HC1")))
se2 <- sqrt(diag(vcovHC(reg2, type = "HC1")))
se3 <- sqrt(diag(vcovHC(reg2.5, type = "HC1")))
se4 <- sqrt(diag(vcovHC(reg3, type = "HC1")))


stargazer(reg1, reg2, reg2.5, reg3, se = list(se1, se2, se3, se4), type = "text", 
          covariate.labels = c("Health", "Income", "Married or Common-Law", "Age", "University"),
          dep.var.labels = c("Satisfaction"))


# robustness check
reg4.0 <- lm(mental_health ~ health + adjusted_income, data = data)
reg4.1 <- lm(mental_health ~ health + adjusted_income + married_or_common_law, data = data)
reg4.15 <- lm(mental_health ~ health + adjusted_income + married_or_common_law + age, data = data)
reg4.2 <- lm(mental_health ~ health + adjusted_income + married_or_common_law + age + university, data = data)

bptest(reg4.0)

se4.0 <- sqrt(diag(vcovHC(reg4.0, type = "HC1")))
se4.1 <- sqrt(diag(vcovHC(reg4.1, type = "HC1")))
se4.15 <- sqrt(diag(vcovHC(reg4.15, type = "HC1")))
se4.2 <- sqrt(diag(vcovHC(reg4.2, type = "HC1")))

stargazer(reg4.0, reg4.1, reg4.15, reg4.2, se = list(se4.0, se4.1, se4.15, se4.2), type = "text", 
          covariate.labels = c("Health", "Income", "Married or Common-Law", "Age", "University"),
          dep.var.labels = c("Mental Health"))

# robustness check 2.0

reg5 <- lm(satisfaction ~ household_income, data = data)
reg6 <- lm(satisfaction ~ health + household_income + married_or_common_law, data = data)
reg6.5 <- lm(satisfaction ~ health + household_income + married_or_common_law + age, data = data)
reg7 <- lm(satisfaction ~ health + household_income + married_or_common_law + age + university, data = data)

bptest(reg7)

se5 <- sqrt(diag(vcovHC(reg5, type = "HC1")))
se6 <- sqrt(diag(vcovHC(reg6, type = "HC1")))
se6.5 <- sqrt(diag(vcovHC(reg6.5, type = "HC1")))
se7 <- sqrt(diag(vcovHC(reg7, type = "HC1")))


stargazer(reg5, reg6, reg6.5, reg7, se = list(se5, se6, se7), 
          covariate.labels = c("Health", "$20,000 to $39,999", "$40,000 to $59,999", "$60,000 to $79,999", 
                               "$80,000 or more", "Married or Common-Law", "Age", "University"),
          dep.var.labels = c("Satisfaction"), type = "text")


#credits to my partners on this project Jiayin Kralik and Hanze Wang with this code
