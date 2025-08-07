# Load Libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(devtools)
library(stringr)
library(fuzzyjoin)
library(ggridges)
library(ggplot2)
library(grf)
library(lmtest)
library(sandwich)

# Load SBA 7(a) Data
sba_7a_2010 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/foia-7a-fy2010-fy2019-asof-250331.csv")
sba_7a_2020 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/foia-7a-fy2020-present-asof-250331.csv")

# Combining data sets for SBA 7a data from 2010-2025
sba <- rbind(sba_7a_2010, sba_7a_2020)

# Remove non-combined data
rm(sba_7a_2010, sba_7a_2020)

# Making binary variables 
sba$fixedrate <- ifelse(sba$FixedOrVariableInterestInd == "F", 1, 0)

sba$securitization <- ifelse(sba$SoldSecMrktInd == "Y", 1, 0)

sba$collateral <- ifelse(sba$CollateralInd == "Y", 1, 0)

# Create binary variables for Loan Status Categories
sba$commit <- ifelse(sba$LoanStatus == "COMMIT", 1, 0)
sba$pif <- ifelse(sba$LoanStatus == "PIF", 1, 0)
sba$charge_off <- ifelse(sba$LoanStatus == "CHGOFF", 1, 0)
sba$cancelled <- ifelse(sba$LoanStatus == "CANCLD", 1, 0)
sba$exempt <- ifelse(sba$LoanStatus == "EXEMPT", 1, 0)


# Load PPP loan data
ppp_1 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_150k_plus_240930.csv")
ppp_2 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_1_240930.csv")
ppp_3 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_2_240930.csv")
ppp_4 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_3_240930.csv")
ppp_5 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_4_240930.csv")
ppp_6 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_5_240930.csv")
ppp_7 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_6_240930.csv")
ppp_8 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_7_240930.csv")
ppp_9 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_8_240930.csv")
ppp_10<- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_9_240930.csv")
ppp_11<- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_10_240930.csv")
ppp_12<- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_11_240930.csv")
ppp_13<- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/public_up_to_150k_12_240930.csv")

ppp <- rbind(ppp_1, ppp_2, ppp_3, ppp_4, ppp_5, ppp_6, ppp_7, ppp_8, ppp_9, 
             ppp_10, ppp_11, ppp_12, ppp_13)

# Remove non-combined data
rm(ppp_1, ppp_2, ppp_3, ppp_4, ppp_5, ppp_6, ppp_7, ppp_8, ppp_9,
   ppp_10, ppp_11, ppp_12, ppp_13)

# Create binary indicator variables for PPP data
ppp$nonprofit <- ifelse(ppp$NonProfit == "Y", 1, 0)
ppp$urban <- ifelse(ppp$RuralUrbanIndicator == "U", 1, 0)
ppp$hubzone<- ifelse(ppp$HubzoneIndicator == "Y", 1, 0)

# create different names for variables with matching names between two data frames
ppp$franchise_name_ppp <- ppp$FranchiseName
sba$franchise_name_7a <- sba$FranchiseName

ppp$naics_code_ppp <- ppp$NAICSCode
sba$naics_code_7a <- sba$NaicsCode


ppp$processing_method_ppp <- ppp$ProcessingMethod
sba$processing_method_7a <- sba$ProcessingMethod

# Business Name Cleaning Before Matching
clean_names <- function(name_col) {
  name_col |>
    str_to_lower() |>                         # lower case
    str_replace_all("&", "and") |>          # replace ampersands
    str_replace_all("[^a-z0-9 ]", "") |>     # remove punctuation
    str_replace_all("\\b(inc|llc|corp|co|ltd|plc)\\b", "") |>  # remove common suffixes
    str_squish()                               
}

sba$borr_name_clean <- clean_names(sba$BorrName)

ppp$borr_name_clean <- clean_names(ppp$BorrowerName)


# joining based on matching names
# exact name match

exact_name <- merge(sba, ppp, by = "borr_name_clean")

# number of businesses with a franchise name or code
#7a Has a franchise
exact_name$franchise_name_ppp <- iconv(exact_name$franchise_name_ppp, from = "", to = "UTF-8", sub = "")
exact_name$franchise_1 <- ifelse(nchar(exact_name$franchise_name_7a)>0, 1, 0)
summary(exact_name$franchise_1)

exact_name$franchise_2 <- ifelse(nchar(exact_name$franchise_name_ppp) > 0, 1, 0)
summary(exact_name$franchise_2)

# remove invalid characters from PPP franchise names


exact_name$has_franchise <- ifelse(nchar(exact_name$franchise_name_ppp) > 0 |
                            nchar(exact_name$franchise_name_7a) > 0, 1, 0)
summary(exact_name$has_franchise)


# remove matches before 2020
  
df <- exact_name %>%
  mutate(ApprovalDate = as.Date(ApprovalDate, format = "%m/%d/%Y")) %>%
  filter(ApprovalDate > as.Date("2020-04-03")) 



# check if NAICS code matches

df$naics_match <- df$NaicsCode == df$NAICSCode

summary(df$naics_match)
# majority of output doesn't match NAICS code
# need to match on NAICS code too instead of just names
# match based on first 2 characters of NAICS code to just get matching sector first



df$sba_naics_2 <- substr(as.character(df$NaicsCode), 1, 2)
df$ppp_naics_2 <- substr(as.character(df$NAICSCode), 1, 2)

df$naics_sector_match <- df$sba_naics_2 == df$ppp_naics_2

summary(df$naics_sector_match)

# keep only sector matches
df_matched <- df[df$naics_sector_match == TRUE,]

# keep only state matches OR those that have a franchise


df_matched$state_match <- df_matched$BorrState == df_matched$BorrowerState

summary(df_matched$state_match)



dfm <- df_matched[df_matched$state_match == TRUE | df_matched$has_franchise ==1 ,]
# down to 78148 observations with matching names, states, and NAICS sector codes
# about 6000 observations for franchises
# find companies that have not had PPP loans refunded

sum(dfm$InitialApprovalAmount > dfm$ForgivenessAmount, na.rm = TRUE)

summary(dfm$ForgivenessAmount)

# PPP loan LMI indicator
dfm$LMI <- ifelse(dfm$LMIIndicator == "Y", 1, 0)

# NAICS Sector Categories
dfm$agriculture      <- ifelse(dfm$sba_naics_2 == 11, 1, 0)
dfm$mining           <- ifelse(dfm$sba_naics_2 == 21, 1, 0)
dfm$utilities        <- ifelse(dfm$sba_naics_2 == 22, 1, 0)
dfm$construction     <- ifelse(dfm$sba_naics_2 == 23, 1, 0)
dfm$manufacturing    <- ifelse(dfm$sba_naics_2 == 31, 1, 0)
dfm$wholesale_trade  <- ifelse(dfm$sba_naics_2 == 42, 1, 0)
dfm$retail_trade     <- ifelse(dfm$sba_naics_2 == 44 | dfm$sba_naics_2 == 45, 1, 0)
dfm$transport_ware   <- ifelse(dfm$sba_naics_2 == 48 | dfm$sba_naics_2 == 49, 1, 0)
dfm$information      <- ifelse(dfm$sba_naics_2 == 51, 1, 0)
dfm$finance_ins      <- ifelse(dfm$sba_naics_2 == 52, 1, 0)
dfm$real_estate      <- ifelse(dfm$sba_naics_2 == 53, 1, 0)
dfm$professional     <- ifelse(dfm$sba_naics_2 == 54, 1, 0)
dfm$admin_support    <- ifelse(dfm$sba_naics_2 == 56, 1, 0)
dfm$education        <- ifelse(dfm$sba_naics_2 == 61, 1, 0)
dfm$healthcare       <- ifelse(dfm$sba_naics_2 == 62, 1, 0)
dfm$arts_entertain   <- ifelse(dfm$sba_naics_2 == 71, 1, 0)
dfm$accommodation    <- ifelse(dfm$sba_naics_2 == 72, 1, 0)
dfm$other_services   <- ifelse(dfm$sba_naics_2 == 81, 1, 0)
dfm$public_admin     <- ifelse(dfm$sba_naics_2 == 92, 1, 0)



summary(dfm$agriculture)
summary(dfm$mining)

# 7a processing categories
dfm$processing_method_7a <- dfm$ProcessingMethod.x
dfm$processing_method_ppp <- dfm$ProcessingMethod.y

dfm$processing_plp <- ifelse(dfm$processing_method_7a == "Preferred Lenders Program",
                             1, 0)


processing_methods <- c(
  "Preferred Lenders Program" = "processing_plp",
  "SBA Express Program" = "processing_express",
  "7a General" = "processing_7a_general",
  "International Trade Loans" = "processing_international_trade",
  "Standard Asset Base Working Capital Line of Credit (CAPLine)" = "processing_standard_capline",
  "Community Advantage Initiative" = "processing_community_advantage",
  "Contract Loan Line of Credit (CAPLine)" = "processing_contract_capline",
  "Community Advantage Recovery Loan" = "processing_ca_recovery",
  "7a with WCP" = "processing_7a_wcp",
  "Export Express" = "processing_export_express",
  "Builders Line of Credit (CAPLine)" = "processing_builders_capline",
  "Preferred Lenders with EWCP" = "processing_preferred_ewcp",
  "7a with EWCP" = "processing_7a_ewcp",
  "Preferred Lenders with WCP" = "processing_preferred_wcp",
  "Seasonal Line of Credit (CAPLine)" = "processing_seasonal_capline",
  "Community Advantage International Trade" = "processing_ca_international_trade",
  "Community Advantage RLOC" = "processing_ca_rloc"
)

for (method in names(processing_methods)) {
  col_name <- processing_methods[[method]]
  dfm[[col_name]] <- ifelse(dfm$processing_method_7a == method, 1, 0)
}


# summary statistics 
# 91% of 7a loans are processed through SBA express or PLP
summary(dfm$processing_express)
summary(dfm$processing_plp)
# 97.5% of 7a loans processed through PLP, Express, or 7a General
summary(dfm$processing_7a_general)

# business age categories
distinct(dfm, BusinessAge)
dfm$BusinessAgeDescription
distinct(dfm, BusinessAgeDescription)

sum(is.na(dfm$BusinessAge))
sum(is.na(dfm$BusinessAgeDescription))


business_age_categories <- c(
  "Existing or more than 2 years old" = "age_established",
  "New Business or 2 years or less" = "age_new",
  "Unanswered" = "age_unanswered",
  "Startup, Loan Funds will Open Business" = "age_startup",
  "Change of Ownership" = "age_changed_owner"
)

for (category in names(business_age_categories)) {
  col_name <- business_age_categories[[category]]
  dfm[[col_name]] <- ifelse(dfm$BusinessAge == category, 1, 0)
}

# summary statistics

summary(dfm$age_established)
summary(dfm$age_changed_owner)
summary(dfm$age_new)
summary(dfm$age_startup)
summary(dfm$age_unanswered)

# create measure of of the quantity of SBA loans by year
# for companies that already have a PPP loan

# Look at changes in reported employment from PPP loans to SBA loans
# PPP Jobs Supported Statement
summary(dfm$JobsReported)

# 7a Jobs Supported Statement
summary(dfm$JobsSupported)

dfm$jobs_reported_difference <- (dfm$JobsSupported - dfm$JobsReported)

summary(dfm$jobs_reported_difference)

t.test(dfm$JobsSupported,dfm$JobsReported)

reg1 <- lm(dfm$JobsSupported ~ as.Date.numeric(dfm$ApprovalDate) + dfm$LMI 
           +dfm$urban + dfm$hubzone + dfm$processing_plp )
summary(reg1)

# Need to Compare Inflation Adjusted Size of 7a Loans to PPP Loans
# Compare Gross Approval (7a) to Current Approval Amount
summary(dfm$CurrentApprovalAmount)
summary(dfm$GrossApproval)

reg1 <- lm(GrossApproval ~ CurrentApprovalAmount, data = dfm)
summary(reg1)


# loan approval and sector codes
reg2 <- lm(GrossApproval ~ agriculture + mining + utilities + construction + manufacturing
           + wholesale_trade + transport_ware + information + finance_ins + 
             real_estate + professional + admin_support + education + accommodation 
           + other_services + public_admin, data = dfm)

summary(reg2)

reg3 <- lm(CurrentApprovalAmount ~ agriculture + mining + utilities + construction + manufacturing
           + wholesale_trade + transport_ware + information + finance_ins + 
             real_estate + professional + admin_support + education + accommodation 
           + other_services + public_admin, data = dfm)
summary(reg3)

# Random Forest Prediction Model for 7a Gross Approval Amount

# Cluster Data by Month of SBA Loan Approval
# Use Best Linear Projection to try to find prediction based on specified items
# create approval month variable for clustering
dfm$approval_month <- format(as.Date(dfm$ApprovalDate), "%Y-%m")

# x as data frame
X_df <- dfm %>% select(LMI, urban, hubzone, processing_plp)

# create missingness indicators
X_miss_ind <- X_df %>%
  mutate(across(everything(), ~ as.integer(is.na(.)), .names = "{.col}_miss"))

# impute using column medians
X_imp <- X_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# combine imputed values with missingness indicators
X_final <- cbind(X_imp, X_miss_ind) %>% as.matrix()

# clean y and w
Y <- dfm$GrossApproval
W <- dfm$CurrentApprovalAmount

valid_rows <- !(is.na(Y) | is.na(W))
Y_clean <- Y[valid_rows]
W_clean <- W[valid_rows]
X_clean <- X_final[valid_rows, ]

# grf causal forest
cf <- causal_forest(X = X_clean, Y = Y_clean, W = W_clean, clusters = as.factor(dfm$ApprovalMonth[valid_rows]))

# estimate treatment effects
tau.hat <- predict(cf)$predictions

# summarize ate
ate <- average_treatment_effect(cf, target.sample = "all")
print(ate)




