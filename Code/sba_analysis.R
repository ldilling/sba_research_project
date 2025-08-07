# Load Libraries
library(tidyverse)
library(devtools)
library(ggplot2)
library(grf)
library(lmtest)
library(sandwich)
library(estimatr)
library(hrbrthemes)
library(viridis)
library(tidycensus)
library(sf)
library(tigris)
library(modelsummary)
library(here)
library(stargazer)
library(lubridate)
library(caret)
library(kableExtra)
library(htmltools)
library(pandoc)

# Load Census API Key for ACS Data
k <- read.csv("Data/Census Bureau API Key - Sheet1.csv")

# Add to tidycensus package and install to keep for use in future sessions
census_api_key(k$Key.Number, install = T, overwrite = T)

# Load SBA 7(a) Data
sba <- read.csv("Data/SBAInstitutionTypes.csv")

# Load PPP loan data
ppp_1 <- read.csv("Data/public_150k_plus_240930.csv")
ppp_2 <- read.csv("Data/public_up_to_150k_1_240930.csv")
ppp_3 <- read.csv("Data/public_up_to_150k_2_240930.csv")
ppp_4 <- read.csv("Data/public_up_to_150k_3_240930.csv")
ppp_5 <- read.csv("Data/public_up_to_150k_4_240930.csv")
ppp_6 <- read.csv("Data/public_up_to_150k_5_240930.csv")
ppp_7 <- read.csv("Data/public_up_to_150k_6_240930.csv")
ppp_8 <- read.csv("Data/public_up_to_150k_7_240930.csv")
ppp_9 <- read.csv("Data/public_up_to_150k_8_240930.csv")
ppp_10<- read.csv("Data/public_up_to_150k_9_240930.csv")
ppp_11<- read.csv("Data/public_up_to_150k_10_240930.csv")
ppp_12<- read.csv("Data/public_up_to_150k_11_240930.csv")
ppp_13<- read.csv("Data/public_up_to_150k_12_240930.csv")

ppp <- rbind(ppp_1, ppp_2, ppp_3, ppp_4, ppp_5, ppp_6, ppp_7, ppp_8, ppp_9, 
             ppp_10, ppp_11, ppp_12, ppp_13)

fed_funds <- read.csv("Data/fed_funds.csv")

# Remove non-combined data
rm(ppp_1, ppp_2, ppp_3, ppp_4, ppp_5, ppp_6, ppp_7, ppp_8, ppp_9,
   ppp_10, ppp_11, ppp_12, ppp_13)

# Create Log Approval Amount of SBA Loans
sba$log_approval_amount <- log(sba$GrossApproval)
ppp$log_ppp_amount <- log(ppp$InitialApprovalAmount)

# Making binary variables for 7a loan characteristics
sba$fixedrate <- ifelse(sba$FixedOrVariableInterestInd == "F", 1, 0)

sba$securitization <- ifelse(sba$SoldSecMrktInd == "Y", 1, 0)

sba$collateral <- ifelse(sba$CollateralInd == "Y", 1, 0)

# SBA Financial Institution Variables
institution_size <- c(
  "Regional Bank/CU ($10B<Assets<$100B)" = "regional_bank",
  "Community Bank/CU (Assets<$10B)" = "community_bank",
  "National Bank/CU (Assets>$100B)" = "national_bank",
  "Other Non-Bank/CU" = "other_institution",
  "CDFI" = "cdfi",
  "Fintech" = "fintech"
)

for (i in names(institution_size)) {
  col_name <- institution_size[[i]]
  sba[[col_name]] <-  ifelse(sba$InstitutionSize == i, 1, 0)
}



# Create binary variables for 7a Loan Status Categories
sba$commit <- ifelse(sba$LoanStatus == "COMMIT", 1, 0)
sba$pif <- ifelse(sba$LoanStatus == "PIF", 1, 0)
sba$charge_off <- ifelse(sba$LoanStatus == "CHGOFF", 1, 0)
sba$cancelled <- ifelse(sba$LoanStatus == "CANCLD", 1, 0)
sba$exempt <- ifelse(sba$LoanStatus == "EXEMPT", 1, 0)


# create different names for variables with matching names between two data frames
ppp$franchise_name_ppp <- ppp$FranchiseName
sba$franchise_name_7a <- sba$FranchiseName

ppp$naics_code_ppp <- ppp$NAICSCode
sba$naics_code_7a <- sba$NaicsCode



ppp$processing_method_ppp <- ppp$ProcessingMethod
sba$processing_method_7a <- sba$ProcessingMethod


# Add Race and Ethnicity Variables
# Adding Ethnicity Variables
ppp <- ppp |>
  mutate(
    hispanic = ifelse(Ethnicity == "Hispanic or Latino", 1, 0),
    not_hispanic = ifelse(Ethnicity == "Not Hispanic or Latino", 1, 0),
    ethnicity_unanswered = ifelse(Ethnicity == "Unknown/NotStated", 1, 0)
  )

# PPP Race Variables
ppp <- ppp |>
  mutate(
    race_white = if_else(Race == "White", 1, 0),
    race_black = if_else(Race == "Black or African American", 1, 0),
    race_asian = if_else(Race == "Asian", 1, 0),
    race_native = if_else(Race == "American Indian or Alaska Native" |
                            Race == "Eskimo & Aleut", 1, 0),
    race_pacific = if_else(Race == "Native Hawaiian or Other Pacific Islander", 1, 0),
    race_multi = if_else(Race == "Multi Group", 1, 0),
    race_puerto_rican = if_else(Race == "Puerto Rican", 1, 0),
    race_unanswered = if_else(Race == "Unanswered", 1, 0)
  )

# Merge Federal Funds Data with SBA Data Frame
sba_post <- sba |>
  mutate(
    approval_year_month = ym(ApprovalYearMonth)) |>
  filter(approval_year_month > as.Date("2020-03-31"))


sba_post <- sba_post |>
  mutate(
    merge_key = ym(ApprovalYearMonth)
  )

fed_funds <- fed_funds |>
  mutate(
    merge_key = floor_date(as.Date(observation_date), unit = "month")
  )

sba_post <- inner_join(sba_post, fed_funds, by = "merge_key")

# Business Name Cleaning Before Matching Function
clean_names <- function(name_col) {
  name_col |>
    str_to_lower() |>                         # lower case
    str_replace_all("&", "and") |>          # replace ampersands
    str_replace_all("[^a-z0-9 ]", "") |>     # remove punctuation
    str_replace_all("\\b(inc|llc|corp|co|ltd|plc)\\b", "") |>  # remove common suffixes
    str_squish()                               
}

# Clean business names
sba_post$borr_name_clean <- clean_names(sba_post$BorrName)

ppp$borr_name_clean <- clean_names(ppp$BorrowerName)


# joining based on matching names
# exact name match after cleaning steps
dfm <- merge(sba_post, ppp, by = "borr_name_clean")

# remove invalid characters from PPP franchise names
dfm$franchise_name_ppp <- iconv(dfm$franchise_name_ppp, from = "", to = "UTF-8", sub = "")

# 7a Has a franchise
dfm$franchise_1 <- ifelse(nchar(dfm$franchise_name_7a)>0, 1, 0)

# sba_post has a franchise
sba_post$has_franchise <- ifelse(nchar(sba_post$franchise_name_7a)>0, 1, 0)

# PPP has a franchise
dfm$franchise_2 <- ifelse(nchar(dfm$franchise_name_ppp) > 0, 1, 0)

dfm$has_franchise <- ifelse(nchar(dfm$franchise_name_ppp) > 0 |
                            nchar(dfm$franchise_name_7a) > 0, 1, 0)

# NAICS Sector Match
# Define a lookup table for sector groupings
naics_sector_map <- tibble(
  sector_group = c("11", "21", "22", "23", "31-33", "42", "44-45", "48-49",
                   "51", "52", "53", "54", "55", "56", "61", "62", "71",
                   "72", "81", "92"),
  sector_codes = list(
    "11", "21", "22", "23",
    c("31", "32", "33"),
    "42", c("44", "45"), c("48", "49"),
    "51", "52", "53", "54", "55", "56",
    "61", "62", "71", "72", "81", "92"
  )
)

# Extract first two digits
dfm <- dfm |>
  mutate(
    sba_sector_code = substr(as.character(naics_code_7a), 1, 2),
    ppp_sector_code = substr(as.character(naics_code_ppp), 1, 2)
  )

# Create a helper function to assign group
get_sector_group <- function(code) {
  matched_group <- naics_sector_map$sector_group[
    purrr::map_lgl(naics_sector_map$sector_codes, ~ code %in% .x)
  ]
  if (length(matched_group) == 1) return(matched_group)
  return(NA_character_)
}

# Apply mapping
dfm <- dfm |>
  mutate(
    sba_sector_group = purrr::map_chr(sba_sector_code, get_sector_group),
    ppp_sector_group = purrr::map_chr(ppp_sector_code, get_sector_group)
  )

# Filter or join on common sector groups
dfm <- dfm |>
  filter(sba_sector_group == ppp_sector_group)

# keep only state matches OR those that have a franchise

dfm$state_match <- dfm$BorrState == dfm$BorrowerState


#matching on state or franchise
dfm <- dfm[dfm$state_match == TRUE | dfm$has_franchise == 1 ,]

# NAICS Sector Categories
dfm$agriculture      <- ifelse(dfm$sba_sector_code == 11, 1, 0)
dfm$mining           <- ifelse(dfm$sba_sector_code == 21, 1, 0)
dfm$utilities        <- ifelse(dfm$sba_sector_code == 22, 1, 0)
dfm$construction     <- ifelse(dfm$sba_sector_code == 23, 1, 0)
dfm$manufacturing    <- ifelse(dfm$sba_sector_code == 31, 1, 0)
dfm$wholesale_trade  <- ifelse(dfm$sba_sector_code == 42, 1, 0)
dfm$retail_trade     <- ifelse(dfm$sba_sector_code == 44 | dfm$sba_sector_code == 45, 1, 0)
dfm$transport_ware   <- ifelse(dfm$sba_sector_code == 48 | dfm$sba_sector_code == 49, 1, 0)
dfm$information      <- ifelse(dfm$sba_sector_code == 51, 1, 0)
dfm$finance_ins      <- ifelse(dfm$sba_sector_code == 52, 1, 0)
dfm$real_estate      <- ifelse(dfm$sba_sector_code == 53, 1, 0)
dfm$professional     <- ifelse(dfm$sba_sector_code == 54, 1, 0)
dfm$admin_support    <- ifelse(dfm$sba_sector_code == 56, 1, 0)
dfm$education        <- ifelse(dfm$sba_sector_code == 61, 1, 0)
dfm$healthcare       <- ifelse(dfm$sba_sector_code == 62, 1, 0)
dfm$arts_entertain   <- ifelse(dfm$sba_sector_code == 71, 1, 0)
dfm$accommodation    <- ifelse(dfm$sba_sector_code == 72, 1, 0)
dfm$other_services   <- ifelse(dfm$sba_sector_code == 81, 1, 0)
dfm$public_admin     <- ifelse(dfm$sba_sector_code == 92, 1, 0)

sba_post$agriculture      <- ifelse(sba_post$naics_code_7a == 11, 1, 0)
sba_post$mining           <- ifelse(sba_post$naics_code_7a == 21, 1, 0)
sba_post$utilities        <- ifelse(sba_post$naics_code_7a == 22, 1, 0)
sba_post$construction     <- ifelse(sba_post$naics_code_7a == 23, 1, 0)
sba_post$manufacturing    <- ifelse(sba_post$naics_code_7a == 31, 1, 0)
sba_post$wholesale_trade  <- ifelse(sba_post$naics_code_7a == 42, 1, 0)
sba_post$retail_trade     <- ifelse(sba_post$naics_code_7a == 44 | sba_post$naics_code_7a == 45, 1, 0)
sba_post$transport_ware   <- ifelse(sba_post$naics_code_7a == 48 | sba_post$naics_code_7a == 49, 1, 0)
sba_post$information      <- ifelse(sba_post$naics_code_7a == 51, 1, 0)
sba_post$finance_ins      <- ifelse(sba_post$naics_code_7a == 52, 1, 0)
sba_post$real_estate      <- ifelse(sba_post$naics_code_7a == 53, 1, 0)
sba_post$professional     <- ifelse(sba_post$naics_code_7a == 54, 1, 0)
sba_post$admin_support    <- ifelse(sba_post$naics_code_7a == 56, 1, 0)
sba_post$education        <- ifelse(sba_post$naics_code_7a == 61, 1, 0)
sba_post$healthcare       <- ifelse(sba_post$naics_code_7a == 62, 1, 0)
sba_post$arts_entertain   <- ifelse(sba_post$naics_code_7a == 71, 1, 0)
sba_post$accommodation    <- ifelse(sba_post$naics_code_7a == 72, 1, 0)
sba_post$other_services   <- ifelse(sba_post$naics_code_7a == 81, 1, 0)
sba_post$public_admin     <- ifelse(sba_post$naics_code_7a == 92, 1, 0)

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
  sba_post[[col_name]] <- ifelse(sba_post$processing_method_7a == method, 1, 0)
}

for (method in names(processing_methods)) {
  col_name <- processing_methods[[method]]
  dfm[[col_name]] <- ifelse(dfm$processing_method_7a == method, 1, 0)
}

# Business Age Category Creation
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

for (category in names(business_age_categories)) {
  col_name <-business_age_categories[[category]]
  sba_post[[col_name]] <- ifelse(sba_post$BusinessAge == category, 1, 0)
}

# Create categorical region variables
northeast <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")
midwest <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
south <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", 
           "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")
west <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")

regions <- list(
  northeast = northeast,
  midwest = midwest,
  south = south,
  west = west
)

# Loop to create binary region variables
for (region_name in names(regions)) {
  dfm[[region_name]] <- ifelse(dfm$BorrState %in% regions[[region_name]], 1, 0)
}
for (region_name in names(regions)) {
  sba_post[[region_name]] <-ifelse(sba_post$BorrState %in% regions[[region_name]], 1, 0)
}

# Remove matched borrowers to get control group
sba_post <- sba_post |>
  mutate(name_match = as.integer(borr_name_clean %in% dfm$borr_name_clean))

sba_control <- sba_post |>
  filter(name_match == 0) |>
  select(-name_match)
# create clusters by month
start_date <- as.Date("2020-03-31")

# Convert approval dates to integer month offsets
dfm$approval_month_index <- 
  interval(start_date, dfm$approval_year_month) %/% months(1)

sba_control$approval_month_index <- 
  interval(start_date, sba_control$approval_year_month) %/% months(1)


# Add treatment indicator
dfm$ppp_treated <- 1
sba_control$ppp_treated <- 0


# Use Census data for race, ethnicity, LMI, and urban indicators
# race and ethnicity data
race_vars <- c(
  total = "B02001_001",
  white = "B02001_002",
  black = "B02001_003",
  native = "B02001_004",
  asian = "B02001_005",
  pacific = "B02001_006",
  other = "B02001_007",
  multiracial = "B02001_008"
)

zip_race <- get_acs(
  geography = "zip code tabulation area",
  variables = race_vars,
  year = 2020,
  survey = "acs5",
  output = "wide"
) |>
  mutate(
    pct_white = 100 * whiteE / totalE,
    pct_black = 100 * blackE / totalE,
    pct_native = 100 * nativeE / totalE,
    pct_asian = 100 * asianE / totalE,
    pct_pacific = 100 * pacificE / totalE,
    pct_other = 100 * otherE / totalE,
    pct_multiracial = 100 * multiracialE / totalE
  ) |>
  select(
    GEOID, NAME, total_pop = totalE,
    pct_white, pct_black, pct_native, pct_asian,
    pct_pacific, pct_other, pct_multiracial
  )

# Get ZIP median household income for LMI variable
zip_income <- get_acs(
  geography = "zip code tabulation area",
  variables = c(median_income = "B19013_001"),
  year = 2020,
  survey = "acs5",
  output = "wide"
) |>
  select(GEOID, median_income = median_incomeE)


# combine into a single main table
zcta_main <- zip_race |>
  left_join(zip_income, by = "GEOID")|> 
  st_drop_geometry()

# need to match variables to zip codes in dfm and sba control
dfm <- dfm |>
  mutate(zip_code = sprintf("%05s", BorrZip))

sba_control <- sba_control |>
  mutate(zip_code = sprintf("%05s", BorrZip))

ppp <- ppp |>
  mutate(zip_code = sprintf("%05s", BorrowerZip))

dfm_joined <- dfm |>
  left_join(
    zcta_main |>
      st_drop_geometry(),
    by = c("zip_code" = "GEOID")
  )

sba_control_joined <- sba_control |>
  left_join(
    zcta_main |>
      st_drop_geometry(),
    by = c("zip_code" = "GEOID")
  )

ppp_zip_joined <- ppp |>
  left_join(
    zcta_main|>
      st_drop_geometry(),
    by = c("zip_code" = "GEOID")
  )


# Control for Racial Variables
# PPP Initial Approval Amount
dfm_joined$log_ppp_amount <- log(dfm_joined$InitialApprovalAmount)

##### Robust Regression with Month Clustering for Analysis
reg <- lm_robust(log_approval_amount ~ race_black + hispanic + race_asian +
                   race_native + race_white + race_pacific + not_hispanic +
                   JobsSupported + processing_plp + processing_express + 
                   community_bank + regional_bank + 
                    FEDFUNDS, data = dfm_joined, se = "stata",
                 clusters = approval_month_index)
summary(reg)

modelsummary(reg, stars = T)

# Are the ACS race and ethnicity variables correlated with the ones we have from PPP
lm1 <- lm(pct_black ~ race_black + race_white + race_asian +
             race_native + race_multi + race_pacific + 
             race_puerto_rican, data = dfm_joined)

summary(lm1)

lm2 <- lm(pct_white ~ race_black + race_white + race_asian +
            race_native + race_multi + race_pacific + 
            race_puerto_rican, data = dfm_joined)

summary(lm2)
sd(dfm_joined$pct_black, na.rm = T)


###### ML Guide Sample Generation
# Create Sample for ML Guide
# Create Data Month Variable for PPP

# Create Limited Variables to Keep List for the ML Guide that Doesn't Have Race
variables_to_keep <- c("log_approval_amount", "ppp_treated", 
                       "agriculture", "mining", "utilities", "construction", "manufacturing",
                       "wholesale_trade", "retail_trade", "transport_ware", "information",
                       "finance_ins", "real_estate", "professional", "admin_support",
                       "education", "healthcare", "arts_entertain", "accommodation",
                       "other_services", "public_admin",
                       "age_established", "age_new", "age_startup", "age_changed_owner",
                       "processing_plp", "processing_express", "processing_7a_general",
                       "collateral", "commit", "pif", "ppp_tranche_1", "ppp_tranche_2",
                       "ppp_tranche_3", "charge_off", "cancelled", "JobsSupported",  
                       "InitialInterestRate", "has_franchise", "west", "northeast", "midwest",
                       "LMI_status", "approval_month_index",
                       "ppp_tranche_1", "ppp_tranche_2", "ApprovalYearMonth", 
                       "hispanic", "not_hispanic", "race_black", "race_asian",
                       "race_white", "race_multi", "race_pacific", "race_alaskan_native",
                       "race_puerto_rican", "race_unanswered", "race_native", "ethnicity_unanswered",
                       "zip_code")

#sample_dfm <- dfm_joined[, variables_to_keep] 

#sample_dfm <- na.omit(sample_dfm)

#sample_index <- createDataPartition(sample_dfm$log_approval_amount, p = .9,
#                                    list = FALSE)

#sample_dfm_small <- sample_dfm[-sample_index, ]

#write.csv(sample_dfm_small, file = "Data/sba_sample_data.csv")


##### ML Model 1. Log Loan Amount After PPP Treatment
# Align variables for stacking
vars_to_keep <- c("log_approval_amount", "ppp_treated", 
                  "agriculture", "mining", "utilities", "construction", "manufacturing",
                  "wholesale_trade", "retail_trade", "transport_ware", "information",
                  "finance_ins", "real_estate", "professional", "admin_support",
                  "education", "healthcare", "arts_entertain", "accommodation",
                  "other_services", "public_admin",
                  "age_established", "age_new", "age_startup", "age_changed_owner",
                  "processing_plp", "processing_express", "processing_7a_general",
                  "collateral", "commit", "pif", "FEDFUNDS",
                   "charge_off", "cancelled", "JobsSupported" 
                  , "has_franchise", "west", "northeast", "midwest",
                  "pct_black", "pct_native", "pct_asian",
                 "pct_pacific", "pct_other", "pct_multiracial", "approval_month_index",
                   "regional_bank", "community_bank", "fintech", "cdfi", 
                 "other_institution")


# Keep only complete cases for consistency
dfm_clean <- dfm_joined[, vars_to_keep] 
sba_control_clean <- sba_control_joined[, vars_to_keep] 

# Combine treated and control into a single data set
grf_data <- rbind(dfm_clean, sba_control_clean)


### Regression with Census Race Data
reg1 <- lm_robust(log_approval_amount ~ pct_black + pct_asian + pct_native +
                    pct_multiracial + pct_other + pct_pacific + 
                    JobsSupported + processing_plp + processing_express + 
                    community_bank + regional_bank + 
                     FEDFUNDS + has_franchise, data = grf_data, se = "stata",
                  clusters = approval_month_index)

modelsummary(reg1, stars = T)

summary(dfm_joined$age_changed_owner)
#Different Race Variables Comparison Table
modelsummary(
  list(
    "PPP Race Data" = reg,
    "ACS Race Data" = reg1
  ),
  title = "Comparison of Models with Paycheck Protection and American Community Survey Data",
  statistic = "std.error",
  stars = T,
  output = "Visuals/ppp_acs_reg_comparison.docx",
  gof_omit = "IC|Log|F|Adj|Std|AIC|BIC"
)
#grf_train_index <- createDataPartition(grf_data$ppp_treated, p = 0.9, list = FALSE)

# Split the dataset
#grf_train <- grf_data[grf_train_index, ]
#grf_test  <- grf_data[-grf_train_index, ]


Y <- grf_data$log_approval_amount

W <- grf_data$ppp_treated

X <- grf_data[,!(names(grf_data) %in% c("log_approval_amount", "ppp_treated"))] |>
  as.matrix()

# remove na values to get matching row numbers
grf_cln <- data.frame(Y = Y, W = W, X) |>
  na.omit()

Y <- grf_cln$Y

W <- grf_cln$W

X <- grf_cln[, !(names(grf_cln) %in% c("Y", "W"))] |>
  as.matrix()

cluster_var <- grf_cln$approval_month_index

# R-Learner and Orthogonalization
# Propensity Score Matching
prop_forest <- regression_forest(X, W, 
                                       num.trees = 1500, 
                                       min.node.size = 8)
# Predicted Propensity Scores
prop_scores <- predict(prop_forest)$predictions

# Outcome Forest
out_forest <- regression_forest(X, Y, 
                                    num.trees = 1500, 
                                    min.node.size = 8)
# Marginal Outcome Prediction
marginal_out <- predict(out_forest)$predictions

# Train Causal Forest on residual treatment and outcome 
residual_treat <- W - prop_scores
residual_out <- Y - marginal_out

# Remove Propensity and Outcome Forests to save on Memory
rm(prop_forest, out_forest)

rlearner <- causal_forest(X, residual_out, residual_treat,
                             num.trees = 1500, 
                             min.node.size = 8,
                             clusters = grf_cln$approval_month_index)

tau_hat <- predict(rlearner,)$predictions

summary(tau_hat)


var_imp <- variable_importance(rlearner)

var_imp_df <- data.frame(
  variable = colnames(X),
  importance = var_imp
)

# Arrange in descending order of importance
var_imp_df <- var_imp_df |>
  arrange(desc(importance))


var_imp_plot <- ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "#ffab40") +
  coord_flip() +
  labs(
    title = "Variable Importance Plot",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_ipsum()

ggsave(
  filename = "Visuals/variable_importance_plot_1.png",
  device = "png",
  plot = var_imp_plot,
  width = 8,
  height = 10,
  dpi = 300,
  units = "in"
)

# Density Plots for Treatment Effect
tau_df <- tibble(tau_hat = as.vector(tau_hat))

# Plot density of tau_hat
cate_density_plot <- ggplot(tau_df, aes(x = tau_hat)) +
  geom_density(fill = "steelblue", alpha = 0.6, color = "darkblue") +
  geom_vline(xintercept = median(tau_df$tau_hat), 
             linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Distribution of Conditional Average Treatment Effects",
    x = "Conditional Average Treatment Effect",
    y = "Density"
  ) +
  theme_minimal()

ggsave(
  plot = cate_density_plot,
  filename = "Visuals/amount_cate_density_plot.png",
  dpi = 300,
  unit = "in",
  width = 13.86,
  height = 6.24,
  device = "png"
)

# create a density plot for treated and untreated
treat_df <- grf_cln |>
  mutate(cate = tau_hat)

treat_group <- treat_df |>
  filter(W == 1)
control_group <- treat_df |>
  filter(W == 0)

# Creating Combined Object for Comparison Plot
treat_control_plot <- rbind(
  data.frame(cate = treat_group$cate, group = "PPP Recipients"),
  data.frame(cate = control_group$cate, group = "Control")
)

cate_comparison_plot <- ggplot(treat_control_plot, aes(x = cate, fill = group, color = group)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  labs(
    title = "Conditional Average Treatment Effect Distribution",
    x = "Estimated Treatment Effect (CATE)",
    y = "Density"
  ) + 
  geom_vline(xintercept = mean(treat_group$cate), color = "aquamarine")+
  geom_vline(xintercept = mean(control_group$cate), color = "red")+
  theme_minimal()

ggsave(
  plot = cate_comparison_plot,
  filename = "Visuals/loan_amount_cate_treat_control_plot.png",
  device = "png",
  dpi = 300,
  units = "in",
  height = 6.24,
  width = 13.86
)

# Compare Between PLP and Express
# Group by Processing Method
group_express <- treat_df |> filter(processing_express == 1)
group_plp <- treat_df |> filter(processing_plp == 1)

compare_df <- rbind(
  data.frame(cate = group_express$cate, group = "7(a) Express"),
  data.frame(cate = group_plp$cate, group = "Preferred Lender Program")
)

# compare with histogram
cate_processing_comparison_plot <- ggplot(
  compare_df, aes(x = cate, fill = group, color = group)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  labs(
    title = "CATE Distribution: Express vs PLP",
    x = "Estimated Treatment Effect (CATE)",
    y = "Density"
  ) +
  theme_minimal()

ggsave(
  filename = "Visuals/cate_processing_comparison_plot.png",
  plot = cate_processing_comparison_plot,
  dpi = 300,
  units = "in",
  width = 13.86,
  height = 6.24,
  device = "png"
)

# Create 95% Confidence Intervals
n <- length(tau_hat)

# Mean and standard error
mean_cate <- mean(tau_hat)
se_cate <- sd(tau_hat) / sqrt(n)

# 95% Confidence Interval using normal approx
ci_lower <- mean_cate - 1.96 * se_cate
ci_upper <- mean_cate + 1.96 * se_cate

cat("Mean CATE (Test):", round(mean_cate, 4), "\n")
cat("95% CI:", round(ci_lower, 4), "to", round(ci_upper, 4), "\n")

# PLP and Express
plp_cate <- group_plp$cate
express_cate <- group_express$cate

# Summary stats function
compute_summary <- function(x) {
  mean_x <- mean(x)
  se_x <- sd(x) / sqrt(length(x))
  ci_lower <- mean_x - 1.96 * se_x
  ci_upper <- mean_x + 1.96 * se_x
  tibble(
    mean = mean_x,
    std_error = se_x,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

# Compute stats
main_stats <- compute_summary(tau_hat) |> mutate(group = "Overall CATE")
plp_stats <- compute_summary(plp_cate) |> mutate(group = "PLP")
express_stats <- compute_summary(express_cate) |> mutate(group = "Express")

# Combine into one table
summary_table <- bind_rows(main_stats, plp_stats, express_stats) |>
  select(group, mean, std_error, ci_lower, ci_upper)

print(summary_table)

# Presentation Table
presentation_table <- summary_table |>
  mutate(
    Mean = round(mean, 4),
    `Standard Error` = round(std_error, 4),
    `95% CI` = sprintf("[%.4f, %.4f]", ci_lower, ci_upper)
  ) |>
  select(Group = group, Mean, `Standard Error`, `95% CI`) |>
  kable(format = "html", escape = FALSE, align = "c", 
        caption = "Conditional Average Treatment Effect by Processing Method") |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    font_size = 14
  ) |>
  row_spec(0, bold = TRUE, background = "#f2f2f2") |>
  column_spec(1, bold = TRUE)

presentation_table


# Best Linear Projection
# create list of important covariates
important_covariates <- c("processing_express", "processing_plp", "community_bank",
                          "JobsSupported", "approval_month_index", "FEDFUNDS",
                          "has_franchise", "regional_bank", "pct_other",
                          "pct_black", "pct_multiracial", "pct_asian",
                          "pct_native", "pct_pacific")

imp_cov <-  X[ , important_covariates]
 
  
blp_rlearner <- best_linear_projection(rlearner, A = imp_cov)
print(blp_rlearner)

# Create Comparison Table for BLP and Robust Linear Regression Model Table
modelsummary(
  list(
    "Best Linear Projection" = blp_rlearner,
    "Clustered Model" = reg1 
  ),
  statistic = "std.error",
  stars = T,
  output = "Visuals/blp_lm_comparison.docx",
  title = "Best Linear Projection and Linear Model Comparison Table",
  gof_omit = "AIC|BIC"
)


##### ML Model for Interest Rate Treatment
v_to_keep <- c("InitialInterestRate", "ppp_treated", 
                  "agriculture", "mining", "utilities", "construction", "manufacturing",
                  "wholesale_trade", "retail_trade", "transport_ware", "information",
                  "finance_ins", "real_estate", "professional", "admin_support",
                  "education", "healthcare", "arts_entertain", "accommodation",
                  "other_services", "public_admin",
                  "age_established", "age_new", "age_startup", "age_changed_owner",
                  "processing_plp", "processing_express", "processing_7a_general",
                  "collateral", "commit", "pif", "FEDFUNDS",
                  "charge_off", "cancelled", "JobsSupported" 
                  , "has_franchise", "west", "northeast", "midwest",
                  "pct_black", "pct_native", "pct_asian",
                  "pct_pacific", "pct_other", "pct_multiracial", "approval_month_index",
                  "regional_bank", "community_bank", "fintech", "cdfi", 
                  "other_institution", "LMIIndicator", "Race", "ApprovalYearMonth",
               "JobsSupported", "GrossApproval")



# Keep only complete cases for consistency
dfm_clean_2 <- dfm_joined[, v_to_keep] 
sba_control_clean_2 <- sba_control_joined[, v_to_keep] 

# Combine treated and control into a single data set
grf_data_2<- rbind(dfm_clean_2, sba_control_clean_2)


Y2 <- grf_data_2$InitialInterestRate

W2 <- grf_data_2$ppp_treated

X2 <- grf_data_2[,!(names(grf_data_2) %in% c("InitialInterestRate", "ppp_treated"))] |>
  as.matrix()

# remove na values to get matching row numbers
grf_clean <- data.frame(Y2 = Y2, W2 = W2, X2) |>
  na.omit()

Y2 <- grf_clean$Y2

W2 <- grf_clean$W2

X2 <- grf_clean[, !(names(grf_clean) %in% c("Y2", "W2"))] |>
  as.matrix()

cluster_var <- grf_clean$approval_month_index

# R-Learner and Orthogonalization
# Propensity Score Matching
prop_forest_2 <- regression_forest(X2, W2, 
                                 num.trees = 1500, 
                                 min.node.size = 8)
# Predicted Propensity Scores
prop_scores_2 <- predict(prop_forest_2)$predictions

# Outcome Forest
out_forest_2 <- regression_forest(X2, Y2, 
                                num.trees = 1500, 
                                min.node.size = 8)
# Marginal Outcome Prediction
marginal_out_2 <- predict(out_forest_2)$predictions

# Train Causal Forest on residual treatment and outcome 
residual_treat <- W - prop_scores
residual_out <- Y - marginal_out

# Remove Propensity and Outcome Forests to save on Memory
rm(prop_forest_2, out_forest_2)

rlearner_2 <- causal_forest(X2, residual_out_2, residual_treat_2,
                          num.trees = 1500, 
                          min.node.size = 8,
                          clusters = grf_cln$approval_month_index)

tau_hat_2 <- predict(rlearner_2,)$predictions

summary(tau_hat_2)

var_imp_2 <- variable_importance(rlearner_2)

var_imp_df_2 <- data.frame(
  variable = colnames(X2),
  importance = var_imp_2
)

# Arrange in descending order of importance
var_imp_df_2 <- var_imp_df_2 |>
  arrange(desc(importance))


var_imp_plot_2 <- ggplot(var_imp_df_2, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "#ffab40") +
  coord_flip() +
  labs(
    title = "Variable Importance Plot",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_ipsum()

ggsave(
  filename = "Visuals/variable_importance_plot_2.png",
  device = "png",
  plot = var_imp_plot_2,
  width = 8,
  height = 10,
  dpi = 300,
  units = "in"
)

##### Evaluation Causal Forest Splitting Off Treatment and Control Groups
# Create test and training index to split data
set.seed(123)  
grf_train_index <- createDataPartition(grf_data_2$ppp_treated, p = 0.9, list = FALSE)

# Split the dataset
grf_train <- grf_data_2[grf_train_index, ]
grf_test  <- grf_data_2[-grf_train_index, ]

write.csv(grf_test, file = "Data/sba_sample_data.csv")

# Outcome
Y2 <- grf_train$log_approval_amount

# Treatment
W2 <- grf_train$ppp_treated

# Covariates matrix
X2 <- grf_train[, !(names(grf_data) %in% c("log_approval_amount", "ppp_treated"))] |> as.matrix()

grf_clean <- data.frame(Y2 = Y2, W2 = W2, X2) |>
  na.omit()

# Treatment
grf_test_clean <- grf_test |>
  select(log_approval_amount, ppp_treated, everything()) |> 
  na.omit()

# Assign outcome and treatment
Y2_test <- grf_test_clean$log_approval_amount
W2_test <- grf_test_clean$ppp_treated

# Drop outcome and treatment from covariates
X2_test <- grf_test_clean |>
  select(-log_approval_amount, -ppp_treated) |>
  as.matrix()

# Cluster vector (if applicable)
cluster_test <- grf_test_clean$approval_month_index

# Extract cleaned variables
Y2 <- grf_clean$Y2
W2 <- grf_clean$W2
X2 <- grf_clean[, !(names(grf_clean) %in% c("Y2", "W2"))] |>
  as.matrix()

# R-Learner and Orthogonalization
# Propensity Score Matching
propensity_forest <- regression_forest(X2, W2, 
                                       num.trees = 1500, 
                                       min.node.size = 8)
# Predicted Propensity Scores
propensity_scores <- predict(propensity_forest)$predictions

# Outcome Forest
outcome_forest <- regression_forest(X2, Y2, 
                                    num.trees = 1500, 
                                    min.node.size = 8)
# Marginal Outcome Prediction
marginal_outcomes <- predict(outcome_forest)$predictions

# Train Causal Forest on residual treatment and outcome 
residual_treatment <- W2 - propensity_scores
residual_outcome <- Y2 - marginal_outcomes

# Remove Propensity and Outcome Forests to save on Memory
rm(propensity_forest, outcome_forest)

rlearner_cf <- causal_forest(X2, residual_outcome, residual_treatment,
                             num.trees = 1500, 
                             min.node.size = 8,
                             clusters = grf_clean$approval_month_index)

# predict treatment effects
tau_hat_rlearner <- predict(rlearner_cf)$predictions
tau_hat_eval <- predict(rlearner_cf, newdata = X2_test)$predictions

summary(tau_hat_rlearner)

# Variable importance plot
vi <- variable_importance(rlearner_cf)

vi_df <- data.frame(
  variable = colnames(X2),
  importance = vi
)

# Arrange in descending order of importance
vi_df <- vi_df |>
  arrange(desc(importance))


var_imp_plot_2 <- ggplot(vi_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "#ffab40") +
  coord_flip() +
  labs(
    title = "Variable Importance Plot",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_ipsum()

ggsave(
  filename = "Visuals/variable_importance_plot.png",
  device = "png",
  plot = var_imp_plot_2,
  width = 8,
  height = 10,
  dpi = 300,
  units = "in"
)

# evaluate predictions
# Predict CATEs on training set
tau_hat_train <- predict(rlearner_cf, newdata = X2)$predictions

# Predict CATEs on test set
tau_hat_test <- predict(rlearner_cf, newdata = X2_test)$predictions

# Summarize training predictions
summary(tau_hat_train)

# Summarize test predictions
summary(tau_hat_test)

# Get means and SDs
cat("Mean CATE (Train):", mean(tau_hat_train), "\n")
cat("Mean CATE (Test):", mean(tau_hat_test), "\n")

cat("SD CATE (Train):", sd(tau_hat_train), "\n")
cat("SD CATE (Test):", sd(tau_hat_test), "\n")

# Combine into one data frame
cate_df <- rbind(
  data.frame(cate = tau_hat_train, group = "Train"),
  data.frame(cate = tau_hat_test, group = "Test")
)

# Plot to compare CATE distributions
cate_dist_plot <- ggplot(cate_df, aes(x = cate, fill = group, color = group)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  labs(
    title = "Distribution of Conditional Average Treatment Effects",
    x = "Estimated Treatment Effect (CATE)",
    y = "Density") +
  geom_vline(xintercept = median(tau_hat_test), color = "black")+
  theme_minimal()

ggsave(
  plot = cate_dist_plot,
  filename = "Visuals/cate_hist.png",
  device = "png",
  dpi = 300,
  unit = "in",
  height = 6.24,
  width = 13.86
)

##### Comparing CATEs for Treated and Non-Treated Groups
# Add predictions and treatment status to a data frame
test_results <- grf_test_clean |>
  mutate(cate = tau_hat_test)

# Split test set into treated and control groups
test_treated <- test_results |> filter(ppp_treated == 1)
test_control <- test_results |> filter(ppp_treated == 0)

# Summary statistics
cat("Treated group:\n")
summary(test_treated$cate)
cat("\nControl group:\n")
summary(test_control$cate)

# Optional: Compare means and SDs
cat("Mean CATE (Treated):", mean(test_treated$cate), "\n")
cat("Mean CATE (Control):", mean(test_control$cate), "\n")
cat("SD CATE (Treated):", sd(test_treated$cate), "\n")
cat("SD CATE (Control):", sd(test_control$cate), "\n")

# Combine for plotting
test_plot_df <- rbind(
  data.frame(cate = test_treated$cate, group = "PPP Recipients"),
  data.frame(cate = test_control$cate, group = "Control")
)

# Density plot to compare distributions
cate_comparison_plot <- ggplot(test_plot_df, aes(x = cate, fill = group, color = group)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  labs(
    title = "Conditional Average Treatment Effect Distribution",
    x = "Estimated Treatment Effect (CATE)",
    y = "Density"
  ) + 
  geom_vline(xintercept = mean(test_treated$cate), color = "aquamarine")+
  geom_vline(xintercept = mean(test_control$cate), color = "red")+
  theme_minimal()

ggsave(
  filename = "Visuals/cate_comparison_plot.png",
  plot = cate_comparison_plot,
  dpi = 300,
  units = "in",
  device = "png",
  height = 6.24,
  width = 13.86
) 

# Confidence interval for the mean CATE of the test group
# Sample size
n <- length(tau_hat_test)

# Mean and standard error
mean_cate <- mean(tau_hat_test)
se_cate <- sd(tau_hat_test) / sqrt(n)

# 95% Confidence Interval using normal approx
ci_lower <- mean_cate - 1.96 * se_cate
ci_upper <- mean_cate + 1.96 * se_cate

cat("Mean CATE (Test):", round(mean_cate, 4), "\n")
cat("95% CI:", round(ci_lower, 4), "to", round(ci_upper, 4), "\n")

##### Comparing CATE for different processing methods
# Filter by group membership
group_express <- test_results |> filter(processing_express == 1)
group_plp <- test_results |> filter(processing_plp == 1)

# Summarize each group's CATEs
cat("Processing EXPRESS Group:\n")
summary(group_express$cate)
cat("Mean CATE:", mean(group_express$cate), "\n")
cat("SD CATE:", sd(group_express$cate), "\n\n")

cat("Processing PLP Group:\n")
summary(group_plp$cate)
cat("Mean CATE:", mean(group_plp$cate), "\n")
cat("SD CATE:", sd(group_plp$cate), "\n\n")

# visualization and comparison of CATE distribution
# Combine for plotting
compare_df <- rbind(
  data.frame(cate = group_express$cate, group = "7(a) Express"),
  data.frame(cate = group_plp$cate, group = "Preferred Lender Program")
)

# compare with histogram
cate_processing_comparison_plot <- ggplot(
  compare_df, aes(x = cate, fill = group, color = group)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  labs(
    title = "CATE Distribution: Express vs PLP",
    x = "Estimated Treatment Effect (CATE)",
    y = "Density"
  ) +
  theme_minimal()

ggsave(
  filename = "Visuals/cate_processing_comparison_plot.png",
  plot = cate_processing_comparison_plot,
  dpi = 300,
  units = "in",
  width = 13.86,
  height = 6.24,
  device = "png"
)

# estimate ATE
ate_rlearner <- average_treatment_effect(rlearner_cf, target.sample = "all")
print(ate_rlearner)

ate_rlearner_treated <- average_treatment_effect(rlearner_cf,
                                                 target.sample = treated)

ate_rlearner_control <- average_treatment_effect(rlearner_cf,
                                                 target.sample = control)

print(ate_rlearner_treated)
print(ate_rlearner_control)

# Estimate Group Average Treatment Effects
group_estimates <- average_treatment_effect(
  rlearner_cf,
  target.sample = "all",
  cluster = grf_clean$approval_month_index
)

# Evaluate the Model
eval_forest <- causal_forest(
  X2_test, Y2_test, W2_test,
  clusters = cluster_test,
  num.trees = 1500,
  min.node.size = 8
)

# Rank Average Treatment Effect (TOC)
tau_hat_eval <- predict(rlearner_cf, newdata = X2_test)$predictions
rate_cate <- rank_average_treatment_effect(eval_forest, tau_hat_eval)

# Plot TOC
plot(rate_cate, main = "TOC Curve: R-Learner on Test Data")

# Best Linear Projection of R-Learner
blp_rlearner <- best_linear_projection(rlearner_cf, X2)
print(blp_rlearner)

desc(blp_rlearner)

# Get Scores of R-Learner
scores_rlearner <- get_scores(rlearner_cf)
print(scores_rlearner)

blp_rlearner$

# Regression using predicted treatment effects
lm_model <- lm(tau_hat_rlearner ~ X2)
coeftest(lm_model, vcov = cluster.vcov(lm_model, grf_data$approval_month_index))


# Testing Calibration of R-Learner
test_calibration(rlearner_cf)

# assessing fit
e.hat <- rlearner_cf$W.hat

# Creating CI for R-Learner

high_effect <- tau_hat_rlearner > median(tau_hat_rlearner)
ate_high <- average_treatment_effect(rlearner_cf, subset = high_effect)
ate_low <- average_treatment_effect(rlearner_cf, subset = !high_effect)

ci <- ate_high[["estimate"]] - ate_low[["estimate"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(ate_high[["std.err"]]^2 + ate_low[["std.err"]]^2)

print(ci)

# Histogram of Bias over Standard Deviation of the Outcome
# find bias
p <- mean(W2)
Y.hat.0 <- rlearner_cf$Y.hat - e.hat * tau_hat_rlearner
Y.hat.1 <- rlearner_cf$Y.hat + (1 - e.hat) * tau_hat_rlearner

bias <- (e.hat - p) * (p * (Y.hat.0 - mean(Y.hat.0))  + (1 - p) * (Y.hat.1 - mean(Y.hat.1)))
bias_sd <- bias/sd(Y2)
# plot histogram
basic_bias_hist <-hist(bias / sd(Y2))

# BLP chart
blp_table <- stargazer(blp_rlearner, title = "Best Linear Projection of Model", align = T,
          type = "text")

# R-Learner DAta Visualization
# hist of estimated treatment effects
tau_df <- data.frame(tau_hat = tau_hat_rlearner)

rlearner_ate_hist <- ggplot(tau_df, aes(x = tau_hat)) +
  geom_histogram(bins = 50, color = "black", fill = "#065f92") +
  labs(title = "Distribution of Estimated Treatment Effects",
       x = "Estimated Treatment Effect",
       y = "Count") +
  theme_minimal()

# Treatment Heterogeneity Across a Sectors


# BLP Plot
blp_df <- data.frame(
  variable = names(blp_rlearner$coefficients),
  coefficient = blp_rlearner$coefficients,
  se = blp_rlearner$std.errors
)

rlearner_blp_plt <- ggplot(blp_df, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96 * se, ymax = coefficient + 1.96 * se), width = 0.2) +
  coord_flip() +
  labs(title = "Best Linear Projection Coefficients",
       x = "Covariate",
       y = "Coefficient") +
  theme_minimal()


# Plotting R-Learner Forest
# install and load the diagrammeR and diagrammeRsvg package
plot(rlearner_cf) 

##### Create Data Set for Guide
v_to_keep <- c("InitialInterestRate", "ppp_treated", 
               "agriculture", "mining", "utilities", "construction", "manufacturing",
               "wholesale_trade", "retail_trade", "transport_ware", "information",
               "finance_ins", "real_estate", "professional", "admin_support",
               "education", "healthcare", "arts_entertain", "accommodation",
               "other_services", "public_admin",
               "age_established", "age_new", "age_startup", "age_changed_owner",
               "processing_plp", "processing_express", "processing_7a_general",
               "collateral", "commit", "pif",
               "charge_off", "cancelled" 
               , "has_franchise", "west", "northeast", "midwest",
               "pct_black", "pct_native", "pct_asian",
               "pct_pacific", "pct_other", "pct_multiracial", "approval_month_index",
               "regional_bank", "community_bank", "fintech", "cdfi", 
               "other_institution", "LMIIndicator", "Race", "ApprovalYearMonth",
               "JobsSupported", "GrossApproval", "race_black", "race_asian",
               "race_native", "race_white", "race_unanswered", 
               "race_multi", "race_pacific", "race_puerto_rican","hispanic", 
               "not_hispanic", "ethnicity_unanswered", "log_approval_amount",
               "zip_code") 
# Create Data for Guide
sample_dfm <- dfm_joined[, v_to_keep] 

sample_dfm <- na.omit(sample_dfm)

sample_index <- createDataPartition(sample_dfm$processing_plp, p = .9,
                                    list = FALSE)

sample_dfm_small <- sample_dfm[-sample_index, ]

write.csv(sample_dfm_small, file = "Data/sba_sample_data.csv")

######## Summary Stat Geographic Plots
# aggregate dollar amount and loan number by zip
sba_zip_summary <- sba_control |>
  filter(!is.na(zip_code) & zip_code != "") |>
  group_by(zip_code) |>
  summarise(
    total_amount = sum(GrossApproval, na.rm = TRUE),
    loan_count = n(),
    
  ) |>
  rename(ZIP = zip_code)


# Load ZCTA geometries
zcta_shapes <- zctas(year = 2020) |>
  st_make_valid()

# Load state geometries to get valid CONUS + AK, HI mask
states_shapes <- states(cb = TRUE, year = 2020) |>
  filter(
    !STUSPS %in% c("PR", "VI", "GU", "MP", "AS", "UM")  # exclude territories
  ) |>
  st_transform(crs = st_crs(zcta_shapes))

# Keep only ZCTAs intersecting the 50 states
zcta_shapes_50 <- zcta_shapes |>
  st_filter(states_shapes, .predicate = st_intersects)

# Add ZIP code column for joining
zcta_shapes_50 <- zcta_shapes_50 |>
  mutate(ZIP = ZCTA5CE20)

# join information
sba_zip_geo <- zcta_shapes_50 |>
  left_join(sba_zip_summary, by = "ZIP")


# Plot dollar amount by zip code
ggplot(sba_zip_geo) +
  geom_sf(aes(fill = total_amount), color = NA) +
  scale_fill_viridis(
    option = "magma",
    trans = "log",
    na.value = "grey90",
    name = "Total Loan Amount\n(log scale)"
  ) +
  labs(
    title = "Total Dollar Amount of SBA 7(a) Loans by ZIP Code",
    subtitle = "FY2020-present, log scale",
    caption = "Source: SBA FOIA Data"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme_void()

# Plot number of 7a loans by zip code
ggplot(sba_zip_geo) +
  geom_sf(aes(fill = loan_count), color = NA) +
  scale_fill_viridis(
    option = "magma",
    trans = "log",
    na.value = "grey90",
    name = "Loan Count\n(log scale)"
  ) +
  labs(
    title = "Number of SBA 7(a) Loans by ZIP Code",
    subtitle = "FY2020-present, log scale",
    caption = "Source: SBA FOIA Data"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme_void()

# urban areas
sba_zip_geo <- sba_zip_geo |>
  left_join(zcta_urban_flag |> st_drop_geometry(), by = "ZIP")

# LMI areas using 80% of state avg income
state_income <- get_acs(
  geography = "state",
  variables = c(median_income = "B19013_001"),
  year = 2020,
  survey = "acs5",
  output = "wide"
) |>
  transmute(
    state_name = NAME,
    state_median_income = median_incomeE,
    lmi_threshold = 0.8 * median_incomeE
  )

# get zip code income
zip_income <- get_acs(
  geography = "zip code tabulation area",
  variables = c(median_income = "B19013_001"),
  year = 2020,
  survey = "acs5",
  output = "wide"
) |>
  transmute(
    ZIP = GEOID,
    median_income = median_incomeE
  )

# zip code crosswalk
zip_state_xwalk <- zcta_shapes_50 |>
  st_join(states_shapes |> select(STUSPS, NAME), left = TRUE) |>
  st_drop_geometry() |>
  select(ZIP = ZCTA5CE20, state_name = NAME)

zip_income <- zip_income |>
  left_join(zip_state_xwalk, by = "ZIP") |>
  left_join(state_income, by = "state_name") |>
  mutate(
    LMI_status = if_else(median_income < lmi_threshold, 1, 0)
  )
zip_income_geo <- zcta_shapes_50 |>
  left_join(zip_income, by = "ZIP")
# LMI Plot


ggplot(zip_income_geo) +
  geom_sf(aes(fill = LMI_status), color = NA) +
  labs(
    title = "Low and Moderate-Income ZIP Codes",
    subtitle = "Defined as ZIPs with Median Household Income < 80% of State Median (ACS 2020)",
    caption = "Source: ACS 2020 5-Year Estimates",
    x = NULL, y = NULL
  ) +
  coord_sf(crs = 3857) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10)
  )+
  theme_void()

# Income distribution across zip codes
ggplot(zip_income_geo) +
  geom_sf(aes(fill = median_income), color = NA) +
  scale_fill_viridis(
    option = "magma",
    na.value = "grey90",
    name = "Median Household\nIncome (USD)"
  ) +
  labs(
    title = "Median Household Income by ZIP Code (ACS 2020)",
    subtitle = "ZIP Code Tabulation Areas",
    caption = "Source: ACS 2020 5-Year Estimates"
  ) +
  coord_sf(crs = 3857) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10)
  )+
  theme_void()

# plot PPP loan volume by ZIP code

zcta_sf <- get_acs(
  geography = "zip code tabulation area",
  variables = "B01003_001",
  year = 2020,
  survey = "acs5",
  geometry = TRUE,
  output = "wide",
  resolution = "20m"
) |>
  select(zip_code = GEOID, geometry)

zcta_sf <- shift_geometry(zcta_sf)

dfm_zip_summary <- dfm |>
  filter(!is.na(zip_code), !is.na(log_ppp_amount)) |>
  group_by(zip_code) |>
  summarise(
    mean_log_approval = mean(log_ppp_amount, na.rm = TRUE),
    loan_count = n(),
    .groups = "drop"
  )
dfm_map_data <- zcta_sf |>
  left_join(dfm_zip_summary, by = "zip_code")

state_borders <- states(cb = TRUE, resolution = "20m") |>
  shift_geometry()

ppp_loan_volume_plot <- ggplot()+
  geom_sf(data = dfm_map_data, aes(fill = loan_count), color = NA)+
  geom_sf(data = state_borders, fill = NA, color = "black", linewidth = .3)+
  scale_fill_viridis(
    name = "Avg. PPP Log Loan Volume by ZIP",
    option = "plasma",
    direction = -1,
    na.value = "grey"
  ) + labs(
    title = " SBA PPP Log Loan Volume by ZIP Code",
    caption = "Source: SBA PPP FOIA and ACS"
  ) + theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )+
  theme_void()

ggsave(
  plot = ppp_loan_volume_plot,
  filename = "Visuals/ppp_volume_by_zip.png",
  device = "png",
  dpi = 300,
  units = "in",
  width = 12,
  height = 10
)


loan_approval_plot <- ggplot() +
  geom_sf(data = sba_map_data, aes(fill = mean_log_approval), color = NA) +
  geom_sf(data = state_borders, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(
    name = "Avg. Log Loan Amount",
    option = "plasma",
    direction = -1,
    na.value = "grey90"
  ) +
  labs(
    title = "SBA 7(a) Log Loan Amount by ZIP Code",
    caption = "Data: Small Business Administration + ACS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )+
  theme_void()


# Trying to compare summary statistics between Express and PLP Groups
exp_dfm <- dfm_joined |>
  filter( , dfm_joined$processing_express == 1)

plp_dfm <- dfm_joined |>
  filter( , dfm_joined$processing_plp == 1)
#######
institution_types <- c(
  "fintech", "regional_bank", "national_bank",
  "community_bank", "cdfi", "other_institution"
)

# Count proportions in each subset
get_proportions <- function(data) {
  data |>
    summarise(across(
      all_of(institution_types),
      ~ mean(.x == 1, na.rm = TRUE)
    ))
}

exp_props <- get_proportions(exp_dfm)
plp_props <- get_proportions(plp_dfm)

comparison_table <- bind_rows(
  express = exp_props,
  plp = plp_props,
  .id = "loan_type"
) |>
  pivot_longer(
    cols = -loan_type,
    names_to = "institution_type",
    values_to = "proportion"
  ) |>
  pivot_wider(
    names_from = loan_type,
    values_from = proportion
  ) |>
  mutate(across(c(express, plp), scales::percent_format(accuracy = 0.1)))

# Create a gt table
gt_table <- comparison_table |>
  gt() |>
  tab_header(
    title = "Institution Type Proportions by Loan Type"
  ) |>
  cols_label(
    institution_type = "Institution Type",
    express = "7(a) Express",
    plp = "Preferred Lender Program"
  ) |>
  fmt_missing(everything(), missing_text = "N/A")


gtsave(gt_table, "Visuals/institution_comparison_table.pdf", expand = 10)
########
summary(plp_dfm$pct_white)
summary(exp_dfm$pct_white)

summary(exp_dfm$median_income)

summary(plp_dfm$median_income)

summary(plp_dfm$cdfi)
summary(exp_dfm$cdfi)

summary(plp_dfm$community_bank)
summary(exp_dfm$community_bank)

summary(exp_dfm$national_bank)
summary(plp_dfm$national_bank)

summary(exp_dfm$fintech)
summary(plp_dfm$fintech)



reg2 <- lm(log_approval_amount ~ median_income +
             pct_white + pct_black + pct_asian +
             pct_native + pct_multiracial +age_established + age_startup + age_new + 
             age_changed_owner, data = dfm_joined)

summary(reg2)

ppp$gender_unanswered <- ifelse(ppp$Gender == "Unanswered", 1, 0)


# Race and Median Income Regressions and Tables
dfm_joined$pct_minority <- (100 - dfm_joined$pct_white)
dfm_joined$log_med_inc <- log(dfm_joined$median_income)


lm1 <-lm(log_approval_amount ~ pct_black + pct_asian +
           pct_native + pct_multiracial + pct_pacific + log_med_inc, data = dfm_joined)
summary(lm1)

lm2 <-lm(log_approval_amount ~ pct_black + pct_asian +
           pct_native + pct_multiracial + pct_pacific + log_med_inc +
           age_new + age_changed_owner + age_startup + age_established +
           midwest + south + west + has_franchise + construction + JobsSupported +
           processing_plp + processing_express
         , data = dfm_joined)
summary(lm2)

dfm_joined$so
lm3 <-lm(log_approval_amount ~ pct_minority+ log_med_inc +
           age_new + age_changed_owner + age_startup + age_established +
           midwest + south + west + has_franchise + construction + JobsSupported +
           processing_plp + processing_express
         , data = dfm_joined)
summary(lm3)


sd(dfm_joined$log_med_inc, na.rm = T)

sd(dfm_joined$pct_asian, na.rm = T)


summary(plp_dfm$GrossApproval)
summary(exp_dfm$GrossApproval)


## Plot of 7a Loan Approvals by Month
grf_cln <- grf_cln |>
  mutate(approval_year_month = as.Date(approval_year_month))

# Aggregate counts per month
monthly_counts <- grf_cln |>
  count(approval_month_index)

# Plot with trend line
monthly_counts |>
  ggplot(aes(x = approval_month_index, y = n)) +
  geom_col(fill = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue", linewidth = 1) +
  labs(
    title = "Monthly Approval Counts",
    x = "Approval Date",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  filename = "Visuals/loan_approvals_by_month.png",
  plot = sba_loans_by_month,
  dpi = 300,
  units = "in",
  height = 6.24,
  width = 13.86,
  device = "png"
)


# Model Combining both types of race variables
# Combining the data sets using both data points

combined_reg <- lm_robust(log_approval_amount ~ race_black + hispanic + race_asian +
                            race_native + race_white + race_pacific + not_hispanic +
                            pct_black + pct_asian + pct_native +
            pct_multiracial + pct_other + pct_pacific + 
            JobsSupported + processing_plp + processing_express + 
            community_bank + regional_bank + 
            FEDFUNDS + has_franchise, data = dfm_joined, se = "stata",
          clusters = approval_month_index)
summary(combined_reg)

modelsummary(combined_reg, stars = T)


lm_robust(log_approval_amount ~ race_black + hispanic + race_asian +
            race_native + race_white + race_pacific + not_hispanic +
            JobsSupported + processing_plp + processing_express + 
            community_bank + regional_bank + 
            FEDFUNDS, data = dfm_joined, se = "stata",
          clusters = approval_month_index)

# Getting a Count of Loans under $100,000 by Financial Institution
# Loan Count by Institution Size
sba |>
  filter(GrossApproval < 100000) |>
  count(InstitutionSize, name = "n_under_100k")
# Total Count
sba |>
  count(InstitutionSize)

# proportion of small dollar lending
small_loans <- sba |>
    filter(GrossApproval < 100000) |>
    count(InstitutionSize, name = "n_under_100k")
 
all_loans <-sba |>
    count(InstitutionSize)
   
#Proportion of Loans under $100,000
small_loans$n_under_100k /all_loans$n

# Table
target_sizes <- c(
  "Regional Bank/CU ($10B<Assets<$100B)",
  "Community Bank/CU (Assets<$10B)",
  "National Bank/CU (Assets>$100B)",
  "Other Non-Bank/CU",
  "CDFI",
  "Fintech"
)

loan_summary <- sba |>
  filter(InstitutionSize %in% target_sizes) |>
  group_by(InstitutionSize) |>
  summarise(
    total_loans = n(),
    loans_under_100k = sum(GrossApproval < 100000, na.rm = TRUE),
    prop_under_100k = loans_under_100k / total_loans
  ) |>
  arrange(desc(prop_under_100k))


loan_summary
loan_summary |>
  mutate(prop_under_100k = scales::percent(prop_under_100k, accuracy = 0.1)) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Loan Distribution by Institution Size"
  )

##