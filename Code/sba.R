# Load Libraries
library(tidyverse)
library(dplyr)
library(readxl)

# Load Data
sba_7a_2010 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/foia-7a-fy2010-fy2019-asof-250331.csv")
sba_7a_2020 <- read.csv("C:/Users/Colin Colter/FinRegLab Dropbox/Luke Dillingham/SBA Project/Data/foia-7a-fy2020-present-asof-250331.csv")

# Combining datasets for SBA 7a data from 2010-2025
sba_7a <- rbind(sba_7a_2010, sba_7a_2020)


