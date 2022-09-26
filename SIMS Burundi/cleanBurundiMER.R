library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
#library(tidylog)
library(here)
setwd(here())

##Load & Clean MER data (from Panorama - Site_IM extract by country) 
mer_raw <- read_tsv('MER_Structured_Datasets_Site_IM_FY20-23_20220617_v2_1_Burundi.txt',
                    col_types = cols(targets = col_double(),
                                     otherdisaggregate = col_character(),
                                     qtr1 = col_double(),
                                     qtr3 = col_double(),
                                     hiv_treatment_status = col_character(),
                                     statustb = col_character(),
                                     otherdisaggregate_sub = col_character(),
                                     modality = col_character()))


# note: 'Genie_MERFACT_Burundi_Daily_8816905c-660c-4527-9e36-5512fe2978cb.txt' doesn't have the orgunituid


# Modified to keep total denominator or total numerator for PVLS data
#not working

mer_clean <- mer_raw%>%
  clean_names()%>%
  select(!c(country, snuprioritization, dreams, 
            award_number, mech_code, typemilitary,
            categoryoptioncomboname, source_name, statuscx, statushiv, statustb, hiv_treatment_status)) %>%
  mutate(sitename = str_to_title(sitename),
         facility = str_to_title(facility),
         primepartner = str_to_title(prime_partner_name),
         mech_name = str_to_title(mech_name)) %>%
  mutate(site_type_cleaned = ifelse(sitetype == 'Facility', 'Facility',
                                    ifelse(sitetype == 'Community', 'Community', 'Other'))) %>%
  filter(indicator == 'TX_CURR' | indicator == 'TX_NEW' | indicator == 'HTS_TST' | indicator == 'TX_PVLS') %>%
  filter(standardizeddisaggregate == 'Total Numerator' | standardizeddisaggregate == 'Total Denominator' ) %>%
  pivot_longer(cols = c('qtr1':'qtr4'), names_to = 'quarter', values_to = 'quarter_results') %>%
  remove_empty(which = 'cols', quiet = FALSE) %>%
  arrange(orgunituid)




