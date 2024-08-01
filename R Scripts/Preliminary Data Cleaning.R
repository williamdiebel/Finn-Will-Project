# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# The purpose of this script is to combine panel data from BoardEX with my
# existing panel data.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Preamble ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Edit working directory accordingly before importing data ********
setwd(file.path("/Users/williamdiebel/Library/CloudStorage", 
                "Dropbox/Will:Finn Shared/Data"))

# Load required packages: tidyverse and fixest ********
library(tidyverse)
library(fixest)

# Load datasets
panel_wd <- readRDS("data_essay2_robustness_v2.rds") 
panel_fp <- read_csv("IndependentVariables_07232024.csv",
                     col_types = "ciciiiiii")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data cleaning ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # aligning vbl names for merging
colnames(panel_fp)[1] <- "cusip_fp" # avoids merging based on cusip for now
colnames(panel_fp)[2] <- "year"

  # quick attempt to merge w/o any cleaning
panel_joined <- left_join(panel_wd, panel_fp)
  # merging generates following warning:

    # Joining with `by = join_by(gvkey, year)`
    # Warning message:
    #   In left_join(panel_wd, panel_fp) :
    #   Detected an unexpected many-to-many relationship between `x` and `y`.
    # ℹ Row 2219 of `x` matches multiple rows in `y`.
    # ℹ Row 79266 of `y` matches multiple rows in `x`.
    # ℹ If a many-to-many relationship is expected, set `relationship = "many-
    #   to-many"` to silence this warning.

  # checking for duplicates across all variables
panel_fp %>% duplicated() %>% which() %>% length()
  
    # [1] 13680

  # removing duplicates
panel_fp <- panel_fp[-which(duplicated(panel_fp)), ]

  # looking for additional duplicates at the gvkey-year level
panel_fp %>% group_by(gvkey, year) %>% count() %>% group_by(n) %>% count()

    # Storing counts in `nn`, as `n` already present in input
    # ℹ Use `name = "new_name"` to pick a new name.
    # # A tibble: 2 × 2
    # # Groups:   n [2]
    #       n    nn
    #   <int> <int>
    #       1     1 76731
    #       2     2   105

  # the above indicates that there are 105 duplicates at the firm/year level
  # remaining

  # inspecting additional duplicates at the f/y level
panel_fp$gvkey_year <- paste(panel_fp$gvkey, panel_fp$year) # creating f/y ids
duplicate_gvkey_year_list <- panel_fp$gvkey_year[which(
  duplicated(panel_fp$gvkey_year))] # creating list of duplicated f/y ids
panel_fp %>% filter(gvkey_year%in%duplicate_gvkey_year_list) %>% view()
  # looks like there are some duplicate f/y combinations with
  # different CSO & in fewer cases, ESG Committee values.
  # *** probably makes most sense to choose CSO/ESG Committee values == 1 in 
  # case of such discrepancies, in line with observed CSO Year and ESG 
  # Committee Year values. ***   
  
  # separating obs associated with duplicates for cleaning before remerging
panel_fp_dupes <- panel_fp %>% filter(gvkey_year%in%duplicate_gvkey_year_list)
panel_fp_non_dupes <- panel_fp %>% 
  filter(!(gvkey_year%in%duplicate_gvkey_year_list))

  # for each duplicated f/y combo, keep the one with CSO or ESG == 1
panel_fp_dupes_cleaned <- panel_fp_dupes %>%
  group_by(gvkey_year) %>%
  arrange(desc(CSO), desc(ESG_Committee)) %>% 
  slice(1) %>%
  ungroup()

panel_fp_cleaned <- rbind(panel_fp_dupes_cleaned, panel_fp_non_dupes)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data merging and inspection ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

panel_joined <- left_join(panel_wd, panel_fp_cleaned)

  # looking at obs w successful matches
panel_joined_complete_cases <- panel_joined %>% filter(!(is.na(CSO)))
  # 2618/12616 = 21% of obs matched

  # breaking down by unique firms
panel_joined_complete_cases %>%
  summarise(unique_gvkey_count = n_distinct(gvkey)) %>%
  pull(unique_gvkey_count)
  # 257 unique firms from my dataset w/ matched CSO data
  
  # counting treated firms w CSO data
panel_joined_complete_cases %>%
  filter(treated == 1) %>%
  summarise(unique_gvkey_count = n_distinct(gvkey)) %>%
  pull(unique_gvkey_count)
  # 49 are treated obs

  # counting control firms w CSO data
panel_joined_complete_cases %>%
  filter(treated == 0) %>%
  summarise(unique_gvkey_count = n_distinct(gvkey)) %>%
  pull(unique_gvkey_count)
  # 208 are control obs

  # since my panel has already undergone matching, its maybe most relevant to
  # look within cem matching strata to understand how many strata contain both
  # at least 1 treated and 1 control obs.

  # identify strata with at least one treated and one control observation
strata_with_treated_and_control <- panel_joined_complete_cases %>%
  group_by(strata) %>%
  filter(any(treated == 1) & any(treated == 0)) %>%
  ungroup()

  # count the number of unique strata
strata_with_treated_and_control %>%
  summarise(unique_strata_count = n_distinct(strata)) %>%
  pull(unique_strata_count)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DD models ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# main model
model <- feols(total_incident_count ~ cdp_sc_member + log(at_usd) + 
                 prop_suppliers_cdp_sc + roa_winsor + 
                 supplier_count + CSO | firm_strata + year,
               data = strata_with_treated_and_control,
               cluster = "firm_strata",
               weights = ~w)
summary(model)

# spotlight mechanism test
model <- feols(total_incident_count ~ cdp_sc_member*log(at_usd) + 
                 prop_suppliers_cdp_sc + roa_winsor + 
                 supplier_count + CSO | firm_strata + year,
               data = strata_with_treated_and_control,
               cluster = "firm_strata",
               weights = ~w)
summary(model)

# CSO interaction test
model <- feols(total_incident_count ~ cdp_sc_member*CSO + log(at_usd) +
                 prop_suppliers_cdp_sc + roa_winsor + 
                 supplier_count | firm_strata + year,
               data = strata_with_treated_and_control,
               cluster = "firm_strata",
               weights = ~w)
summary(model)

# CSO three-way interaction test
model <- feols(total_incident_count ~ cdp_sc_member*log(at_usd)*CSO + 
                 prop_suppliers_cdp_sc + roa_winsor + 
                 supplier_count | firm_strata + year,
               data = strata_with_treated_and_control,
               cluster = "firm_strata",
               weights = ~w)
summary(model)

# ESG Committee interaction test
model <- feols(total_incident_count ~ cdp_sc_member*ESG_Committee + log(at_usd) +
                 prop_suppliers_cdp_sc + roa_winsor + 
                 supplier_count | firm_strata + year,
               data = strata_with_treated_and_control,
               cluster = "firm_strata",
               weights = ~w)
summary(model)

# ESG Committee three-way interaction test
model <- feols(total_incident_count ~ cdp_sc_member*log(at_usd)*ESG_Committee + 
                 prop_suppliers_cdp_sc + roa_winsor + 
                 supplier_count | firm_strata + year,
               data = strata_with_treated_and_control,
               cluster = "firm_strata",
               weights = ~w)
summary(model)
