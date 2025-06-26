# Edit working directory accordingly before importing data ********
# setwd("~/Dropbox/Will:Finn Shared/Data")
setwd("C:/Users/fipeters/OneDrive - Indiana University/07_Research/05_GovernanceAndIncidents/Data/secdata")

# Load required packages: tidyverse and fixest ********
library(tidyverse)
library(fixest)
library(robustHD)
library(MatchIt)
library(cem)
library(cobalt) # For balance diagnostics
library(ggplot2)
library(gridExtra) # For arranging multiple plots

# Load datasets
# *Feb 2025 update: reading in the rrpanel_comp_fs_fortune_cdp.rds data
# instead of pre-matched data_essay2_robustness_v2.rds
panel_wd <- readRDS("rrpanel_comp_fs_fortune_cdp.rds")
panel_fp <- read.csv("IndependentVariables_06252025.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data cleaning ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# aligning vbl names for merging
colnames(panel_fp)[1] <- "cusip_fp" # avoids merging based on cusip for now
colnames(panel_fp)[2] <- "year"

# transdorming gvkey to character
panel_fp$gvkey <- as.character(panel_fp$gvkey)

# removing duplicates
panel_fp <- panel_fp[-which(duplicated(panel_fp)), ]

# looking for additional duplicates at the gvkey-year level
panel_fp %>%
    group_by(gvkey, year) %>%
    count() %>%
    group_by(n) %>%
    count()

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
    duplicated(panel_fp$gvkey_year)
)] # creating list of duplicated f/y ids
panel_fp %>%
    filter(gvkey_year %in% duplicate_gvkey_year_list) %>%
    view()
# looks like there are some duplicate f/y combinations with
# different CSO & in fewer cases, ESG Committee values.
# *** probably makes most sense to choose CSO/ESG Committee values == 1 in
# case of such discrepancies, in line with observed CSO Year and ESG
# Committee Year values. ***

# separating obs associated with duplicates for cleaning before remerging
panel_fp_dupes <- panel_fp %>% filter(gvkey_year %in% duplicate_gvkey_year_list)
panel_fp_non_dupes <- panel_fp %>%
    filter(!(gvkey_year %in% duplicate_gvkey_year_list))

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

# looking at obs w successful matches (CSO is appropriate to filter on since
# all obs in the panel_fp_cleaned data have non-NA CSO values)
panel_joined_complete_cases <- panel_joined %>%
    filter(!is.na(CSO))
# 29455/76836 = 38% of obs matched


panel_intersection <- panel_joined_complete_cases

# Retaining firms in US and Canada
panel_intersection <- panel_intersection %>% filter(headquarter_country %in% c("United States of America", "Canada"))

# Winsorizing total assets and roa
panel_intersection$roa_winsorized_1 <- winsorize(panel_intersection$roa, probs = 0.01)
panel_intersection$at_gbp_winsorized_1 <- winsorize(panel_intersection$at_gbp, probs = 0.01)

# Correcting for missing supply base factors: 1. Remove obs with no factset ID; 2. Code 0 for obs with missing supplier count data
# 1. Remove obs with no factset ID
panel_intersection <- panel_intersection[-which(is.na(panel_intersection$factset_id)), ]

# 2. code 0 for obs with missing supplier count data

panel_intersection$supplier_count[which(is.na(panel_intersection$supplier_count))] <- 0
panel_intersection$cdp_supplier_inv_count[which(is.na(panel_intersection$cdp_supplier_inv_count))] <- 0
panel_intersection$cdp_supplier_sc_count[which(is.na(panel_intersection$cdp_supplier_sc_count))] <- 0
panel_intersection$cdp_supplier_member_count[which(is.na(panel_intersection$cdp_supplier_member_count))] <- 0
panel_intersection$prop_suppliers_cdp_sc[which(is.na(panel_intersection$prop_suppliers_cdp_sc))] <- 0
panel_intersection$prop_suppliers_cdp_inv[which(is.na(panel_intersection$prop_suppliers_cdp_inv))] <- 0
panel_intersection$prop_suppliers_cdp[which(is.na(panel_intersection$prop_suppliers_cdp))] <- 0


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Analysis ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

panel_intersection <- panel_intersection %>%
    filter(!is.na(CSO_Peer)) # removing NA OtherCSO values for IV model


# TWFE model
model <- feols(
    total_incident_count ~ CSO |
        reprisk_id + year, # Firm and time fixed effects
    data = panel_intersection,
    cluster = "reprisk_id"
)
summary(model)

model <- feols(
    total_incident_count ~ CSO + supplier_count + cdp_sc_member + log(at_gbp) + prop_suppliers_cdp_sc + roa |
        reprisk_id + year, # Firm and time fixed effects
    data = panel_intersection,
    cluster = "reprisk_id"
)
summary(model)

# IV model
# First stage: predicting CSO using CSO_Peer and other controls
model <- feols(
    total_incident_count ~ supplier_count + cdp_sc_member + log(at_gbp) + prop_suppliers_cdp_sc + roa |
        reprisk_id + year | # Firm and time fixed effects
        CSO ~ CSO_Peer, # Instrumental variable
    data = panel_intersection,
    cluster = "reprisk_id"
)
summary(model)


## IV model with 0-stage probit model
model_probit <- feglm(
    CSO ~ CSO_Peer + supplier_count + cdp_sc_member + log(at_gbp) + prop_suppliers_cdp_sc + roa |
        year,
    data = panel_intersection,
    fixef.rm = "none",
    family = binomial(link = "probit")
)
summary(model_probit)

panel_intersection$CSOfitted <- fitted(model_probit)
# First stage: predicting CSO using fitted values from 0-stage probit
model <- feols(
    total_incident_count ~ supplier_count + cdp_sc_member + log(at_gbp) + prop_suppliers_cdp_sc + roa |
        reprisk_id + year | # Firm and time fixed effects
        CSO ~ CSOfitted, # Instrumental variable
    data = panel_intersection,
    cluster = "reprisk_id"
)
summary(model)
