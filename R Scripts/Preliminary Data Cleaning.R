# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Preamble ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Edit working directory accordingly before importing data ********
setwd(file.path("/Users/williamdiebel/Library/CloudStorage", 
                "Dropbox/Will:Finn Shared/Data"))

# Load necessary packages
library(tidyverse, fixest)

# Load datasets
panel_wd <- readRDS("data_essay2_robustness_v2.rds") 
panel_fp <- read_csv("IndependentVariables_07232024.csv",
                     col_types = "ciciiiiii")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data cleaning and merging ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # aligning vbl names for merging
colnames(panel_fp)[1] <- "cusip_fp" # avoids merging based on cusip for now
colnames(panel_fp)[2] <- "year"

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

  # inspecting additional duplicates at the gvkey-year level
panel_fp$gvkey_year <- paste(panel_fp$gvkey, panel_fp$year)
duplicate_gvkey_year_list <- panel_fp$gvkey_year[which(
  duplicated(panel_fp$gvkey_year))]
panel_fp %>% filter(gvkey_year%in%duplicate_gvkey_year_list) %>% view()
  # looks like there are some duplicate gvkey-year combinations with
  # different CSO values -- looks to be associated with CSO_Year variable.
  # *** probably makes most sense to choose CSO value == 1 in case of such
  # discrepancies. ***   
  # going to refer to other non-duplicate data to see what's normal before
  # making a final decision.

