# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# The purpose of this script is to combine panel data from BoardEX with my
# existing panel data.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Preamble ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Edit working directory accordingly before importing data ********
setwd("~/Dropbox/Will:Finn Shared/Data")

# Load required packages: tidyverse and fixest ********
library(tidyverse)
library(fixest)

# Load datasets 
      # *Feb 2025 update: reading in the rrpanel_comp_fs_fortune_cdp.rds data 
      # instead of pre-matched data_essay2_robustness_v2.rds
panel_wd <- readRDS("rrpanel_comp_fs_fortune_cdp.rds") 
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
saveRDS(panel_fp_cleaned, "panel_fp_cleaned.rds")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data merging and inspection ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

panel_joined <- left_join(panel_wd, panel_fp_cleaned)

  # looking at obs w successful matches (CSO is appropriate to filter on since
  # all obs in the panel_fp_cleaned data have non-NA CSO values)
panel_joined_complete_cases <- panel_joined %>% filter(!(is.na(CSO)))
  # 29455/76836 = 38% of obs matched

  # Looking deeper into aspects of the merging (attempting to understand the
  # matching rate above)
panel_fp_cleaned$gvkey %>% n_distinct()
      # 7515
panel_joined_complete_cases$gvkey %>% n_distinct()
      # 2919
      # 2919/7515 = 39% of firms from Finn's data matched

  ## Looking at characteristics of merged sample ####
    ### Unique Firms ####
      # gvkey
        panel_fp$gvkey %>% n_distinct()
        # 7515
        panel_wd %>% filter(headquarter_country%in%c("United States of America",
                                                     "Canada")) %>% 
          select(reprisk_id) %>% n_distinct()
        # 3854
        
        # 2919/3854 = 76% of firm from Will's data matched

    ### Country characteristics ####
        panel_joined_complete_cases$headquarter_country %>% unique()
        
        # [1]  "Canada"                                               "United States of America"                            
        # [3]  "Switzerland"                                          "United Kingdom of Great Britain and Northern Ireland"
        # [5]  "Australia"                                            "Ireland"                                             
        # [7]  "China"                                                "Netherlands"                                         
        # [9]  "Greece"                                               "Cayman Islands"                                      
        # [11] "Bermuda Islands (UK)"                                 "Virgin Islands; British"                             
        # [13] "Panama"                                               "Israel"                                              
        # [15] "Germany"                                              "United States Minor Outlying Islands"
        
        # It would be helpful to know why non-North American headquartered firms 
        # are included.
        
        # Follow up: open a Git hub issue -- provide at least one or two 
        # examples of non-North American firms.
        
        panel_joined_complete_cases %>% 
          filter(!headquarter_country%in%c("United States of America",
                                           "Canada")) %>%
          group_by(conm, gvkey, headquarter_country) %>% summarise()
        
          # # A tibble: 48 × 3
          # # Groups:   conm, gvkey [48]
          #    conm                         gvkey  headquarter_country                                 
          #    <chr>                        <chr>  <chr>                                               
          #  1 ACCENTURE PLC                143357 Ireland                                             
          #  2 ADVANCED BATTERY TECH INC    2831   China                                               
          #  3 ALLIED HEALTHCARE INTL INC   25972  United Kingdom of Great Britain and Northern Ireland
          #  4 ALPHA AND OMEGA SEMICONDUCTO 184604 Bermuda Islands (UK)                                
          #  5 AMCOR PLC                    100243 United Kingdom of Great Britain and Northern Ireland
          #  6 AMICAS INC                   65072  Germany                                             
          #  7 AON PLC                      3221   Ireland                                             
          #  8 APCO OIL AND GAS INTL INC    1682   Cayman Islands                                      
          #  9 ARGO GROUP INTL HOLDINGS LTD 15364  Bermuda Islands (UK)                                
          # 10 ASML HOLDING NV              61214  Netherlands
        
    ### Industrial characteristics ####
        panel_joined_complete_cases$FourDigitName %>% unique()
        
        #  [1] "Materials"                                      "Retailing"                                      "Semiconductors & Semiconductor Equipment"      
        #  [4] "Software & Services"                            "Energy"                                         "Commercial & Professional Services"            
        #  [7] "Technology Hardware & Equipment"                "Consumer Durables & Apparel"                    "Diversified Financials"                        
        # [10] "Capital Goods"                                  "Utilities"                                      "Household & Personal Products"                 
        # [13] "Transportation"                                 "Consumer Services"                              "Health Care Equipment & Services"              
        # [16] "Pharmaceuticals, Biotechnology & Life Sciences" "Banks"                                          "Food, Beverage & Tobacco"                      
        # [19] "Media & Entertainment"                          "Real Estate"                                    "Insurance"                                     
        # [22] "Automobiles & Components"                       "Telecommunication Services"                     "Food & Staples Retailing"      

        # Diverse industrial characteristics... eventually, it might be worth
        # looking more narrowly at mfg firms.
        
# Exporting data ####
saveRDS(panel_joined_complete_cases, "panel_intersection.rds")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Baseline TWFE

model <- feols(total_incident_count ~ cdp_sc_member + log(at_gbp) + 
               + roa + 
               + CSO | reprisk_id + year,
               data = panel_joined_complete_cases,
               cluster = "reprisk_id")
summary(model)
        