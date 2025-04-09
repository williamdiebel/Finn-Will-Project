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
library(robustHD)
library(MatchIt)
library(cem)
library(cobalt)  # For balance diagnostics
library(ggplot2)
library(gridExtra)  # For arranging multiple plots

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
panel_fp_cleaned <- readRDS("panel_fp_cleaned.rds")
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
        
# Resuming here ####
panel_intersection <- readRDS("panel_intersection.rds")
  
  # Retaining firms in US and Canada
  panel_intersection <- panel_intersection %>% filter(headquarter_country%in%c("United States of America", "Canada"))

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
  
  # Saving results
    saveRDS(panel_intersection, "panel_intersection_v2.rds")
    panel_intersection <- readRDS("panel_intersection_v2.rds")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
        # Summary stats
            # Want to look at composition of CSO firms and non-CSO firms
            
            # First, extracting a list of all CSO firms in the sample
            CSO_firms <- panel_intersection %>% filter(CSO==1)
            CSO_firms <- unique(CSO_firms$conm) # creates list of all firms that appoint a CSO
            
              # and all non-CSO firms
              non_CSO_firms <- panel_intersection %>% filter(!(conm%in%CSO_firms))
              non_CSO_firms <- unique(non_CSO_firms$conm)
        
            # Subsample panel characteristics
              # CSO firms
              CSO_firms_panel <- panel_intersection %>% filter(conm%in%CSO_firms)
              # Non-CSO firms
              non_CSO_firms_panel <- panel_intersection %>% filter(conm%in%non_CSO_firms)
          
                # Comparing total assets
                ggplot(CSO_firms_panel, aes(at_gbp)) + 
                  geom_histogram() + geom_boxplot()
                ggplot(non_CSO_firms_panel, aes(at_gbp)) + 
                  geom_histogram() + geom_boxplot()
                
                t.test(CSO_firms_panel$at_gbp, non_CSO_firms_panel$at_gbp)
                t.test(CSO_firms_panel$at_gbp_winsorized_1, non_CSO_firms_panel$at_gbp_winsorized_1)
                  # CSO firms are smaller in total assets
                # Comparing roa
                ggplot(CSO_firms_panel, aes(roa)) + 
                  geom_histogram(bins = 100) + geom_boxplot()
                ggplot(non_CSO_firms_panel, aes(roa)) + 
                  geom_histogram(bins = 100) + geom_boxplot()
                
                t.test(CSO_firms_panel$roa, non_CSO_firms_panel$roa)
                t.test(CSO_firms_panel$roa_winsorized_1, non_CSO_firms_panel$roa_winsorized_1)
                
            # Subsample cross-sectional characteristics
              # CSO firms
              CSO_firms_cross_sectional <- data.frame(conm = CSO_firms, 
                                                      stringsAsFactors = FALSE)
              CSO_firms_cross_sectional <- left_join(CSO_firms_cross_sectional, 
                                                     panel_intersection %>% 
                                                       select(conm, 
                                                              FourDigitName, 
                                                              headquarter_country), 
                                                     multiple = "first")
              # Non-CSO firms
              non_CSO_firms_cross_sectional <- data.frame(conm = non_CSO_firms,
                                                          stringsAsFactors = FALSE)
              non_CSO_firms_cross_sectional <- left_join(non_CSO_firms_cross_sectional,
                                                         panel_intersection %>%
                                                           select(conm,
                                                                  FourDigitName,
                                                                  headquarter_country),
                                                         multiple = "first")
            
              # Headquarter country tables
              CSO_firms_cross_sectional %>% group_by(headquarter_country) %>% count() %>% arrange(desc(n))
              non_CSO_firms_cross_sectional %>% group_by(headquarter_country) %>% count() %>% arrange(desc(n))
              # GICS Industry Group tables
              CSO_firms_cross_sectional %>% group_by(FourDigitName) %>% count() %>% arrange(desc(n))
              non_CSO_firms_cross_sectional %>% group_by(FourDigitName) %>% count() %>% arrange(desc(n))
              
        # TWFE models
        model <- feols(total_incident_count ~ cdp_sc_member + log(at_gbp) + supplier_count + prop_suppliers_cdp_sc +
               + roa 
               + CSO
               | reprisk_id + year, # Firm and time fixed effects
               data = panel_intersection,
               cluster = "reprisk_id")
        summary(model)
        
        model <- feols(total_incident_count ~ cdp_sc_member + log(at_gbp) + supplier_count + prop_suppliers_cdp_sc +
               + roa 
               + CSO
               | FourDigitName + year, # Industry and time fixed effects
               data = panel_intersection,
               cluster = "reprisk_id")
        summary(model)
        
    # Winsorized models
        
        # TWFE models
        model <- feols(total_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
               + roa_winsorized_1 
               + CSO
               | reprisk_id + year, # Firm and time fixed effects
               data = panel_intersection,
               cluster = "reprisk_id")
        summary(model)
        
        model <- feols(total_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
               + roa_winsorized_1 
               + CSO
               | FourDigitName + year, # Industry and time fixed effects
               data = panel_intersection,
               cluster = "reprisk_id")
        summary(model)
        
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Looking at the missing firms from the Compustat, CRSP, BoardEx merging table
          
          # Importing the link table
        link <- data.table::data.table(haven::read_dta('/Users/william.diebel/Dropbox/Will:Finn Shared/02_Data/rawdata/boardex_crsp_compustat_linking_table_16032021.dta'))
        link$gvkey <- as.character(link$gvkey)
          # Unique firms in the linking table (double checking to make sure each gvkey is distinct)
        link %>% n_distinct(.[, gvkey]) # [1] 10027: checks out
          # Next: 1. counting how many gvkeys from my initial "raw" panel show up in the linking table
          #       2. counting how many gvkeys from finn's data show up in the linking table (should be all?)
          #       3. counting how many gvkeys from panel_intersection show up in the linking table
                    
            # 1. counting how many gvkeys from my initial "raw" panel show up in the linking table
            gvkeys_panel_wd <- panel_wd %>% distinct(gvkey)
            length(which(gvkeys_panel_wd$gvkey%in%link$gvkey)) # [1] 1014: much lower than anticipated
              # following finn's approach in "Independent Variable Creation.R"
                # first, retaining only the american and canadian firms
            panel_wd_us_can <- panel_wd %>% filter(headquarter_country_code %in% c("US","CA"))
            panel_wd_us_can$gvkey %>% n_distinct() # [1] 3854: number of unique US/CAN firms in my raw data
                # then, filtering based on firms in the linking table
            panel_wd_us_can <- panel_wd %>% filter(gvkey %in% link$gvkey)
            panel_wd_us_can$gvkey %>% n_distinct() # [1] 1014: number of unique firms after filtering based on what's in the linking table. i.e., same outcome as identified in line 320
            
            # 2. counting how many gvkeys from finn's data show up in the linking table (should be all?)
              # counting the initial amount of firms before filtering on the linking table
            panel_fp_cleaned$gvkey %>% n_distinct() # [1] 7515: unique gvkeys
              # filtering on linking table
            panel_fp_cleaned <- panel_fp_cleaned %>% filter(gvkey%in%link$gvkey)
            panel_fp_cleaned$gvkey %>% n_distinct() # [1] 2549: gvkeys found in the linking table
            
            # 3. counting how many gvkeys from panel_intersection show up in the linking table
            gvkeys_panel_intersection <- panel_intersection %>% distinct(gvkey)
            gvkeys_panel_intersection$gvkey %>% n_distinct() # [1] 2542: unique gvkeys in the merged data
            length(which(gvkeys_panel_intersection$gvkey%in%link$gvkey)) # [1] 834: gvkeys found in the linking table
            
# CEM ####
# For each treated unit, we'll define the first period of treatment as t = 0
# Then, we'll identify matches based on data corresponding to t = - 1

            # Step 1: Identify when each unit first gets treated
            panel_intersection <- panel_intersection %>%
              group_by(reprisk_id) %>% # operation looks within each firm
              mutate(treatment_start = ifelse(any(CSO == 1), # identifies whether it is "treated", i.e., CSO == 1
                                              min(year[CSO == 1], na.rm = TRUE), NA)) %>% # and, if so, assigns the initial treatment period (min year)
              ungroup()

            panel_intersection %>% select(conm, year, CSO, treatment_start) %>% filter(conm == "POLYMET MINING CORP") # visually inspecting a CSO firm to ensure the above worked as intended
              # looks good
            
            # Step 2: Identify pre-treatment period observations (t - 1), ensuring the pre-treatment period is correctly identified at the unit level
            panel_intersection <- panel_intersection %>%
              group_by(reprisk_id) %>%  # Grouping ensures correct reference to treatment_start
              mutate(pre_treatment = ifelse(!is.na(treatment_start) & year == treatment_start - 1, 1, 0)) %>%
              ungroup()
            
            panel_intersection %>% select(conm, year, CSO, treatment_start, pre_treatment) %>% filter(conm == "POLYMET MINING CORP") # visually inspecting a CSO firm to ensure the above worked as intended
              # looks good
            
            # Step 3: Subset pre-treatment treated units & potential controls
            pre_treatment_observations <- panel_intersection %>% filter(pre_treatment == 1) # these are all the firms for which we observe CSO entries
            never_treated <- panel_intersection %>% filter(!(conm %in% pre_treatment_observations$conm)) # these are all the obs for which we do not observe CSO entries
              # since some firms have CSOs prior to the beginning of our observational periods (e.g., "VISA INC"), let's initially filter out those obs as potential controls
            never_treated <- never_treated %>% filter(CSO==0) # removing firms with CSO prior to panel
            
            # Step 4: Assigning matching covariates
            exact_matching_vars <- c("year", "headquarter_country", "FourDigitName")  # Exact match on these
            coarsened_vars <- c("at_gbp_winsorized_1", "roa_winsorized_1")  # Apply coarsened exact matching            

            # Step 5: Create cem table by merging the pre-treatment observations with the never treated observations
            cem_prepped_matching_table <- rbind(pre_treatment_observations, never_treated) 
            
                # Ensure categorical variables are properly formatted
                cem_prepped_matching_table <- cem_prepped_matching_table %>%
                  mutate(across(all_of(exact_matching_vars), as.factor)) %>%
                  mutate(across(all_of(coarsened_vars), as.numeric)) %>%
                  drop_na(all_of(c(exact_matching_vars, coarsened_vars)))  # Remove rows with missing covariates
                
            # Step 6: Apply cem using default Sturges' rule
            
            cem_match <- cem(
              treatment = "pre_treatment",
              data = cem_prepped_matching_table,
              drop = setdiff(names(cem_prepped_matching_table), c("pre_treatment", exact_matching_vars, coarsened_vars)),  # Keep only relevant vars
              grouping = exact_matching_vars,  # Ensure exact matching on these
              keep.all = TRUE  # Keep unmatched units for balance diagnostics
            )
            
            cem_matches <- cem_prepped_matching_table[cem_match$matched, ] # extracting the matched dataset
            cem_matches$w <- cem_match$w[cem_match$matched] # extracting the cem weights
            cem_matches$strata <- cem_match$strata[cem_match$matched] # extracting the cem strata

            # summary of matching results
                # number of control/treated firms
                table(cem_matches$pre_treatment)
                                                    # 0   1 
                                                    # 873 267 
                
                # number of unique matching strata in matches
                cem_matches$strata %>% n_distinct()
                                                    # [1] 234
                
            # saving cem matches following Sturges' rule
                saveRDS(cem_matches, "cem_matches_Sturges_Mar5.rds")
                
# Next steps ####

# 1. Inspect matched strata

# Examining the cutpoints                                
cutpoints <- cem_match$breaks 
print(cutpoints)              # reveals 16 cutpoints for each

# Looking at balance

print(cem_match)            # reveals that the matching was successful
                
# 2. Run analyses with cem matched sample

cem_matches <- readRDS("cem_matches_Sturges_Mar5.rds")                

# Extract all observations for matched firms including stata and matching weights
panel_matched <- left_join(
  panel_intersection,
  cem_matches %>% select(reprisk_id, w, strata),
  by = "reprisk_id"
)
panel_matched$reprisk_strata <- paste(panel_matched$reprisk_id, panel_matched$strata, sep = "_")

# Run analyses with matched sample
model <- feols(total_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
                 + CSO
                 | reprisk_strata + year, # Firm and time fixed effects
               data = panel_matched,
               cluster = "reprisk_strata",
               weights = panel_matched$w)
summary(model)

# 3. Use new DV (from RepRisk) to run analyses
# Reading in the raw RepRisk data
reprisk_incidents <- read_csv('/Users/william.diebel/Dropbox/Mac (2)/Documents/Data/RepRisk Datafeed/Incidents.csv')
reprisk_incidents$reprisk_id <- as.character(reprisk_incidents$reprisk_id)
reprisk_incidents$year <- year(reprisk_incidents$incident_date)
  # filtering for environmental incidents
reprisk_environmental_incidents <- reprisk_incidents %>% filter(environment == TRUE)
  # aggregating incidents by firm and year
reprisk_environmental_incidents <- reprisk_environmental_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(environmental_incident_count = n()) %>%
  ungroup()
  # merging with panel_matched
panel_matched <- left_join(panel_matched, reprisk_environmental_incidents, by = c("reprisk_id", "year")) 

panel_matched <- panel_matched %>% filter(w > 0) # removing obs with zero weight          

  # having a look at the new DV compared to the environmental supply chain incidents (total_incident_count)
  # mainly just wanted to verify that the total_incident_count is represented in a similar way to the environmental_incident_count
panel_matched %>% select(conm, year, total_incident_count, environmental_incident_count) %>% arrange(desc(total_incident_count)) %>% view()
  # manual inspection reveals that the environmental SC incidents tracks with total env incidents and is less than env incidents in all but one case (2 vs 1) which generally indicates high internal validity
panel_matched$environmental_incident_count <- panel_matched$environmental_incident_count %>% replace_na(0) # replacing NAs with 0s

# Run analyses with new DV
model <- feols(environmental_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
               + CSO
               | reprisk_strata + year, # Firm and time fixed effects
               data = panel_matched,
               cluster = "reprisk_strata",
               weights = panel_matched$w)
summary(model)

model <- feols(environmental_incident_count ~ cdp_sc_member + CSO*log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
               | reprisk_strata + year, # Firm and time fixed effects
               data = panel_matched,
               cluster = "reprisk_strata",
               weights = panel_matched$w)
summary(model)

# Adding some other potential DV vbls
    # Social incidents
reprisk_social_incidents <- reprisk_incidents %>% filter(social == TRUE)
reprisk_social_incidents <- reprisk_social_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(social_incident_count = n()) %>%
  ungroup()
    # Climate incidents
reprisk_climate_incidents <- reprisk_incidents %>% filter(climate_ghg_pollution == TRUE | greenhouse_gas_emissions == TRUE)
reprisk_climate_incidents <- reprisk_climate_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(climate_incident_count = n()) %>%
  ungroup()

panel_matched <- left_join(panel_matched, reprisk_social_incidents, by = c("reprisk_id", "year")) 
panel_matched <- left_join(panel_matched, reprisk_climate_incidents, by = c("reprisk_id", "year")) 

panel_matched$social_incident_count <- panel_matched$social_incident_count %>% replace_na(0) # replacing NAs with 0s
panel_matched$climate_incident_count <- panel_matched$climate_incident_count %>% replace_na(0) # replacing NAs with 0s

# Run analyses with new DVs
model <- feols(social_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
               + CSO
               | reprisk_strata + year, # Firm and time fixed effects
               data = panel_matched,
               cluster = "reprisk_strata",
               weights = panel_matched$w)
summary(model)

model <- feols(climate_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
               + CSO
               | reprisk_strata + year, # Firm and time fixed effects
               data = panel_matched,
               cluster = "reprisk_strata",
               weights = panel_matched$w)
summary(model)

# Looking at mfg industries
panel_matched %>% filter(pre_treatment == 1) %>% group_by(FourDigitName) %>% count() %>% arrange(desc(n)) %>% view()

mfg<-c("Materials", "Automobiles & Components", "Capital Goods", 
       "Household & Personal Products", "Food & Staples Retailing",
       "Technology Hardware & Equipment", "Food, Beverage & Tobacco",
       "Pharmaceuticals, Biotechnology & Life Sciences", "Transportation",
       "Semiconductors & Semiconductor Equipment"
)
mfg_subset <- panel_matched %>% filter(FourDigitName%in%mfg)

model <- feols(environmental_incident_count ~ log(at_gbp_winsorized_1) 
                 + roa_winsorized_1 
               + CSO
               | reprisk_strata + year, # Firm and time fixed effects
               data = mfg_subset,
               cluster = "reprisk_strata",
               weights = mfg_subset$w)
summary(model)

model <- feols(social_incident_count ~ log(at_gbp_winsorized_1)
                 + roa_winsorized_1 
               + CSO
               | reprisk_strata + year, # Firm and time fixed effects
               data = mfg_subset,
               cluster = "reprisk_strata",
               weights = mfg_subset$w)
summary(model)

model <- feols(climate_incident_count ~ log(at_gbp_winsorized_1)
                 + roa_winsorized_1 
               + CSO
               | reprisk_strata + year, # Firm and time fixed effects
               data = mfg_subset,
               cluster = "reprisk_strata",
               weights = mfg_subset$w)
summary(model)

treated_mfg <- mfg_subset %>% filter(pre_treatment == 1)
treated_mfg$reprisk_strata %>% n_distinct()

mfg_subset$reprisk_strata %>% n_distinct()



panel_intersection <- left_join(panel_intersection, reprisk_environmental_incidents, by = c("reprisk_id", "year"))
panel_intersection <- left_join(panel_intersection, reprisk_social_incidents, by = c("reprisk_id", "year"))
panel_intersection <- left_join(panel_intersection, reprisk_climate_incidents, by = c("reprisk_id", "year"))

panel_intersection$social_incident_count <- panel_intersection$social_incident_count %>% replace_na(0) # replacing NAs with 0s
panel_intersection$environmental_incident_count <- panel_intersection$environmental_incident_count %>% replace_na(0) # replacing NAs with 0s
panel_intersection$climate_incident_count <- panel_intersection$climate_incident_count %>% replace_na(0) # replacing NAs with 0s

model <- feols(environmental_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
               + CSO
               | reprisk_id + year, # Firm and time fixed effects
               data = panel_intersection,,
               cluster = "reprisk_id")
summary(model)

model <- feols(social_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
               + CSO
               | reprisk_id + year, # Firm and time fixed effects
               data = panel_intersection,
               cluster = "reprisk_id")
summary(model)

model <- feols(climate_incident_count ~ cdp_sc_member + log(at_gbp_winsorized_1) + supplier_count + prop_suppliers_cdp_sc +
                 + roa_winsorized_1 
               + CSO
               | reprisk_id + year, # Firm and time fixed effects
               data = panel_intersection,
               cluster = "reprisk_id")
summary(model)

saveRDS(panel_intersection, "panel_intersection_april9.rds")

# April 9, 2025 ####
setwd("~/Dropbox/Will:Finn Shared/Data")
library(tidyverse)
panel_intersection <- readRDS("panel_intersection_april9.rds") # 
# Looking at pre- and post- average incident values in-line with
# discussion on April 9 2025.

  # Capturing all CSO firms
treated <- panel_intersection %>% filter(CSO == 1)
  # Capturing all CSO firm-year observations
treated <- panel_intersection[which(panel_intersection$reprisk_id %in% 
                                      treated$reprisk_id), ]
  # Subsetting CSO firm-year observations based on pre- and post-treatment firm-year observations
treated_pre <- treated %>% filter(CSO==0)
treated_post <- treated %>% filter(CSO==1)
  # Creating a table to report pre- and post-treatment means
pre_and_post_dvs <- data.frame(DV = c("Environmental Incident Count", 
                                 "Social Incident Count", 
                                 "Climate Incident Count"),
                               pre_mean = c(mean(treated_pre$environmental_incident_count, na.rm = TRUE),
                                       mean(treated_pre$social_incident_count, na.rm = TRUE),
                                       mean(treated_pre$climate_incident_count, na.rm = TRUE)),
                               post_mean = c(mean(treated_post$environmental_incident_count, na.rm = TRUE),
                                        mean(treated_post$social_incident_count, na.rm = TRUE),
                                        mean(treated_post$climate_incident_count, na.rm = TRUE)))
pre_and_post_dvs %>% view()
treated_pre$reprisk_id %>% n_distinct() # [1] 516
treated_post$reprisk_id %>% n_distinct() # [1] 528 
  # More reprisk_id in treated_post... should look into.



