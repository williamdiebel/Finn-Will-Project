# --------------------------------------------------- #

# Governance and RepRisk Incidents

# --------------------------------------------------- #

# Code: Finn Petersen (pet03435@umn.edu)
# February 2025, written using R version 4.3.3 (64-bit).


# ------------------------------ # ----
# Prepare environment #
# ------------------------------ # ----
library(tidyverse)
library(lubridate)
library(data.table)
library(haven)
library(fixest)
library(lfe)


## change to working directory of project. Directory should have a folder
## names `02_Data` and within that two folders named `secdata` and `rawdata`
setwd("C:/Users/fipeters/OneDrive - Indiana University/07_Research/05_GovernanceAndIncidents/Data")


# ------------------------------ # ----
# Define main program            #
# ------------------------------ # ----

# Load data from Will
panel_wd <- as.data.table(readRDS("./secdata/data_essay2_robustness_v2.rds"))
panel_wd <- as.data.table(readRDS("./secdata/rrpanel_comp_fs_fortune_cdp.rds"))

panel_wd <- panel_wd %>%
  .[headquarter_country_code %in% c("US", "CA")]

## Link table between Compustat and BoardEx
link <- as.data.table(read_dta("./rawdata/boardex_crsp_compustat_linking_table_16032021.dta"))
link <- unique(link, by = "companyid")
link <- link[, gvkey := as.numeric(gvkey)]

# Loosing ~8k observations when evaluating for gvkey in the link table.
# This accounts for the majority of unmatched observations in Will's data (discussion from 02/17/2025)
panel_wd_BoarExCompustatLink <- panel_wd %>%
  .[gvkey %in% unique(link$gvkey)]



# Load data from Compustat
## Link table between Compustat and BoardEx
link <- as.data.table(read_dta("./rawdata/boardex_crsp_compustat_linking_table_16032021.dta"))
link <- unique(link, by = "companyid")
link <- link[, gvkey := as.numeric(gvkey)]


dt_master <- as.data.table(read.csv("./rawdata/Compustat_2008-2024.csv")) %>%
  .[!is.na(fyear)] %>%
  .[order(gvkey, fyear)] %>%
  .[gvkey %in% unique(link$gvkey)] %>%
  .[, Year := fyear]


# Merge with CSO and ESG Committee information from BoardEx
# dt_BoardEx <- load_BoardEx(link_tbl = link)  # BoardEx files were too big for the Dropbox, so I saved the output of this function instead. The code is still at the end of this document.
# dt_BoardEx <- as.data.table(readRDS("./secdata/BoardEx.rds"))
dt_master <- merge(dt_master[, .(gvkey, cusip, Year)], dt_BoardEx[, .(gvkey, Year, CSO, ESG_Committee, OtherCSO, OtherESG_Committee, CSO_Peer)],
  all.x = TRUE,
  by.x = c("gvkey", "Year"), by.y = c("gvkey", "Year")
)

dt_master <- dt_master %>%
  .[, ESG_Committee := ifelse(is.na(ESG_Committee), 0, ESG_Committee)] %>%
  .[, CSO := ifelse(is.na(CSO), 0, CSO)]

# Create a new column with the maximum CSO value from prior years
dt_master[, CSO_max_prior := shift(cummax(CSO), fill = 0, type = "lag"), by = gvkey]
dt_master[, ESG_Committee_max_prior := shift(cummax(ESG_Committee), fill = 0, type = "lag"), by = gvkey]

# Replace the current CSO value with the max prior CSO value
dt_master[, CSO := CSO_max_prior]
dt_master[, ESG_Committee := ESG_Committee_max_prior]

# Remove the temporary column
dt_master[, CSO_max_prior := NULL]
dt_master[, ESG_Committee_max_prior := NULL]

# Create Treatment variables
dt_master <- dt_master %>%
  .[, CSO_Year := ifelse(all(CSO == 0), 0, min(Year[CSO == 1])), by = gvkey] %>%
  .[, CSO_Year := ifelse(CSO_Year == 0, NA, CSO_Year)] %>%
  .[, ESG_Committee_Year := ifelse(all(ESG_Committee == 0), 0, min(Year[ESG_Committee == 1])), by = gvkey] %>%
  .[, ESG_Committee_Year := ifelse(ESG_Committee_Year == 0, NA, ESG_Committee_Year)]


# Merge with Asset 4 information
dt_Refinitiv <- load_Refinitive()

dt_Refinitiv <- dt_Refinitiv %>%
  .[, ESG_Incentives := ifelse(is.na(PolicyExecutiveCompensationESGPerformance), 0, PolicyExecutiveCompensationESGPerformance)] %>%
  .[, SupplierESGTraining := ifelse(is.na(SupplierESGTraining), 0, SupplierESGTraining)]

dt_master <- merge(dt_master, dt_Refinitiv[, .(
  cusip, year, ESG_Incentives,
  SupplierESGTraining, ESGScore_Value, ESGCombinedScore_Value, ESGCControversiesScore_Value
)],
all.x = TRUE,
by.x = c("cusip", "Year"), by.y = c("cusip", "year")
)


write.csv(dt_master,
  row.names = FALSE,
  file = "./secdata/IndependentVariables_06252025.csv"
)


# Merge panel_wd with dt_master
dt_master[, gvkey := as.character(gvkey)]
dt_final <- merge(dt_master[, .(gvkey, Year)],
  panel_wd[, .(gvkey, year)],
  all.x = FALSE, # do not retain non matching observations
  by.x = c("gvkey", "Year"), by.y = c("gvkey", "year")
)
# Interestingly, I get 32,941 and not 29k matched observations



# ------------------------------ # ----
# Define main functions          #
# ------------------------------ # ----

load_Refinitive <- function() {
  dt_screening <- fread("./rawdata/Asset4.csv")


  dt_screening <- dt_screening %>%
    .[fieldname %in% c(
      "ESGCombinedScore",
      "ESGScore",
      "PolicyExecutiveCompensationESGPerformance",
      "SupplierESGTraining",
      "EnvironmentalSupplyChainManagement",
      "EnvironmentalSupplyChainMonitoring",
      "ESGCControversiesScore"
    )] %>%
    .[cusip != ""] %>%
    .[, value_nr := ifelse(value == "true", 1, 0)]

  dt_screening[, cusip := as.character(cusip)]
  dt_screening[, orgpermid := as.character(orgpermid)]

  dt_screening <- unique(dt_screening, by = c("cusip", "year", "fieldname"))

  dt <- dcast(dt_screening, cusip + year ~ fieldname, value.var = c("value_nr"))

  dt[, ESGScore := factor(ESGScore, levels = c("D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"))]
  dt[, ESGScore_Value := as.integer(ESGScore)]

  dt[, ESGCombinedScore := factor(ESGCombinedScore, levels = c("D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"))]
  dt[, ESGCombinedScore_Value := as.integer(ESGCombinedScore)]

  dt[, ESGCControversiesScore := factor(ESGCControversiesScore, levels = c("D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"))]
  dt[, ESGCControversiesScore_Value := as.integer(ESGCControversiesScore)]


  return(dt)
}

load_board_Refinitive <- function() {
  dt_board <- fread("C:/Users/pet03435/Downloads/Refinitive/Screening.csv")


  setnames(dt_board, "CG_BD_CP_DP025", "Exec_Gender")
  setnames(dt_board, "CG_BD_BS_DP026", "BM_Gender")
  setnames(dt_board, "CG_BD_CP_DP043", "Board_Total_Compensation")
  setnames(dt_board, "CG_BD_CP_DP031", "Executive_Total_Compensation")

  setnames(dt_board, "CG_BD_CP_DP029", "CSR_Responsibility")
  setnames(dt_board, "CG_BD_CP_DP030", "Role")

  setnames(dt_board, "CG_BD_CP_DP039", "CSR_Incentive")

  dt_board <- dt_board %>%
    .[, Man := case_when(
      IsBMember == 1 & BM_Gender == "Man" ~ 1,
      IsBMember == 1 & BM_Gender == "man" ~ 1,
      IsBMember == 1 & BM_Gender == "Woman" ~ 0,
      IsBMember == 0 & Exec_Gender == "Man" ~ 1,
      IsBMember == 0 & Exec_Gender == "Woman" ~ 0
    )]

  dt_board <- dt_board %>%
    .[, Compensation := case_when(
      IsBMember == 1 ~ Board_Total_Compensation,
      IsBMember == 0 ~ Executive_Total_Compensation
    )]


  dt_board <- dt_board %>%
    .[, CSO := case_when(
      Role %like% "ustaina" ~ 1,
      Role %like% "nviron" ~ 1,
      Role %like% "ocial" ~ 1,
      Role %like% "esponsibility" ~ 1,
      Role %like% "CSR" ~ 1,
      TRUE ~ 0
    )] %>%
    .[, CSO_F := case_when(
      CSO == 1 & Man == 0 ~ 1,
      TRUE ~ 0
    )] %>%
    .[, CSO_M := case_when(
      CSO == 1 & Man == 1 ~ 1,
      TRUE ~ 0
    )] %>%
    .[, COO_Role := case_when(
      Role %like% "upply" ~ 1,
      Role %like% "peration" ~ 1,
      Role %like% "roduction" ~ 1,
      TRUE ~ 0
    )]


  dt_board <- dt_board[dt_board[, min(FisYear), by = c("OrgID", "MemberID")], on = c("OrgID", "MemberID"), first.year := V1][] %>%
    .[, New := ifelse(FisYear == first.year, 1, 0)]

  dt_board[, cusip := substr(Isin, 3, 11)]

  dt_board <- dt_board[
    , .(
      CSO = max(CSO),
      CSO_M = max(CSO_M),
      CSO_F = max(CSO_F),
      CSR_Incentive = mean(CSR_Incentive, na.rm = TRUE),
      FBR = 1 - mean(Man, na.rm = TRUE)
    ),
    .(cusip, FisYear)
  ]


  return(dt_board)
}
