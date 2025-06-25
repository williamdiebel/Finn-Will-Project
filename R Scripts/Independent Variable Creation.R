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
dt_master <- merge(dt_master[, .(gvkey, cusip, Year)], dt_BoardEx[, .(gvkey, Year, CSO, ESG_Committee, OtherCSO, OtherESG_Committee)],
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

load_BoardEx <- function(link_tbl = NULL) {
  # CSO Role ----
  dt_TMT <- as.data.table(read_dta("C:/Users/fipeters/OneDrive - Indiana University/07_Research/02_CSO and Environmental Injustice/02_Data/rawdata/organization_composition_officers_directors_1990_2021.dta"))



  # CSO Gender
  dt_TMT_Profiles <- as.data.table(read_dta("C:/Users/fipeters/OneDrive - Indiana University/07_Research/02_CSO and Environmental Injustice/02_Data/rawdata/individual_profile_details.dta"))

  dt_TMT <- merge(
    dt_TMT,
    dt_TMT_Profiles[, c("DirectorID", "Gender")]
  )

  dt_TMT <- dt_TMT %>%
    # .[,CSO := RoleName %in% roles[roles$Include==1,]$Role]
    .[, CSO := ifelse(grepl("sustainability", RoleName, ignore.case = TRUE), TRUE, FALSE)] %>% # sustainability
    .[, CSO := case_when(
      CSO ~ TRUE,
      grepl("sustainable", RoleName, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    )] %>%
    .[, CSO := case_when(
      CSO ~ TRUE,
      grepl("responsib", RoleName, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    )] %>%
    .[, CSO := case_when(
      CSO ~ TRUE,
      grepl("ethics", RoleName, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    )] %>%
    .[, CSO := case_when(
      CSO ~ TRUE,
      grepl("environment", RoleName, ignore.case = TRUE) ~ TRUE, # environment
      TRUE ~ FALSE
    )] %>%
    .[, CSO_M := CSO & Gender == "M"] %>%
    .[, CSO_F := CSO & Gender == "F"] %>%
    .[, CSO_SD := ifelse(CSO & Seniority == "Supervisory Director", TRUE, FALSE)] %>%
    .[, CSO_ED := ifelse(CSO & Seniority == "Executive Director", TRUE, FALSE)] %>%
    .[, CSO_Board := ifelse(CSO & Seniority != "Senior Manager", TRUE, FALSE)] %>%
    .[, CSO_SM := ifelse(CSO & Seniority == "Senior Manager", TRUE, FALSE)]

  ## Create a long data table for each director-company
  dt_TMT <- dt_TMT %>%
    .[, StartYear := year(DateStartRole)] %>%
    .[, EndYear := year(as.Date(ifelse(is.na(DateEndRole), ymd("20210315"), DateEndRole), origin = "1970-01-01"))] %>%
    .[, Duration := EndYear - StartYear + 1] %>%
    .[!is.na(Duration)] %>%
    uncount(., Duration, .id = "rep_id")

  dt_TMT <- as.data.table(dt_TMT) %>%
    .[, Year := StartYear + rep_id - 1] %>%
    .[Year >= 1999]




  # Committees ----
  dt_committee <- as.data.table(read_dta("C:/Users/fipeters/OneDrive - Indiana University/07_Research/02_CSO and Environmental Injustice/02_Data/rawdata/organization_board_director_committees.dta"))

  committees <- read.csv("C:/Users/fipeters/OneDrive - Indiana University/07_Research/02_CSO and Environmental Injustice/02_Data/secdata/organization_board_director_committees.csv") %>%
    filter(Combined == "x") %>%
    select(Committee.Name)

  dt_committee <- dt_committee %>%
    .[, Year := year(ymd(AnnualReportDate))] %>%
    .[, ESG_Committee := CommitteeName %in% committees$Committee.Name] %>%
    .[Year >= 1999]

  dt_committee <- dt_committee[, .(ESG_Committee = max(ESG_Committee)), .(BoardID, Year)]

  # Create IVs ----

  if (TRUE) {
    dt_CSO <- dt_TMT[, .(
      CSOs = max(CSO),
      CSOs_M = max(CSO_M),
      CSOs_F = max(CSO_F),
      CSOs_SD = max(CSO_SD),
      CSOs_ED = max(CSO_ED),
      CSOs_SM = max(CSO_SM),
      CSOs_Board = max(CSO_Board)
    ), .(CompanyID, Year)]
    # sum over all boards per board member
    dt_TMT <- merge(dt_TMT, dt_CSO, by = c("CompanyID", "Year"))
    dt_TMT <- merge(dt_TMT, dt_committee,
      by.x = c("CompanyID", "Year"), by.y = c("BoardID", "Year"),
      all.x = TRUE
    )
    dt_TMT$ESG_Committee <- ifelse(is.na(dt_TMT$ESG_Committee), 0, dt_TMT$ESG_Committee)

    # dt_TMT_IV <- dt_TMT
    # Number of other firms that the director is a part of that have a CSO
    # dt_TMT <- dt_TMT %>%
    #  .[Seniority %in% c("Executive Director","Supervisory Director")]

    dt_TMT <- unique(dt_TMT, by = c("CompanyID", "Year", "CompanyID"))

    dt_TMT <- dt_TMT[dt_TMT[, sum(CSOs), .(DirectorID, Year)], on = c("DirectorID", "Year"), CSOs2 := V1][]
    dt_TMT$OtherCSO <- ifelse(dt_TMT$CSOs2 - dt_TMT$CSOs > 0, 1, 0)

    dt_TMT <- dt_TMT[dt_TMT[, sum(ESG_Committee), .(DirectorID, Year)], on = c("DirectorID", "Year"), ESG_Committees := V1][]
    dt_TMT$OtherESG_Committees <- ifelse(dt_TMT$ESG_Committees - dt_TMT$ESG_Committee > 0, 1, 0)

    dt_TMT <- dt_TMT %>%
      .[, All := pmax(CSOs, ESG_Committee)] %>%
      .[, OtherAll := pmax(OtherCSO, OtherESG_Committees)]

    # Merge with link table ----
    dt_TMT <- merge(dt_TMT, link_tbl[, .(companyid, gvkey, PERMCO)],
      by.x = c("CompanyID"),
      by.y = c("companyid"), all.x = FALSE, all.y = FALSE
    )

    # Create IVs as "fraction of directors on the board who sit on other boards of other companies (excluding focal company) with XYZ"
  }


  # Create final table ----

  dt_BoardEx <- dt_TMT[
    , .(
      CSO = max(CSOs),
      # CSO_M=max(CSOs_M),
      # CSO_F=max(CSOs_F),
      # CSO_SD=max(CSOs_SD),
      # CSO_ED=max(CSOs_ED),
      # CSO_SM=max(CSOs_SM),
      # CSO_Board=max(CSOs_Board),
      OtherCSO = mean(OtherCSO),
      ESG_Committee = max(ESG_Committee),
      OtherESG_Committee = mean(OtherESG_Committees),
      # All=max(All),
      # OtherAll=mean(OtherAll),
      # naics_2 = first(naics_2),
      Count_Executives = sum(Seniority == "Executive Director")
    ),
    .(gvkey, Year)
  ]

  # dt_BoardEx[, Industry_CSO_Inclusive := mean(CSO, na.rm = TRUE), by = .(Year, naics_2)]
  # dt_BoardEx[, Industry_CSO_Exclusive := mean(CSO[gvkey != .BY[[1]]], na.rm = TRUE), by = .(Year, naics_2)]

  return(dt_BoardEx)
}

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
