library(tidyverse)
library(lubridate)
library(data.table)

dt_TMT <- fread("C:/Users/fipeters/OneDrive - Indiana University/07_Research/00_Data/BoardEx/Data/Organization - Composition of Officers, Directors and Senior Managers.csv")


# CSO Gender
dt_TMT_Profiles <- fread("C:/Users/fipeters/OneDrive - Indiana University/07_Research/00_Data/BoardEx/Data/Individual Profile Details.csv")

dt_TMT <- merge(
    dt_TMT,
    dt_TMT_Profiles[, c("directorid", "gender")]
)

dt_TMT <- dt_TMT %>%
    .[, CEO := ifelse(grepl("CEO", rolename, ignore.case = TRUE), TRUE, FALSE)] %>%
    .[, CSO := ifelse(grepl("sustainability", rolename, ignore.case = TRUE), TRUE, FALSE)] %>% # sustainability
    .[, CSO := case_when(
        CSO ~ TRUE,
        grepl("sustainable", rolename, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
    )] %>%
    .[, CSO := case_when(
        CSO ~ TRUE,
        grepl("responsib", rolename, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
    )] %>%
    .[, CSO := case_when(
        CSO ~ TRUE,
        grepl("ethics", rolename, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
    )] %>%
    .[, CSO := case_when(
        CSO ~ TRUE,
        grepl("environment", rolename, ignore.case = TRUE) ~ TRUE, # environment
        TRUE ~ FALSE
    )] %>%
    .[, CSO_M := CSO & gender == "M"] %>%
    .[, CSO_F := CSO & gender == "F"] %>%
    .[, CSO_SD := ifelse(CSO & seniority == "Supervisory Director", TRUE, FALSE)] %>%
    .[, CSO_ED := ifelse(CSO & seniority == "Executive Director", TRUE, FALSE)] %>%
    .[, CSO_Board := ifelse(CSO & seniority != "Senior Manager", TRUE, FALSE)] %>%
    .[, CSO_SM := ifelse(CSO & seniority == "Senior Manager", TRUE, FALSE)]

## Create a long data table for each director-company
dt_TMT <- dt_TMT %>%
    .[, StartYear := year(datestartrole)] %>%
    .[, StartYear_adj := ifelse(StartYear < 1999, 1999, StartYear)] %>%
    .[, EndYear := year(ifelse(is.na(dateendrole) | dateendrole == ymd("9000-01-01") | dateendrole == ymd("9999-12-31"), ymd("20250101"), dateendrole))] %>%
    .[EndYear >= 1999] %>%
    .[, Duration := EndYear - StartYear_adj + 1] %>%
    .[!is.na(Duration)]


# seperate board and CSO/CEO
dt_CSO <- dt_TMT[CSO == TRUE]
dt_CEO <- dt_TMT[CEO == TRUE]
dt_Board <- dt_TMT[seniority == "Supervisory Director" | seniority == "Executive Director"]

# Compute presence of CSO at the company-year level
dt_CSO <- dt_CSO %>%
    uncount(., Duration, .id = "rep_id") %>%
    .[, year := StartYear_adj + rep_id - 1]

dt_CSO <- dt_CSO[, .(
    CSOs = max(CSO),
    CSOs_M = max(CSO_M),
    CSOs_F = max(CSO_F),
    CSOs_SD = max(CSO_SD),
    CSOs_ED = max(CSO_ED),
    CSOs_SM = max(CSO_SM),
    CSOs_Board = max(CSO_Board)
), .(companyid, year)]

# Compute presence of prosocial CEO at the company-year level
# tbd

# Compute ratio of directors on board of other firm with CSO

## Load linktable to identify relevant directors based on Compustat link table
link <- fread("C:/Users/fipeters/OneDrive - Indiana University/07_Research/00_Data/BoardEx/Data/wrdsapps_link_crsp_comp_bdx.csv")
colnames(link) <- c("PERMCO", "gvkey", "companyid", "score", "preferred", "duplicate")

link <- unique(link, by = c("companyid", "gvkey")) # remove duplicates in link table
# for remaining duplicates, remove duplicate values with score != 1
duplicate_gvkey_year_list <- link$companyid[which(duplicated(link$companyid))]
link <- link[, duplicated := ifelse(companyid %in% duplicate_gvkey_year_list, 1, 0)]
link <- link[duplicated == 0 | score == 1] # keep only one observation per companyid, if score != 1, remove it

dt_Board <- dt_Board %>%
    uncount(., Duration, .id = "rep_id") %>%
    .[, year := StartYear_adj + rep_id - 1]

dt_Board <- merge(dt_Board[, .(directorid, companyid, year)], dt_CSO[, .(companyid, year, CSOs)],
    by = c("companyid", "year"), all.x = TRUE
)

dt_Board <- dt_Board %>%
    .[, CSOs := ifelse(is.na(CSOs), 0, CSOs)]

# Extract directors with CSO experience
temp <- dt_Board[, .(
    CSOExperience = sum(CSOs)
), by = .(directorid, year)]

# merge with original dt_Board
dt_Board <- merge(dt_Board, temp,
    by = c("directorid", "year"),
    all.x = TRUE
)

# Exclude focal firm CSO experience and compute
dt_Board <- dt_Board %>%
    .[, CSOExperience_Excl := CSOExperience - CSOs] %>%
    .[, CSOExperience := ifelse(CSOExperience > 0, 1, 0)] %>%
    .[, CSOExperience_Excl := ifelse(CSOExperience_Excl > 0, 1, 0)]

# Summarize by firm-year
dt_BoardEx <- dt_Board[, .(
    BoardCSOExperience = mean(CSOExperience),
    BoardCSOExperience_Excl = mean(CSOExperience_Excl)
), by = .(companyid, year)]


# Merge with prosocial CEO data
# tbd


# Merge with CSO data
dt_BoardEx <- merge(dt_BoardEx, dt_CSO[, .(companyid, year, CSOs)],
    by = c("companyid", "year"), all.x = TRUE
) %>%
    .[, CSO := ifelse(is.na(CSOs), 0, CSOs)] %>%
    .[, CSOs := NULL]


# Merge with BoardEx link table
dt_BoardEx <- merge(dt_BoardEx, link[, .(companyid, gvkey, PERMCO)],
    by.x = c("companyid"),
    by.y = c("companyid"),
    all.x = FALSE, all.y = FALSE
)

# Create a new column with the maximum CSO value from prior years
dt_BoardEx[, CSO_max_prior := shift(cummax(CSO), fill = 0, type = "lag"), by = gvkey]


# Ratio of indirect peer firms (5 to 50) with a CSO
## Merge with Hoberg-Phillips data
dt_hoberg <- fread("C:/Users/fipeters/OneDrive - Indiana University/07_Research/00_Data/Hoberg-Phillips/Data/ETNIC2_top50.csv",
    header = TRUE
)

dt_hoberg <- merge(dt_hoberg, dt_BoardEx[, .(gvkey, year, CSO_max_prior)],
    all.x = TRUE,
    by.x = c("gvkey2", "year"),
    by.y = c("gvkey", "year")
)

# summarize hoberg data by firm-year for top 5-50 peers
# Considers only those peers that have a BoardEx record, could be changed by setting NA to 0
dt_hoberg <- dt_hoberg %>%
    .[year >= 1999] %>%
    .[peer_rating >= 5 & peer_rating <= 50] %>%
    .[, .(CSO_Peer = mean(CSO_max_prior, na.rm = TRUE)), by = .(gvkey1, year)]

# Merge with BoardEx data
dt_BoardEx <- merge(dt_BoardEx, dt_hoberg,
    all.x = TRUE,
    by.x = c("gvkey", "year"),
    by.y = c("gvkey1", "year")
)

saveRDS(dt_BoardEx, file = "C:/Users/fipeters/OneDrive - Indiana University/07_Research/00_Data/BoardEx/Data/BoardEx_withVariables.rds")
