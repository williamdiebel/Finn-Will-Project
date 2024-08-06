# Finn-Will-Project

This repository contains all code for the Finn-Will project. Data and other relevant project files are stored in the [Dropbox folder](https://www.dropbox.com/scl/fo/rxu4bupzz2pm7h7mgnq30/AJN6kdN9XvdZKCnHY0weMFY?rlkey=f0bxb1i794icjvk4zq0jbb9me&dl=0).

See **Data Files and Relevant Measures** and **Scripts** sections below for relevant information. Please edit these sections alongside any significant updates.

## Data Files and Relevant Measures

### 1. IndependentVariables_07232024.csv
- **Description:**
  - Panel data for all firms in BoardEx/Compustat, including [X] observations and [Y] unique firms *(Finn, please check/correct the description of the sample here)*.
- **ID Variables:**
  - `gvkey`
  - `cusip`
- **Key Measures:**
  - `CSO`: A binary variable taking the value 1 for all years after the firm first appointed a CSO and zero otherwise. *BoardEx*
  - `CSO_Year`: The year of the first CSO appointment and NA if no appointment happened until the end of the sample. *BoardEx* 
  - `ESG_Committee`: A binary variable taking the value 1 for all years after the firm first had an ESG committee and zero otherwise. *BoardEx*
  - `ESG_Committee_Year`: The year of the first ESG_Committee and NA if no committee existed until the end of the sample. *BoardEx*
  - `ESG_Incentives`: A binary variable indicating whether the firm has an ESG-related compensation policy. *Thomson/Refinitiv ESG (Asset4)*
  - `SupplierESGTraining`: A binary variable indicating whether the firm provides training in environmental, social, or governance factors for its suppliers. *Thomson/Refinitiv ESG (Asset4)*

### 2. data_essay2_robustness_v2.rds
- **Description:**
  - Panel data for a subset of firms in `rrpanel_comp_fs_fortune_cdp.rds`. This unbalanced panel contains 12616 firm-year observations from 2007 to 2020, including 124 CDP CDP members and 1094 CEM matched control firms.
- **ID Variables:**
  - `gvkey` *(Compustat)*
  - `cusip` *(Compustat)*
  - `isin` *(Compustat)*
  - `conm` *(Compustat)*
  - `reprisk_id` *(RepRisk)*
  - `primary_isin` *(RepRisk)*
  - `all_isins` *(RepRisk)*
  - `primary_cusip` *(RepRisk)*
  - `name` *(RepRisk)*
- **Key Measures:**
  - `total_incident_count`: Annual number of reported environmental supply chain incidents. *(RepRisk)*
  - `annual_rri`: Aggregate reputation risk index measure, including all ESG categories. *(RepRisk)*
  - `cdp_sc_member`: Indicates whether firm was a CDP SCP member in a given year. *(CDP SCP Annual Reports)*
- **Other Relevant Measures:**
  - `at_usd`: Total assets (USD). *(Compustat)*
  - `roa`: Return on assets. *(Compustat)*

### 3. rrpanel_comp_fs_fortune_cdp.rds
- **Description:**
  - Panel data for firms in RepRisk and Compustat. This unbalanced panel contains 171323 firm-year observations from 2007 to 2020, including 14640 unique firms.
- **ID Variables:**
  - Same as **2. data_essay2_robustness_v2.rds**.
- **Key Measures:**
  - Same as **2. data_essay2_robustness_v2.rds**.
- **Other Relevant Measures:**
  - Same as **2. data_essay2_robustness_v2.rds**.

### 4. supplier_years_4.rds
- **Description:**
  - Panel data for suppliers (defined as firms with >0 `customer_count`: # of buyer relationships in FactSet SC data) in RepRisk and Compustat. This unbalanced panel contains 52038 firm-year observations from 2007 to 2020, including 7891 unique firms.
- **ID Variables:**
  - Same as **2. data_essay2_robustness_v2.rds**.
- **Key Measures:**
  - `supplier_total_incidents`: annual number of environmental supply chain incidents at the firm(supplier)-level. *(RepRisk)*
  - `scope1 `: Scope 1 carbon emissions. *(Bloomberg ESG)*
  - `scope2 `: Scope 2 carbon emissions. *(Bloomberg ESG)*
  - See also **2. data_essay2_robustness_v2.rds**.
- **Other Relevant Measures:**
  - Same as **2. data_essay2_robustness_v2.rds**.

## Scripts

### 1. Preliminary Data Cleaning.R
- **Description:**
  - This script adds data from `IndependentVariables_07232024.csv` to the `data_essay2_robustness_v2.rds` panel and runs several regressions.

