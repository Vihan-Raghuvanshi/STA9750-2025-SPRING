if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  if(!require("stringr")) install.packages("stringr")
  library(stringr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}
#changing the name of csv file  
nyc_payroll <- read_csv("data/mp01/nyc_payroll_export.csv")  
nyc_payroll


#Change formatting of certain columns
nyc_clean <- nyc_payroll %>%
  mutate(across(c("agency_name", "last_name", "first_name",
                  "work_location_borough", "title_description",
                  "leave_status_as_of_june_30"),
                str_to_title))

#Crating column for total compensation
nyc_clean <- nyc_clean %>%
  mutate(
    total_compensation = case_when(
      # High-ranking officials with fixed salary
      pay_basis == "per Annum" ~ base_salary,
      
      # Hourly employees: regular pay + 1.5x for OT
      pay_basis == "per Hour" ~ base_salary * (regular_hours + 1.5 * ot_hours),
      
      # Day-rate employees: convert total hours to days (7.5 hours/day)
      pay_basis == "Per Day" ~ base_salary * ((regular_hours + ot_hours) / 7.5),
      
      # Fallback for unexpected pay types
      TRUE ~ NA_real_
    )
  )

#renaming columns fiscal_year as Fiscal Year, title_description as Position, agency_name as Agency, and total_compensation as Total Salary
nyc_clean <- nyc_clean %>%
  rename(
    Fiscal_Year = fiscal_year,
    Position = title_description,
    Agency = agency_name,
    Total_Salary = total_compensation
  )

#Crating career table for Mr.Eric L. Adams showing fiscal year, position, agency, and total salary
eric_adams <- nyc_clean %>%
  filter( first_name == "Eric", last_name == "Adams", mid_init == "L") %>%
  select(Fiscal_Year, Position, Agency, Total_Salary)
eric_adams


#Step-4 Answering the following questions
#Q1. Which job title has the highest base rate of pay? (If needed, assume a standard 2000 hour work year and no overtime.)

nyc_clean %>%
  filter(!is.na(base_salary)) %>%
  select(Position, base_salary) %>%
  arrange(desc(base_salary)) %>% head(1)
  

# Answer
# Position      base_salary
# Chair         414707


#Q2 Which individual & in what year had the single highest city total payroll (regular and overtime combined)?
  nyc_clean %>%
  select(first_name, last_name, Fiscal_Year, Total_Salary) %>%
  arrange(desc(Total_Salary))  %>% head(1)
  
# Answer
#   first_name last_name Fiscal_Year Total_Salary
#   Gregory    Russ             2022       414707

#Q3 Which individual worked the most overtime hours in this data set?
nyc_clean %>%
  select(first_name, last_name, Fiscal_Year, ot_hours) %>%
  arrange(desc(ot_hours)) %>% head(1)

#Answer
#first_name last_name   Fiscal_Year ot_hours
# James      Internicola        2022    3693.

#Q4 Which agency has the highest average total annual payroll (base and overtime pay per employee)?
nyc_clean %>%
  group_by(Agency) %>%
  summarise(avg_total_pay = mean(Total_Salary, na.rm = TRUE)) %>%
  arrange(desc(avg_total_pay)) %>% head(1) %>%
  ungroup()
  

#Answer
#  Agency                         avg_total_pay
#  Office Of Racial Equity              151093.

#Q5 Which agency has the most employees on payroll in each different year? (rework on this solution)
nyc_clean %>%
  filter(Fiscal_Year >= 2014 & Fiscal_Year <= 2022) %>%
  group_by(Fiscal_Year, Agency) %>%
  summarise(totalemp = n(), .groups = "drop") %>%
  group_by(Fiscal_Year) %>%
  top_n(1, totalemp) %>%
  ungroup()

#Answer
#Fiscal_Year Agency                 totalemp
#2014 Dept Of Ed Pedagogical   100589
#2015 Dept Of Ed Pedagogical   111857
#2016 Dept Of Ed Pedagogical   106263
#2017 Dept Of Ed Pedagogical   104629
#2018 Dept Of Ed Pedagogical   107956
#2019 Dept Of Ed Pedagogical   112067
#2020 Dept Of Ed Pedagogical   114999
#2021 Dept Of Ed Pedagogical   113523
#2022 Dept Of Ed Pedagogical   120453

#Q6  Which agency has the highest overtime usage (compared to regular hours)? 
nyc_clean %>%
  group_by(Agency) %>%
  summarise(total_ot_hours = sum(ot_hours, na.rm = TRUE),
            total_regular_hours = sum(regular_hours, na.rm = TRUE),
            time_ratio = total_ot_hours / total_regular_hours) %>%
  arrange(desc(time_ratio)) %>%
  ungroup()

#Answer
#Agency                        total_ot_hours total_regular_hours   time_ratio
#Board Of Election                  3062029.           15339960.     0.200


#Q7  (check right code) What is the average salary of employees who work outside the five boroughs? (That is, whose work_location_borough is not one of the five counties.)
nyc_clean %>%
  summarise(avg_salary = mean(Total_Salary, na.rm = TRUE))

#answer
#  avg_salary
#   60798.

#Q8 How much has the city’s aggregate payroll grown over the past 10 years

payroll_growth <- nyc_clean %>%
  filter(Fiscal_Year >= max(Fiscal_Year) - 10) %>%
  group_by(Fiscal_Year) %>%
  summarise(
    Total_Payroll = sum(Total_Salary, na.rm = TRUE) 
  ) %>%
  arrange(Fiscal_Year)

growth_rate <- payroll_growth %>%
  summarise(
    Growth = (last(Total_Payroll) / first(Total_Payroll) - 1) * 100
  )

print(payroll_growth)
print(paste("Aggregate payroll grew by", round(growth_rate$Growth, 2), "% over 10 years."))

library(ggplot2)
ggplot(payroll_growth, aes(x = Fiscal_Year, y = Total_Payroll)) +
  geom_line() +
  labs(title = "NYC Aggregate Payroll Growth (Past 10 Years)", x = "Year", y = "Total Payroll ($)")

#answer
# "Aggregate payroll grew by 44.84 % over 10 years."

### ----------------------------------------------------------------------------
### Policy I: Capping Salaries at Mayoral Level
### ----------------------------------------------------------------------------

# --------------------------------------------------------
# Step 1: Identify Mayor's Salary for Each Fiscal Year
# --------------------------------------------------------

mayor_salary <- nyc_clean %>%
  filter(
    Position == "Mayor" | 
      Agency == "Office Of The Mayor" 
  ) %>%
  group_by(Fiscal_Year) %>%
  summarise(Mayor_Salary = max(Total_Salary, na.rm = TRUE))

mayor_salary
# ---------------------------------------------------------
# Step 2: Identify Employees Earning More Than the Mayor
# ---------------------------------------------------------

employees_above_mayor <- nyc_clean %>%
  filter(!(Position == "Mayor" | Agency == "Office Of The Mayor")) %>%
  left_join(mayor_salary, by = "Fiscal_Year") %>%
  mutate(
    Earns_More_Than_Mayor = case_when(
      Total_Salary > Mayor_Salary ~ "Yes",
      TRUE ~ "No"),
    Excess_Amount = case_when(
      Earns_More_Than_Mayor == "Yes" ~ Total_Salary - Mayor_Salary,
      TRUE ~ 0)) %>%
  filter(Earns_More_Than_Mayor == "Yes") %>%
  select(Fiscal_Year, first_name, Agency, Position, Total_Salary, Mayor_Salary, Excess_Amount) %>%
  arrange(Fiscal_Year)
employees_above_mayor


# ---------------------------------------------------------
# Step 3: Compute Total Savings and Impacted Groups
# ---------------------------------------------------------

employees_above_mayor %>%
  group_by(Fiscal_Year) %>%
  summarise(
    Overpaid_Employees = n(),
    Total_Excess = sum(Excess_Amount)) %>%
  ungroup()

total_excess <- sum(employees_above_mayor$Excess_Amount, na.rm = TRUE)
print(paste("Total excess across all years:", scales::dollar(total_excess)))


#Answer "Total excess across all years: $2,657,215"

# Total number of overpaid employee on Agency level
employees_above_mayor %>%
  count(Agency, name = "Number_Overpaid") %>%
  arrange(desc(Number_Overpaid))

#Answer    Agency                         Number_Overpaid
#1 Office Of The Comptroller                   29
#2 Office Of The Actuary                       12
#3 Dept Of Ed Pedagogical                       9
#4 Nyc Housing Authority                        5
#5 Community College (Laguardia)                4
#6 Community College (Bronx)                    3
#7 Dept Of Health/Mental Hygiene                2
#8 Community College (Hostos)                   1
#9 Community College (Kingsboro)                1
#10 Community College (Manhattan)                1
#11 Community College (Queensboro)               1

# Total number of overpaid employee by Position
employees_above_mayor %>%
  count(Position, name = "Count") %>%
  arrange(desc(Count))


#Answer
#   Position                     Count
#   Director Of  Investments        13
#   Chief Actuary                   11
#   President                       11
#   Pension Investment Advisor      10
#   Chancellor                       7
#   Administrative Staff Analyst     6
#   Chair                            3
#   Assistant Superintendent         2
#   City Medical Specialist          2
#   Executive Agency Counsel         2
#   Administrative Actuary           1
# -------------------------------------------------------
# Policy Impact Summary  
# Savings: $2.65M total (2014–2024).  
# Top Affected Agencies:  
# - Comptroller (29 employees)  
# - Actuary (12)  
# - Education (9)  
# Key Roles Impacted:  
# - Investment Directors (13)  
# - Chief Actuaries (11)  
# - Pension Advisors (10)  
# Risks:  
# - Loss of specialized talent (actuaries, investment roles).  
# - Morale decline in capped positions.  
# -------------------------------------------------------
#RECOMMENDATION  
#1. Adopt with Modifications:  
#- Exempt Critical Roles: Exempt positions where specialized skills are essential (e.g., actuaries, investment directors) to retain talent.  
#- Phase-In Caps: Gradually reduce salaries over 2–3 years to minimize attrition.  
#- Benchmark Market Rates: Adjust the mayor’s salary periodically to reflect inflation and market trends for key roles.  
#2. Monitor and Report:  
#- Track turnover rates in impacted agencies (e.g., Comptroller’s Office) post-implementation.
#- Compare savings against recruitment/training costs for replacements.  
#3. Prioritize Equity:  
#- Address disparities where lower-level roles (e.g., City Medical Specialists) are overpaid due to overtime or longevity pay, while ensuring caps don’t harm frontline workers.  
#Final Note: While the policy saves $2.6M, the risks to specialized roles outweigh the benefits if implemented indiscriminately.
# A targeted approach with exemptions for high-skill positions would balance fiscal responsibility with operational stability.
# -------------------------------------------------------


### ----------------------------------------------------------------------------
### Policy II: Increasing Staffing to Reduce Overtime Expenses
### ----------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Step 1: Calculate FTEs Needed to Eliminate Overtime by Agency/Job Title
# -------------------------------------------------------------------------

overtime_analysis <- nyc_clean %>%
  # Filter to valid hourly rates and years
  filter(
    Fiscal_Year >= 2014,
    between(regular_hours, 1000, 4000)
  ) %>%
  # Calculate hourly rates SAFELY
  mutate(
    Hourly_Rate = case_when(
      pay_basis == "per Annum" ~ Total_Salary / 2080,  # Standard annual hours
      pay_basis == "Prorated Annual" ~ Total_Salary / 2080,
      pay_basis == "per Hour" ~ Total_Salary,
      pay_basis == "per Day" ~ Total_Salary / 8,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(between(Hourly_Rate, 15, 200)) %>%
  
  # Group PROPERLY to count multiple employees
  group_by(Fiscal_Year, Agency, Position) %>%
  summarise(
    Total_Overtime_Hours = sum(ot_hours, na.rm = TRUE),
    Avg_Hourly_Rate = median(Hourly_Rate, na.rm = TRUE),  # Use median
    Employee_Count = n(),  # Actual number of employees
    .groups = "drop"
  ) %>%
  
  # Add sanity checks
  filter(
    Total_Overtime_Hours < 8760,  # Max = 24 hrs/day × 365 days
    Employee_Count > 1             # Require at least 2 employees
  ) %>%
  
  # Calculate realistic savings
  mutate(
    FTEs_Needed = Total_Overtime_Hours / 2080,
    Potential_Savings = (Total_Overtime_Hours * 1.5 * Avg_Hourly_Rate) - 
      (FTEs_Needed * 2080 * Avg_Hourly_Rate)
  ) %>%
  arrange(Fiscal_Year, desc(Potential_Savings))


# ---------------------------------------
# Step 2: Aggregate savings by agency
# ---------------------------------------

agency_summary <- overtime_analysis %>%
  group_by(Agency, Fiscal_Year) %>%
  summarise(
    Yearly_Savings = sum(Potential_Savings, na.rm = TRUE),
    Positions_Affected = n_distinct(Position),
    .groups = "drop"
  ) %>%
  group_by(Agency) %>%
  summarise(
    Avg_Annual_Savings = mean(Yearly_Savings, na.rm = TRUE),
    Total_10_Year_Savings = sum(Yearly_Savings, na.rm = TRUE),
    Avg_Positions_Per_Year = round(mean(Positions_Affected, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(Avg_Annual_Savings > 100000) %>%
  mutate(
    Avg_Annual_Savings = scales::dollar(Avg_Annual_Savings),
    Total_10_Year_Savings = scales::dollar(Total_10_Year_Savings)
  ) %>%
  arrange(desc(Avg_Annual_Savings))

agency_summary

# -------------------------------------------------------
#RECOMMENDATION
#Recommendation to CATS Commissioners:Adopt a targeted hiring policy to reduce overtime costs, prioritizing:
#Top Agencies (Annual Savings):Sanitation (1M), Correction (967K), Building (938K) 
#Focus: Roles with >3,000 overtime hours/year (e.g., Sanitation Mechanics, Correction Legal Coordinators).
#Immediate Action:
#Phase 1 (2024): Hire 3–5 FTEs in Sanitation/Correction.
#Exemptions: Retain overtime for emergency roles (Firefighters, Police during crises).
#Outcome:$75M+ savings over 10 years with phased hiring.
#Audit data anomalies (e.g., DEP’s $32M savings) before scaling.
# -------------------------------------------------------


### ----------------------------------------------------------------------------
### Policy III: Optimizing Management Ratios
### ----------------------------------------------------------------------------


# Optimized Managerial Ratio Analysis
nyc_proposal3 <- nyc_clean %>%
  mutate(
    # 1. Broad manager classification
    is_manager = str_detect(Position, "(?i)manager|director|chief|supervisor|head|chair|commissioner"),
    
    # 2. Calculate non-manager salary benchmarks
    non_mgr_salary = median(Total_Salary[!is_manager], na.rm = TRUE),
    .by = Agency
  ) %>%
  
  # 3. Aggressive staffing ratios
  mutate(
    non_mgr_count = sum(!is_manager),
    max_managers = pmax(ceiling(non_mgr_count/15), 1), # 1:15 ratio
    excess_managers = pmax(sum(is_manager) - max_managers, 0),
    .by = c(Agency, Fiscal_Year)
  ) %>%
  
  # 4. Identify top-paid excess managers
  mutate(
    manager_rank = if_else(is_manager, rank(-Total_Salary), NA_integer_),
    cut_candidate = is_manager & manager_rank <= excess_managers,
    .by = c(Agency, Fiscal_Year)
  ) %>%
  
  # 5. Calculate maximum potential savings
  mutate(
    replacement_cost = if_else(cut_candidate, non_mgr_salary * 1.3, Total_Salary),
    annual_savings = Total_Salary - replacement_cost
  )

# Summary Results
policy3_summary <- nyc_proposal3 %>%
  filter(cut_candidate) %>%
  summarise(
    managers_affected = n(),
    total_10yr_savings = sum(annual_savings) * 10,
    avg_salary = mean(Total_Salary),
    .by = Agency
  ) %>%
  arrange(desc(total_10yr_savings)) %>%
  filter(total_10yr_savings > 0)

print(policy3_summary, n = 20)


