## Task-1 Data Import

ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE))
}

ensure_package(httr2)
ensure_package(rvest)
ensure_package(datasets)
ensure_package(purrr)
ensure_package(DT)
ensure_package(stringr)
ensure_package(tidyverse)
ensure_package(knitr)
ensure_package(dplyr)
ensure_package(tidyr)
ensure_package(ggplot2)
ensure_package(scales)
ensure_package(forcats)

get_eia_sep <- function(state, abbr){
  state_formatted <- str_to_lower(state) |> str_replace_all("\\s", "")
  
  dir_name <- file.path("data", "mp02")
  file_name <- file.path(dir_name, state_formatted)
  
  dir.create(dir_name, showWarnings=FALSE, recursive=TRUE)
  
  if(!file.exists(file_name)){
    BASE_URL <- "https://www.eia.gov"
    REQUEST <- request(BASE_URL) |> 
      req_url_path("electricity", "state", state_formatted)
    
    RESPONSE <- req_perform(REQUEST)
    
    resp_check_status(RESPONSE)
    
    writeLines(resp_body_string(RESPONSE), file_name)
  }
  
  TABLE <- read_html(file_name) |> 
    html_element("table") |> 
    html_table() |>
    mutate(Item = str_to_lower(Item))
  
  if("U.S. rank" %in% colnames(TABLE)){
    TABLE <- TABLE |> rename(Rank = `U.S. rank`)
  }
  
  CO2_MWh <- TABLE |> 
    filter(Item == "carbon dioxide (lbs/mwh)") |>
    pull(Value) |> 
    str_replace_all(",", "") |>
    as.numeric()
  
  PRIMARY <- TABLE |> 
    filter(Item == "primary energy source") |> 
    pull(Rank)
  
  RATE <- TABLE |>
    filter(Item == "average retail price (cents/kwh)") |>
    pull(Value) |>
    as.numeric()
  
  GENERATION_MWh <- TABLE |>
    filter(Item == "net generation (megawatthours)") |>
    pull(Value) |>
    str_replace_all(",", "") |>
    as.numeric()
  
  data.frame(CO2_MWh               = CO2_MWh, 
             primary_source        = PRIMARY,
             electricity_price_MWh = RATE * 10, # / 100 cents to dollars &
             # * 1000 kWh to MWH 
             generation_MWh        = GENERATION_MWh, 
             state                 = state, 
             abbreviation          = abbr
  )
}

EIA_SEP_REPORT <- map2(state.name, state.abb, get_eia_sep) |> list_rbind()



##EIA_SIP_REPORT in decreasing CO2 emission
ensure_package(scales)
ensure_package(DT)

EIA_SEP_REPORT |> 
  select(-abbreviation) |>
  arrange(desc(CO2_MWh)) |>
  mutate(CO2_MWh = number(CO2_MWh, big.mark=","), 
         electricity_price_MWh = dollar(electricity_price_MWh), 
         generation_MWh = number(generation_MWh, big.mark=",")) |>
  rename(`Pounds of CO2 Emitted per MWh of Electricity Produced`=CO2_MWh, 
         `Primary Source of Electricity Generation`=primary_source, 
         `Average Retail Price for 1000 kWh`=electricity_price_MWh, 
         `Total Generation Capacity (MWh)`= generation_MWh, 
         State=state) |>
  datatable()

#Task-2 (Initial exploring)
##Which state has the most expensive retail electricity?
most_expensive_state <- EIA_SEP_REPORT |>
  arrange(desc(electricity_price_MWh)) |>
  slice(1) |>
  pull(state)

most_expensive_state

#Answer - "Hawaii"

#Q-1 Which state has the ‘dirtiest’ electricity mix?
dirtiest_state <- EIA_SEP_REPORT |>
  arrange(desc(CO2_MWh)) |>
  slice(1) |>
  pull(state)

#Q-2 Get the CO2_MWh value for the dirtiest state
dirtiest_CO2_MWh <- EIA_SEP_REPORT |>
  filter(state == dirtiest_state) |>
  pull(CO2_MWh)
cat("The state with the dirtiest electricity mix is", dirtiest_state, "with", dirtiest_CO2_MWh, "pounds of CO2 emitted per MWh of electricity produced.\n")

#Answer - The state with the dirtiest electricity mix is West Virginia with 1925 pounds of CO2 emitted per MWh of electricity produced.

#Q-3 On average, how many pounds of CO2 are emitted per MWh of electricity produced in the US? (Note that you will need to use a suitably weighted average here.)

# Calculate the weighted average CO2_MWh
weighted_avg_CO2_MWh <- EIA_SEP_REPORT |>
  summarize(weighted_avg = sum(CO2_MWh * generation_MWh) / sum(generation_MWh)) |>
  pull(weighted_avg) 

weighted_avg_CO2_MWh

#Answer - 805

#Q-4 What is the rarest primary energy source in the US? What is the associated cost of electricity and where is it used?

rarest_info <- EIA_SEP_REPORT |>
  count(primary_source) |>
  slice_min(n, n = 1) |>
  left_join(EIA_SEP_REPORT, by = "primary_source") |>
  summarize(
    rarest_source = first(primary_source),
    cost = dollar(first(electricity_price_MWh)),
    states = paste(state, collapse = ", ")
  ) |>
  kable(
    col.names = c("Rarest Primary Source", "Cost (per 1000 kWh)", "States Using This Source"),
    align = "l"
  )

rarest_info

#Q-5 My home state, Texas, has a reputation as being the home of “dirty fossil fuels” while NY has a reputation as a leader in clean energy. How many times cleaner is NY’s energy mix than that of Texas?

tx_co2 <- EIA_SEP_REPORT |>
  filter(state == "Texas") |>
  pull(CO2_MWh)

ny_co2 <- EIA_SEP_REPORT |>
  filter(state == "New York") |>
  pull(CO2_MWh)

cleanliness_ratio_NY_VS_TX <- tx_co2 / ny_co2

cleanliness_ratio_NY_VS_TX
# Answer - 1.637931


## Downloading 2023 Annual Database Energy Consumption

ensure_package(readxl)
# Create 'data/mp02' directory if not already present
DATA_DIR <- file.path("data", "mp02")
dir.create(DATA_DIR, showWarnings=FALSE, recursive=TRUE)

NTD_ENERGY_FILE <- file.path(DATA_DIR, "2023_ntd_energy.xlsx")

if(!file.exists(NTD_ENERGY_FILE)){
  DS <- download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-10/2023%20Energy%20Consumption.xlsx", 
                      destfile=NTD_ENERGY_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_ENERGY_FILE)$size == 0)){
    cat("I was unable to download the NTD Energy File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_ENERGY_RAW <- read_xlsx(NTD_ENERGY_FILE)

ensure_package(tidyr)
to_numeric_fill_0 <- function(x){
  replace_na(as.numeric(x), 0)
}

NTD_ENERGY <- NTD_ENERGY_RAW |> 
  select(-c(`Reporter Type`, 
            `Reporting Module`, 
            `Other Fuel`, 
            `Other Fuel Description`)) |>
  mutate(across(-c(`Agency Name`, 
                   `Mode`,
                   `TOS`), 
                to_numeric_fill_0)) |>
  group_by(`NTD ID`, `Mode`, `Agency Name`) |>
  summarize(across(where(is.numeric), sum), 
            .groups = "keep") |>
  mutate(ENERGY = sum(c_across(c(where(is.numeric))))) |>
  filter(ENERGY > 0) |>
  select(-ENERGY) |>
  ungroup()

# Display 10 random rows
slice_sample(NTD_ENERGY , n=10)

#Task-3 Recoding the mode column
NTD_ENERGY |> distinct(Mode)
#Answer 1 DR   2 FB   3 MB   4 SR   5 TB   6 VP   7 CB   8 RB   9 LR   10 MG   11 CR   12 AR   13 TR   14 HR   15 YR   16 IP   17 PB   18 CC 


NTD_ENERGY <- NTD_ENERGY |>
  mutate(Mode=case_when(
    Mode == "DR" ~ "Demand Response", 
    Mode == "FB" ~ "Ferryboat",
    Mode == "MB" ~ "Bus",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "VP" ~ "Vanpool",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "LR" ~ "Light Rail",
    Mode == "MG" ~ "Ted Guideway",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "TR" ~ "Aerial Tramway",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "YR" ~ "Hybrid Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "PB" ~ "Publico",
    Mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"))
NTD_ENERGY |> distinct(Mode) |> kable()


#Downloading 2023 Service by Agency
NTD_SERVICE_FILE <- file.path(DATA_DIR, "2023_service.csv")
if(!file.exists(NTD_SERVICE_FILE)){
  DS <- download.file("https://data.transportation.gov/resource/6y83-7vuw.csv", 
                      destfile=NTD_SERVICE_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_SERVICE_FILE)$size == 0)){
    cat("I was unable to download the NTD Service File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_SERVICE_RAW <- read_csv(NTD_SERVICE_FILE)

NTD_SERVICE <- NTD_SERVICE_RAW |>
  mutate(`NTD ID` = as.numeric(`_5_digit_ntd_id`)) |> 
  rename(Agency = agency, 
         City   = max_city, 
         State  = max_state,
         UPT    = sum_unlinked_passenger_trips_upt, 
         MILES  = sum_passenger_miles) |>
  select(matches("^[A-Z]", ignore.case=FALSE)) |>
  filter(MILES > 0)


## TASK-4 Exploring NTD Service Data

# Q-1 Which transit service has the most UPT annually?
most_upt_service <- NTD_SERVICE |>
  group_by(Agency) |>  
  summarize(Total_UPT = sum(UPT, na.rm = TRUE)) |>  
  arrange(desc(Total_UPT)) |> 
  slice(1) |> kable()

most_upt_service

# Q-2 What is the average trip length of a trip on MTA NYC? 
mta_nyc_data <- NTD_SERVICE |>
  filter(State == "NY",  
         City %in% c("New York", "Brooklyn" , "Staten Island")) |>  
  filter(grepl("MTA", Agency)) |> 
  summarize(
    Total_MILES = sum(MILES, na.rm = TRUE),  
    Total_UPT = sum(UPT, na.rm = TRUE),
    Average_Trip_Length = Total_MILES / Total_UPT
  ) |> kable()

mta_nyc_data

# Q-3 Which transit service in NYC has the longest average trip length?
# (NB: You need to be careful with the City column here. Certain MTA services are officially located in “New York City” while others are located in Brooklyn.)
longest_avg_trip <- NTD_SERVICE |>
  filter(
    State == "NY",  
    City %in% c("New York", "Brooklyn" , "Staten Island")  
  ) |>
  group_by(Agency) |>  
  summarize(
    Total_MILES = sum(MILES, na.rm = TRUE), 
    Total_UPT = sum(UPT, na.rm = TRUE),  
    Average_Trip_Length = Total_MILES / Total_UPT  
  ) |>
  filter(Total_UPT > 0) |>  
  arrange(desc(Average_Trip_Length)) |>  
  slice(1) |>  
  select(Agency, Average_Trip_Length) |>
  kable()

longest_avg_trip

# Q-4 Which state has the fewest total miles travelled by public transit?
state_min_miles <- NTD_SERVICE |>
  group_by(State) |>  # Group by state
  summarize(Total_MILES = sum(MILES, na.rm = TRUE)) |>  # Sum miles for each state
  arrange(Total_MILES) |>  # Sort from smallest to largest
  slice(1) |>  # Select the state with the smallest total
  select(State, Total_MILES) |> kable()

state_min_miles

# Q-5 Are all states represented in this data? If no, which ones are missing? The state.name and state.abb objects we used above may be useful here.
states_in_data <- na.omit(unique(NTD_SERVICE$State))
valid_states_in_data <- states_in_data[states_in_data %in% state.abb]

missing_abbs <- setdiff(state.abb, valid_states_in_data)
missing_states <- state.name[state.abb %in% missing_abbs]

if (length(missing_states) == 0) {
  cat("All 50 U.S. states are represented in the dataset.\n")
} else {
  # Print missing states in a table
  data.frame(Missing_States = missing_states) |>
    kable(
      caption = "States Missing from the Dataset",
      align = "l"
    )
}


#task-5 Calculate Total Emissions

# Step 1: Clean agency names
NTD_SERVICE <- NTD_SERVICE %>%
  mutate(AgencyClean = gsub(", dba:.*$", "", Agency))

# Step 2: Join NTD_SERVICE with NTD_ENERGY using left_join
combined_df <- NTD_ENERGY %>%
  left_join(NTD_SERVICE %>% select(`NTD ID`, State, AgencyClean, City, UPT, MILES),
            by = "NTD ID")

# Step 3: Join with EIA_SEP_REPORT using State
emissions_df <- combined_df %>%
  left_join(EIA_SEP_REPORT %>% select(abbreviation, CO2_MWh),
            by = c("State" = "abbreviation"))

# Step 4: Pivot the fuel columns to long format
long_emissions <- emissions_df %>%
  pivot_longer(
    cols = c(`Bio-Diesel`, `Bunker Fuel`, `C Natural Gas`, `Diesel Fuel`, 
             `Electric Battery`, `Electric Propulsion`, Ethanol, Methonal, 
             Gasoline, Hydrogen, Kerosene, `Liquified Nat Gas`, `Liquified Petroleum Gas`),
    names_to = "Fuel_Type",
    values_to = "Fuel_Amount"
  ) %>%
  filter(Fuel_Amount > 0) # Remove zero fuel consumption

# Step 5: Define emission factors mapping
emission_factors <- tibble(
  Fuel_Type = c("Bio-Diesel", "Bunker Fuel", "C Natural Gas", "Diesel Fuel", 
                "Gasoline", "Kerosene", "Liquified Nat Gas", "Liquified Petroleum Gas"),
  Emission_Factor = c(22.45, 26.00, 120.85, 22.45, 
                      18.73, 21.78, 120.85, 12.68),
  Unit = c("gallon", "gallon", "thousand cubic feet", "gallon", 
           "gallon", "gallon", "thousand cubic feet", "gallon")
)

# Step 6: Join emission factors to the long format data
emissions_with_factors <- long_emissions %>%
  left_join(emission_factors, by = "Fuel_Type")

# Step 7: Calculate emissions
emissions_calculated <- emissions_with_factors %>%
  mutate(
    # Calculate CO2 for combustion fuels
    CO2_Emissions = case_when(
      Fuel_Type %in% c("Electric Battery", "Electric Propulsion") ~ 
        Fuel_Amount / 1000 * CO2_MWh, # Convert kWh to MWh
      !is.na(Emission_Factor) ~ Fuel_Amount * Emission_Factor,
      TRUE ~ 0
    )
  )

# Step 8: Pivot back to wide format for the final table
agency_mode_emissions <- emissions_calculated %>%
  group_by(`NTD ID`, `Agency Name`, Mode, State, CO2_MWh) %>%
  summarize(
    Total_CO2_Emissions = sum(CO2_Emissions, na.rm = TRUE),
    .groups = "drop"
  )

# Step 9: Create a more detailed table with fuel breakdowns
detailed_emissions <- emissions_calculated %>%
  group_by(`NTD ID`, `Agency Name`, Mode, State, CO2_MWh, Fuel_Type) %>%
  summarize(
    Fuel_Amount = sum(Fuel_Amount, na.rm = TRUE),
    CO2_Emissions = sum(CO2_Emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(`NTD ID`, `Agency Name`, Mode, State, CO2_MWh),
    names_from = Fuel_Type,
    values_from = c(Fuel_Amount, CO2_Emissions),
    values_fill = 0
  ) %>%
  mutate(
    Total_CO2_Emissions = rowSums(select(., starts_with("CO2_Emissions_")), na.rm = TRUE)
  )

# View the final tables
print(agency_mode_emissions)
print(detailed_emissions)  


#Task-6 Normalize Emissions to Transit Usage

library(ggplot2)

# Step 1: Sum emissions across modes for each agency
agency_emissions <- emissions_calculated %>%
  group_by(`NTD ID`, `Agency Name`, State) %>%
  summarize(
    Total_CO2_Emissions = sum(CO2_Emissions, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Join with NTD_SERVICE to get UPT and passenger miles
agency_efficiency <- agency_emissions %>%
  left_join(NTD_SERVICE %>% select(`NTD ID`, UPT, MILES), by = "NTD ID") %>%
  filter(!is.na(UPT) & !is.na(MILES) & UPT > 0 & MILES > 0)

# Step 3: Calculate emissions per UPT and per passenger mile
agency_efficiency <- agency_efficiency %>%
  mutate(
    CO2_per_UPT = Total_CO2_Emissions / UPT,
    CO2_per_Mile = Total_CO2_Emissions / MILES,
    # Create size categories based on UPT
    Size_Category = case_when(
      UPT < 1000000 ~ "Small",
      UPT < 10000000 ~ "Medium",
      TRUE ~ "Large"
    )
  )

# Step 4: Identify most efficient agencies by size category
most_efficient_upt <- agency_efficiency %>%
  filter(!is.infinite(CO2_per_UPT) & !is.na(CO2_per_UPT)) %>%
  group_by(Size_Category) %>%
  arrange(CO2_per_UPT) %>%
  slice_head(n = 5) %>%
  select(Size_Category, `Agency Name`, State, UPT, Total_CO2_Emissions, CO2_per_UPT)

most_efficient_mile <- agency_efficiency %>%
  filter(!is.infinite(CO2_per_Mile) & !is.na(CO2_per_Mile)) %>%
  group_by(Size_Category) %>%
  arrange(CO2_per_Mile) %>%
  slice_head(n = 5) %>%
  select(Size_Category, `Agency Name`, State, MILES, Total_CO2_Emissions, CO2_per_Mile)

# Step 5: Create visualizations
# Plot emissions per UPT by size category
upt_plot <- ggplot(agency_efficiency, aes(x = Size_Category, y = CO2_per_UPT)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "CO2 Emissions per Unlinked Passenger Trip by Agency Size",
       y = "CO2 (pounds) per UPT (log scale)",
       x = "Agency Size Category") +
  theme_minimal()

# Plot emissions per passenger mile by size category
mile_plot <- ggplot(agency_efficiency, aes(x = Size_Category, y = CO2_per_Mile)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "CO2 Emissions per Passenger Mile by Agency Size",
       y = "CO2 (pounds) per Mile (log scale)",
       x = "Agency Size Category") +
  theme_minimal()

mile_plot

# View results
print("Most Efficient Agencies by UPT:")
print(most_efficient_upt)

print("Most Efficient Agencies by Passenger Mile:")
print(most_efficient_mile)

# Print summary statistics
summary_stats <- agency_efficiency %>%
  group_by(Size_Category) %>%
  summarize(
    Count = n(),
    Median_CO2_per_UPT = median(CO2_per_UPT, na.rm = TRUE),
    Median_CO2_per_Mile = median(CO2_per_Mile, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary Statistics by Agency Size:")
print(summary_stats)

#Task-7
#1. Greenest Transit Agency Award (based on CO2 per passenger mile)
greenest_agencies <- agency_efficiency %>%
  group_by(Size_Category) %>%
  arrange(CO2_per_Mile) %>%
  slice_head(n = 1) %>%
  ungroup()
greenest_agencies

## 2. Most Emissions Avoided Award
# Step 1: Calculate emissions if all transit usage used cars instead
agency_emissions_avoided <- agency_efficiency %>%
  mutate(
    # Average car fuel efficiency (mpg) based on CAFE standards (27.5 mpg for 2020)
    Car_Gallons_If_Driving = MILES / 27.5,
    
    # Convert gallons to CO2 emissions using gasoline emission factor (18.73 lbs CO2/gallon)
    Car_CO2_If_Driving = Car_Gallons_If_Driving * 18.73,
    
    # Calculate emissions avoided
    CO2_Emissions_Avoided = Car_CO2_If_Driving - Total_CO2_Emissions,
    
    # Calculate percentage of emissions avoided
    Percent_Emissions_Avoided = (CO2_Emissions_Avoided / Car_CO2_If_Driving) * 100
  )
#step2. Most Emissions Avoided (by size)
emissions_avoided_winners <- agency_emissions_avoided %>%
  group_by(Size_Category) %>%
  arrange(desc(CO2_Emissions_Avoided)) %>%
  slice_head(n = 1) %>%
  ungroup()

emissions_avoided_winners


# 3. Highest Electrification Award
# Join back with original data to get fuel breakdowns
agency_fuels <- emissions_calculated %>%
  mutate(
    Is_Electric = Fuel_Type %in% c("Electric Battery", "Electric Propulsion")
  ) %>%
  group_by(`NTD ID`, `Agency Name`, State, Is_Electric) %>%
  summarize(
    Fuel_Total = sum(Fuel_Amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(`NTD ID`, `Agency Name`, State),
    names_from = Is_Electric,
    names_prefix = "Fuel_",
    values_from = Fuel_Total,
    values_fill = 0
  ) %>%
  rename(
    Electric_Fuel = `Fuel_TRUE`,
    NonElectric_Fuel = `Fuel_FALSE`
  ) %>%
  mutate(
    Total_Fuel = Electric_Fuel + NonElectric_Fuel,
    Percent_Electric = (Electric_Fuel / Total_Fuel) * 100
  ) %>%
  left_join(agency_efficiency %>% select(`NTD ID`, Size_Category), by = "NTD ID")

# 3. Highest Electrification (by size)
electrification_winners <- agency_fuels %>%
  filter(Total_Fuel > 0) %>%
  group_by(Size_Category) %>%
  arrange(desc(Percent_Electric)) %>%
  slice_head(n = 1) %>%
  ungroup()
electrification_winners

#Step-8
# Visualization 1: Greenest Transit Agencies (CO2 per passenger mile)
# Calculate the median CO2 per passenger mile
median_co2_per_mile <- median(greenest_agencies$CO2_per_Mile, na.rm = TRUE)

# Then create your visualization
greenest_viz <- ggplot() +
  # Add bars for the winners
  geom_col(data = greenest_agencies, 
           aes(x = reorder(`Agency Name`, -CO2_per_Mile), y = CO2_per_Mile, fill = Size_Category),
           alpha = 0.8) +
  # Add reference line for median
  geom_hline(yintercept = median_co2_per_mile, linetype = "dashed", color = "red") +
  geom_text(aes(0, median_co2_per_mile, label = paste0("Median: ", round(median_co2_per_mile, 2), " lbs CO2/mile")), 
            hjust = -0.1, vjust = -0.5, color = "red") +
  # Labels and formatting
  labs(title = "GTA IV Award: Greenest Transit Agencies by Size Category",
       subtitle = "Pounds of CO2 emissions per passenger mile",
       x = NULL,
       y = "CO2 Emissions (lbs) per Passenger Mile",
       caption = "Source: National Transit Database (NTD) & EPA Carbon Emissions Factors") +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  )

greenest_viz

# Visualization 2: Most Emissions Avoided
emissions_avoided_viz <- ggplot() +
  # Add bars for winners
  geom_col(data = emissions_avoided_winners, 
           aes(x = reorder(`Agency Name`, CO2_Emissions_Avoided), 
               y = CO2_Emissions_Avoided / 1000000, fill = Size_Category),
           alpha = 0.8) +
  # Add reference line for median
  geom_hline(yintercept = median_emissions_avoided / 1000000, 
             linetype = "dashed", color = "red") +
  geom_text(aes(0, median_emissions_avoided / 1000000, 
                label = paste0("Median: ", round(median_emissions_avoided / 1000000, 2), " million lbs")), 
            hjust = -0.1, vjust = -0.5, color = "red") +
  # Labels and formatting
  labs(title = "GTA IV Award: Most Emissions Avoided by Transit Agencies",
       subtitle = "Millions of pounds of CO2 emissions avoided compared to car travel",
       x = NULL,
       y = "CO2 Emissions Avoided (million lbs)",
       caption = "Source: National Transit Database (NTD), EPA Carbon Emissions Factors & CAFE Standards") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  )

emissions_avoided_viz
# Visualization 3: Electrification Leaders
# First, check if there are any NAs in your data
electrification_winners <- na.omit(electrification_winners)
# OR replace any NA values with a meaningful category
# electrification_winners$Size_Category[is.na(electrification_winners$Size_Category)] <- "Other"

# Calculate the median percentage of electric fuel usage
median_percent_electric <- median(electrification_winners$Percent_Electric, na.rm = TRUE)

# Create the visualization with guides to explicitly control the legend
electrification_viz <- ggplot() +
  # Add bars for winners
  geom_col(data = electrification_winners, 
           aes(x = reorder(`Agency Name`, Percent_Electric), 
               y = Percent_Electric, fill = Size_Category),
           alpha = 0.8) +
  # Add reference line for median
  geom_hline(yintercept = median_percent_electric, 
             linetype = "dashed", color = "red") +
  geom_text(aes(0, median_percent_electric, 
                label = paste0("Median: ", round(median_percent_electric, 1), "%")), 
            hjust = -0.1, vjust = -0.5, color = "red") +
  # Labels and formatting
  labs(title = "GTA IV Award: Transit Electrification Leaders",
       subtitle = "Percentage of transit fuel from electric sources",
       x = NULL,
       y = "Electric Fuel Usage (%)",
       caption = "Source: National Transit Database (NTD)") +
  scale_fill_brewer(palette = "Purples", direction = -1, drop = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  # Explicitly control which values appear in the legend
  guides(fill = guide_legend(override.aes = list(alpha = 0.8),
                             breaks = c("Large", "Medium", "Small")))
electrification_viz
