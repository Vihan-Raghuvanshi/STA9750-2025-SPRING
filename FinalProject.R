# Load required packages 
required_packages <- c("dplyr", "readr", "dplyr", "tidyr", "stringr", "jsonlite", "httr", "purrr", 
                       "ggplot2", "lubridate", "scales", "ggpubr", "viridis", 
                       "gridExtra", "cowplot", "plotly", "proxy", "igraph", 
                       "patchwork", "ggforce", "knitr" , "kableExtra", "utils")

# Install missing packages
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Function to load MTA daily data
load_mta_daily_data <- function(use_cache = TRUE) {
  # Create directory if it doesn't exist
  dir_name <- file.path("data", "finalproject")
  dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  # Define processed data path (cache)
  processed_path <- file.path(dir_name, "processed_daily_data.rds")
  
  # Define the source path in Downloads folder
  downloads_folder <- file.path(Sys.getenv("HOME"), "Downloads")
  source_file <- "MTA_Daily_Ridership_Data__2020_-_2025_20250505.csv"
  source_path <- file.path(downloads_folder, source_file)
  
  # Use cache if available and requested
  if (use_cache && file.exists(processed_path)) {
    message("Using cached processed data...")
    return(readRDS(processed_path))
  }
  
  # Check if source file exists
  if (!file.exists(source_path)) {
    stop("Source file not found in Downloads folder. Please check the file name and location.")
  }
  
  # Read the data
  message("Reading data from CSV file...")
  daily_data <- readr::read_csv(source_path, show_col_types = FALSE)
  
  # Cache the data
  saveRDS(daily_data, processed_path)
  message("Data cached successfully")
  
  return(daily_data)
}

# Load the data
daily_ridership <- load_mta_daily_data(use_cache = FALSE)

# Show just a glimpse of the table
glimpse(daily_ridership)


####3 formatting the daily ridership

# Format the data
daily_ridership <- daily_ridership %>%
  # Convert Date to proper date format
  mutate(
    Date = mdy(Date),
    
    # Create day of week, month, year columns
    day_of_week = weekdays(Date),
    is_weekend = day_of_week %in% c("Saturday", "Sunday"),
    month = month(Date),
    year = year(Date),
    
    # Create quarter column
    quarter = quarter(Date)
  )

# Show the updated structure
View(daily_ridership)

# Cache the processed data
saveRDS(daily_ridership, file.path("data", "finalproject", "processed_daily_data.rds"))

################### Time series analysis of ridership trends
# Analyze subway ridership trends over time


# Overall subway ridership over time
ggplot(daily_ridership, aes(x = Date, y = `Subways: Total Estimated Ridership`)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "NYC Subway Daily Ridership (2020-2023)",
    x = "Date",
    y = "Total Estimated Ridership",
    caption = "Source: MTA Daily Ridership Data"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Recovery percentage over time
ggplot(daily_ridership, aes(x = Date, y = `Subways: % of Comparable Pre-Pandemic Day`)) +
  geom_line(color = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "NYC Subway Ridership Recovery",
    x = "Date",
    y = "Percentage of Pre-Pandemic Levels",
    caption = "Source: MTA Daily Ridership Data"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

########### Weekday VS Weekend Analysis (remote work indicator)
# Compare weekday and weekend recovery patterns
weekday_weekend_analysis <- daily_ridership %>%
  group_by(day_of_week, year, month) %>%
  summarize(
    avg_ridership = mean(`Subways: Total Estimated Ridership`, na.rm = TRUE),
    avg_recovery_pct = mean(`Subways: % of Comparable Pre-Pandemic Day`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_year = as.Date(paste(year, month, "01", sep = "-")))

# Plot weekday vs weekend recovery
ggplot(weekday_weekend_analysis, 
       aes(x = month_year, y = avg_recovery_pct, color = day_of_week)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Weekday vs. Weekend Recovery Patterns",
    x = "Month",
    y = "Average % of Pre-Pandemic Ridership",
    color = "Day Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)


########## Revenue Impact Estimation:
# 4. Revenue Impact Estimation with Updated Fare Information

# Define fare rates based on time periods
fare_analysis <- daily_ridership %>%
  mutate(
    # Convert Date to proper date format if not done already
    Date = if(is.character(Date)) mdy(Date) else Date,
    
    # Apply correct fare rates based on date periods
    fare_rate = case_when(
      Date < as.Date("2023-08-20") ~ 2.75, # $2.75 (March 22, 2015 – August 19, 2023)
      TRUE ~ 2.90 # $2.90 (August 20, 2023 – present)
    ),
    
    # Estimate 2019 equivalent ridership by dividing current by recovery percentage
    # Handle cases with 0% recovery to avoid division by zero
    estimated_2019_equivalent = case_when(
      `Subways: % of Comparable Pre-Pandemic Day` > 0 ~ 
        `Subways: Total Estimated Ridership` / `Subways: % of Comparable Pre-Pandemic Day`,
      TRUE ~ NA_real_
    ),
    
    # Calculate ridership gap
    ridership_gap = estimated_2019_equivalent - `Subways: Total Estimated Ridership`,
    
    # Estimate daily revenue loss (using variable fare rates)
    daily_revenue_loss = ridership_gap * fare_rate,
    
    # Create month-year for grouping
    month_year = floor_date(Date, "month")
  )

# Monthly revenue loss summary
monthly_revenue_loss <- fare_analysis %>%
  group_by(year, month) %>%
  summarize(
    total_ridership = sum(`Subways: Total Estimated Ridership`, na.rm = TRUE),
    total_estimated_2019 = sum(estimated_2019_equivalent, na.rm = TRUE),
    total_ridership_gap = sum(ridership_gap, na.rm = TRUE),
    total_revenue_loss = sum(daily_revenue_loss, na.rm = TRUE),
    avg_recovery_pct = mean(`Subways: % of Comparable Pre-Pandemic Day`, na.rm = TRUE),
    avg_fare_rate = mean(fare_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month_year = as.Date(paste(year, month, "01", sep = "-")),
    total_revenue_loss_millions = total_revenue_loss / 1000000
  )

# 5. Remote Work Impact Analysis with Fare Considerations

# Create Rush Hour Impact Analysis (proxy for remote work)
remote_work_impact <- daily_ridership %>%
  mutate(
    # Convert Date to proper date format if not done already
    Date = if(is.character(Date)) mdy(Date) else Date,
    
    # Create day_type if not already done
    day_of_week = weekdays(Date),
    day_type = if_else(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
    
    # Apply correct fare rates
    fare_rate = case_when(
      Date < as.Date("2023-08-20") ~ 2.75,
      TRUE ~ 2.90
    ),
    
    # Calculate daily revenue at pre-pandemic ridership levels
    estimated_prepandemic_ridership = `Subways: Total Estimated Ridership` / 
      `Subways: % of Comparable Pre-Pandemic Day`,
    estimated_prepandemic_revenue = estimated_prepandemic_ridership * fare_rate,
    
    # Calculate actual revenue
    actual_revenue = `Subways: Total Estimated Ridership` * fare_rate,
    
    # Calculate daily revenue loss
    daily_revenue_loss = estimated_prepandemic_revenue - actual_revenue
  ) %>%
  group_by(year, month, day_type) %>%
  summarize(
    avg_recovery_pct = mean(`Subways: % of Comparable Pre-Pandemic Day`, na.rm = TRUE),
    avg_daily_revenue_loss = mean(daily_revenue_loss, na.rm = TRUE),
    total_monthly_revenue_loss = sum(daily_revenue_loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_year = as.Date(paste(year, month, "01", sep = "-")))

# Create a wider dataset to compare weekday vs weekend
remote_work_comparison <- remote_work_impact %>%
  pivot_wider(
    id_cols = c(year, month, month_year),
    names_from = day_type,
    values_from = c(avg_recovery_pct, avg_daily_revenue_loss, total_monthly_revenue_loss),
    names_sep = "_"
  ) %>%
  mutate(
    # Calculate recovery gap between weekday and weekend
    weekday_weekend_gap = avg_recovery_pct_Weekend - avg_recovery_pct_Weekday,
    
    # Estimate remote work impact as proportion of weekday revenue loss
    # If weekend recovery is higher than weekday, attribute the difference to remote work
    remote_work_proportion = pmax(0, weekday_weekend_gap),
    
    # Calculate estimated remote work revenue impact
    remote_work_revenue_impact = remote_work_proportion * total_monthly_revenue_loss_Weekday
  )

# 6. Total Fare Revenue Loss Calculation with Updated Fare Rates

# Calculate total estimated fare revenue loss from 2020-2023
total_revenue_impact <- fare_analysis %>%
  filter(year >= 2020 & year <= 2023) %>%
  summarize(
    total_actual_ridership = sum(`Subways: Total Estimated Ridership`, na.rm = TRUE),
    total_expected_ridership = sum(estimated_2019_equivalent, na.rm = TRUE),
    total_ridership_gap = sum(ridership_gap, na.rm = TRUE),
    total_revenue_loss = sum(daily_revenue_loss, na.rm = TRUE),
    avg_recovery_percentage = mean(`Subways: % of Comparable Pre-Pandemic Day`, na.rm = TRUE)
  )

# Further breakdown by year to see annual progression
yearly_revenue_impact <- fare_analysis %>%
  filter(year >= 2020 & year <= 2023) %>%
  group_by(year) %>%
  summarize(
    total_actual_ridership = sum(`Subways: Total Estimated Ridership`, na.rm = TRUE),
    total_expected_ridership = sum(estimated_2019_equivalent, na.rm = TRUE),
    total_ridership_gap = sum(ridership_gap, na.rm = TRUE),
    total_revenue_loss = sum(daily_revenue_loss, na.rm = TRUE),
    avg_recovery_percentage = mean(`Subways: % of Comparable Pre-Pandemic Day`, na.rm = TRUE),
    avg_fare_rate = mean(fare_rate, na.rm = TRUE)
  ) %>%
  mutate(
    total_revenue_loss_millions = total_revenue_loss / 1000000,
    total_ridership_gap_millions = total_ridership_gap / 1000000
  )

# Calculate attribution to remote work (based on weekday/weekend difference)
remote_work_attribution <- remote_work_comparison %>%
  group_by(year) %>%
  summarize(
    total_remote_work_revenue_impact = sum(remote_work_revenue_impact, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(yearly_revenue_impact, by = "year") %>%
  mutate(
    remote_work_impact_millions = total_remote_work_revenue_impact / 1000000,
    remote_work_percentage = total_remote_work_revenue_impact / total_revenue_loss,
    non_remote_work_percentage = 1 - remote_work_percentage
  )

# Display the final results
total_revenue_impact %>%
  mutate(
    total_revenue_loss_billions = total_revenue_loss / 1000000000,
    total_ridership_gap_billions = total_ridership_gap / 1000000000,
    avg_recovery_percentage = scales::percent(avg_recovery_percentage)
  ) %>%
  select(
    avg_recovery_percentage,
    total_ridership_gap_billions,
    total_revenue_loss_billions
  ) %>%
  rename(
    "Average Recovery %" = avg_recovery_percentage,
    "Total Ridership Gap (Billions)" = total_ridership_gap_billions,
    "Total Revenue Loss ($ Billions)" = total_revenue_loss_billions
  )

# Display yearly breakdown
yearly_revenue_impact %>%
  select(
    year,
    avg_recovery_percentage,
    total_ridership_gap_millions,
    total_revenue_loss_millions,
    avg_fare_rate
  ) %>%
  rename(
    "Year" = year,
    "Recovery %" = avg_recovery_percentage,
    "Ridership Gap (Millions)" = total_ridership_gap_millions,
    "Revenue Loss ($ Millions)" = total_revenue_loss_millions,
    "Avg Fare Rate ($)" = avg_fare_rate
  )

# Display remote work attribution
remote_work_attribution %>%
  select(
    year, 
    remote_work_impact_millions,
    total_revenue_loss_millions,
    remote_work_percentage
  ) %>%
  rename(
    "Year" = year,
    "Remote Work Impact ($ Millions)" = remote_work_impact_millions,
    "Total Revenue Loss ($ Millions)" = total_revenue_loss_millions,
    "Remote Work Attribution %" = remote_work_percentage
  )

# Load necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(knitr)
library(kableExtra)

# Assuming fare_analysis, monthly_revenue_loss, remote_work_impact, 
# remote_work_comparison, and other data frames are already created

# Visualization for Section 4: Revenue Impact Estimation
# 4.1: Plot monthly revenue loss over time
plot_monthly_loss <- ggplot(monthly_revenue_loss, aes(x = month_year, y = total_revenue_loss_millions)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.8) +
  geom_line(aes(y = avg_recovery_pct * 100), color = "steelblue", size = 1) +
  geom_text(aes(label = sprintf("%.1f%%", avg_recovery_pct * 100), 
                y = avg_recovery_pct * 100 + 5), 
            size = 3, vjust = 0, color = "steelblue", check_overlap = TRUE) +
  scale_y_continuous(
    name = "Revenue Loss ($ Millions)",
    sec.axis = sec_axis(~./100, name = "Recovery Percentage", labels = function(x) paste0(x*100, "%"))
  ) +
  labs(
    title = "Monthly Fare Revenue Loss and Recovery Percentage",
    subtitle = "2020-2023",
    x = "Month",
    caption = "Source: MTA Daily Ridership Data"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "darkred"),
    axis.title.y.right = element_text(color = "steelblue"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# 4.2: Create a summary table of annual revenue loss
annual_revenue_table <- monthly_revenue_loss %>%
  group_by(year) %>%
  summarize(
    total_ridership = sum(total_ridership, na.rm = TRUE),
    total_ridership_gap = sum(total_ridership_gap, na.rm = TRUE),
    total_revenue_loss = sum(total_revenue_loss, na.rm = TRUE),
    avg_recovery_pct = mean(avg_recovery_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_ridership = format(round(total_ridership / 1000000, 2), nsmall = 2),
    total_ridership_gap = format(round(total_ridership_gap / 1000000, 2), nsmall = 2),
    total_revenue_loss = format(round(total_revenue_loss / 1000000, 2), nsmall = 2),
    avg_recovery_pct = paste0(round(avg_recovery_pct * 100, 1), "%")
  ) %>%
  rename(
    "Year" = year,
    "Total Ridership (M)" = total_ridership,
    "Ridership Gap (M)" = total_ridership_gap,
    "Revenue Loss ($M)" = total_revenue_loss,
    "Avg Recovery %" = avg_recovery_pct
  )

# Convert to a formatted table
annual_revenue_table_formatted <- kable(annual_revenue_table, format = "html", caption = "Annual Subway Ridership and Revenue Impact") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# Visualization for Section 5: Remote Work Impact Analysis
# 5.1: Create a comparison of weekday vs weekend recovery
plot_weekday_weekend <- ggplot(remote_work_impact, aes(x = month_year, y = avg_recovery_pct, color = day_type)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_manual(values = c("Weekday" = "darkblue", "Weekend" = "darkred")) +
  labs(
    title = "Recovery Patterns: Weekday vs. Weekend",
    subtitle = "Evidence of Remote Work Impact",
    x = "Month",
    y = "Recovery Percentage",
    color = "Day Type",
    caption = "Source: MTA Daily Ridership Data"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = percent_format()) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# 5.2: Create a bar chart showing revenue loss by day type
plot_revenue_by_daytype <- ggplot(remote_work_impact, 
                                  aes(x = month_year, y = total_monthly_revenue_loss/1000000, fill = day_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Weekday" = "darkblue", "Weekend" = "darkred")) +
  labs(
    title = "Monthly Revenue Loss by Day Type",
    x = "Month",
    y = "Revenue Loss ($ Millions)",
    fill = "Day Type",
    caption = "Source: MTA Daily Ridership Data"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# Visualization for Section 6: Total Fare Revenue Loss Calculation
# 6.1: Create a summary table of remote work attribution
remote_work_table <- remote_work_attribution %>%
  mutate(
    remote_work_percentage = round(remote_work_percentage * 100, 1),
    non_remote_work_percentage = round(non_remote_work_percentage * 100, 1),
    remote_work_impact_millions = round(remote_work_impact_millions, 2),
    total_revenue_loss_millions = round(total_revenue_loss_millions, 2),
    other_factors_millions = total_revenue_loss_millions - remote_work_impact_millions
  ) %>%
  select(
    year, 
    remote_work_impact_millions,
    other_factors_millions,
    total_revenue_loss_millions,
    remote_work_percentage,
    non_remote_work_percentage
  ) %>%
  rename(
    "Year" = year,
    "Remote Work Impact ($M)" = remote_work_impact_millions,
    "Other Factors ($M)" = other_factors_millions,
    "Total Revenue Loss ($M)" = total_revenue_loss_millions,
    "Remote Work %" = remote_work_percentage,
    "Other Factors %" = non_remote_work_percentage
  )

# Convert to a formatted table
remote_work_table_formatted <- kable(remote_work_table, format = "html", 
                                     caption = "Attribution of Revenue Loss to Remote Work vs. Other Factors") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# 6.2: Create a stacked bar chart showing attribution of revenue loss
plot_attribution <- ggplot(remote_work_attribution, 
                           aes(x = factor(year))) +
  geom_bar(aes(y = remote_work_impact_millions, fill = "Remote Work"), 
           stat = "identity", position = "stack") +
  geom_bar(aes(y = total_revenue_loss_millions - remote_work_impact_millions, 
               fill = "Other Factors"), 
           stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Remote Work" = "darkred", "Other Factors" = "steelblue")) +
  labs(
    title = "Attribution of Annual Revenue Loss",
    subtitle = "Remote Work vs. Other Factors",
    x = "Year",
    y = "Revenue Loss ($ Millions)",
    fill = "Factor",
    caption = "Source: MTA Daily Ridership Data"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# 6.3: Create a pie chart for overall attribution
overall_attribution <- remote_work_attribution %>%
  summarize(
    total_remote_work = sum(total_remote_work_revenue_impact, na.rm = TRUE),
    total_loss = sum(total_revenue_loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    remote_work_pct = total_remote_work / total_loss,
    other_factors_pct = 1 - remote_work_pct
  ) %>%
  pivot_longer(
    cols = c(remote_work_pct, other_factors_pct),
    names_to = "factor",
    values_to = "percentage"
  ) %>%
  mutate(
    factor = case_when(
      factor == "remote_work_pct" ~ "Remote Work",
      factor == "other_factors_pct" ~ "Other Factors"
    ),
    label = paste0(factor, "\n", round(percentage * 100, 1), "%")
  )

plot_overall_attribution <- ggplot(overall_attribution, aes(x = "", y = percentage, fill = factor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Remote Work" = "darkred", "Other Factors" = "steelblue")) +
  labs(
    title = "Overall Attribution of Revenue Loss (2020-2023)",
    fill = "Factor",
    caption = "Source: MTA Daily Ridership Data"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

# Print all visualizations and tables
print(plot_monthly_loss)
annual_revenue_table_formatted
print(plot_weekday_weekend)
print(plot_revenue_by_daytype)
remote_work_table_formatted
print(plot_attribution)
print(plot_overall_attribution)

# Function to download MTA Subway Hourly Ridership data
download_mta_ridership_data <- function() {
  # Create directory if it doesn't exist
  dir_name <- file.path("data", "finalproject")
  dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  # Define output file paths
  output_file <- file.path(dir_name, "mta_subway_hourly_ridership.csv")
  
  # Load required packages
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  # Get existing data if available
  if(file.exists(output_file)) {
    cat("Found existing data file. Checking for additional data sources...\n")
    existing_data <- TRUE
  } else {
    existing_data <- FALSE
  }
  
  # API endpoints for MTA Subway Hourly Ridership data - try multiple sources
  # The original endpoint you were using
  ENDPOINTS <- c(
    "https://data.ny.gov/resource/wujg-7c2s.json",  # 2020-2024 dataset
  )
  
  # Use a file connection to write data incrementally
  temp_file <- file.path(dir_name, "mta_ridership_additional.csv")
  
  # Track if we found any additional data
  found_additional_data <- FALSE
  
  # Try each endpoint
  for(endpoint_idx in 1:length(ENDPOINTS)) {
    ENDPOINT <- ENDPOINTS[endpoint_idx]
    cat("Trying endpoint:", ENDPOINT, "\n")
    
    # Reduce batch size to prevent timeouts
    BATCH_SIZE <- 100000
    OFFSET <- 0
    END_OF_EXPORT <- FALSE
    first_batch <- FALSE
    
    # Download data in batches with retry logic
    while(!END_OF_EXPORT) {
      cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "from endpoint", endpoint_idx, "\n")
      
      # Add retry logic
      max_retries <- 3
      retry_count <- 0
      success <- FALSE
      
      while(!success && retry_count < max_retries) {
        tryCatch({
          req <- request(ENDPOINT) |>
            req_url_query(`$limit` = BATCH_SIZE,
                          `$offset` = OFFSET) |>
            req_timeout(300)  # Set a reasonable timeout (5 minutes)
          
          resp <- req_perform(req)
          
          # Set flatten=TRUE to handle nested structures
          batch_data <- fromJSON(resp_body_string(resp), flatten = TRUE)
          
          # If we got here, the request was successful
          success <- TRUE
          
          # Check if any data was returned
          if(NROW(batch_data) > 0) {
            # Convert any list columns to character
            for(col in names(batch_data)) {
              if(is.list(batch_data[[col]])) {
                batch_data[[col]] <- sapply(batch_data[[col]], 
                                            function(x) if(length(x) == 0) NA else paste(x, collapse=","))
              }
            }
            
            # Append to CSV directly instead of keeping in memory
            if(!first_batch) {
              write_csv(batch_data, temp_file)
              first_batch <- TRUE
              found_additional_data <- TRUE
            } else {
              write_csv(batch_data, temp_file, append = TRUE)
            }
            
            # Force memory cleanup
            rm(batch_data)
            gc()
          }
          
          # Check if we've reached the end for this endpoint
          if(NROW(batch_data) < BATCH_SIZE) {
            END_OF_EXPORT <- TRUE
            cat("End of Data Export Reached for endpoint", endpoint_idx, "\n")
          } else {
            OFFSET <- OFFSET + BATCH_SIZE
          }
        }, error = function(e) {
          retry_count <- retry_count + 1
          cat("Error on attempt", retry_count, "for endpoint", endpoint_idx, ":", conditionMessage(e), "\n")
          
          # If memory limit error, try to free memory
          if(grepl("memory limit", conditionMessage(e))) {
            gc()  # Force garbage collection
            cat("Memory limit reached. Attempting to free memory...\n")
          }
          
          # If we get a 404 or other API error, end this endpoint
          if(grepl("404", conditionMessage(e)) || 
             grepl("403", conditionMessage(e)) || 
             grepl("401", conditionMessage(e))) {
            cat("API error. Moving to next endpoint.\n")
            END_OF_EXPORT <- TRUE
            success <- TRUE  # Mark as success to break retry loop
          } else {
            cat("Retrying in 5 seconds...\n")
            Sys.sleep(5)
          }
        })
      }
      
      # Check if all retries failed
      if(!success) {
        cat("Failed after", max_retries, "attempts for endpoint", endpoint_idx, ". Moving to next endpoint.\n")
        break
      }
    }
  }
  
  # Check if we found additional data
  if(found_additional_data) {
    cat("Found additional data. Merging with existing data...\n")
    
    # If we already have existing data, merge the files
    if(existing_data) {
      # Load existing and new data
      cat("Reading existing data...\n")
      existing_df <- read_csv(output_file, show_col_types = FALSE)
      cat("Reading additional data...\n")
      additional_df <- read_csv(temp_file, show_col_types = FALSE)
      
      # Write merged data
      cat("Writing merged dataset...\n")
      write_csv(all_df, file.path(dir_name, "mta_subway_hourly_ridership_complete.csv"))
      
      # Clean up
      rm(existing_df, additional_df, all_df)
      gc()
      
      # Return a sample
      return(read_csv(file.path(dir_name, "mta_subway_hourly_ridership_complete.csv"), 
                      n_max = 1000, show_col_types = FALSE))
    } else {
      # Just rename the temp file to the output file
      file.rename(temp_file, output_file)
      cat("Data export complete and saved to", output_file, "\n")
      
      # Return a sample
      return(read_csv(output_file, n_max = 1000, show_col_types = FALSE))
    }
  } else {
    cat("No additional data found from any endpoint.\n")
    if(existing_data) {
      cat("Using existing data file:", output_file, "\n")
      return(read_csv(output_file, n_max = 1000, show_col_types = FALSE))
    } else {
      cat("No data was retrieved. Check your connection or API endpoint.\n")
      return(NULL)
    }
  }
}

# Call the function to download the data
mta_data <- download_mta_ridership_data()
