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

# Function to download MTA Subway Hourly Ridership data in batches and save to separate files
download_mta_ridership_data <- function() {
  # Create directory if it doesn't exist
  dir_name <- file.path("data", "finalproject", "mta_batches")
  dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  # Load required packages
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  # Check for existing metadata file
  metadata_file <- file.path("data", "finalproject", "mta_ridership_metadata.csv")
  existing_metadata <- NULL
  last_offset <- 0
  last_batch <- 0
  
  if(file.exists(metadata_file)) {
    existing_metadata <- read_csv(metadata_file, show_col_types = FALSE)
    if(nrow(existing_metadata) > 0) {
      last_batch <- max(existing_metadata$batch_number)
      last_offset <- max(existing_metadata$offset) + max(existing_metadata$rows[existing_metadata$offset == max(existing_metadata$offset)])
      cat("Found existing metadata with", nrow(existing_metadata), "batches.\n")
      cat("Will resume from batch", last_batch + 1, "with offset", last_offset, "\n")
    }
  }
  
  # Initialize metadata if it doesn't exist
  if(is.null(existing_metadata)) {
    metadata <- data.frame(batch_number = integer(),
                           offset = integer(),
                           rows = integer(),
                           filename = character(),
                           stringsAsFactors = FALSE)
  } else {
    metadata <- existing_metadata
  }
  
  # API endpoint for MTA Subway Hourly Ridership data
  ENDPOINT <- "https://data.ny.gov/resource/wujg-7c2s.json"
  
  # Define the specific columns we want to retrieve
  COLUMNS <- c("transit_timestamp", "transit_mode", "station_complex_id", 
               "station_complex", "borough", "payment_method", "ridership")
  
  # Create a $select query parameter to only get the columns we need
  select_query <- paste(COLUMNS, collapse=",")
  
  # Batch size and starting offset
  BATCH_SIZE <- 50000  # Reduced to improve reliability
  OFFSET <- last_offset
  BATCH_COUNT <- last_batch
  END_OF_EXPORT <- FALSE
  
  # Download data in batches with retry logic
  while(!END_OF_EXPORT) {
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    # Add retry logic
    max_retries <- 3
    retry_count <- 0
    success <- FALSE
    
    while(!success && retry_count < max_retries) {
      tryCatch({
        req <- request(ENDPOINT) |>
          req_url_query(`$limit` = BATCH_SIZE,
                        `$offset` = OFFSET,
                        `$select` = select_query) |>
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
          
          # Increment batch count
          BATCH_COUNT <- BATCH_COUNT + 1
          
          # Define batch filename
          batch_filename <- sprintf("mta_ridership_batch_%04d.csv", BATCH_COUNT)
          batch_filepath <- file.path(dir_name, batch_filename)
          
          # Write batch to individual CSV file
          write_csv(batch_data, batch_filepath)
          
          # Update metadata
          new_row <- data.frame(batch_number = BATCH_COUNT,
                                offset = OFFSET,
                                rows = NROW(batch_data),
                                filename = batch_filename,
                                stringsAsFactors = FALSE)
          metadata <- rbind(metadata, new_row)
          
          # Write updated metadata
          write_csv(metadata, metadata_file)
          
          cat("Saved batch", BATCH_COUNT, "with", NROW(batch_data), "rows to", batch_filepath, "\n")
        }
        
        # Check if we've reached the end
        if(NROW(batch_data) < BATCH_SIZE) {
          END_OF_EXPORT <- TRUE
          cat("End of Data Export Reached\n")
        } else {
          OFFSET <- OFFSET + BATCH_SIZE
        }
      }, error = function(e) {
        retry_count <- retry_count + 1
        cat("Error on attempt", retry_count, ":", conditionMessage(e), "\n")
        cat("Retrying in 5 seconds...\n")
        Sys.sleep(5)
      })
    }
    
    # Check if all retries failed
    if(!success) {
      cat("Failed after", max_retries, "attempts. Saving metadata and exiting.\n")
      write_csv(metadata, metadata_file)
      break
    }
  }
  
  # Create convenience function for reading all batches
  read_all_batches <- function() {
    if(file.exists(metadata_file)) {
      metadata <- read_csv(metadata_file, show_col_types = FALSE)
      all_data <- data.frame()
      
      for(i in 1:nrow(metadata)) {
        cat("Reading batch", i, "of", nrow(metadata), "\n")
        batch_file <- file.path(dir_name, metadata$filename[i])
        batch_data <- read_csv(batch_file, show_col_types = FALSE)
        
        if(NROW(all_data) == 0) {
          all_data <- batch_data
        } else {
          all_data <- rbind(all_data, batch_data)
        }
      }
      
      return(all_data)
    } else {
      cat("No metadata file found. Cannot read batches.\n")
      return(NULL)
    }
  }
  
  # Save the read_all_batches function as an RDS file for later use
  saveRDS(read_all_batches, file.path("data", "finalproject", "read_all_batches.rds"))
  
  # Return metadata
  cat("Download complete:", BATCH_COUNT, "batches saved.\n")
  cat("Use readRDS('data/finalproject/read_all_batches.rds')() to read all batches.\n")
  return(metadata)
}

# Call the function to download the data
mta_metadata <- download_mta_ridership_data()

####################
library(dplyr)
library(lubridate)
library(readr)

# Function to process a batch file and categorize by time slot
process_batch_by_time <- function(batch_file) {
  batch_data <- read_csv(batch_file, show_col_types = FALSE)
  
  # Convert the timestamp to a datetime
  batch_data <- batch_data %>%
    mutate(transit_datetime = as_datetime(transit_timestamp),
           hour = hour(transit_datetime),
           # Create time slot category
           time_slot = case_when(
             hour >= 6 & hour < 10 ~ "Morning Rush (6AM-10AM)",
             hour >= 10 & hour < 15 ~ "Midday (10AM-3PM)",
             hour >= 15 & hour < 19 ~ "Evening Rush (3PM-7PM)",
             TRUE ~ "Off-Hours (7PM-6AM)"
           ),
           # Also add a day type for weekday/weekend analysis
           is_weekday = if_else(wday(transit_datetime) %in% 2:6, "Weekday", "Weekend"),
           # Extract date for time series analysis
           date = as_date(transit_datetime)
    )
  
  return(batch_data)
}

# Function to summarize ridership by time slot across all batches
analyze_time_slots <- function() {
  # Read the metadata
  metadata <- read_csv("data/finalproject/mta_ridership_metadata.csv", show_col_types = FALSE)
  
  # Initialize results dataframe
  results <- data.frame()
  
  # Process each batch
  for(i in 1:nrow(metadata)) {
    batch_file <- file.path("data/finalproject/mta_batches", metadata$filename[i])
    
    # Process this batch
    batch_result <- process_batch_by_time(batch_file) %>%
      group_by(date, time_slot, is_weekday) %>%
      summarize(total_ridership = sum(ridership, na.rm = TRUE),
                station_count = n_distinct(station_complex_id),
                .groups = "drop")
    
    # Append to results
    results <- bind_rows(results, batch_result)
    
    # Print progress
    if(i %% 100 == 0 || i == nrow(metadata)) {
      cat("Processed", i, "of", nrow(metadata), "batches\n")
    }
  }
  
  # Aggregate by date and time slot
  final_results <- results %>%
    group_by(date, time_slot, is_weekday) %>%
    summarize(total_ridership = sum(total_ridership),
              unique_stations = sum(station_count),
              .groups = "drop")
  
  return(final_results)
}

# Run the analysis
time_slot_analysis <- analyze_time_slots()

# Save the results
write_csv(time_slot_analysis, "data/finalproject/time_slot_analysis.csv")


# Load the saved analysis if it's not already in memory
time_slot_analysis <- read_csv("data/finalproject/time_slot_analysis.csv", show_col_types = FALSE)

# Get a quick glimpse of the data structure
glimpse(time_slot_analysis)
glimpse(daily_ridership)


# Load required libraries
library(knitr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# 1. RIDERSHIP ANALYSIS: How has total MTA ridership changed from 2019 to 2023?
# Create year and month columns for easier aggregation
daily_ridership$year_month <- format(daily_ridership$Date, "%Y-%m")

# Calculate yearly averages for subway only
yearly_subway_ridership <- daily_ridership %>%
  group_by(year) %>%
  summarize(
    `Avg Daily Ridership` = mean(`Subways: Total Estimated Ridership`),
    `% of Pre-Pandemic Level` = mean(`Subways: % of Comparable Pre-Pandemic Day`) * 100
  ) %>%
  filter(year <= 2023) %>%  # Filter to years through 2023
  as.data.frame()

# Format for better readability
yearly_subway_ridership$`Avg Daily Ridership` <- round(yearly_subway_ridership$`Avg Daily Ridership`, 0)
yearly_subway_ridership$`% of Pre-Pandemic Level` <- round(yearly_subway_ridership$`% of Pre-Pandemic Level`, 1)

# Display table
print("Yearly Subway Ridership (with % of Pre-Pandemic Levels):")
kable(yearly_subway_ridership, format = "markdown")

# Visualization for yearly ridership
plot1 <- ggplot(yearly_subway_ridership, aes(x = as.factor(year))) +
  geom_col(aes(y = `Avg Daily Ridership`/1000000), fill = "steelblue") +
  geom_line(aes(y = `% of Pre-Pandemic Level`/20, group = 1), color = "red", size = 1.5) +
  geom_point(aes(y = `% of Pre-Pandemic Level`/20), color = "red", size = 3) +
  scale_y_continuous(
    name = "Average Daily Ridership (Millions)",
    sec.axis = sec_axis(~.*20, name = "% of Pre-Pandemic Level")
  ) +
  labs(
    title = "NYC Subway Ridership Recovery by Year",
    subtitle = "Average daily ridership and percentage of pre-pandemic levels",
    x = "Year",
    caption = "Source: MTA Ridership Data"
  ) +
  theme_minimal()

print(plot1)

# 2. Analyze weekday vs weekend patterns for subway only
weekday_weekend_analysis <- daily_ridership %>%
  group_by(year, is_weekend) %>%
  summarize(
    `Avg Daily Ridership` = mean(`Subways: Total Estimated Ridership`),
    `% of Pre-Pandemic Level` = mean(`Subways: % of Comparable Pre-Pandemic Day`) * 100
  ) %>%
  filter(year <= 2023) %>%  # Filter to years through 2023
  as.data.frame()

# Format for better readability
weekday_weekend_analysis$`Avg Daily Ridership` <- round(weekday_weekend_analysis$`Avg Daily Ridership`, 0)
weekday_weekend_analysis$`% of Pre-Pandemic Level` <- round(weekday_weekend_analysis$`% of Pre-Pandemic Level`, 1)
weekday_weekend_analysis$is_weekend <- ifelse(weekday_weekend_analysis$is_weekend, "Weekend", "Weekday")

print("\nWeekday vs Weekend Subway Ridership Recovery:")
kable(weekday_weekend_analysis, format = "markdown")

# Visualization for weekday vs weekend recovery
plot2 <- ggplot(weekday_weekend_analysis, aes(x = as.factor(year), y = `% of Pre-Pandemic Level`, 
                                              fill = is_weekend, group = is_weekend)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(`% of Pre-Pandemic Level`, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Weekday vs Weekend Subway Ridership Recovery",
    subtitle = "Percentage of pre-pandemic levels by year",
    x = "Year",
    y = "% of Pre-Pandemic Ridership",
    fill = "Day Type",
    caption = "Source: MTA Ridership Data"
  ) +
  scale_fill_manual(values = c("Weekday" = "steelblue", "Weekend" = "orange")) +
  theme_minimal()

print(plot2)

# 3. REVENUE IMPACT: Calculate estimated fare revenue loss for subway only
# Create helper function to determine fare for each date
get_fare <- function(date) {
  if (date >= as.Date("2023-08-20")) {
    return(2.90)
  } else {
    return(2.75)
  }

# 3. REVENUE IMPACT: Calculate estimated fare revenue loss for subway only
# Create helper function to determine fare for each date
get_fare <- function(date) {
  if (date >= as.Date("2023-08-20")) {
    return(2.90)
  } else {
    return(2.75)
  }
}

# Apply fare logic to each row
daily_ridership$fare <- sapply(daily_ridership$Date, get_fare)

# Reconstruct pre-pandemic ridership using the percentage column
daily_ridership$pre_pandemic_subway_ridership <- 
  daily_ridership$`Subways: Total Estimated Ridership` / 
  daily_ridership$`Subways: % of Comparable Pre-Pandemic Day`

# Calculate actual and counterfactual revenue for subway only
daily_ridership$actual_revenue <- daily_ridership$`Subways: Total Estimated Ridership` * daily_ridership$fare
daily_ridership$counterfactual_revenue <- daily_ridership$pre_pandemic_subway_ridership * daily_ridership$fare
daily_ridership$revenue_loss <- daily_ridership$counterfactual_revenue - daily_ridership$actual_revenue

# Aggregate by year
revenue_summary <- daily_ridership %>%
  filter(year <= 2023) %>%  # Filter to years through 2023
  group_by(year) %>%
  summarize(
    `Actual Revenue (Million $)` = sum(actual_revenue) / 1000000,
    `Expected Pre-Pandemic Revenue (Million $)` = sum(counterfactual_revenue) / 1000000,
    `Revenue Loss (Million $)` = sum(revenue_loss) / 1000000,
    `Daily Avg Loss (Million $)` = mean(revenue_loss) / 1000000
  ) %>%
  as.data.frame()

# Format for better readability
revenue_summary$`Actual Revenue (Million $)` <- round(revenue_summary$`Actual Revenue (Million $)`, 1)
revenue_summary$`Expected Pre-Pandemic Revenue (Million $)` <- round(revenue_summary$`Expected Pre-Pandemic Revenue (Million $)`, 1)
revenue_summary$`Revenue Loss (Million $)` <- round(revenue_summary$`Revenue Loss (Million $)`, 1)
revenue_summary$`Daily Avg Loss (Million $)` <- round(revenue_summary$`Daily Avg Loss (Million $)`, 2)

print("\nEstimated Yearly Subway Fare Revenue (in millions):")
kable(revenue_summary, format = "markdown")

# Visualization for revenue impact
# Using tidyr instead of reshape2
library(tidyr)

# Create a long format dataframe for plotting
revenue_long <- revenue_summary %>%
  select(year, `Actual Revenue (Million $)`, `Revenue Loss (Million $)`) %>%
  pivot_longer(cols = c(`Actual Revenue (Million $)`, `Revenue Loss (Million $)`),
               names_to = "Revenue_Type", 
               values_to = "Amount")

plot3 <- ggplot(revenue_long, aes(x = as.factor(year), y = Amount, fill = Revenue_Type)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(label = paste0("$", Amount)), position = position_stack(vjust = 0.5), color = "white") +
  labs(
    title = "MTA Subway Revenue Loss Due to Ridership Decline",
    subtitle = "Actual revenue vs. estimated loss compared to pre-pandemic levels",
    x = "Year",
    y = "Revenue (Million $)",
    fill = "Revenue Type",
    caption = "Source: MTA Ridership Data, Fare: $2.75 (2015-Aug 2023), $2.90 (Aug 2023-present)"
  ) +
  scale_fill_manual(values = c("Actual Revenue (Million $)" = "steelblue", 
                               "Revenue Loss (Million $)" = "firebrick"),
                    labels = c("Actual Revenue", "Revenue Loss")) +
  theme_minimal()

print(plot3)


# 4. REMOTE WORK INDICATORS: Analyze time slot patterns
# First, calculate yearly average by time slot for weekdays only
weekday_time_slots <- subset(time_slot_analysis, is_weekday == "Weekday")
weekday_time_slots$year <- format(weekday_time_slots$date, "%Y")

time_slot_yearly <- weekday_time_slots %>%
  filter(as.numeric(year) <= 2023) %>%  # Filter to years through 2023
  group_by(year, time_slot) %>%
  summarize(avg_ridership = mean(total_ridership)) %>%
  as.data.frame()

# Create pivot table for easier comparison using tidyr instead of reshape2
time_slot_wide <- time_slot_yearly %>%
  pivot_wider(names_from = time_slot, values_from = avg_ridership)

# Calculate ratios (key indicators of remote work impact)
time_slot_wide$`Morning Rush / Midday Ratio` <- 
  time_slot_wide$`Morning Rush (6AM-10AM)` / time_slot_wide$`Midday (10AM-3PM)`
time_slot_wide$`Evening Rush / Midday Ratio` <- 
  time_slot_wide$`Evening Rush (3PM-7PM)` / time_slot_wide$`Midday (10AM-3PM)`

# Round for display
time_slot_wide$`Morning Rush (6AM-10AM)` <- round(time_slot_wide$`Morning Rush (6AM-10AM)`, 0)
time_slot_wide$`Midday (10AM-3PM)` <- round(time_slot_wide$`Midday (10AM-3PM)`, 0)
time_slot_wide$`Evening Rush (3PM-7PM)` <- round(time_slot_wide$`Evening Rush (3PM-7PM)`, 0)
time_slot_wide$`Off-Hours (7PM-6AM)` <- round(time_slot_wide$`Off-Hours (7PM-6AM)`, 0)
time_slot_wide$`Morning Rush / Midday Ratio` <- round(time_slot_wide$`Morning Rush / Midday Ratio`, 2)
time_slot_wide$`Evening Rush / Midday Ratio` <- round(time_slot_wide$`Evening Rush / Midday Ratio`, 2)

print("\nWeekday Time Slot Analysis (Remote Work Indicator):")
kable(time_slot_wide, format = "markdown")

# Visualization for time slot trends (remote work indicator)
time_slot_long <- time_slot_yearly %>%
  filter(time_slot %in% c("Morning Rush (6AM-10AM)", "Midday (10AM-3PM)", "Evening Rush (3PM-7PM)"))

plot4 <- ggplot(time_slot_long, aes(x = year, y = avg_ridership/1000, color = time_slot, group = time_slot)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Changing Time-of-Day Ridership Patterns",
    subtitle = "Decreasing peak/off-peak ratios indicate remote work impact",
    x = "Year",
    y = "Average Ridership (Thousands)",
    color = "Time Slot",
    caption = "Source: MTA Ridership Data"
  ) +
  scale_color_manual(values = c("Morning Rush (6AM-10AM)" = "firebrick", 
                                "Midday (10AM-3PM)" = "steelblue",
                                "Evening Rush (3PM-7PM)" = "darkgreen")) +
  theme_minimal()

print(plot4)


# 6 Calculate the estimated impact specifically attributable to remote work
# Compare weekday vs weekend recovery in 2023
remote_work_impact <- daily_ridership %>%
  filter(year == 2023) %>%
  group_by(is_weekend) %>%
  summarize(`% of Pre-Pandemic Level` = mean(`Subways: % of Comparable Pre-Pandemic Day`) * 100) %>%
  as.data.frame()

# Calculate the gap between weekday and weekend recovery
weekend_recovery <- remote_work_impact$`% of Pre-Pandemic Level`[remote_work_impact$is_weekend == TRUE]
weekday_recovery <- remote_work_impact$`% of Pre-Pandemic Level`[remote_work_impact$is_weekend == FALSE]
recovery_gap <- weekend_recovery - weekday_recovery

# Calculate the impact
# If weekdays were recovering at same rate as weekends, what would the additional ridership be?
avg_weekday_ridership_2023 <- mean(daily_ridership$`Subways: Total Estimated Ridership`[
  daily_ridership$year == 2023 & daily_ridership$is_weekend == FALSE])

avg_pre_pandemic_weekday <- avg_weekday_ridership_2023 / (weekday_recovery/100)
potential_weekday_ridership <- avg_pre_pandemic_weekday * (weekend_recovery/100)
riders_lost_to_remote_work <- potential_weekday_ridership - avg_weekday_ridership_2023

# Calculate the revenue impact
avg_fare_2023 <- mean(daily_ridership$fare[daily_ridership$year == 2023])
daily_revenue_impact <- riders_lost_to_remote_work * avg_fare_2023
annual_revenue_impact <- daily_revenue_impact * 250  # Assuming 250 weekdays per year

# Create summary table
remote_work_summary <- data.frame(
  Metric = c("Weekday Recovery Rate (%)", "Weekend Recovery Rate (%)", 
             "Recovery Gap (percentage points)", 
             "Estimated Daily Riders Lost to Remote Work",
             "Estimated Annual Revenue Impact from Remote Work ($M)"),
  Value = c(round(weekday_recovery, 1), round(weekend_recovery, 1), 
            round(recovery_gap, 1), 
            round(riders_lost_to_remote_work, 0),
            round(annual_revenue_impact/1000000, 1))
)

print("\nEstimated Impact Attributable to Remote Work (2023):")
kable(remote_work_summary, format = "markdown")

# Visualization for remote work impact
# Create a pie chart showing the breakdown of ridership loss
# Assume total loss = 100% - weekday recovery rate
total_loss_percent <- 100 - weekday_recovery
remote_work_percent <- (recovery_gap / total_loss_percent) * 100
other_factors_percent <- 100 - remote_work_percent

impact_data <- data.frame(
  Factor = c("Remote Work", "Other Factors (Tourism, Health Concerns, etc.)"),
  Percentage = c(remote_work_percent, other_factors_percent)
)

plot6 <- ggplot(impact_data, aes(x = "", y = Percentage, fill = Factor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Estimated Factors Contributing to Subway Ridership Decline (2023)",
    subtitle = paste0("Remote work accounts for approximately ", 
                      round(remote_work_percent), "% of the total ridership decline"),
    fill = "Factor",
    caption = "Source: MTA Ridership Data, Analysis based on weekday/weekend recovery gap"
  ) +
  theme_void() +
  scale_fill_manual(values = c("Remote Work" = "firebrick", 
                               "Other Factors (Tourism, Health Concerns, etc.)" = "steelblue"))

print(plot6)