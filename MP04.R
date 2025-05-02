#Task-1
# Load required libraries
if (!require("utils")) {
  install.packages("utils")
  library(utils)
}

# Create data directory if it doesn't exist
data_dir <- "data/mp04"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Created directory: ", data_dir)
} else {
  message("Directory already exists: ", data_dir)
}

# Define the base URL and file details
# We'll start with the highest resolution (500k) and fall back to coarser if needed
base_url <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/"

# The possible resolutions, from most detailed to least
resolutions <- c("500k", "5m", "20m")
resolution_index <- 1  # Start with the highest resolution

# Check each resolution until one works
success <- FALSE
while (!success && resolution_index <= length(resolutions)) {
  current_resolution <- resolutions[resolution_index]
  
  # Current file to download
  filename <- paste0("cb_2024_us_county_", current_resolution, ".zip")
  local_file <- file.path(data_dir, filename)
  url <- paste0(base_url, filename)
  
  # Check if file already exists locally
  if (file.exists(local_file)) {
    message("File already exists: ", local_file)
    success <- TRUE
  } else {
    # Try to download the file
    message("Attempting to download file from: ", url)
    
    download_result <- tryCatch({
      download.file(url, local_file, mode = "wb")
      TRUE
    }, error = function(e) {
      message("Download failed: ", e$message)
      FALSE
    })
    
    if (download_result) {
      message("Download successful: ", local_file)
      success <- TRUE
      
      # Unzip the file
      unzip(local_file, exdir = file.path(data_dir, paste0("county_", current_resolution)))
      message("File extracted to: ", file.path(data_dir, paste0("county_", current_resolution)))
    } else {
      # Try the next resolution
      resolution_index <- resolution_index + 1
      if (resolution_index <= length(resolutions)) {
        message("Trying lower resolution: ", resolutions[resolution_index])
      } else {
        message("All resolution downloads failed.")
      }
    }
  }
}


# Display summary
if (success) {
  message("\nSummary:")
  message("- Downloaded US County shapefile at ", current_resolution, " resolution")
  message("- Files stored in: ", data_dir)
  message("- Extracted files in: ", file.path(data_dir, paste0("county_", current_resolution)))
}


#Task-2: 2024 Election Data Processing
library(dplyr)
library(stringr)
library(httr2)
library(rvest)

# Function to fetch election data from Wikipedia
get_election_results <- function(state) {
  # Special case for Alaska
  if(state == "Alaska") {
    url <- "https://en.wikipedia.org/wiki/2024_United_States_presidential_election_in_Alaska"
  } else {
    # Format state name for URL
    state_formatted <- str_replace_all(state, "\\s", "_")
    url <- paste0("https://en.wikipedia.org/wiki/2024_United_States_presidential_election_in_", state_formatted)
  }
  
  # Create directory for storing data
  dir_name <- file.path("data", "election2024")
  file_name <- file.path(dir_name, paste0(gsub("\\s", "_", state), ".html"))
  dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  # Download data if not cached
  if (!file.exists(file_name)) {
    tryCatch({
      RESPONSE <- req_perform(request(url))
      writeLines(resp_body_string(RESPONSE), file_name)
      cat("Downloaded data for", state, "\n")
    }, error = function(e) {
      warning(paste("Error fetching data for", state, ":", e$message))
      return(NULL)
    })
  } else {
    cat("Using cached data for", state, "\n")
  }
  
  # Exit if file doesn't exist
  if (!file.exists(file_name)) return(NULL)
  
  # Parse HTML
  page <- tryCatch(read_html(file_name), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  # Extract tables
  tables <- tryCatch(page |> html_elements("table.wikitable") |> 
                       html_table(na.strings = c("", "N/A", "—")), 
                     error = function(e) list())
  
  if (length(tables) == 0) return(NULL)
  
  # Find county results table
  county_table <- NULL
  
  # Look for county column names
  for (i in seq_along(tables)) {
    if (ncol(tables[[i]]) < 3) next
    
    col_names <- colnames(tables[[i]])
    if (is.null(col_names) || any(is.na(col_names))) next
    
    # Look for county identifiers in column names
    if (any(str_detect(col_names, regex("County|Parish|Borough|Census Area|Municipality", ignore_case = TRUE)))) {
      county_table <- tables[[i]]
      break
    }
  }
  
  # Check for county values in first column
  if (is.null(county_table)) {
    for (i in seq_along(tables)) {
      if (ncol(tables[[i]]) < 3 || nrow(tables[[i]]) == 0 || is.null(tables[[i]][[1]])) next
      
      first_col <- tables[[i]][[1]]
      first_col_clean <- first_col[!is.na(first_col)]
      
      if (length(first_col_clean) > 0 && 
          any(str_detect(as.character(first_col_clean), 
                         regex("County|Parish|Borough|Census Area", ignore_case = TRUE)))) {
        county_table <- tables[[i]]
        break
      }
    }
  }
  
  # Look for candidate names
  if (is.null(county_table)) {
    for (i in seq_along(tables)) {
      if (ncol(tables[[i]]) < 3) next
      
      # Check column names
      col_names <- colnames(tables[[i]])
      if (!is.null(col_names) && !any(is.na(col_names)) &&
          any(str_detect(col_names, regex("Trump|Harris|Republican|Democrat", ignore_case = TRUE)))) {
        county_table <- tables[[i]]
        break
      }
    }
  }
  
  # Last resort - largest table
  if (is.null(county_table) && length(tables) > 0) {
    valid_tables <- tables[sapply(tables, function(t) ncol(t) >= 3 && nrow(t) >= 3)]
    if (length(valid_tables) > 0) {
      county_table <- valid_tables[[which.max(sapply(valid_tables, nrow))]]
    }
  }
  
  if (is.null(county_table)) return(NULL)
  
  # Format table
  result <- tryCatch({
    # Find county column
    county_col <- which(str_detect(colnames(county_table), 
                                   regex("County|Parish|Borough|Census Area|Municipality|District", ignore_case = TRUE)))
    county_col <- if(length(county_col) > 0) county_col[1] else 1
    
    result <- county_table
    names(result)[county_col] <- "County"
    result$State <- state
    
    return(result)
  }, error = function(e) NULL)
  
  return(result)
}

# Function to standardize election data
standardize_election_data <- function(df, state) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Extract numeric values from string
  extract_numeric <- function(values) {
    if (is.null(values)) return(rep(NA, nrow(df)))
    chars <- as.character(values)
    chars <- gsub(",|%|\\s", "", chars)
    suppressWarnings(as.numeric(chars))
  }
  
  # Find candidate columns
  find_candidate_columns <- function(candidate, df_names) {
    cols <- which(str_detect(df_names, regex(candidate, ignore_case = TRUE)))
    if (length(cols) >= 2) {
      vote_col <- NULL
      pct_col <- NULL
      
      for (col in cols) {
        col_name <- df_names[col]
        if (str_detect(col_name, regex("%|percent", ignore_case = TRUE))) {
          pct_col <- col
        } else if (str_detect(col_name, regex("votes|#", ignore_case = TRUE))) {
          vote_col <- col
        }
      }
      
      if (is.null(vote_col) && length(cols) >= 1) vote_col <- cols[1]
      if (is.null(pct_col) && length(cols) >= 2) pct_col <- cols[2]
      
      return(list(vote_col = vote_col, pct_col = pct_col))
    } else if (length(cols) == 1) {
      return(list(vote_col = cols[1], pct_col = NULL))
    } else {
      return(list(vote_col = NULL, pct_col = NULL))
    }
  }
  
  # Ensure County column
  if (!"County" %in% names(df)) {
    county_col <- which(str_detect(names(df), 
                                   regex("County|Parish|Borough|Census Area|Municipality|District|City", ignore_case = TRUE)))
    if (length(county_col) > 0) {
      names(df)[county_col[1]] <- "County"
    } else {
      names(df)[1] <- "County"
    }
  }
  
  # Find candidate and total columns
  trump_cols <- find_candidate_columns("Trump|Republican", names(df))
  harris_cols <- find_candidate_columns("Harris|Democratic|Democrat", names(df))
  other_cols <- find_candidate_columns("Other|Independent|Third", names(df))
  total_col <- which(str_detect(names(df), regex("Total|Sum|Cast", ignore_case = TRUE)))
  total_col <- if (length(total_col) > 0) total_col[length(total_col)] else NULL
  
  # Create standardized dataframe
  result <- data.frame(
    County = df$County,
    State = state,
    Trump_Votes = if (!is.null(trump_cols$vote_col)) extract_numeric(df[[trump_cols$vote_col]]) else NA,
    Trump_Percent = if (!is.null(trump_cols$pct_col)) extract_numeric(df[[trump_cols$pct_col]]) else NA,
    Harris_Votes = if (!is.null(harris_cols$vote_col)) extract_numeric(df[[harris_cols$vote_col]]) else NA,
    Harris_Percent = if (!is.null(harris_cols$pct_col)) extract_numeric(df[[harris_cols$pct_col]]) else NA,
    Other_Votes = if (!is.null(other_cols$vote_col)) extract_numeric(df[[other_cols$vote_col]]) else NA,
    Other_Percent = if (!is.null(other_cols$pct_col)) extract_numeric(df[[other_cols$pct_col]]) else NA,
    Total_Votes = if (!is.null(total_col)) extract_numeric(df[[total_col]]) else 
      rowSums(cbind(
        if (!is.null(trump_cols$vote_col)) extract_numeric(df[[trump_cols$vote_col]]) else 0,
        if (!is.null(harris_cols$vote_col)) extract_numeric(df[[harris_cols$vote_col]]) else 0,
        if (!is.null(other_cols$vote_col)) extract_numeric(df[[other_cols$vote_col]]) else 0
      ), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Process all states
process_election_data <- function() {
  states <- state.name
  all_data <- list()
  
  for (state in states) {
    cat("Processing", state, "...\n")
    raw_data <- get_election_results(state)
    
    if (!is.null(raw_data)) {
      std_data <- standardize_election_data(raw_data, state)
      
      if (!is.null(std_data) && nrow(std_data) > 0) {
        all_data[[state]] <- std_data
      }
    }
  }
  
  # Combine all data
  combined_data <- do.call(rbind, all_data)
  
  # Clean data - remove problematic rows
  clean_data <- combined_data %>%
    filter(
      !is.na(Trump_Votes) & !is.na(Harris_Votes) & 
        !str_detect(County, regex("^County$|^County\\[|^Total", ignore_case = TRUE))
    ) %>%
    mutate(County = gsub("\\[\\d+\\]", "", County),
           County = trimws(County))
  
  # Save results
  write.csv(clean_data, "data/election_results_2024.csv", row.names = FALSE)
  
  # Create summary by state
  state_summary <- clean_data %>%
    group_by(State) %>%
    summarize(
      Counties = n(),
      Trump_Total = sum(Trump_Votes, na.rm = TRUE),
      Harris_Total = sum(Harris_Votes, na.rm = TRUE),
      Other_Total = sum(Other_Votes, na.rm = TRUE),
      Total_Votes = sum(Total_Votes, na.rm = TRUE),
      Trump_Pct = Trump_Total / Total_Votes * 100,
      Harris_Pct = Harris_Total / Total_Votes * 100
    ) %>%
    arrange(desc(Total_Votes))
  
  write.csv(state_summary, "data/election_results_2024_summary.csv", row.names = FALSE)
  
  return(state_summary)
}

# Run the process and display results
election_summary <- process_election_data()
print(election_summary)


#Task-3: 2020 Election Data Processing
library(dplyr)
library(stringr)
library(httr2)
library(rvest)


# Add this at the beginning for web scraping
if (!require("rvest")) {
  install.packages("rvest")
  library(rvest)
}
if (!require("httr2")) {
  install.packages("httr2")
  library(httr2)
}

# Function to fetch 2020 election data from Wikipedia
get_2020_election_results <- function(state) {
  # Format state name for URL
  state_formatted <- str_replace_all(state, "\\s", "_")
  url <- paste0("https://en.wikipedia.org/wiki/2020_United_States_presidential_election_in_", state_formatted)
  
  # Create directory for storing data
  dir_name <- file.path("data", "election2020")
  file_name <- file.path(dir_name, paste0(gsub("\\s", "_", state), ".html"))
  dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  # Download data if not cached
  if (!file.exists(file_name)) {
    tryCatch({
      RESPONSE <- req_perform(request(url))
      writeLines(resp_body_string(RESPONSE), file_name)
      cat("Downloaded 2020 data for", state, "\n")
    }, error = function(e) {
      warning(paste("Error fetching 2020 data for", state, ":", e$message))
      return(NULL)
    })
  } else {
    cat("Using cached 2020 data for", state, "\n")
  }
  
  # Exit if file doesn't exist
  if (!file.exists(file_name)) return(NULL)
  
  # Parse HTML
  page <- tryCatch(read_html(file_name), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  # Extract tables
  tables <- tryCatch(page |> html_elements("table.wikitable") |> 
                       html_table(na.strings = c("", "N/A", "—")), 
                     error = function(e) list())
  
  if (length(tables) == 0) return(NULL)
  
  # Find county results table
  county_table <- NULL
  
  # Look for county column names
  for (i in seq_along(tables)) {
    if (ncol(tables[[i]]) < 3) next
    
    col_names <- colnames(tables[[i]])
    if (is.null(col_names) || any(is.na(col_names))) next
    
    # Look for county identifiers in column names
    if (any(str_detect(col_names, regex("County|Parish|Borough|Census Area|Municipality", ignore_case = TRUE)))) {
      county_table <- tables[[i]]
      break
    }
  }
  
  # Check for county values in first column
  if (is.null(county_table)) {
    for (i in seq_along(tables)) {
      if (ncol(tables[[i]]) < 3 || nrow(tables[[i]]) == 0 || is.null(tables[[i]][[1]])) next
      
      first_col <- tables[[i]][[1]]
      first_col_clean <- first_col[!is.na(first_col)]
      
      if (length(first_col_clean) > 0 && 
          any(str_detect(as.character(first_col_clean), 
                         regex("County|Parish|Borough|Census Area", ignore_case = TRUE)))) {
        county_table <- tables[[i]]
        break
      }
    }
  }
  
  # Look for candidate names for 2020 election (Trump vs Biden)
  if (is.null(county_table)) {
    for (i in seq_along(tables)) {
      if (ncol(tables[[i]]) < 3) next
      
      # Check column names
      col_names <- colnames(tables[[i]])
      if (!is.null(col_names) && !any(is.na(col_names)) &&
          any(str_detect(col_names, regex("Trump|Biden|Republican|Democrat", ignore_case = TRUE)))) {
        county_table <- tables[[i]]
        break
      }
      
      # Check first few rows for candidates
      if (nrow(tables[[i]]) > 2) {
        first_rows_char <- lapply(tables[[i]][1:min(5, nrow(tables[[i]])),], function(x) {
          ifelse(is.na(x), NA_character_, as.character(x))
        })
        
        found_candidates <- FALSE
        for (j in 1:length(first_rows_char)) {
          col_values <- first_rows_char[[j]]
          col_values <- col_values[!is.na(col_values)]
          
          if (length(col_values) > 0 &&
              any(str_detect(col_values, regex("Trump|Republican", ignore_case = TRUE))) && 
              any(str_detect(col_values, regex("Biden|Democratic|Democrat", ignore_case = TRUE)))) {
            county_table <- tables[[i]]
            found_candidates <- TRUE
            break
          }
        }
        if (found_candidates) break
      }
    }
  }
  
  # Last resort - largest table
  if (is.null(county_table) && length(tables) > 0) {
    valid_tables <- tables[sapply(tables, function(t) ncol(t) >= 3 && nrow(t) >= 3)]
    if (length(valid_tables) > 0) {
      county_table <- valid_tables[[which.max(sapply(valid_tables, nrow))]]
    }
  }
  
  if (is.null(county_table)) return(NULL)
  
  # Format table
  result <- tryCatch({
    # Find county column
    county_col <- which(str_detect(colnames(county_table), 
                                   regex("County|Parish|Borough|Census Area|Municipality|District", ignore_case = TRUE)))
    county_col <- if(length(county_col) > 0) county_col[1] else 1
    
    result <- county_table
    names(result)[county_col] <- "County"
    result$State <- state
    
    return(result)
  }, error = function(e) NULL)
  
  return(result)
}

# Function to standardize 2020 election data
standardize_2020_election_data <- function(df, state) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Extract numeric values from string
  extract_numeric <- function(values) {
    if (is.null(values)) return(rep(NA, nrow(df)))
    chars <- as.character(values)
    chars <- gsub(",|%|\\s", "", chars)
    suppressWarnings(as.numeric(chars))
  }
  
  # Find candidate columns - specific to 2020 election (Trump vs Biden)
  find_candidate_columns <- function(candidate, df_names) {
    cols <- which(str_detect(df_names, regex(candidate, ignore_case = TRUE)))
    if (length(cols) >= 2) {
      vote_col <- NULL
      pct_col <- NULL
      
      for (col in cols) {
        col_name <- df_names[col]
        if (str_detect(col_name, regex("%|percent", ignore_case = TRUE))) {
          pct_col <- col
        } else if (str_detect(col_name, regex("votes|#", ignore_case = TRUE))) {
          vote_col <- col
        }
      }
      
      if (is.null(vote_col) && length(cols) >= 1) vote_col <- cols[1]
      if (is.null(pct_col) && length(cols) >= 2) pct_col <- cols[2]
      
      return(list(vote_col = vote_col, pct_col = pct_col))
    } else if (length(cols) == 1) {
      return(list(vote_col = cols[1], pct_col = NULL))
    } else {
      return(list(vote_col = NULL, pct_col = NULL))
    }
  }
  
  # Ensure County column
  if (!"County" %in% names(df)) {
    county_col <- which(str_detect(names(df), 
                                   regex("County|Parish|Borough|Census Area|Municipality|District|City", ignore_case = TRUE)))
    if (length(county_col) > 0) {
      names(df)[county_col[1]] <- "County"
    } else {
      names(df)[1] <- "County"
    }
  }
  
  # Find candidate and total columns for 2020 (Trump vs Biden)
  trump_cols <- find_candidate_columns("Trump|Republican", names(df))
  biden_cols <- find_candidate_columns("Biden|Democratic|Democrat", names(df))
  other_cols <- find_candidate_columns("Other|Independent|Third|Jorgensen|Hawkins", names(df))
  total_col <- which(str_detect(names(df), regex("Total|Sum|Cast", ignore_case = TRUE)))
  total_col <- if (length(total_col) > 0) total_col[length(total_col)] else NULL
  
  # Create standardized dataframe
  result <- data.frame(
    County = df$County,
    State = state,
    Trump_Votes = if (!is.null(trump_cols$vote_col)) extract_numeric(df[[trump_cols$vote_col]]) else NA,
    Trump_Percent = if (!is.null(trump_cols$pct_col)) extract_numeric(df[[trump_cols$pct_col]]) else NA,
    Biden_Votes = if (!is.null(biden_cols$vote_col)) extract_numeric(df[[biden_cols$vote_col]]) else NA,
    Biden_Percent = if (!is.null(biden_cols$pct_col)) extract_numeric(df[[biden_cols$pct_col]]) else NA,
    Other_Votes = if (!is.null(other_cols$vote_col)) extract_numeric(df[[other_cols$vote_col]]) else NA,
    Other_Percent = if (!is.null(other_cols$pct_col)) extract_numeric(df[[other_cols$pct_col]]) else NA,
    Total_Votes = if (!is.null(total_col)) extract_numeric(df[[total_col]]) else 
      rowSums(cbind(
        if (!is.null(trump_cols$vote_col)) extract_numeric(df[[trump_cols$vote_col]]) else 0,
        if (!is.null(biden_cols$vote_col)) extract_numeric(df[[biden_cols$vote_col]]) else 0,
        if (!is.null(other_cols$vote_col)) extract_numeric(df[[other_cols$vote_col]]) else 0
      ), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Process all states for 2020 election
process_2020_election_data <- function() {
  states <- state.name
  all_data <- list()
  
  for (state in states) {
    cat("Processing 2020 data for", state, "...\n")
    raw_data <- get_2020_election_results(state)
    
    if (!is.null(raw_data)) {
      std_data <- standardize_2020_election_data(raw_data, state)
      
      if (!is.null(std_data) && nrow(std_data) > 0) {
        all_data[[state]] <- std_data
      }
    }
  }
  
  # Combine all data
  combined_data <- do.call(rbind, all_data)
  
  # Clean data - remove problematic rows
  clean_data <- combined_data %>%
    filter(
      !is.na(Trump_Votes) & !is.na(Biden_Votes) & 
        !str_detect(County, regex("^County$|^County\\[|^Total", ignore_case = TRUE))
    ) %>%
    mutate(County = gsub("\\[\\d+\\]", "", County),
           County = trimws(County))
  
  # Save results
  write.csv(clean_data, "data/election_results_2020.csv", row.names = FALSE)
  
  # Create summary by state
  state_summary <- clean_data %>%
    group_by(State) %>%
    summarize(
      Counties = n(),
      Trump_Total = sum(Trump_Votes, na.rm = TRUE),
      Biden_Total = sum(Biden_Votes, na.rm = TRUE),
      Other_Total = sum(Other_Votes, na.rm = TRUE),
      Total_Votes = sum(Total_Votes, na.rm = TRUE),
      Trump_Pct = Trump_Total / Total_Votes * 100,
      Biden_Pct = Biden_Total / Total_Votes * 100
    ) %>%
    arrange(desc(Total_Votes))
  
  write.csv(state_summary, "data/election_results_2020_summary.csv", row.names = FALSE)
  
  # Create national summary
  national_summary <- clean_data %>%
    summarize(
      Total_Counties = n(),
      Trump_Total = sum(Trump_Votes, na.rm = TRUE),
      Biden_Total = sum(Biden_Votes, na.rm = TRUE),
      Other_Total = sum(Other_Votes, na.rm = TRUE),
      Total_Votes = sum(Total_Votes, na.rm = TRUE),
      Trump_Pct = Trump_Total / Total_Votes * 100,
      Biden_Pct = Biden_Total / Total_Votes * 100
    )
  
  write.csv(national_summary, "data/election_results_2020_national.csv", row.names = FALSE)
  
  return(list(state_summary = state_summary, national_summary = national_summary))
}

# Run the process for 2020 data
election_results_2020 <- process_2020_election_data()
print(election_results_2020$state_summary)
cat("\nNational 2020 Election Results:\n")
print(election_results_2020$national_summary)


#Task-4
#Task-4: Combine County Shapes with 2020 and 2024 Election Results
library(dplyr)
library(sf)

combine_election_data <- function() {
  # Load county shapefile from Task-1
  data_dir <- "data/mp04"
  shp_dirs <- list.dirs(data_dir, recursive = FALSE)
  county_dir <- shp_dirs[grep("county", shp_dirs)]
  
  if (length(county_dir) == 0) {
    stop("County shapefile directory not found. Run Task-1 first.")
  }
  
  # Find shapefile in the directory
  shp_files <- list.files(county_dir, pattern = "\\.shp$", full.names = TRUE)
  counties_sf <- sf::st_read(shp_files[1])
  
  # Load election data from Task-2 and Task-3
  election_2020 <- read.csv("data/election_results_2020.csv")
  election_2024 <- read.csv("data/election_results_2024.csv")
  
  # Prepare county shapefile for joining
  counties_sf <- counties_sf %>%
    mutate(
      County = NAME,
      StateAbbr = STUSPS
    )
  
  # Create state abbreviation lookup for joining
  state_lookup <- data.frame(
    StateAbbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
    State = state.name
  )
  
  # Add state names to shapefile
  counties_sf <- counties_sf %>%
    left_join(state_lookup, by = "StateAbbr")
  
  # Clean county names for better joining
  counties_sf$County <- gsub(" County$| Parish$| Borough$| Census Area$| Municipality$", "", counties_sf$County)
  election_2020$County <- gsub(" County$| Parish$| Borough$| Census Area$| Municipality$", "", election_2020$County)
  election_2024$County <- gsub(" County$| Parish$| Borough$| Census Area$| Municipality$", "", election_2024$County)
  
  # Add year identifiers to election data
  election_2020$Year <- 2020
  election_2024$Year <- 2024
  
  # Create join keys
  counties_sf$join_key <- paste(counties_sf$County, counties_sf$State)
  election_2020$join_key <- paste(election_2020$County, election_2020$State)
  election_2024$join_key <- paste(election_2024$County, election_2024$State)
  
  # Join shapefile with election data
  counties_with_2020 <- counties_sf %>%
    left_join(election_2020, by = "join_key")
  
  counties_with_both <- counties_with_2020 %>%
    left_join(election_2024, by = "join_key", suffix = c("_2020", "_2024"))
  
  # Save the combined data
  saveRDS(counties_with_both, "data/mp04/combined_election_data.rds")
  
  # Save as shapefile (with simplified column names if needed)
  st_write(counties_with_both, "data/mp04/combined_counties_elections.shp", delete_layer = TRUE)
  
  return(counties_with_both)
}

# Run the function
combined_data <- combine_election_data()

#4.1 Which county or counties cast the most votes for Trump (in absolute terms) in 2024?
library(knitr)
library(dplyr)

# Load the combined data
combined_data <- readRDS("data/mp04/combined_election_data.rds")

# Find the county with most Trump votes in 2024
top_trump_counties_2024 <- combined_data %>%
  arrange(desc(Trump_Votes_2024)) %>%
  select(County.y, State.y, Trump_Votes_2024) %>% 
  na.omit() %>%
  head(5)

# Print the table
print(top_trump_counties_2024)

#4.2Which county or counties cast the most votes for Biden (as a fraction of total votes cast) in 2020?
# Find the county with highest percentage for Biden in 2020
top_biden_counties_2020 <- combined_data %>%
  mutate(Biden_Fraction = Biden_Votes / Total_Votes_2020) %>%
  arrange(desc(Biden_Fraction)) %>%
  select(County.y, State.y, Biden_Fraction, Biden_Votes, Total_Votes_2020) %>%
  na.omit() %>%
  head(5)

# Print the table
print(top_biden_counties_2020)

#4.3 Which county or counties had the largest shift towards Trump (in absolute terms) in 2024?
# Calculate the shift in percentage points towards Trump in 2024
trump_shift_counties <- combined_data %>%
  mutate(
    Trump_Pct_2020 = Trump_Votes_2020 / Total_Votes_2020 * 100,
    Trump_Pct_2024 = Trump_Votes_2024 / Total_Votes_2024 * 100,
    Trump_Shift_Pct = Trump_Pct_2024 - Trump_Pct_2020
  ) %>%
  filter(!is.na(Trump_Shift_Pct)) %>%
  arrange(desc(Trump_Shift_Pct)) %>%
  select(County.y, State.y, Trump_Pct_2020, Trump_Pct_2024, Trump_Shift_Pct) %>%
  head(5)

# Print the table
print(trump_shift_counties)


#4.4 Which state had the largest shift towards Harris (or smallest shift towards Trump) in 2024?
# Calculate state-level shifts
state_shifts <- combined_data %>%
  group_by(State.y) %>%
  summarize(
    Trump_Votes_2020 = sum(Trump_Votes_2020, na.rm = TRUE),
    Biden_Votes_2020 = sum(Biden_Votes, na.rm = TRUE),
    Total_Votes_2020 = sum(Total_Votes_2020, na.rm = TRUE),
    Trump_Votes_2024 = sum(Trump_Votes_2024, na.rm = TRUE),
    Harris_Votes_2024 = sum(Harris_Votes, na.rm = TRUE),
    Total_Votes_2024 = sum(Total_Votes_2024, na.rm = TRUE)
  ) %>%
  mutate(
    Trump_Pct_2020 = Trump_Votes_2020 / Total_Votes_2020 * 100,
    Trump_Pct_2024 = Trump_Votes_2024 / Total_Votes_2024 * 100,
    Trump_Shift = Trump_Pct_2024 - Trump_Pct_2020,
    Harris_Shift = -Trump_Shift  # Shift towards Harris is opposite of shift towards Trump
  ) %>%
  filter(!is.na(Harris_Shift) & Total_Votes_2020 > 0 & Total_Votes_2024 > 0) %>%
  arrange(desc(Harris_Shift)) %>%
  head(5)

# Print the table
print(state_shifts[, c("State.y", "Trump_Pct_2020", "Trump_Pct_2024", "Trump_Shift", "Harris_Shift")])


#4.5 What is the largest county, by area, in this data set?
# Find the largest county by area 
largest_counties <- combined_data %>%
  # Filter out rows with missing county names
  filter(!is.na(County.y) & !is.na(State.y)) %>%
  # Convert ALAND to numeric and calculate area in square kilometers
  mutate(Area_sq_km = as.numeric(ALAND) / 1000000) %>%
  # Sort by area in descending order
  arrange(desc(Area_sq_km)) %>%
  # Select relevant columns
  select(County.y, State.y, Area_sq_km, ALAND) %>%
  # Remove geometry for cleaner display
  sf::st_drop_geometry() %>%
  head(5)

# Print the table without the geometry
print(largest_counties[, c("County.y", "State.y", "Area_sq_km")])

#4.6 Which county has the highest voter density (voters per unit of area) in 2020?
# Calculate voter density with proper handling of NA values
voter_density_2020 <- combined_data %>%
  # Filter out rows with missing county names or election data
  filter(!is.na(County.y) & !is.na(State.y) & !is.na(Total_Votes_2020) & !is.na(ALAND)) %>%
  # Convert ALAND to numeric and calculate area in square kilometers
  mutate(Area_sq_km = as.numeric(ALAND) / 1000000,
         Voter_Density_2020 = Total_Votes_2020 / Area_sq_km) %>%
  # Ensure area is greater than zero to avoid division by zero
  filter(Area_sq_km > 0) %>%
  # Sort by density in descending order
  arrange(desc(Voter_Density_2020)) %>%
  # Select relevant columns
  select(County.y, State.y, Total_Votes_2020, Area_sq_km, Voter_Density_2020) %>%
  # Remove geometry for cleaner display
  sf::st_drop_geometry() %>%
  head(5)

# Print the table without the geometry
print(voter_density_2020)

#4.7 Which county had the largest increase in voter turnout in 2024?
# Calculate change in turnout from 2020 to 2024 with proper handling
turnout_change <- combined_data %>%
  # Filter out rows with missing county names or election data
  filter(!is.na(County.y) & !is.na(State.y) & 
           !is.na(Total_Votes_2020) & !is.na(Total_Votes_2024)) %>%
  # Calculate absolute and percentage change in turnout
  mutate(
    Turnout_Change = Total_Votes_2024 - Total_Votes_2020,
    Turnout_Change_Pct = (Total_Votes_2024 - Total_Votes_2020) / Total_Votes_2020 * 100
  ) %>%
  # Sort by absolute change in descending order
  arrange(desc(Turnout_Change)) %>%
  # Select relevant columns
  select(County.y, State.y, Total_Votes_2020, Total_Votes_2024, Turnout_Change, Turnout_Change_Pct) %>%
  # Remove geometry for cleaner display
  sf::st_drop_geometry() %>%
  head(5)

# Print the table
print(turnout_change)

#Task-5
#############################################################
# Reproduce NYT Election Shift Map
# Showing county-level shifts from 2020 to 2024 elections
#############################################################

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)

# Load the combined data
combined_data <- readRDS("data/mp04/combined_election_data.rds")

# 1. Calculate the shift rightwards for each county
election_shift <- combined_data %>%
  # Calculate Trump percentage in 2020 and 2024
  mutate(
    Trump_Pct_2020 = ifelse(is.na(Trump_Votes_2020) | is.na(Total_Votes_2020), NA, 
                            Trump_Votes_2020 / Total_Votes_2020 * 100),
    Trump_Pct_2024 = ifelse(is.na(Trump_Votes_2024) | is.na(Total_Votes_2024), NA, 
                            Trump_Votes_2024 / Total_Votes_2024 * 100),
    # Calculate the shift (positive = rightward shift toward Trump)
    Trump_Shift = Trump_Pct_2024 - Trump_Pct_2020,
    # Create a column to identify the direction of shift
    Shift_Direction = ifelse(Trump_Shift > 0, "Right", "Left"),
    # Scale for arrow length
    Arrow_Length = case_when(
      abs(Trump_Shift) < 1 ~ 0,           # No visible arrow for very small shifts
      abs(Trump_Shift) < 5 ~ 0.5,         # Small arrows for small shifts
      abs(Trump_Shift) < 10 ~ 1.0,        # Medium arrows for medium shifts
      TRUE ~ 1.5                          # Large arrows for large shifts
    )
  ) %>%
  # Filter out rows with NA values in shift and keep only valid geometry
  filter(!is.na(Trump_Shift) & !st_is_empty(geometry))

# Print summary of shift data
cat("Summary of county-level shifts:\n")
print(summary(election_shift$Trump_Shift))
cat("\nCounties shifting right:", sum(election_shift$Trump_Shift > 0), "\n")
cat("Counties shifting left:", sum(election_shift$Trump_Shift < 0), "\n")

# 2 & 3. Modify geometry and prepare map
# Use tigris::shift_geometry to relocate Alaska and Hawaii
shifted_data <- shift_geometry(election_shift, 
                               position = "below",  
                               preserve_area = FALSE)

# 4. Compute the centroid of each county
shifted_data <- shifted_data %>%
  mutate(
    # Calculate centroids
    centroid = st_centroid(geometry),
    # Extract coordinates 
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  )

# 5. Create the map with arrows
nyt_plot <- ggplot() +
  # Base map layer
  geom_sf(data = shifted_data, fill = "white", color = "#cccccc", size = 0.1) +
  # Add arrows - only for counties with non-zero Arrow_Length
  geom_segment(data = filter(shifted_data, Arrow_Length > 0),
               aes(x = lon, y = lat,
                   xend = lon + ifelse(Trump_Shift > 0, 1, -1) * Arrow_Length,
                   yend = lat,
                   color = Shift_Direction),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               size = 0.3, alpha = 0.8) +
  # Set colors for arrows
  scale_color_manual(values = c("Left" = "blue", "Right" = "red"),
                     name = "",
                     labels = c("Left" = "More Dem.", "Right" = "More Rep.")) +
  # Customize appearance
  theme_void() +
  labs(title = "Shift in margin from 2020 to 2024",
       caption = "By The New York Times") +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic", size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Display the plot
print(nyt_plot)


#Task-6
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sf)
library(infer)
library(patchwork)

# Load the combined data
combined_data <- readRDS("data/mp04/combined_election_data.rds")

# Calculate the shift rightwards for each county
election_shift <- combined_data %>%
  # Filter out rows with missing values in key columns
  filter(!is.na(Trump_Votes_2020) & !is.na(Trump_Votes_2024) & 
           !is.na(Total_Votes_2020) & !is.na(Total_Votes_2024)) %>%
  # Calculate percentages and shifts
  mutate(
    Trump_Pct_2020 = Trump_Votes_2020 / Total_Votes_2020 * 100,
    Trump_Pct_2024 = Trump_Votes_2024 / Total_Votes_2024 * 100,
    Trump_Shift = Trump_Pct_2024 - Trump_Pct_2020,
    # Add demographic classifications based on available data
    Hispanic_County = ifelse(grepl("over 25% Hispanic", County.y, ignore.case=TRUE) | 
                               (County.y %in% c("Starr", "Webb", "Hidalgo", "Cameron")), TRUE, FALSE),
    Urban_County = ifelse(Total_Votes_2020 > 100000, TRUE, FALSE)  # Simple proxy for urban counties
  )

# Drop geometry for faster computation
election_data <- st_drop_geometry(election_shift)

#----------------------------------------------------------------------------------
# TALKING POINT 1: The Great Republican Shift - Most counties moved right
#----------------------------------------------------------------------------------

# Count counties shifting right vs left
shift_counts <- election_data %>%
  summarize(
    Total_Counties = n(),
    Right_Shift = sum(Trump_Shift > 0, na.rm = TRUE),
    Left_Shift = sum(Trump_Shift < 0, na.rm = TRUE),
    Right_Shift_Pct = Right_Shift / Total_Counties * 100
  )

# Create a pie chart of the county shifts
pie_data <- data.frame(
  Direction = c("Shifted Right", "Shifted Left"),
  Count = c(shift_counts$Right_Shift, shift_counts$Left_Shift)
)

pie_chart <- ggplot(pie_data, aes(x = "", y = Count, fill = Direction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Shifted Right" = "red", "Shifted Left" = "blue")) +
  labs(
    title = "The Great Republican Shift",
    subtitle = paste0(round(shift_counts$Right_Shift_Pct, 1), 
                      "% of counties shifted toward Republicans in 2024"),
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
print(pie_chart)
#----------------------------------------------------------------------------------
# TALKING POINT 2: The Hispanic Realignment
#----------------------------------------------------------------------------------

# Compare shifts in Hispanic vs non-Hispanic counties
hispanic_analysis <- election_data %>%
  group_by(Hispanic_County) %>%
  summarize(
    Counties = n(),
    Avg_Shift = mean(Trump_Shift, na.rm = TRUE),
    Median_Shift = median(Trump_Shift, na.rm = TRUE)
  )

# Create a bar chart comparing Hispanic vs non-Hispanic counties
hispanic_chart <- ggplot(hispanic_analysis, 
                         aes(x = ifelse(Hispanic_County, "Hispanic Counties", "Other Counties"), 
                             y = Avg_Shift, 
                             fill = ifelse(Hispanic_County, "Hispanic", "Other"))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Hispanic" = "darkred", "Other" = "lightcoral")) +
  labs(
    title = "The Hispanic Realignment",
    subtitle = "Hispanic counties shifted dramatically toward Republicans",
    y = "Average Shift (percentage points)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12, face = "bold")
  )
print(hispanic_chart)
#----------------------------------------------------------------------------------
# TALKING POINT 3: Urban Areas Turn Right
#----------------------------------------------------------------------------------

# Compare shifts in urban vs rural counties
urban_analysis <- election_data %>%
  group_by(Urban_County) %>%
  summarize(
    Counties = n(),
    Avg_Shift = mean(Trump_Shift, na.rm = TRUE),
    Median_Shift = median(Trump_Shift, na.rm = TRUE)
  )

# Create a chart showing urban vs rural shifts
urban_chart <- ggplot(urban_analysis, 
                      aes(x = ifelse(Urban_County, "Urban Counties", "Rural Counties"), 
                          y = Avg_Shift, 
                          fill = ifelse(Urban_County, "Urban", "Rural"))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Urban" = "firebrick", "Rural" = "indianred")) +
  labs(
    title = "Cities Turn Right",
    subtitle = "Urban areas reject Democratic policies",
    y = "Average Shift (percentage points)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12, face = "bold")
  )
print(urban_chart)

# COMPUTATIONAL TEST 1: Testing if median county shift exceeds 5%
# Instead of using mu, we'll use a different approach for the median test

# Calculate observed median
observed_median <- median(test_data$shift, na.rm = TRUE)

# Create bootstrap distribution for the median
median_bootstrap <- test_data %>%
  specify(response = shift) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "median")

# Get confidence interval
median_ci <- median_bootstrap %>%
  get_confidence_interval(level = 0.95)

# Create a one-sided p-value manually by counting samples
# Null hypothesis: median shift <= 5%
# Alternative hypothesis: median shift > 5%
p_value_median <- mean(median_bootstrap$stat >= 5)

# Create a plot showing the bootstrap distribution and confidence interval
median_test_plot <- median_bootstrap %>%
  visualize() +
  shade_confidence_interval(endpoints = median_ci) +
  geom_vline(xintercept = 5, color = "red", linetype = "dashed") +
  labs(
    title = "The Great American Shift",
    subtitle = paste0("Median county shift: ", round(observed_median, 2), 
                      " percentage points (95% CI: ", 
                      round(median_ci$lower_ci, 2), ", ", 
                      round(median_ci$upper_ci, 2), ")"),
    x = "Median Shift (percentage points)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(median_test_plot)

# COMPUTATIONAL TEST 2: Testing if urban counties had significant rightward shift
# For a simple mean test, we can use t_test
urban_data <- election_data %>%
  filter(Urban_County == TRUE) %>%
  select(shift = Trump_Shift)

# Calculate observed mean shift in urban counties
observed_urban_mean <- mean(urban_data$shift, na.rm = TRUE)

# Create bootstrap distribution for urban shift
urban_bootstrap <- urban_data %>%
  specify(response = shift) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "mean")

# Get confidence interval
urban_ci <- urban_bootstrap %>%
  get_confidence_interval(level = 0.95)

# Calculate p-value
# Null hypothesis: urban shift <= 0
# Alternative hypothesis: urban shift > 0
p_value_urban <- mean(urban_bootstrap$stat <= 0)

# Create a plot showing the urban shift bootstrap distribution
urban_test_plot <- urban_bootstrap %>%
  visualize() +
  shade_confidence_interval(endpoints = urban_ci) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Urban Areas Abandon Democrats",
    subtitle = paste0("Average urban county shift: ", round(observed_urban_mean, 2), 
                      " percentage points (p < ", round(p_value_urban, 3), ")"),
    x = "Mean Urban Shift (percentage points)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(urban_test_plot)
