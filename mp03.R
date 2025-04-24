# Load required packages - simplified version
required_packages <- c(
  "dplyr", "tidyr", "stringr", "jsonlite", "httr", "purrr", 
  "ggplot2", "lubridate", "scales", "ggpubr", "viridis", 
  "gridExtra", "cowplot", "plotly", "proxy", "igraph", 
  "patchwork", "ggforce", "knitr" , "kableExtra"
)


# Install missing packages
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))


#task-1

# Create a function called load_songs() first
load_songs <- function() {
  # Create directory if it doesn't exist
  dir_name <- file.path("data", "mp03")
  dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  # Define file path
  file_path <- file.path(dir_name, "spotify_songs.csv")
  
  # Download file if it doesn't exist
  if (!file.exists(file_path)) {
    download.file(
      url = "https://raw.githubusercontent.com/gabminamedez/spotify-data/refs/heads/master/data.csv",
      destfile = file_path,
      method = "auto",
      quiet = FALSE
    )
  }
  
  # Read the data into R
  songs_data <- read.csv(file_path)
  
  # Clean and transform artists column
  clean_artist_string <- function(x) {
    str_replace_all(x, "\\['", "") |> 
      str_replace_all("'\\]", "") |>
      str_replace_all(" '", "")
  }
  
  # Process the data
  songs_clean <- songs_data |> 
    separate_longer_delim(artists, ",") |>
    mutate(artist = clean_artist_string(artists)) |>
    select(-artists)
  
  # Return the cleaned data frame
  return(songs_clean)
}

# Load and clean the data
songs_data <- load_songs()

#task-2
load_playlists <- function(n_files = NULL) {
  
  # Create directory if it doesn't exist
  dir_name <- file.path("data", "mp03", "playlists")
  dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  # Base URL for the GitHub repository
  base_url <- "https://github.com/DevinOgrady/spotify_million_playlist_dataset/raw/main/data1/"
  
  # Get the list of available files from the repository
  repo_url <- "https://api.github.com/repos/DevinOgrady/spotify_million_playlist_dataset/contents/data1"
  response <- httr::GET(repo_url)
  
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch file list from GitHub repository")
  }
  
  # Parse the response to get file names
  files_info <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  file_names <- files_info$name
  
  # Filter to only include JSON files
  json_files <- file_names[grepl("\\.json$", file_names)]

  
  # Function to download a file if it doesn't exist locally
  download_if_needed <- function(file_name) {
    local_path <- file.path(dir_name, file_name)
    
    if (!file.exists(local_path)) {
      message(paste("Downloading", file_name, "..."))
      
      # Add a small delay to avoid hitting rate limits
      Sys.sleep(0.5)
      
      file_url <- paste0(base_url, file_name)
      tryCatch({
        download.file(file_url, local_path, method = "auto", quiet = TRUE)
      }, error = function(e) {
        warning(paste("Failed to download", file_name, ":", e$message))
        return(NULL)
      })
    } else {
      message(paste(file_name, "already exists locally. Skipping download."))
    }
    
    return(local_path)
  }
  
  # Download all files (if needed)
  local_files <- sapply(json_files, download_if_needed)
  
  # Process playlist file - improved for speed and reliability
  process_playlist_file_alt <- function(file_path) {
    if (is.null(file_path) || !file.exists(file_path)) {
      warning(paste("File does not exist:", file_path))
      return(NULL)
    }
    
    tryCatch({
      # Read the JSON data - set simplifyDataFrame to FALSE to avoid conversion issues
      playlist_data <- jsonlite::fromJSON(file_path, simplifyVector = TRUE, simplifyDataFrame = FALSE)
      
      # For speed, we'll use data.table instead of dplyr for the heavy lifting
      library(data.table)
      
      # Prepare playlists as a list
      playlists <- playlist_data$playlists
      message(paste("Processing", length(playlists), "playlists..."))
      
      # Create an empty data.table to store results
      all_tracks <- data.table::data.table()
      
      # Process each playlist
      for (pl in playlists) {
        # Skip if no tracks
        if (length(pl$tracks) == 0) next
        
        # Create a data.table for tracks in this playlist
        tracks_dt <- data.table::rbindlist(
          lapply(pl$tracks, function(t) {
            data.table::data.table(
              playlist_name = pl$name,
              playlist_id = pl$pid,
              playlist_followers = pl$num_followers,
              playlist_position = t$pos,
              artist_name = t$artist_name,
              track_name = t$track_name,
              album_name = t$album_name,
              duration = t$duration_ms,
              artist_id = sub("spotify:artist:", "", t$artist_uri),
              track_id = sub("spotify:track:", "", t$track_uri), 
              album_id = sub("spotify:album:", "", t$album_uri)
            )
          }),
          fill = TRUE  # Handle missing fields
        )
        
        # Add to all tracks
        all_tracks <- rbindlist(list(all_tracks, tracks_dt), fill = TRUE)
      }
      
      # Convert back to tibble for consistency with original code
      result <- tibble::as_tibble(all_tracks) %>%
        # Select columns in the same order as original
        dplyr::select(
          playlist_name,
          playlist_position,
          playlist_id, 
          playlist_followers,
          artist_name,
          track_name, 
          album_name,
          duration,
          artist_id,
          track_id,
          album_id
        )
      
      return(result)
    }, error = function(e) {
      warning(paste("Error processing file", file_path, ":", e$message))
      return(NULL)
    })
  }
  
  # Process all downloaded files
  message("Processing playlist files...")
  all_playlists_data <- tibble::tibble()
  
  for (i in seq_along(local_files)) {
    file_path <- local_files[i]
    if (!is.null(file_path)) {
      file_name <- basename(file_path)
      message(paste("Processing file", i, "of", length(local_files), ":", file_name))
      
      result <- process_playlist_file_alt(file_path)
      
      if (!is.null(result) && nrow(result) > 0) {
        all_playlists_data <- bind_rows(all_playlists_data, result)
        
        # Provide progress update
        message(paste("  Added", nrow(result), "tracks. Total now:", nrow(all_playlists_data)))
      }
    }
  }
  
  # Check and report column names
  message("Final column names: ", paste(colnames(all_playlists_data), collapse = ", "))
  
  return(all_playlists_data)
}

# Make sure required packages are loaded
library(dplyr)
library(tibble)
library(jsonlite)
library(httr)
library(data.table)

# Usage example
playlists <- load_playlists(n_files = 1)  # Start with just 1 file for testing

# Print a summary of the dataset using kable
if (!is.null(playlists) && nrow(playlists) > 0) {
  # Check if columns exist before using them
  columns_to_check <- c("artist_id", "track_id", "album_id")
  missing_columns <- columns_to_check[!columns_to_check %in% names(playlists)]
  
  if (length(missing_columns) > 0) {
    message("Missing columns: ", paste(missing_columns, collapse = ", "))
    # Create the missing columns with NA values
    for (col in missing_columns) {
      playlists[[col]] <- NA
    }
  }
  
  # Show summary
  message("Dataset loaded successfully!")
  message("Number of rows: ", nrow(playlists))
  message("Number of unique playlists: ", length(unique(playlists$playlist_id)))
  
  # Display sample
  playlists %>% 
    slice_sample(n = min(10, nrow(playlists)))
} else {
  message("No data was loaded. Check the error messages above.")
}


#task-3
playlist_data <- function(playlists) {
  # Check if data is empty or NULL
  if (is.null(playlists) || nrow(playlists) == 0) {
    message("Warning: Input data has 0 rows. Returning empty dataframe with required columns.")
    return(tibble::tibble(
      playlist_name = character(),
      playlist_followers = integer(),
      track_name = character(),
      artist_name = character(),
      album_name = character(),
      duration = integer()
    ))
  }
  
  # Print column names for diagnosis
  message("Input columns: ", paste(names(playlists), collapse = ", "))
  
  # Create a safer copy of the dataframe - using tibble to maintain consistency
  result <- tibble::as_tibble(playlists)
  
  # Add missing columns if needed
  required_cols <- c("playlist_name", "playlist_followers", "track_name", 
                     "artist_name", "album_name", "duration")
  
  for (col in required_cols) {
    if (!col %in% names(result)) {
      message(paste("Adding missing column:", col))
      # Add column with appropriate default type
      if (col == "playlist_followers" || col == "duration") {
        result[[col]] <- as.integer(0)  # Default integer value
      } else {
        result[[col]] <- NA_character_  # Default character value
      }
    }
  }
  
  # Convert numeric columns safely
  result <- result %>%
    dplyr::mutate(
      playlist_followers = as.integer(playlist_followers),
      duration = as.integer(duration)
    )
  
  # Replace NAs with defaults
  result <- result %>%
    dplyr::mutate(
      playlist_followers = dplyr::if_else(is.na(playlist_followers), as.integer(0), playlist_followers),
      duration = dplyr::if_else(is.na(duration), as.integer(0), duration)
    )
  
  # Select only the columns we need
  result <- result %>%
    dplyr::select(dplyr::all_of(required_cols))
  
  message("Output dimensions: ", nrow(result), " rows, ", ncol(result), " columns")
  return(result)
}

# Apply the function to the playlists data from Task 2
playlist_data <- playlist_data(playlists)

# Save the rectangular data to a file for further use
save_file_path <- file.path("data", "mp03", "playlist_data.rds")
dir.create(dirname(save_file_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(playlist_data, save_file_path)
message(paste("Rectangular playlist data saved to:", save_file_path))
  
#task -4
# Load required packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales)

# Define Spotify color scheme to use consistently
spotify_green <- "#1DB954"
spotify_black <- "#191414" 
spotify_gray <- "#535353"

# Load the playlist data
playlist_data <- readRDS("data/mp03/playlist_data.rds")

# Question 1: How many distinct tracks and artists are represented in the playlist data?
count_distinct_items <- function(playlist_data) {
  # Count distinct tracks
  distinct_tracks <- playlist_data %>%
    distinct(track_name) %>%
    nrow()
  
  # Count distinct artists 
  distinct_artists <- playlist_data %>%
    distinct(artist_name) %>%
    nrow()
  
  # Create results list for return value
  results <- list(
    distinct_tracks = distinct_tracks,
    distinct_artists = distinct_artists
  )
  
  # Create a data frame for display
  results_df <- data.frame(
    Metric = c("Number of distinct tracks", "Number of distinct artists"),
    Count = c(distinct_tracks, distinct_artists)
  )
  
  # Create and print the table
  table_output_1 <- results_df %>%
    kable("html", align = "l") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = FALSE,
                  position = "left")
  
  # Print the table from within the function
  print(table_output_1)
  
  return(results)
}

# To use this function:
distinct_items <- count_distinct_items(playlist_data)

# Question 2: What are the 5 most popular tracks in the playlist data?
find_most_popular_tracks <- function(playlist_data, top_n = 5) {
  # Group by track details and count occurrences
  popular_tracks <- playlist_data %>%
    group_by(track_name, artist_name) %>%
    summarize(
      playlist_appearances = n(),      
      distinct_playlists = n_distinct(playlist_name),  
      .groups = "drop"
    ) %>%
    arrange(desc(playlist_appearances)) %>%  
    head(top_n)                             
  
  # Create a formatted dataframe for display
  display_tracks <- popular_tracks %>%
    mutate(
      Rank = row_number(),
      Track = paste0("\"", track_name, "\" by ", artist_name),
      `Playlist Appearances` = playlist_appearances,
      `Distinct Playlists` = distinct_playlists
    ) %>%
    select(Rank, Track, `Playlist Appearances`, `Distinct Playlists`)
  
  # Print table with Spotify-inspired styling
  table_output_2 <- display_tracks %>%
    kable("html", align = "l") %>%
    kable_styling(bootstrap_options = c("hover", "condensed"), 
                  full_width = FALSE) %>%
    row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
    row_spec(1:top_n, background = "white", color = spotify_black) %>%
    row_spec(seq(1, top_n, 2), background = "#f8f8f8") # light gray alternating rows
  
  # Print the table explicitly
  print(table_output_2)
  
  # Create visualization with Spotify colors
  track_plot <- ggplot(popular_tracks, aes(x = reorder(paste0(track_name, " - ", artist_name), playlist_appearances), 
                                           y = playlist_appearances)) +
    geom_bar(stat = "identity", fill = spotify_green) +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Most Popular Tracks in Playlists"),
      x = "Track - Artist",
      y = "Number of Appearances"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", color = spotify_black),
      axis.title = element_text(color = spotify_gray),
      axis.text = element_text(color = spotify_black),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_line(color = "#E0E0E0"),
      panel.grid.minor = element_line(color = "#F0F0F0")
    )
  
  popular_tracks <- find_most_popular_tracks(playlist_data, top_n = 5)
}

# Question 3: What is the most popular track that does not have a corresponding entry in the song characteristics data?
find_popular_missing_track <- function(playlist_data, songs_data) {
  # Define Spotify color scheme
  spotify_green <- "#1DB954"
  spotify_black <- "#191414" 
  spotify_gray <- "#535353"
  
  # Ensure track_id is available in both datasets for joining
  # If track_id is not available in the datasets, we'll use track_name and artist_name for joining
  if (!"track_id" %in% colnames(playlist_data) || !"id" %in% colnames(songs_data)) {
    # Create a joint key using track name and artist for matching
    playlist_data <- playlist_data %>%
      mutate(track_artist = paste(track_name, artist_name, sep = " - "))
    
    songs_data <- songs_data %>%
      mutate(track_artist = paste(name, artist, sep = " - "))
    
    # Find tracks in playlists that don't exist in songs data
    missing_tracks <- playlist_data %>%
      anti_join(songs_data, by = "track_artist")
  } else {
    # If track_id is available, use it for matching
    missing_tracks <- playlist_data %>%
      anti_join(songs_data, by = c("track_id" = "id"))
  }
  
  # Count occurrences of each missing track
  popular_missing_tracks <- missing_tracks %>%
    group_by(track_name, artist_name) %>%
    summarize(
      playlist_appearances = n(),
      distinct_playlists = n_distinct(playlist_name),
      .groups = "drop"
    ) %>%
    arrange(desc(playlist_appearances))
  
  # Get the most popular missing track
  most_popular_missing <- head(popular_missing_tracks, 1)
  
  # Calculate percentage of all playlists
  total_playlists <- n_distinct(playlist_data$playlist_name)
  
  # Check if we found any missing tracks
  if(nrow(most_popular_missing) > 0) {
    appearance_percentage <- round(most_popular_missing$distinct_playlists / total_playlists * 100, 2)
    
    # Create a dataframe for the most popular missing track
    popular_missing_df <- data.frame(
      Metric = c("Track name", "Artist", "Playlist appearances", 
                 "Distinct playlists", "Percentage of all playlists"),
      Value = c(most_popular_missing$track_name, 
                most_popular_missing$artist_name,
                most_popular_missing$playlist_appearances,
                most_popular_missing$distinct_playlists,
                paste0(appearance_percentage, "%"))
    )
    
    # Print first table with Spotify styling
    table3 <- popular_missing_df %>%
      kable("html") %>%
      kable_styling(bootstrap_options = c("hover"), full_width = FALSE) %>%
      row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
      row_spec(1:nrow(popular_missing_df), background = "white", color = spotify_black) %>%
      row_spec(seq(1, nrow(popular_missing_df), 2), background = "#f8f8f8") # light gray alternating rows
    
    print(table3)
    
    # Look at top 10 missing tracks
    top_missing <- head(popular_missing_tracks, 10)
    
    # Print second table with Spotify styling
    table3a <- top_missing %>%
      kable("html", 
            col.names = c("Track Name", "Artist", "Playlist Appearances", "Distinct Playlists")) %>%
      kable_styling(bootstrap_options = c("hover", "condensed"),
                    full_width = TRUE) %>%
      row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
      row_spec(1:nrow(top_missing), background = "white", color = spotify_black) %>%
      row_spec(seq(1, nrow(top_missing), 2), background = "#f8f8f8")
    
    print(table3a)
  } else {
    cat("No missing tracks found.\n\n")
  }
  
  return(list(
    most_popular_missing = most_popular_missing,
    top_missing_tracks = popular_missing_tracks
  ))
}


missing_tracks <- find_popular_missing_track(playlist_data, songs_data)

# Question 4: According to the song characteristics data, what is the most "danceable" track? How often does it appear in a playlist?
find_most_danceable_track <- function(songs_data, playlist_data) {
  # Define Spotify color scheme
  spotify_green <- "#1DB954"
  spotify_black <- "#191414" 
  spotify_gray <- "#535353"
  
  # Check if the danceability column exists in the songs data
  if (!"danceability" %in% colnames(songs_data)) {
    stop("The songs data does not contain a 'danceability' column.")
  }
  
  # Find the track with highest danceability score
  most_danceable <- songs_data %>%
    arrange(desc(danceability)) %>%
    head(1)
  
  # Create matching criteria for playlist data
  # Match based on track name and artist name
  
  # Function to clean strings for better matching
  clean_string <- function(x) {
    tolower(gsub("[[:punct:]]", "", x))
  }
  
  # Clean track name and artist from most danceable track
  danceable_track_name_clean <- clean_string(most_danceable$name)
  danceable_artist_clean <- clean_string(most_danceable$artist)
  
  # Prepare for matching by cleaning playlist data
  playlist_data_clean <- playlist_data %>%
    mutate(
      track_name_clean = clean_string(track_name),
      artist_name_clean = clean_string(artist_name)
    )
  
  # Find matches - look for both exact and partial matches
  match_track <- playlist_data_clean %>%
    filter(
      track_name_clean == danceable_track_name_clean |
        (track_name_clean %like% danceable_track_name_clean & 
           artist_name_clean %like% danceable_artist_clean)
    )
  
  # If no exact matches found, try fuzzy matching
  if(nrow(match_track) == 0) {
    match_track <- playlist_data_clean %>%
      filter(
        grepl(danceable_track_name_clean, track_name_clean, fixed = TRUE) |
          grepl(danceable_artist_clean, artist_name_clean, fixed = TRUE)
      )
  }
  
  # Count occurrences in playlists
  appearance_count <- nrow(match_track)
  distinct_playlists_count <- n_distinct(match_track$playlist_name)
  
  # Calculate percentage of all playlists
  total_playlists <- n_distinct(playlist_data$playlist_name)
  appearance_percentage <- ifelse(
    distinct_playlists_count > 0,
    round(distinct_playlists_count / total_playlists * 100, 2),
    0
  )
  
  # Create data frame for the most danceable track info
  danceable_info <- data.frame(
    Metric = c("Track name", "Artist", "Danceability score", 
               "Playlist appearances", "Distinct playlists", 
               "Percentage of all playlists"),
    Value = c(
      most_danceable$name,
      most_danceable$artist,
      paste0(round(most_danceable$danceability, 3), " (scale 0-1)"),
      ifelse(appearance_count > 0, appearance_count, "0"),
      ifelse(distinct_playlists_count > 0, distinct_playlists_count, "0"),
      ifelse(appearance_percentage > 0, paste0(appearance_percentage, "%"), "0%")
    )
  )
  
  # Print first table with Spotify styling
  tabledance <- danceable_info %>%
    kable("html") %>%
    kable_styling(bootstrap_options = c("hover"), full_width = FALSE) %>%
    row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
    row_spec(1:nrow(danceable_info), background = "white", color = spotify_black) %>%
    row_spec(seq(1, nrow(danceable_info), 2), background = "#f8f8f8") # light gray alternating rows
  
  print(tabledance)
  
  # Get top 10 most danceable tracks
  top_danceable <- songs_data %>%
    arrange(desc(danceability)) %>%
    head(10) %>%
    select(name, artist, danceability, energy, tempo)
  
  # Print second table with Spotify styling
  table4 <- top_danceable %>%
    kable("html", 
          col.names = c("Track Name", "Artist", "Danceability", "Energy", "Tempo")) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"), 
                  full_width = TRUE) %>%
    row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
    row_spec(1:nrow(top_danceable), background = "white", color = spotify_black) %>%
    row_spec(seq(1, nrow(top_danceable), 2), background = "#f8f8f8") # light gray alternating rows
  
  print(table4)
  
  # Return results in a more structured way
  result <- list(
    most_danceable_track = most_danceable,
    appearance_count = appearance_count,
    distinct_playlists = distinct_playlists_count,
    top_danceable = top_danceable
  )
  
  return(result)
}

danceable_track <- find_most_danceable_track(songs_data, playlist_data)



# Question 5: Which playlist has the longest average track length?
find_longest_avg_playlist <- function(playlist_data, min_tracks = 5) {
  # Define Spotify color scheme
  spotify_green <- "#1DB954"
  spotify_black <- "#191414" 
  spotify_gray <- "#535353"
  
  # Calculate average duration for each playlist
  playlist_durations <- playlist_data %>%
    group_by(playlist_name) %>%
    summarize(
      avg_duration_ms = mean(duration, na.rm = TRUE),
      avg_duration_min = round(mean(duration, na.rm = TRUE) / 1000 / 60, 2),
      total_duration_ms = sum(duration, na.rm = TRUE),
      total_duration_min = round(sum(duration, na.rm = TRUE) / 1000 / 60, 2),
      track_count = n(),
      .groups = "drop"
    ) %>%
    # Filter playlists with minimum number of tracks to avoid outliers
    filter(track_count >= min_tracks) %>%
    arrange(desc(avg_duration_ms))
  
  # Get the playlist with longest average
  longest_avg_playlist <- head(playlist_durations, 1)
  
  # Get track details for this playlist
  longest_playlist_tracks <- playlist_data %>%
    filter(playlist_name == longest_avg_playlist$playlist_name) %>%
    select(track_name, artist_name, duration) %>%
    mutate(duration_min = round(duration / 1000 / 60, 2)) %>%
    arrange(desc(duration))
  
  # Create data frame for playlist info
  playlist_info <- data.frame(
    Metric = c("Playlist name", "Average track duration", 
               "Total playlist duration", "Number of tracks"),
    Value = c(
      longest_avg_playlist$playlist_name,
      paste(longest_avg_playlist$avg_duration_min, "minutes"),
      paste(longest_avg_playlist$total_duration_min, "minutes"),
      longest_avg_playlist$track_count
    )
  )
  
  # Print first table with Spotify styling
  table5 <- playlist_info %>%
    kable("html") %>%
    kable_styling(bootstrap_options = c("hover"), full_width = FALSE) %>%
    row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
    row_spec(1:4, background = "white", color = spotify_black) %>% 
    row_spec(seq(1, 4, 2), background = "#f8f8f8")
  
  print(table5)
  
  # Print second table with Spotify styling - top 5 longest tracks
  table5a <- head(longest_playlist_tracks, 5) %>%
    kable("html", 
          col.names = c("Track Name", "Artist", "Duration (ms)", "Duration (min)")) %>%
    kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
    row_spec(1:min(5, nrow(longest_playlist_tracks)), background = "white", color = spotify_black) %>%
    row_spec(seq(1, min(5, nrow(longest_playlist_tracks)), 2), background = "#f8f8f8")
  
  print(table5a)
  
  return(list(
    longest_avg_playlist = longest_avg_playlist,
    longest_playlist_tracks = longest_playlist_tracks
  ))
}

longest_playlist <- find_longest_avg_playlist(playlist_data, min_tracks = 5)

# Question 6: What is the most popular playlist on Spotify?
find_most_popular_playlist <- function(playlist_data) {
  # Define Spotify color scheme
  spotify_green <- "#1DB954"
  spotify_black <- "#191414" 
  spotify_gray <- "#535353"
  
  # Group by playlist and get follower counts
  playlist_popularity <- playlist_data %>%
    group_by(playlist_name, playlist_followers) %>%
    summarize(
      track_count = n(),
      unique_artists = n_distinct(artist_name),
      .groups = "drop"
    ) %>%
    arrange(desc(playlist_followers))
  
  # Get the most popular playlist
  most_popular <- head(playlist_popularity, 1)
  
  # Get most common artists in this playlist
  popular_playlist_artists <- playlist_data %>%
    filter(playlist_name == most_popular$playlist_name) %>%
    count(artist_name, sort = TRUE) %>%
    head(5)
  
  # Create data frame for playlist info
  popular_info <- data.frame(
    Metric = c("Playlist name", "Number of followers", 
               "Number of tracks", "Number of unique artists"),
    Value = c(
      most_popular$playlist_name,
      most_popular$playlist_followers,
      most_popular$track_count,
      most_popular$unique_artists
    )
  )
  
  # Print first table with Spotify styling
  table6 <- popular_info %>%
    kable("html") %>%
    kable_styling(bootstrap_options = c("hover"), full_width = FALSE) %>%
    row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
    row_spec(1:4, background = "white", color = spotify_black) %>%
    row_spec(seq(1, 4, 2), background = "#f8f8f8")
  
  print(table6)
  
  # Print second table with Spotify styling
  table6a <- popular_playlist_artists %>%
    kable("html", 
          col.names = c("Artist Name", "Number of Tracks")) %>%
    kable_styling(bootstrap_options = c("hover")) %>%
    row_spec(0, background = spotify_black, color = "white", bold = TRUE) %>%
    row_spec(1:nrow(popular_playlist_artists), background = "white", color = spotify_black) %>%
    row_spec(seq(1, nrow(popular_playlist_artists), 2), background = "#f8f8f8")
  
  print(table6a)
  
  # Create visualization with Spotify colors
  popularity_plot <- ggplot(head(playlist_popularity, 10), 
                            aes(x = reorder(substr(playlist_name, 1, 30), playlist_followers), 
                                y = playlist_followers)) +
    geom_bar(stat = "identity", fill = spotify_green) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Top 10 Most Popular Spotify Playlists",
      x = "Playlist Name",
      y = "Number of Followers"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", color = spotify_black),
      axis.title = element_text(color = spotify_gray),
      axis.text = element_text(color = spotify_black),
      axis.text.y = element_text(size = 8),
      panel.grid.major = element_line(color = "#E0E0E0"),
      panel.grid.minor = element_line(color = "#F0F0F0")
    )
  
  print(popularity_plot)
  
  return(list(
    most_popular_playlist = most_popular,
    popular_playlist_artists = popular_playlist_artists,
    top_popular_playlists = head(playlist_popularity, 10)
  ))
}

 popular_playlist <- find_most_popular_playlist(playlist_data)

#TASK-5
#============================================================================
# Task 5: Visually Identifying Characteristics of Popular Songs
#============================================================================

 # Create an inner join between playlist_data and songs_data

 combined_data <- playlist_data %>%
   inner_join(songs_data, 
              by = c("track_name" = "name", 
                     "artist_name" = "artist")) %>%
   # Rename the duration columns to avoid confusion
   rename(
     duration_playlist = duration,
     duration_song = duration_ms
   )
 
 # Display the structure of the combined data
 glimpse(combined_data)
 
 # Check how many matches we found
 match_summary <- combined_data %>%
   summarize(
     total_rows = n(),
     unique_tracks = n_distinct(track_name, artist_name),
     unique_playlists = n_distinct(playlist_name)
   )
 
 print(match_summary)

#=============================================================================
# 1. Is the `popularity` column correlated with the number of playlist appearances?
#=============================================================================
 # Define Spotify color scheme for consistent visuals
 spotify_green <- "#1DB954"
 spotify_black <- "#191414" 
 spotify_gray <- "#535353"
 spotify_palette <- c(spotify_green, "#1ED760", "#2D46B9", "#F573A0", "#509BF5")
 
 # Set theme for all plots
 theme_spotify <- function() {
   theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5, face = "bold", color = spotify_black, size = 14),
       plot.subtitle = element_text(hjust = 0.5, color = spotify_gray, size = 12),
       axis.title = element_text(color = spotify_gray, size = 11),
       axis.text = element_text(color = spotify_black, size = 10),
       panel.grid.major = element_line(color = "#E0E0E0"),
       panel.grid.minor = element_line(color = "#F0F0F0"),
       legend.title = element_text(color = spotify_black, size = 11),
       legend.text = element_text(color = spotify_gray, size = 10),
       legend.position = "bottom"
     )
 }
 
 ### 5.1: Is the `popularity` column correlated with the number of playlist appearances?
 
 # Count track appearances across playlists
 track_appearances <- combined_data %>%
   group_by(id, track_name, artist_name, popularity) %>%
   summarize(
     playlist_appearances = n_distinct(playlist_name),
     .groups = "drop"
   )
 
 # Calculate correlation
 popularity_correlation <- cor(track_appearances$popularity, 
                               track_appearances$playlist_appearances,
                               use = "complete.obs")
 
 # Create a scatter plot with regression line
 pop_correlation_plot <- ggplot(track_appearances, 
                                aes(x = popularity, y = playlist_appearances)) +
   geom_point(alpha = 0.2, color = spotify_green) +
   geom_smooth(method = "lm", color = spotify_black) +
   scale_y_log10(labels = scales::comma_format()) +
   labs(
     title = "Correlation Between Spotify Popularity Score and Playlist Appearances",
     subtitle = paste("Correlation coefficient:", round(popularity_correlation, 3)),
     x = "Spotify Popularity Score",
     y = "Number of Playlist Appearances (log scale)",
     caption = "Data source: Spotify Million Playlist Dataset & Spotify API"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     plot.subtitle = element_text(size = 12),
     axis.title = element_text(size = 11),
     plot.caption = element_text(size = 9, color = "gray50"),
     panel.grid.minor = element_blank()
   )
 
 # Create a table for the correlation result
 corr_table <- data.frame(
   Metric = "Correlation between popularity and playlist appearances",
   Value = round(popularity_correlation, 3)
 )
 
 # Create a boxplot to show playlist appearances distribution by popularity level
 pop_boxplot <- ggplot(track_appearances, aes(x = cut_width(popularity, 10), 
                                              y = playlist_appearances)) +
   geom_boxplot(fill = spotify_green, alpha = 0.7) +
   scale_y_log10() +
   labs(
     title = "Distribution of Playlist Appearances by Popularity Score",
     x = "Spotify Popularity Score",
     y = "Playlist Appearances (log scale)"
   ) +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1),
     plot.title = element_text(face = "bold", size = 14)
   )
 
 # Define popular songs threshold - top 10% of playlist appearances
 popularity_threshold <- quantile(track_appearances$playlist_appearances, 0.90)
 
 # Add a column to identify popular songs
 track_appearances <- track_appearances %>%
   mutate(is_popular = playlist_appearances >= popularity_threshold,
          popularity_category = ifelse(is_popular, "Popular", "Less Popular"))
 
 # Check a song at the threshold
 threshold_song <- track_appearances %>%
   filter(playlist_appearances >= popularity_threshold) %>%
   arrange(playlist_appearances) %>%
   head(1)
 
 # Apply the popular/less popular categorization to the main dataset
 combined_data <- combined_data %>%
   left_join(
     track_appearances %>% 
       select(id, playlist_appearances, is_popular, popularity_category),
     by = "id"
   )
 
 print(pop_correlation_plot)
 print(pop_boxplot)
 print(kable(corr_table, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))
#=============================================================================
# 2. In what year were the most popular songs released?
#=============================================================================

 # Filter out NA years and prepare data
 year_popularity <- combined_data %>%
   filter(!is.na(year)) %>%
   filter(year >= 1950 & year <= 2023) %>% # Filter reasonable years
   group_by(year, popularity_category) %>%
   summarize(
     song_count = n(),
     avg_playlist_appearances = mean(playlist_appearances),
     .groups = "drop"
   )
 
 # Create a stacked bar chart with year on x-axis
 popular_years_plot <- ggplot(year_popularity, 
                              aes(x = year, y = song_count, fill = popularity_category)) +
   geom_bar(stat = "identity", position = "stack") +
   scale_fill_manual(values = c("Popular" = spotify_green, "Less Popular" = "#b3b3b3")) +
   scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
   labs(
     title = "Distribution of Songs by Release Year",
     subtitle = "Showing popular vs. less popular songs",
     x = "Release Year",
     y = "Number of Songs",
     fill = "Popularity Category",
     caption = "Popular songs defined as top 10% by playlist appearances"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     legend.position = "bottom",
     panel.grid.minor.x = element_blank()
   )
 
 # Create a plot showing the proportion of popular songs by year
 proportion_data <- year_popularity %>%
   group_by(year) %>%
   mutate(
     total = sum(song_count),
     proportion = song_count / total
   ) 
 
 # Filter to popular songs only for the proportion plot
 proportion_popular <- proportion_data %>%
   filter(popularity_category == "Popular")
 
 prop_years_plot <- ggplot(proportion_popular, 
                           aes(x = year, y = proportion)) +
   geom_line(color = spotify_green, size = 1) +
   geom_point(color = spotify_green, size = 2) +
   geom_smooth(method = "loess", color = spotify_black, se = FALSE) +
   scale_y_continuous(labels = scales::percent_format()) +
   scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
   labs(
     title = "Proportion of Popular Songs by Release Year",
     subtitle = "Shows when popular songs were most likely to be released",
     x = "Release Year",
     y = "Proportion of Popular Songs",
     caption = "Popular songs defined as top 10% by playlist appearances"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     panel.grid.minor = element_blank()
   )
 
 # Find the year with most popular songs
 most_popular_year <- proportion_popular %>%
   arrange(desc(proportion)) %>%
   head(1)
 
 # Create table for year with highest proportion of popular songs
 popular_year_table <- data.frame(
   Metric = c(
     "Year with highest proportion of popular songs",
     "Proportion of popular songs in that year"
   ),
   Value = c(
     most_popular_year$year,
     scales::percent(most_popular_year$proportion)
   )
 )
 
 print(popular_years_plot)
 print(prop_years_plot)
 print(kable(popular_year_table, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))
#=============================================================================
# 3. In what year did danceability peak?
#=============================================================================

 # Calculate average danceability by year
 danceability_by_year <- combined_data %>%
   filter(!is.na(year)) %>%
   filter(year >= 1950 & year <= 2023) %>%
   group_by(year) %>%
   summarize(
     avg_danceability = mean(danceability, na.rm = TRUE),
     songs_count = n(),
     .groups = "drop"
   ) %>%
   # Filter years with enough data points
   filter(songs_count >= 10)
 
 # Create plot for danceability trends
 danceability_plot <- ggplot(danceability_by_year, 
                             aes(x = year, y = avg_danceability)) +
   geom_line(color = spotify_green, size = 1) +
   geom_point(aes(size = songs_count), color = spotify_green, alpha = 0.7) +
   geom_smooth(method = "loess", color = spotify_black, se = FALSE) +
   scale_size_continuous(range = c(1, 5), name = "Number of Songs") +
   scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
   labs(
     title = "Average Danceability Score by Release Year",
     subtitle = "Point size indicates number of songs from that year",
     x = "Release Year",
     y = "Average Danceability Score",
     caption = "Years with fewer than 10 songs are excluded"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     legend.position = "right",
     panel.grid.minor = element_blank(),
     axis.text.x = element_text(angle = 45, hjust = 1)
   )
 
 # Find the year with peak danceability
 peak_danceability <- danceability_by_year %>%
   arrange(desc(avg_danceability)) %>%
   head(1)
 
 # Create a table for peak danceability
 dance_peak_table <- data.frame(
   Metric = c(
     "Year with peak average danceability",
     "Average danceability score",
     "Number of songs from that year in dataset"
   ),
   Value = c(
     peak_danceability$year,
     round(peak_danceability$avg_danceability, 3),
     peak_danceability$songs_count
   )
 )
 print(danceability_plot)
 print(kable(dance_peak_table, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))
#=============================================================================
# 4. Which decade is most represented on user playlists?
#=============================================================================

 # Calculate decade from year and count representations
 combined_data <- combined_data %>%
   mutate(release_decade = (year %/% 10) * 10)
 
 decade_representation <- combined_data %>%
   filter(!is.na(release_decade)) %>%
   filter(release_decade >= 1950 & release_decade <= 2020) %>%
   group_by(release_decade) %>%
   summarize(
     song_count = n(),
     total_playlist_appearances = sum(playlist_appearances),
     avg_appearances_per_song = mean(playlist_appearances),
     .groups = "drop"
   ) %>%
   arrange(desc(total_playlist_appearances))
 
 # Create a bar chart for decades
 decade_plot <- ggplot(decade_representation, 
                       aes(x = as.factor(release_decade), 
                           y = total_playlist_appearances)) +
   geom_bar(stat = "identity", fill = spotify_green, alpha = 0.8) +
   geom_text(aes(label = scales::comma(total_playlist_appearances)), 
             position = position_stack(vjust = 0.9),
             color = "white", size = 3) +
   labs(
     title = "Total Playlist Appearances by Decade",
     subtitle = "Which decades are most represented in user playlists?",
     x = "Decade",
     y = "Total Playlist Appearances",
     caption = "Based on song release decades"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     axis.text.x = element_text(angle = 0, hjust = 0.5),
     panel.grid.minor = element_blank()
   )
 
 # Create a dual metric plot - songs per decade vs avg appearances
 decade_comparison <- ggplot(decade_representation) +
   geom_col(aes(x = as.factor(release_decade), y = song_count), 
            fill = "#777777", alpha = 0.6) +
   geom_line(aes(x = as.factor(release_decade), 
                 y = avg_appearances_per_song * 100, 
                 group = 1), 
             color = spotify_green, size = 1.5) +
   geom_point(aes(x = as.factor(release_decade), 
                  y = avg_appearances_per_song * 100), 
              color = spotify_green, size = 3) +
   scale_y_continuous(
     name = "Number of Songs",
     sec.axis = sec_axis(~ . / 100, name = "Avg Playlist Appearances per Song")
   ) +
   labs(
     title = "Songs Count vs Average Playlist Appearances by Decade",
     subtitle = "Comparing quantity of songs with their average popularity",
     x = "Decade",
     caption = "Bars show number of songs, line shows average playlist appearances"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     axis.text.x = element_text(angle = 0, hjust = 0.5),
     axis.title.y.left = element_text(color = "#777777"),
     axis.title.y.right = element_text(color = spotify_green),
     panel.grid.minor = element_blank()
   )
 
 # Report findings
 most_represented_decade <- decade_representation %>% 
   arrange(desc(total_playlist_appearances)) %>% 
   head(1)
 
 # Create a table for most represented decade
 decade_table <- data.frame(
   Metric = c(
     "Most represented decade in playlists",
     "Total playlist appearances",
     "Number of songs from this decade",
     "Average appearances per song"
   ),
   Value = c(
     paste0(most_represented_decade$release_decade, "s"),
     scales::comma(most_represented_decade$total_playlist_appearances),
     scales::comma(most_represented_decade$song_count),
     round(most_represented_decade$avg_appearances_per_song, 2)
   )
 )
 print(decade_plot)
 print(decade_comparison)
 print(kable(decade_table, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))
#=============================================================================
# 5. Create a plot of key frequency using polar coordinates
#=============================================================================
 # Mapping of numerical keys to musical notation
 key_mapping <- c(
   "0" = "C",
   "1" = "C♯/D♭",
   "2" = "D",
   "3" = "D♯/E♭",
   "4" = "E",
   "5" = "F",
   "6" = "F♯/G♭",
   "7" = "G",
   "8" = "G♯/A♭",
   "9" = "A",
   "10" = "A♯/B♭",
   "11" = "B"
 )
 
 # Count frequency of each key
 key_frequency <- combined_data %>%
   filter(!is.na(key)) %>%
   mutate(key_name = key_mapping[as.character(key)]) %>%
   group_by(key, key_name) %>%
   summarize(
     song_count = n(),
     total_appearances = sum(playlist_appearances),
     avg_popularity = mean(popularity, na.rm = TRUE),
     .groups = "drop"
   ) %>%
   # Order by circle of fifths (C, G, D, A, E, B, F#, C#, G#, D#, A#, F)
   mutate(
     circle_order = case_when(
       key == 0 ~ 1,   # C
       key == 7 ~ 2,   # G
       key == 2 ~ 3,   # D
       key == 9 ~ 4,   # A
       key == 4 ~ 5,   # E
       key == 11 ~ 6,  # B
       key == 6 ~ 7,   # F#
       key == 1 ~ 8,   # C#
       key == 8 ~ 9,   # G#
       key == 3 ~ 10,  # D#
       key == 10 ~ 11, # A#
       key == 5 ~ 12   # F
     )
   ) %>%
   arrange(circle_order)
 
 # Create a polar coordinate plot
 key_polar_plot <- ggplot(key_frequency, 
                          aes(x = key_name, y = total_appearances, fill = avg_popularity)) +
   geom_bar(stat = "identity", alpha = 0.8) +
   coord_polar() +
   scale_fill_viridis(option = "viridis", direction = -1) +
   labs(
     title = "Musical Key Distribution in Spotify Playlists",
     subtitle = "Circle shows frequency of musical keys with color indicating average popularity",
     y = "Total Playlist Appearances",
     x = NULL,
     fill = "Avg. Popularity"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     axis.text.x = element_text(size = 9),
     legend.position = "bottom",
     panel.grid.major.x = element_blank()
   )
 
 # Create another version with popularity categories
 key_popular_data <- combined_data %>% 
   filter(!is.na(key)) %>%
   mutate(key_name = key_mapping[as.character(key)]) %>%
   group_by(key_name, popularity_category) %>%
   summarize(song_count = n(), .groups = "drop")
 
 key_popular_plot <- ggplot(key_popular_data,
                            aes(x = key_name, y = song_count, fill = popularity_category)) +
   geom_bar(stat = "identity", position = "stack") +
   coord_polar() +
   scale_fill_manual(values = c("Popular" = spotify_green, "Less Popular" = "#1E90FF")) +
   labs(
     title = "Distribution of Musical Keys by Popularity",
     subtitle = "Popular vs Less Popular Songs",
     x = NULL,
     y = "Number of Songs",
     fill = "Popularity Category"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     axis.text.x = element_text(size = 9),
     legend.position = "bottom",
     panel.grid.major.x = element_blank()
   )
 
 # Report most common key
 most_common_key <- key_frequency %>% 
   arrange(desc(total_appearances)) %>% 
   head(1)
 
 # Create a table for the most common key
 key_table <- data.frame(
   Metric = c(
     "Most common musical key",
     "Songs in this key",
     "Total playlist appearances"
   ),
   Value = c(
     most_common_key$key_name,
     most_common_key$song_count,
     scales::comma(most_common_key$total_appearances)
   )
 )
 print(key_polar_plot)
 print(key_popular_plot)
 print(kable(key_table, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))
#=============================================================================
# 6. What are the most popular track lengths?
#=============================================================================

 # Create duration in minutes for easier categorization
 combined_data <- combined_data %>%
   mutate(duration_min = duration_song / 1000 / 60)
 
 # Create duration categories
 duration_analysis <- combined_data %>%
   mutate(
     duration_category = case_when(
       duration_min < 2 ~ "< 2 min",
       duration_min >= 2 & duration_min < 3 ~ "2-3 min",
       duration_min >= 3 & duration_min < 4 ~ "3-4 min",
       duration_min >= 4 & duration_min < 5 ~ "4-5 min",
       duration_min >= 5 & duration_min < 6 ~ "5-6 min",
       duration_min >= 6 ~ "6+ min"
     ),
     duration_category = factor(duration_category, 
                                levels = c("< 2 min", "2-3 min", "3-4 min", 
                                           "4-5 min", "5-6 min", "6+ min"))
   ) %>%
   group_by(duration_category, popularity_category) %>%
   summarize(
     song_count = n(),
     total_appearances = sum(playlist_appearances),
     avg_appearances = mean(playlist_appearances),
     .groups = "drop"
   )
 
 # Create a stacked bar chart
 duration_plot <- ggplot(duration_analysis, 
                         aes(x = duration_category, 
                             y = song_count, 
                             fill = popularity_category)) +
   geom_bar(stat = "identity", position = "stack") +
   scale_fill_manual(values = c("Popular" = spotify_green, "Less Popular" = "#b3b3b3")) +
   labs(
     title = "Song Counts by Duration Category",
     subtitle = "Distribution of popular vs. less popular songs",
     x = "Duration Category",
     y = "Number of Songs",
     fill = "Popularity Category"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     legend.position = "bottom",
     panel.grid.minor = element_blank()
   )
 
 # Calculate average playlist appearances by duration category
 duration_avg_data <- duration_analysis %>% 
   group_by(duration_category) %>%
   summarize(avg_appearances = sum(total_appearances) / sum(song_count))
 
 # Create a plot showing average playlist appearances by duration
 avg_appearances_plot <- ggplot(duration_avg_data,
                                aes(x = duration_category, y = avg_appearances)) +
   geom_bar(stat = "identity", fill = spotify_green, alpha = 0.8) +
   geom_text(aes(label = round(avg_appearances, 1)), 
             vjust = -0.5, color = spotify_black, size = 3.5) +
   labs(
     title = "Average Playlist Appearances by Song Duration",
     subtitle = "Which song lengths are most frequently included in playlists?",
     x = "Duration Category",
     y = "Average Playlist Appearances per Song"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     panel.grid.minor = element_blank()
   )
 
 # Find most popular duration category
 most_popular_duration <- duration_avg_data %>%
   arrange(desc(avg_appearances)) %>%
   head(1)
 
 # Count songs in the most popular duration category
 songs_in_category <- sum(duration_analysis$song_count[
   duration_analysis$duration_category == most_popular_duration$duration_category])
 
 # Create a table for most popular duration
 duration_table <- data.frame(
   Metric = c(
     "Most popular duration category",
     "Average playlist appearances",
     "Number of songs in this category"
   ),
   Value = c(
     as.character(most_popular_duration$duration_category),
     round(most_popular_duration$avg_appearances, 2),
     songs_in_category
   )
 )
 print(duration_plot)
 print(avg_appearances_plot)
 print(kable(duration_table, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))
#=============================================================================
# 7. Exploratory questions
#=============================================================================

# QUESTION 7.1: How do audio features differ between popular and less popular songs?

 # Create a long format dataset for comparing audio features
 audio_features <- c("danceability", "energy", "acousticness", 
                     "valence", "instrumentalness", "speechiness", "liveness")
 
 audio_comparison <- combined_data %>%
   select(all_of(audio_features), popularity_category) %>%
   pivot_longer(
     cols = all_of(audio_features),
     names_to = "feature",
     values_to = "value"
   )
 
 # Create violin plots to compare distributions
 audio_violin_plot <- ggplot(audio_comparison, 
                             aes(x = feature, y = value, fill = popularity_category)) +
   geom_violin(alpha = 0.7, scale = "width", trim = FALSE) +
   scale_fill_manual(values = c("Popular" = spotify_green, "Less Popular" = "#b3b3b3")) +
   labs(
     title = "Audio Feature Distributions: Popular vs. Less Popular Songs",
     subtitle = "Violin plots show distribution shapes across features",
     x = "Audio Feature",
     y = "Value (0-1 scale)",
     fill = "Popularity Category"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     axis.text.x = element_text(angle = 45, hjust = 1),
     legend.position = "bottom",
     panel.grid.minor = element_blank()
   )
 
 # Calculate mean differences for each feature
 feature_differences <- audio_comparison %>%
   group_by(feature, popularity_category) %>%
   summarize(
     mean_value = mean(value, na.rm = TRUE),
     .groups = "drop"
   ) %>%
   pivot_wider(
     names_from = popularity_category,
     values_from = mean_value
   ) %>%
   mutate(difference = Popular - `Less Popular`,
          percent_difference = (difference / `Less Popular`) * 100)
 
 # Create a plot showing the percent differences
 feature_diff_plot <- ggplot(feature_differences, 
                             aes(x = reorder(feature, difference), y = percent_difference)) +
   geom_col(aes(fill = difference > 0)) +
   scale_fill_manual(values = c("FALSE" = "#D32F2F", "TRUE" = spotify_green)) +
   geom_text(aes(label = paste0(round(percent_difference, 1), "%"),
                 vjust = ifelse(percent_difference >= 0, -0.5, 1.5)),
             color = "black", size = 3.5) +
   labs(
     title = "Percent Difference in Audio Features Between Popular and Less Popular Songs",
     subtitle = "Positive values indicate higher values in popular songs",
     x = "Audio Feature",
     y = "Percent Difference (%)"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     legend.position = "none",
     panel.grid.minor = element_blank()
   ) +
   coord_flip()
 print(audio_violin_plot)
 print(feature_diff_plot)
 print(kable(feature_differences, format = "html")%>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))

# QUESTION 7.2: Is there a relationship between energy and danceability, and does it differ for popular songs?
 # Create a scatter plot with density contours
 energy_dance_plot <- ggplot(combined_data, 
                             aes(x = danceability, y = energy, color = popularity_category)) +
   # Start with just points to show the relationship
   geom_point(alpha = 0.3, size = 0.8) +
   geom_smooth(aes(color = popularity_category), method = "loess", se = TRUE, alpha = 0.2) +
   scale_color_manual(values = c("Popular" = spotify_green, "Less Popular" = "#777777")) +
   scale_fill_manual(values = c("Popular" = spotify_green, "Less Popular" = "#777777")) +
   labs(
     title = "Relationship Between Energy and Danceability",
     subtitle = "Curves show trend lines for each popularity category",
     x = "Danceability",
     y = "Energy",
     color = "Popularity Category"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(face = "bold", size = 14),
     legend.position = "bottom",
     panel.grid.minor = element_blank()
   )
 
 # Print the plot
 print(energy_dance_plot)
 
 # For the correlation table
 print(kable(energy_dance_corr, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = FALSE) %>%
         row_spec(0, bold = TRUE, background = "#f2f2f2"))

 
 
#=============================================================================
# Task 6: Finding Related Songs
#=============================================================================

 # Define popularity threshold
 if(!exists("popularity_threshold")) {
   popularity_threshold <- quantile(combined_data$playlist_appearances, 0.85)
   combined_data <- combined_data %>%
     mutate(is_popular = playlist_appearances >= popularity_threshold,
            popularity_category = ifelse(is_popular, "Popular", "Less Popular"))
 }
 
 # Display the popularity threshold
 cat("Popularity threshold: Songs appearing in", round(popularity_threshold), "or more playlists are considered 'Popular'")
 
 find_anchor_songs <- function(song_data, artist_name, track_name) {
   # Search for songs by artist and track name (case insensitive)
   matches <- song_data %>%
     filter(
       str_to_lower(artist) == str_to_lower(artist_name) |
         str_detect(str_to_lower(artist), str_to_lower(artist_name)) 
     ) %>%
     filter(
       str_to_lower(name) == str_to_lower(track_name) |
         str_detect(str_to_lower(name), str_to_lower(track_name))
     )
   
   if(nrow(matches) == 0) {
     # If no exact match, try more flexible search
     matches <- song_data %>%
       filter(
         str_detect(str_to_lower(artist), str_to_lower(artist_name)) |
           str_detect(str_to_lower(name), str_to_lower(track_name))
       )
   }
   
   # Return the best match (or NA if none found)
   if(nrow(matches) > 0) {
     # Sort by popularity if multiple matches
     best_match <- matches %>% 
       arrange(desc(popularity)) %>% 
       head(1)
     
     return(best_match)
   } else {
     warning(paste("No matches found for", track_name, "by", artist_name))
     return(NULL)
   }
 }
 
 
 # Finding anchor songs
 anchor1 <- find_anchor_songs(combined_data, "Kendrick Lamar", "Humble")
 anchor2 <- find_anchor_songs(combined_data, "Desiigner", "Panda")
 
 # Check if both anchor songs were found
 if(is.null(anchor1) || is.null(anchor2)) {
   stop("One or both anchor songs not found in dataset. Please check artist and track names.")
 }
 
 # Combine anchor songs into a single dataframe
 anchor_songs <- bind_rows(anchor1, anchor2)
 
 # Display anchor songs in a nice table
 anchor_songs %>%
   select(name, artist, popularity, playlist_appearances) %>%
   kable(col.names = c("Title", "Artist", "Spotify Popularity", "Playlist Appearances"),
         caption = "Anchor Songs for Recommendations",
         align = "lccc") %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
 
 
 find_related_songs <- function(song_data, anchor_songs, max_candidates = 100) {
   # Extract relevant audio features for similarity calculations
   audio_features <- c("danceability", "energy", "valence", 
                       "acousticness", "instrumentalness", "speechiness",
                       "liveness", "tempo", "loudness", "key", "mode")
   
   # Check if year column exists in the data
   if("year" %in% colnames(song_data)) {
     has_year_data <- TRUE
   } else {
     warning("Year column not found in data. Year-based similarity will be ignored.")
     has_year_data <- FALSE
   }
   
   # 1. Audio Feature Similarity Heuristic
   #----------------------------------------
   # Normalize features for equal weighting
   song_features <- song_data %>%
     # Include year in the selection only if it exists
     select(track_id_clean, name, artist, popularity_category, all_of(audio_features))
   
   # Add year column if it exists
   if(has_year_data) {
     song_features <- song_features %>%
       bind_cols(year = song_data$year)
   }
   
   # Function to normalize a vector to 0-1 range
   normalize <- function(x) {
     if(all(is.na(x)) || max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
       return(x)
     }
     return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
   }
   
   # Normalize all numeric features
   feature_data <- song_features %>%
     mutate(across(all_of(audio_features), normalize))
   
   # Get anchor features
   anchor_features <- feature_data %>%
     filter(track_id_clean %in% anchor_songs$track_id_clean) %>%
     select(all_of(audio_features))
   
   # Compute average anchor feature values
   avg_anchor_features <- colMeans(anchor_features[audio_features], na.rm = TRUE)
   
   # Calculate Euclidean distance to average anchor features
   feature_data <- feature_data %>%
     rowwise() %>%
     mutate(
       audio_distance = sqrt(sum((c_across(all_of(audio_features)) - avg_anchor_features[audio_features])^2, na.rm = TRUE))
     ) %>%
     ungroup()
   
   # 2. Co-occurrence in Playlists Heuristic
   #----------------------------------------
   # Find playlists containing our anchor songs
   anchor_playlists <- playlist_data %>%
     filter(track_id %in% anchor_songs$track_id_clean) %>%
     distinct(playlist_id)
   
   # Find other songs in these playlists
   cooccurrence_songs <- playlist_data %>%
     filter(playlist_id %in% anchor_playlists$playlist_id) %>%
     anti_join(anchor_songs, by = c("track_id" = "track_id_clean")) %>%
     group_by(track_id) %>%
     summarize(
       cooccurrence_count = n_distinct(playlist_id),
       .groups = "drop"
     )
   
   # Join with main dataset
   feature_data <- feature_data %>%
     left_join(cooccurrence_songs, by = c("track_id_clean" = "track_id")) %>%
     mutate(cooccurrence_count = ifelse(is.na(cooccurrence_count), 0, cooccurrence_count))
   
   # 3. Genre Similarity Heuristic
   #----------------------------------------
   # Since there's no genre column, we'll use artist as a proxy for genre
   anchor_artists <- song_data %>%
     filter(track_id_clean %in% anchor_songs$track_id_clean) %>%
     pull(artist) %>%
     unique()
   
   feature_data <- feature_data %>%
     mutate(genre_match = ifelse(artist %in% anchor_artists, 1, 0))
   
   # 4. Release Year Proximity Heuristic
   #----------------------------------------
   if(has_year_data) {
     anchor_years <- song_data %>%
       filter(track_id_clean %in% anchor_songs$track_id_clean) %>%
       pull(year)
     
     avg_anchor_year <- mean(anchor_years, na.rm = TRUE)
     
     feature_data <- feature_data %>%
       mutate(
         year_distance = abs(year - avg_anchor_year),
         year_proximity = 1 - normalize(year_distance)
       )
   } else {
     # If year data doesn't exist, set year_proximity to a neutral value
     feature_data <- feature_data %>%
       mutate(year_proximity = 0.5)
   }
   
   # 5. Custom Heuristic: Lyrical Theme Similarity via track name keywords
   #---------------------------------------------------------------------
   # Extract keywords from anchor song titles
   anchor_title_words <- anchor_songs %>%
     pull(name) %>%
     str_to_lower() %>%
     str_split("\\s+|[[:punct:]]") %>%
     unlist() %>%
     unique()
   
   # Remove common stop words
   stop_words <- c("the", "a", "an", "and", "but", "or", "for", "nor", "on", "at", 
                   "to", "from", "by", "with", "in", "out", "up", "down", "feat")
   
   anchor_keywords <- anchor_title_words[!anchor_title_words %in% stop_words]
   anchor_keywords <- anchor_keywords[nchar(anchor_keywords) > 1]
   
   # Calculate title keyword matches
   feature_data <- feature_data %>%
     mutate(
       title_match_score = sapply(str_to_lower(name), function(title) {
         words <- unlist(str_split(title, "\\s+|[[:punct:]]"))
         sum(words %in% anchor_keywords) / max(1, length(anchor_keywords))
       })
     )
   
   #=============================================================================
   # Combine all heuristics to generate candidate songs
   #=============================================================================
   
   # Assign weights to each heuristic
   candidates <- feature_data %>%
     mutate(
       # Convert distance to similarity (invert)
       audio_similarity = 1 - normalize(audio_distance),
       # Normalize co-occurrence count
       cooccurrence_score = normalize(cooccurrence_count),
       # Handle NAs for year proximity
       year_proximity = ifelse(is.na(year_proximity), 0, year_proximity),
       
       # Combined score (weighted sum)
       combined_score = (0.35 * audio_similarity) + 
         (0.25 * cooccurrence_score) + 
         (0.15 * genre_match) +
         (0.15 * year_proximity) +
         (0.10 * title_match_score)
     ) %>%
     # Filter out anchor songs
     filter(!track_id_clean %in% anchor_songs$track_id_clean) %>%
     # Remove duplicates
     distinct(track_id_clean, .keep_all = TRUE) %>%
     # Sort by combined score
     arrange(desc(combined_score))
   
   # Get top candidates, ensuring a mix of popular and less popular songs
   top_candidates <- candidates %>%
     head(max_candidates)
   
   # Ensure we have at least 8 "Less Popular" songs
   less_popular_count <- sum(top_candidates$popularity_category == "Less Popular")
   
   if(less_popular_count < 8) {
     # Add more less popular songs if needed
     additional_needed <- 8 - less_popular_count
     
     more_less_popular <- candidates %>%
       filter(popularity_category == "Less Popular") %>%
       filter(!track_id_clean %in% top_candidates$track_id_clean) %>%
       head(additional_needed)
     
     top_candidates <- bind_rows(top_candidates, more_less_popular) %>%
       arrange(desc(combined_score))
   }
   
   # Return the candidates with scores
   return(top_candidates)
 }
 
 # Find candidate songs
 candidate_songs <- find_related_songs(combined_data, anchor_songs, max_candidates = 30)
 
 # Create a summary table of recommendation distribution
 recommendation_summary <- data.frame(
   Category = c("Total Recommendations", "Popular Songs", "Less Popular Songs"),
   Count = c(
     nrow(candidate_songs),
     sum(candidate_songs$popularity_category == "Popular"),
     sum(candidate_songs$popularity_category == "Less Popular")
   )
 )
 
 # Display summary with kable
 recommendation_summary %>%
   kable(caption = "Summary of Recommendation Results", align = "lc") %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
 
 # Display top 10 candidates with kable
 candidate_songs %>% 
   select(name, artist, popularity_category, combined_score) %>%
   head(10) %>%
   mutate(combined_score = round(combined_score, 3)) %>%
   kable(col.names = c("Song Title", "Artist", "Popularity Category", "Recommendation Score"),
         caption = "Top 10 Recommended Songs", 
         align = "llcc") %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
   row_spec(1:3, bold = TRUE, background = "#f8f9fa")
 
 # Prepare data for score breakdown visualization
 top_5 <- candidate_songs %>% 
   head(5) %>%
   select(name, artist, audio_similarity, cooccurrence_score, 
          genre_match, year_proximity, title_match_score)
 
 # Create a table showing the score components
 top_5_scores <- top_5 %>%
   mutate(
     song = paste(artist, "-", name),
     audio_similarity = round(audio_similarity, 2),
     cooccurrence_score = round(cooccurrence_score, 2),
     genre_match = round(genre_match, 2),
     year_proximity = round(year_proximity, 2),
     title_match_score = round(title_match_score, 2)
   ) %>%
   select(song, audio_similarity, cooccurrence_score, genre_match, year_proximity, title_match_score)
 
 # Display score components with kable
 top_5_scores %>%
   kable(col.names = c("Song", "Audio Similarity", "Playlist Co-occurrence", 
                       "Genre Match", "Year Proximity", "Title Similarity"),
         caption = "Score Components for Top 5 Recommendations",
         align = "lccccc") %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
   column_spec(1, bold = TRUE)
 
 # Transform to long format for visualization
 score_breakdown <- top_5 %>%
   mutate(song_label = paste(artist, "-", name)) %>%
   pivot_longer(
     cols = c(audio_similarity, cooccurrence_score, genre_match, 
              year_proximity, title_match_score),
     names_to = "feature",
     values_to = "score"
   )
 
 # Create the score breakdown plot
 ggplot(score_breakdown, aes(x = reorder(song_label, -score), y = score, fill = feature)) +
   geom_bar(stat = "identity", position = "stack") +
   coord_flip() +
   labs(title = "Recommendation Score Breakdown for Top 5 Songs",
        x = "Song",
        y = "Score Component",
        fill = "Similarity\nMeasure") +
   theme_minimal() +
   scale_fill_brewer(palette = "Set2") +
   theme(legend.position = "bottom")
 
 ## **Task- 7 Analyzing the Ultimate Playlist**
 
 # Define a thematic filter for songs about power, ambition, and resilience
 filter_theme_keywords <- function(songs_df) {
   # Keywords related to power, ambition, resilience
   theme_keywords <- c("power", "king", "queen", "boss", "money", "rise", "pride", 
                       "humble", "control", "crown", "throne", "empire", "champion",
                       "fight", "win", "strong", "victory", "panda")
   
   # Score songs based on thematic relevance in title
   songs_df %>%
     mutate(
       thematic_score = sapply(str_to_lower(name), function(title) {
         # Calculate keyword matches in title
         words <- unlist(str_split(title, "\\s+|[[:punct:]]"))
         keyword_matches <- sum(sapply(theme_keywords, function(kw) {
           str_detect(title, kw)
         }))
         
         # Base score on direct matches
         base_score <- min(1, keyword_matches * 0.4)
         return(base_score)
       })
     )
 }
 
 # Define the criteria for the Ultimate Playlist
 ultimate_playlist_curator <- function(anchor_songs, candidate_songs,
                                       playlist_size = 15,
                                       popularity_mix = 0.5,
                                       theme_weight = 0.3) {
   
   # Start with our anchor songs
   playlist <- anchor_songs %>%
     select(track_id_clean, name, artist, popularity_category) %>%
     mutate(source = "Anchor")
   
   # Calculate how many songs we need to add
   songs_needed <- playlist_size - nrow(playlist)
   popular_needed <- round(songs_needed * popularity_mix)
   less_popular_needed <- songs_needed - popular_needed
   
   # Filter out songs already in the playlist
   filtered_candidates <- candidate_songs %>%
     filter(!track_id_clean %in% playlist$track_id_clean)
   
   # Apply thematic filtering
   themed_candidates <- filter_theme_keywords(filtered_candidates)
   
   # Calculate final score combining recommendation score and thematic relevance
   final_candidates <- themed_candidates %>%
     mutate(
       final_score = (combined_score * (1 - theme_weight)) + 
         (thematic_score * theme_weight)
     ) %>%
     arrange(desc(final_score))
   
   # Select songs by popularity category
   popular_songs <- final_candidates %>%
     filter(popularity_category == "Popular") %>%
     head(popular_needed) %>%
     select(track_id_clean, name, artist, popularity_category) %>%
     mutate(source = "Recommendation")
   
   less_popular_songs <- final_candidates %>%
     filter(popularity_category == "Less Popular") %>%
     head(less_popular_needed) %>%
     select(track_id_clean, name, artist, popularity_category) %>%
     mutate(source = "Recommendation")
   
   # Combine all selections
   full_playlist <- bind_rows(playlist, popular_songs, less_popular_songs)
   
   # Join with combined_data to get audio features
   audio_features <- c("danceability", "energy", "valence", 
                       "acousticness", "instrumentalness", "speechiness",
                       "liveness", "tempo", "loudness", "key", "mode")
   
   full_playlist_with_features <- full_playlist %>%
     left_join(combined_data %>% 
                 select(track_id_clean, all_of(audio_features), 
                        popularity, playlist_appearances),
               by = "track_id_clean")
   
   return(full_playlist_with_features)
 }
 
 # Create "The Ultimate Playlist"
 ultimate_playlist <- ultimate_playlist_curator(
   anchor_songs = anchor_songs,
   candidate_songs = candidate_songs,
   playlist_size = 15,
   popularity_mix = 0.8,  # 80% popular, 20% less popular
   theme_weight = 0.3     # 30% thematic relevance in scoring
 )
 
 # Assign our playlist a creative name
 playlist_name <- "POWER PARADOX: From Humble Beginnings to Panda Status"
 
 kable(ultimate_playlist %>% 
         head(15), 
       caption = "The Ultimate Playlist: Power Paradox🎧")
 
 
 # Function to calculate diversity metrics
 calculate_diversity_metrics <- function(playlist) {
   # Calculate artist diversity
   artist_diversity <- n_distinct(playlist$artist) / nrow(playlist)
   
   # Calculate variety in audio features
   audio_features <- c("danceability", "energy", "valence", 
                       "acousticness", "instrumentalness", "speechiness")
   
   feature_variance <- playlist %>%
     summarize(across(all_of(audio_features), sd, na.rm = TRUE)) %>%
     unlist() %>%
     mean(na.rm = TRUE)
   
   # Calculate popularity spread
   popularity_spread <- sd(playlist$popularity, na.rm = TRUE) / 
     mean(playlist$popularity, na.rm = TRUE)
   
   # Display diversity metrics
   cat("Playlist Diversity Metrics:\n")
   cat("Artist Diversity:", round(100 * diversity_metrics$artist_diversity, 1), "%\n")
   cat("Audio Feature Variance:", round(diversity_metrics$feature_variance, 3), "\n")
   cat("Popularity Spread:", round(diversity_metrics$popularity_spread, 3), "\n")
 }
 
 
 # Create a function for our unique "orbit" visualization
 create_orbit_visualization <- function(playlist, playlist_name) {
   # Extract key audio features
   features <- c("danceability", "energy", "valence")
   
   # Get anchor songs and recommendations
   anchor_songs <- playlist %>% filter(source == "Anchor")
   recommendation_songs <- playlist %>% filter(source == "Recommendation")
   
   # Calculate the center point (average of anchor songs)
   center_point <- anchor_songs %>%
     summarize(across(all_of(features), mean, na.rm = TRUE))
   
   # Calculate distances from center for each song
   playlist_with_distance <- playlist %>%
     rowwise() %>%
     mutate(
       distance_from_center = sqrt(sum((c_across(all_of(features)) - 
                                          unlist(center_point)[features])^2, 
                                       na.rm = TRUE))
     ) %>%
     ungroup()
   
   # Make sure we have valid distances
   max_distance <- max(playlist_with_distance$distance_from_center, na.rm = TRUE)
   if (is.infinite(max_distance) || max_distance <= 0) {
     max_distance <- 1  # Default if no valid distances
   }
   
   # Add orbit plotting data
   playlist_with_distance <- playlist_with_distance %>%
     mutate(
       # Normalize distance to 0.2-1 range for better visualization
       orbit_radius = 0.2 + 0.8 * (distance_from_center / max_distance),
       # Create angle for positioning (0-360 degrees)
       angle = row_number() * (360 / n()),
       # Convert to radians for plotting
       rad_angle = angle * (pi / 180),
       # Calculate x and y coordinates
       x_pos = orbit_radius * cos(rad_angle),
       y_pos = orbit_radius * sin(rad_angle)
     )
   
   # Create a data frame for orbital paths
   orbit_paths <- data.frame(
     x0 = 0,
     y0 = 0,
     radius = c(0.2, 0.4, 0.6, 0.8, 1.0),
     name = c("Core", "Inner", "Middle", "Outer", "Edge")
   )
   
   # Calculate the energy flow curve
   flow_data <- playlist_with_distance %>%
     arrange(angle) %>%
     mutate(
       # Scale energy for visualization
       flow_radius = ifelse(is.na(energy), 0.1, 0.1 + energy * 0.15),
       flow_x = (orbit_radius + flow_radius) * cos(rad_angle),
       flow_y = (orbit_radius + flow_radius) * sin(rad_angle)
     )
   
   # Generate the plot with Spotify colors
   p <- ggplot() +
     # Add orbital rings
     geom_circle(data = orbit_paths, aes(x0 = x0, y0 = y0, r = radius),
                 color = spotify_colors$grey_light, linetype = "dashed", alpha = 0.5) +
     # Add song points
     geom_point(
       data = playlist_with_distance, 
       aes(x = x_pos, y = y_pos, 
           size = popularity,
           color = popularity_category),
       alpha = 0.8
     ) +
     # Add song labels
     geom_text(
       data = playlist_with_distance,
       aes(x = x_pos * 1.1, y = y_pos * 1.1, 
           label = str_trunc(name, 15), 
           color = popularity_category),
       size = 3, hjust = 0.5, vjust = 0.5, check_overlap = TRUE
     ) +
     # Add energy flow line
     geom_path(
       data = flow_data,
       aes(x = flow_x, y = flow_y),
       color = spotify_colors$green, size = 1, alpha = 0.6
     ) +
     # Center point (anchor songs average)
     geom_point(aes(x = 0, y = 0), color = spotify_colors$green, size = 5, shape = 8) +
     # Add theme style with Spotify colors
     scale_color_manual(values = c("Popular" = spotify_colors$green, "Less Popular" = spotify_colors$grey_dark)) +
     scale_size_continuous(range = c(3, 8)) +
     # Use Spotify-inspired dark theme
     theme_minimal() +
     theme(
       panel.grid = element_blank(),
       panel.background = element_rect(fill = spotify_colors$black),
       plot.background = element_rect(fill = spotify_colors$black),
       text = element_text(color = spotify_colors$white),
       legend.text = element_text(color = spotify_colors$white),
       legend.title = element_text(color = spotify_colors$white),
       plot.title = element_text(color = spotify_colors$green, size = 16, face = "bold"),
       plot.subtitle = element_text(color = spotify_colors$white, size = 12),
       axis.text = element_blank(),
       axis.title = element_blank()
     ) +
     # Set equal aspect ratio for circles
     coord_fixed() +
     # Add title and caption
     labs(
       title = paste("Why", playlist_name, "Is the Ultimate Playlist"),
       subtitle = "A Statistical Orbital Analysis of Musical Power",
       caption = "Larger points = more popular songs | Color = popularity category | Distance from center = audio feature similarity",
       color = "Popularity",
       size = "Spotify Popularity"
     )
   
   return(p)
 }
 
 # Create and display the orbit visualization  
 orbit_plot <- create_orbit_visualization(ultimate_playlist, playlist_name)
 print(orbit_plot)
 
 # Create radar chart for audio features
 radar_chart <- function(playlist) {
   # Prepare data
   features <- c("danceability", "energy", "valence", 
                 "acousticness", "instrumentalness", "speechiness")
   
   # Calculate average for each feature by popularity category
   radar_data <- playlist %>%
     group_by(popularity_category) %>%
     summarize(across(all_of(features), mean, na.rm = TRUE)) %>%
     pivot_longer(
       cols = all_of(features),
       names_to = "feature",
       values_to = "value"
     )
   
   # Add factor levels to ensure consistent order
   radar_data$feature <- factor(radar_data$feature, levels = features)
   
   # Create the radar chart with Spotify colors
   ggplot(radar_data, aes(x = feature, y = value, group = popularity_category, fill = popularity_category)) +
     geom_polygon(aes(color = popularity_category), alpha = 0.3) +
     geom_line(size = 1) +
     geom_point(size = 3) +
     coord_polar() +
     scale_fill_manual(values = c("Popular" = spotify_colors$green, "Less Popular" = spotify_colors$grey_dark)) +
     scale_color_manual(values = c("Popular" = spotify_colors$green, "Less Popular" = spotify_colors$grey_dark)) +
     theme_minimal() +
     theme(
       panel.background = element_rect(fill = spotify_colors$black),
       plot.background = element_rect(fill = spotify_colors$black),
       text = element_text(color = spotify_colors$white),
       axis.text.x = element_text(color = spotify_colors$grey_light),
       axis.text.y = element_blank(),
       panel.grid.major = element_line(color = spotify_colors$grey_dark, linewidth = 0.2),
       legend.text = element_text(color = spotify_colors$white),
       legend.title = element_text(color = spotify_colors$white),
       plot.title = element_text(color = spotify_colors$green, face = "bold")
     ) +
     labs(
       title = "Hidden Gems Analysis",
       subtitle = "How Less Popular Songs Complement The Ultimate Playlist",
       fill = "Popularity Category",
       color = "Popularity Category"
     )
 }
 
 # Create and display the radar chart
 radar_plot <- radar_chart(ultimate_playlist)
 print(radar_plot)
 
 # Create a popularity bar chart with Spotify colors
 create_popularity_chart <- function(playlist) {
   ggplot(playlist, aes(x = reorder(str_trunc(name, 20), popularity), 
                        y = popularity, 
                        fill = popularity_category)) +
     geom_col() +
     coord_flip() +
     scale_fill_manual(values = c("Popular" = spotify_colors$green, "Less Popular" = spotify_colors$grey_dark)) +
     theme_minimal() +
     theme(
       panel.background = element_rect(fill = spotify_colors$black),
       plot.background = element_rect(fill = spotify_colors$black),
       text = element_text(color = spotify_colors$white),
       axis.text = element_text(color = spotify_colors$grey_light),
       legend.position = "bottom",
       panel.grid.major = element_line(color = spotify_colors$grey_dark, linewidth = 0.2),
       panel.grid.major.y = element_blank(),
       plot.title = element_text(color = spotify_colors$green, face = "bold")
     ) +
     labs(
       title = "The Balanced Power Structure",
       subtitle = "Strategic Inclusion of Lesser-Known Tracks",
       x = NULL,
       y = "Spotify Popularity Score"
     )
 }
 
 # Create and display the popularity chart
 popularity_plot <- create_popularity_chart(ultimate_playlist)
 print(popularity_plot)