#' Get Malawi District Names
#'
#' Returns a clean vector of Malawi district names with various filtering options.
#' Useful for validation, selection interfaces, and documentation.
#'
#' @param region Optional region filter. Accepts:
#'   * Full names: "Northern", "Central", "Southern"
#'   * Shortcuts: "n", "c", "s" (case insensitive)
#'   * NULL: returns all districts (default)
#' @param type Character. Type of names to return:
#'   * "standard": Standard district names (default)
#'   * "admin": Administrative names as in shapefile
#'   * "short": Short form names
#'   * "all": Return full data frame with all information
#' @param sorted Logical. Sort alphabetically? Default: TRUE.
#' @param include_ta Logical. Include Traditional Authorities? Default: FALSE.
#' @param quiet Logical. Suppress messages? Default: FALSE.
#'
#' @return If type = "all", returns a data frame with district information.
#'   Otherwise, returns a character vector of district names.
#'
#' @examples
#' \donttest{
#' # All districts
#' mw_districts()
#' 
#' # Southern region districts
#' mw_districts("Southern")
#' mw_districts("s")
#' 
#' # Get all information
#' mw_districts(type = "all")
#' }
#'
#' @export
mw_districts <- function(
  region = NULL,
  type = c("standard", "admin", "short", "all"),
  sorted = TRUE,
  include_ta = FALSE,
  quiet = FALSE
) {
  
  # Match type argument
  type <- match.arg(type)
  
  # First, try to get data from package datasets
  district_data <- NULL
  data_source <- "none"
  
  # Try to get mw_level_2
  if (include_ta) {
    if (exists("mw_level_3") && !is.null(mw_level_3)) {
      district_data <- mw_level_3
      data_source <- "mw_level_3"
      if (!quiet) message("Using mw_level_3 for TA data")
    }
  } else {
    if (exists("mw_level_2") && !is.null(mw_level_2)) {
      district_data <- mw_level_2
      data_source <- "mw_level_2"
      if (!quiet) message("Using mw_level_2 for district data")
    } else if (exists("malawi_data") && !is.null(malawi_data)) {
      district_data <- malawi_data
      data_source <- "malawi_data"
      if (!quiet) message("Using malawi_data as fallback")
    }
  }
  
  # If we have data, extract district names
  if (!is.null(district_data)) {
    
    # Try to find district name columns
    possible_name_cols <- c("ADM2_EN", "ADM1_EN", "NAME_2", "NAME_1", 
                           "name", "District", "DISTRICT", "admin_name")
    possible_region_cols <- c("ADM1_EN", "REGION", "NAME_1", "region", "Region")
    
    # Find the first existing column for names
    name_col <- NULL
    for (col in possible_name_cols) {
      if (col %in% names(district_data)) {
        name_col <- col
        break
      }
    }
    
    # Find the first existing column for regions
    region_col <- NULL
    for (col in possible_region_cols) {
      if (col %in% names(district_data)) {
        region_col <- col
        break
      }
    }
    
    # Extract data based on what we found
    if (!is.null(name_col)) {
      # Get unique names (for districts) or all (for TAs)
      if (include_ta) {
        # For TAs, keep all rows
        name_values <- as.character(district_data[[name_col]])
        if (!is.null(region_col)) {
          region_values <- as.character(district_data[[region_col]])
        } else {
          region_values <- rep("Unknown", length(name_values))
        }
      } else {
        # For districts, get unique values
        unique_indices <- !duplicated(district_data[[name_col]])
        name_values <- as.character(district_data[[name_col]][unique_indices])
        if (!is.null(region_col)) {
          region_values <- as.character(district_data[[region_col]][unique_indices])
        } else {
          region_values <- rep("Unknown", length(name_values))
        }
      }
      
      # Clean the names
      name_values <- trimws(name_values)
      name_values <- name_values[!is.na(name_values) & name_values != ""]
      
      # Create standard names
      standard_names <- tools::toTitleCase(tolower(name_values))
      
      # Create short names (remove common suffixes)
      short_names <- gsub(" District$| T/A$| TA$| Traditional Authority$", 
                         "", standard_names)
      short_names <- trimws(short_names)
      
      # Clean region names if they exist
      if (!is.null(region_col) && length(region_values) == length(name_values)) {
        region_values <- trimws(as.character(region_values))
        region_values <- tools::toTitleCase(tolower(region_values))
        region_values <- gsub(" Region$", "", region_values)
      } else {
        region_values <- rep("Unknown", length(name_values))
      }
      
      # Create the result data frame
      result_df <- data.frame(
        standard = standard_names,
        admin = name_values,
        short = short_names,
        region = region_values,
        stringsAsFactors = FALSE
      )
      
      # Remove any rows with NA or empty standard names
      result_df <- result_df[!is.na(result_df$standard) & result_df$standard != "", ]
      
    } else {
      # No name column found, use fallback
      if (!quiet) message("No district name column found, using fallback list")
      result_df <- create_fallback_district_data(include_ta)
      data_source <- "fallback"
    }
    
  } else {
    # No data found, use fallback
    if (!quiet) message("No district data found, using built-in fallback list")
    result_df <- create_fallback_district_data(include_ta)
    data_source <- "fallback"
  }
  
  # Filter by region if specified
  if (!is.null(region) && nrow(result_df) > 0) {
    
    # Normalize region input
    region_input <- tolower(region)
    
    # Region mapping
    region_map <- list(
      northern = c("n", "north", "northern"),
      central = c("c", "central"),
      southern = c("s", "south", "southern")
    )
    
    # Convert input to standard region names
    mapped_regions <- character(0)
    for (r in region_input) {
      if (r %in% unlist(region_map)) {
        for (std_region in names(region_map)) {
          if (r %in% region_map[[std_region]]) {
            mapped_regions <- c(mapped_regions, tools::toTitleCase(std_region))
            break
          }
        }
      } else {
        mapped_regions <- c(mapped_regions, tools::toTitleCase(r))
      }
    }
    mapped_regions <- unique(mapped_regions)
    
    # Filter the data frame
    result_df$region_lower <- tolower(result_df$region)
    result_df_filtered <- result_df[result_df$region_lower %in% tolower(mapped_regions), ]
    result_df_filtered$region_lower <- NULL
    
    if (nrow(result_df_filtered) == 0) {
      if (!quiet) {
        warning("No districts found for specified region(s). Showing all districts.")
      }
    } else {
      result_df <- result_df_filtered
      if (!quiet) {
        message("Filtered to ", nrow(result_df), " district(s)")
      }
    }
  }
  
  # Remove duplicates if any
  result_df <- result_df[!duplicated(result_df$standard), ]
  
  # Sort if requested
  if (sorted && nrow(result_df) > 0) {
    result_df <- result_df[order(result_df$standard), ]
  }
  
  # Return based on type
  if (type == "all") {
    return(result_df)
    
  } else if (type == "standard") {
    result <- result_df$standard
    
  } else if (type == "admin") {
    result <- result_df$admin
    
  } else if (type == "short") {
    result <- result_df$short
  }
  
  # Remove any NA or empty values
  result <- result[!is.na(result) & result != ""]
  
  return(result)
}

#' Create fallback district data when package data is unavailable
#'
#' @param include_ta Logical. Include Traditional Authorities?
#' @return Data frame with district information
#' @keywords internal
create_fallback_district_data <- function(include_ta = FALSE) {
  
  if (include_ta) {
    # For TAs, we don't have a complete list, so return a message
    warning("Traditional Authorities data not available in fallback mode")
    return(data.frame(
      standard = character(0),
      admin = character(0),
      short = character(0),
      region = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Complete list of Malawi districts by region
  fallback_data <- data.frame(
    standard = c(
      # Northern Region
      "Chitipa", "Karonga", "Likoma", "Mzimba", "Nkhata Bay", "Rumphi",
      # Central Region
      "Dedza", "Dowa", "Kasungu", "Lilongwe", "Mchinji", "Nkhotakota",
      "Ntcheu", "Ntchisi", "Salima",
      # Southern Region
      "Balaka", "Blantyre", "Chikwawa", "Chiradzulu", "Machinga",
      "Mangochi", "Mulanje", "Mwanza", "Nsanje", "Neno", "Phalombe",
      "Thyolo", "Zomba"
    ),
    admin = c(
      "Chitipa", "Karonga", "Likoma", "Mzimba", "Nkhata Bay", "Rumphi",
      "Dedza", "Dowa", "Kasungu", "Lilongwe", "Mchinji", "Nkhotakota",
      "Ntcheu", "Ntchisi", "Salima",
      "Balaka", "Blantyre", "Chikwawa", "Chiradzulu", "Machinga",
      "Mangochi", "Mulanje", "Mwanza", "Nsanje", "Neno", "Phalombe",
      "Thyolo", "Zomba"
    ),
    short = c(
      "Chitipa", "Karonga", "Likoma", "Mzimba", "Nkhata Bay", "Rumphi",
      "Dedza", "Dowa", "Kasungu", "Lilongwe", "Mchinji", "Nkhotakota",
      "Ntcheu", "Ntchisi", "Salima",
      "Balaka", "Blantyre", "Chikwawa", "Chiradzulu", "Machinga",
      "Mangochi", "Mulanje", "Mwanza", "Nsanje", "Neno", "Phalombe",
      "Thyolo", "Zomba"
    ),
    region = c(
      rep("Northern", 6),
      rep("Central", 9),
      rep("Southern", 13)
    ),
    stringsAsFactors = FALSE
  )
  
  return(fallback_data)
}

#' Simple version that always works
#'
#' @param region Optional region filter
#' @return Character vector of district names
#' @export
mw_districts_simple <- function(region = NULL) {
  
  # Complete list of Malawi districts
  all_districts <- c(
    "Chitipa", "Karonga", "Likoma", "Mzimba", "Nkhata Bay", "Rumphi",
    "Dedza", "Dowa", "Kasungu", "Lilongwe", "Mchinji", "Nkhotakota", 
    "Ntcheu", "Ntchisi", "Salima",
    "Balaka", "Blantyre", "Chikwawa", "Chiradzulu", "Machinga", 
    "Mangochi", "Mulanje", "Mwanza", "Nsanje", "Neno", "Phalombe", 
    "Thyolo", "Zomba"
  )
  
  region_map <- data.frame(
    district = all_districts,
    region = c(
      rep("Northern", 6),
      rep("Central", 9),
      rep("Southern", 13)
    ),
    stringsAsFactors = FALSE
  )
  
  if (is.null(region)) {
    return(sort(all_districts))
  }
  
  # Normalize region input
  region_lower <- tolower(region)
  
  # Map to standard region names
  if (region_lower %in% c("n", "north", "northern")) {
    region_std <- "Northern"
  } else if (region_lower %in% c("c", "central")) {
    region_std <- "Central"
  } else if (region_lower %in% c("s", "south", "southern")) {
    region_std <- "Southern"
  } else {
    region_std <- tools::toTitleCase(region)
  }
  
  result <- region_map$district[region_map$region == region_std]
  
  if (length(result) == 0) {
    warning("No districts found for region: ", region)
    return(character(0))
  }
  
  return(sort(result))
}
