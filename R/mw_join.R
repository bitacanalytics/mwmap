#' Join User Data to Malawi Map Geometry
#'
#' Merges user-provided district-level data with Malawi spatial data frames.
#' Handles name matching, case normalization, and provides detailed feedback
#' on matching success.
#'
#' @param data Data frame containing district-level data to join.
#' @param district_col Character. Name of the column in `data` containing
#'   district names. Default: "district".
#' @param map sf object. The map data to join with. Default: mw_level_2.
#' @param by Character. Optional custom join column(s). If NULL, automatically
#'   matches based on district names. Default: NULL.
#' @param name_clean Function. Function to clean/standardize district names.
#'   Default: [mw_clean_names()] (converts to title case, trims spaces).
#' @param map_district_col Character. Column name in map containing district
#'   names. Default: "ADM2_EN" (for level 2).
#' @param keep_all Logical. Keep all map rows (left join) or only matched rows
#'   (inner join)? Default: TRUE (left join).
#' @param verbose Logical. Print detailed matching report. Default: TRUE.
#' @param quiet Logical. Suppress all messages. Overrides verbose. Default: FALSE.
#' @param ... Additional arguments passed to [merge()] or [dplyr::left_join()].
#'
#' @return An sf object with the map geometry and joined data attributes.
#'
#' @examples
#' \donttest{
#' # Basic join
#' df <- data.frame(
#'   district = c("Lilongwe", "Blantyre", "Mzimba"),
#'   cases = c(120, 200, 85)
#' )
#' 
#' joined <- mw_join(df)
#' 
#' # Join with custom district column
#' df2 <- data.frame(
#'   District_Name = c("Lilongwe", "Blantyre"),
#'   population = c(2300000, 1800000)
#' )
#' 
#' joined2 <- mw_join(df2, district_col = "District_Name")
#' 
#' # Join with Traditional Authorities level
#' ta_data <- data.frame(
#'   ta = c("Chitipa", "Mwaulambia", "Mabuka"),
#'   value = c(45, 67, 89)
#' )
#' 
#' joined3 <- mw_join(ta_data, 
#'                    district_col = "ta", 
#'                    map = mw_level_3,
#'                    map_district_col = "ADM3_EN")
#' 
#' # Get matching report without creating object
#' mw_join(df, verbose = TRUE)
#' }
#'
#' @importFrom dplyr left_join inner_join
#' @importFrom stats setNames
#' @importFrom sf st_as_sf st_geometry
#' @export
mw_join <- function(
  data,
  district_col = "district",
  map = mw_level_2,
  by = NULL,
  name_clean = mw_clean_names,
  map_district_col = "ADM2_EN",
  keep_all = TRUE,
  verbose = TRUE,
  quiet = FALSE,
  ...
) {
  
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (!district_col %in% names(data)) {
    stop("Column '", district_col, "' not found in data. ",
         "Available columns: ", paste(names(data), collapse = ", "))
  }
  
  if (!map_district_col %in% names(map)) {
    stop("Column '", map_district_col, "' not found in map data. ",
         "Available columns: ", paste(names(map), collapse = ", "))
  }
  
  # Quiet mode overrides verbose
  if (quiet) verbose <- FALSE
  
  # Make a copy to avoid modifying original
  data_clean <- data
  
  # Clean district names in user data
  if (!is.null(name_clean)) {
    data_clean[[district_col]] <- name_clean(data_clean[[district_col]])
  }
  
  # Get unique districts from user data
  user_districts <- unique(data_clean[[district_col]])
  user_districts <- user_districts[!is.na(user_districts)]
  
  # Get map districts
  map_districts <- unique(map[[map_district_col]])
  
  # Find matches
  matched <- user_districts[user_districts %in% map_districts]
  unmatched <- user_districts[!user_districts %in% map_districts]
  
  # Calculate match rate
  match_rate <- length(matched) / length(user_districts) * 100
  
  # Verbose output
  if (verbose) {
    message("\n=== District Matching Report ===")
    message("Total districts in data: ", length(user_districts))
    message("Successfully matched: ", length(matched), 
            " (", round(match_rate, 1), "%)")
    
    if (length(unmatched) > 0) {
      message("\nUnmatched districts (", length(unmatched), "):")
      message(paste(unmatched, collapse = ", "))
      
      # Suggest closest matches
      message("\nDid you mean?")
      for (u in unmatched) {
        suggestions <- mw_suggest_matches(u, map_districts)
        if (length(suggestions) > 0) {
          message("  '", u, "' -> ", paste(suggestions, collapse = ", "))
        }
      }
    }
    
    if (length(matched) > 0) {
      message("\nMatched districts:")
      message(paste(matched, collapse = ", "))
    }
    message("================================")
  }
  
  # Prepare for join
  if (is.null(by)) {
    # Set up join columns
    join_cols <- stats::setNames(district_col, map_district_col)
    
    # Perform join
    if (keep_all) {
      result <- dplyr::left_join(
        map,
        data_clean,
        by = join_cols,
        ...
      )
    } else {
      result <- dplyr::inner_join(
        map,
        data_clean,
        by = join_cols,
        ...
      )
    }
  } else {
    # Custom join columns
    result <- merge(map, data_clean, by = by, all.x = keep_all, ...)
    if (inherits(map, "sf")) {
      result <- sf::st_as_sf(result)
    }
  }
  
  # Check for duplicate geometries after join
  if (any(duplicated(sf::st_geometry(result)))) {
    warning("Join resulted in duplicate geometries. ",
            "Check for multiple matches in your data.")
  }
  
  return(result)
}

#' Clean District Names for Matching
#'
#' Standardizes district names to improve matching success when joining
#' user data to Malawi map geometries. Converts to title case, removes
#' trailing "District" or "TA" suffixes, and normalises known shapefile
#' name variants (e.g. "Nkhata Bay" -> "Nkhatabay", "Mzuzu" -> "Mzuzu City").
#'
#' @param x Character vector of district names.
#'
#' @return A cleaned character vector the same length as \code{x}.
#'
#' @examples
#' mw_clean_names(c("lilongwe", "BLANTYRE district", "nkhata bay"))
#'
#' @export
mw_clean_names <- function(x) {
  x <- as.character(x)
  x <- trimws(x)                        # Remove leading/trailing spaces
  x <- gsub("\\s+", " ", x)             # Normalize multiple spaces
  x <- tools::toTitleCase(tolower(x))   # Convert to Title Case
  x <- gsub(" District$", "", x)        # Remove trailing "District"
  x <- gsub(" T/A$| TA$", "", x)        # Remove trailing TA indicators

  # Known shapefile name normalisations
  x <- gsub("^Nkhata Bay$",  "Nkhatabay",  x)  # shapefile omits the space
  x <- gsub("^Nkhatabay$",   "Nkhatabay",  x)  # already correct form
  x <- gsub("^Mzuzu$",       "Mzuzu City", x)  # common short form

  return(x)
}

#' Suggest Close Matches for Unmatched District Names
#'
#' Returns the closest matching district names from a candidate list, using
#' edit-distance (Levenshtein) to handle typos and minor spelling differences.
#' Called automatically by \code{\link{mw_join}} when unmatched districts are found.
#'
#' @param x Character. An unmatched district name.
#' @param candidates Character vector of valid district names to search.
#' @param n Integer. Number of suggestions to return. Default: 3.
#'
#' @return A character vector of up to \code{n} suggested matches.
#'
#' @examples
#' mw_suggest_matches("Lilongwee", mw_districts())
#'
#' @importFrom utils adist
#' @export
mw_suggest_matches <- function(x, candidates, n = 3) {
  if (!requireNamespace("utils", quietly = TRUE)) {
    return(character(0))
  }
  
  # Calculate string distances
  distances <- utils::adist(x, candidates, ignore.case = TRUE)[1, ]
  
  # Get closest matches
  closest <- order(distances)[1:min(n, length(candidates))]
  
  return(candidates[closest])
}