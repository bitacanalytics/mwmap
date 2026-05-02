#' Join Data to Malawi Boundaries
#'
#' Join a regular data frame to Malawi administrative boundary geometries. The
#' function is level-aware, so the default join key changes automatically for
#' country, region, district, and Traditional Authority maps.
#'
#' @param data A data frame containing values to map.
#' @param unit_col Column in `data` containing the administrative unit names.
#'   May be quoted or unquoted. Defaults to `country`, `region`, `district`, or
#'   `ta` depending on `level`.
#' @param level Administrative level: `0`/`"country"`, `1`/`"region"`,
#'   `2`/`"district"`, or `3`/`"ta"`.
#' @param map Optional sf object to join to. Defaults to the corresponding
#'   object from \pkg{mwmapdata}.
#' @param map_col Column in `map` containing administrative unit names. Defaults
#'   to the correct ADM column for `level`.
#' @param keep_all If `TRUE`, keep all map features and attach matching values.
#'   If `FALSE`, keep only matched features.
#' @param unmatched One of `"message"`, `"warning"`, `"error"`, or `"ignore"`.
#'   Controls how unmatched input names are reported.
#' @param quiet Suppress matching messages.
#' @param district_col Deprecated alias for `unit_col`.
#' @param name_clean Deprecated. Name matching now uses mwmap's internal
#'   normalisation.
#' @param by Optional explicit join specification passed to dplyr joins.
#' @param verbose Deprecated alias for `!quiet`.
#' @param ... Passed to dplyr joins.
#'
#' @return An sf object with user columns joined to Malawi geometries.
#' @examples
#' \donttest{
#' district_data <- data.frame(
#'   district = c("Lilongwe", "Blantyre", "Mzuzu"),
#'   cases = c(120, 80, 35)
#' )
#' mw_join(district_data)
#'
#' ta_data <- data.frame(
#'   ta = c("Mabuka", "Mwaulambia"),
#'   coverage = c(72, 64)
#' )
#' mw_join(ta_data, level = "ta")
#' }
#' @importFrom dplyr left_join inner_join
#' @importFrom rlang ensym as_string
#' @importFrom sf st_as_sf st_geometry
#' @importFrom stats setNames
#' @export
mw_join <- function(
  data,
  unit_col,
  level = 2,
  map = NULL,
  map_col = NULL,
  keep_all = TRUE,
  unmatched = c("message", "warning", "error", "ignore"),
  quiet = FALSE,
  district_col = NULL,
  name_clean = NULL,
  by = NULL,
  verbose = NULL,
  ...
) {
  if (missing(data)) {
    stop("`data` is required.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  level <- mw_level_key(level)
  map <- map %||% mw_map_data(level)
  map_col <- map_col %||% mw_name_column(level)
  unmatched <- match.arg(unmatched)

  if (!is.null(verbose)) {
    quiet <- !isTRUE(verbose)
  }

  if (!is.null(district_col) && missing(unit_col)) {
    unit_col <- district_col
  }

  unit_col <- if (missing(unit_col) || is.null(unit_col)) {
    mw_default_unit_col(level)
  } else {
    mw_capture_col(substitute(unit_col), parent.frame())
  }

  if (!unit_col %in% names(data)) {
    stop(
      "Column `", unit_col, "` was not found in `data`. Available columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }
  if (!map_col %in% names(map)) {
    stop(
      "Column `", map_col, "` was not found in `map`. Available columns: ",
      paste(names(map), collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(by)) {
    result <- if (keep_all) {
      dplyr::left_join(map, data, by = by, ...)
    } else {
      dplyr::inner_join(map, data, by = by, ...)
    }
    return(sf::st_as_sf(result))
  }

  data_clean <- as.data.frame(data)
  data_clean$.mw_join_key <- mw_clean_key(data_clean[[unit_col]])

  map_clean <- map
  map_clean$.mw_join_key <- mw_clean_key(map_clean[[map_col]])

  user_keys <- unique(data_clean$.mw_join_key[!is.na(data_clean$.mw_join_key)])
  map_keys <- unique(map_clean$.mw_join_key[!is.na(map_clean$.mw_join_key)])
  unmatched_keys <- setdiff(user_keys, map_keys)

  if (length(unmatched_keys) > 0L && unmatched != "ignore") {
    unmatched_values <- unique(data_clean[[unit_col]][data_clean$.mw_join_key %in% unmatched_keys])
    msg <- paste0(
      "Could not match ", length(unmatched_values), " ",
      mw_level_label(level), " name(s): ",
      paste(unmatched_values, collapse = ", ")
    )
    suggestions <- mw_join_suggestions(unmatched_values, map[[map_col]])
    if (length(suggestions) > 0L) {
      msg <- paste0(msg, "\nClosest matches:\n", paste(suggestions, collapse = "\n"))
    }
    if (unmatched == "error") {
      stop(msg, call. = FALSE)
    }
    if (unmatched == "warning") {
      warning(msg, call. = FALSE)
    } else if (!quiet) {
      message(msg)
    }
  }

  duplicate_keys <- unique(data_clean$.mw_join_key[duplicated(data_clean$.mw_join_key)])
  duplicate_keys <- duplicate_keys[!is.na(duplicate_keys) & duplicate_keys != ""]
  if (length(duplicate_keys) > 0L) {
    warning(
      "Some input unit names occur more than once. The join may duplicate map ",
      "features: ", paste(duplicate_keys, collapse = ", "),
      call. = FALSE
    )
  }

  result <- if (keep_all) {
    dplyr::left_join(map_clean, data_clean, by = ".mw_join_key", ...)
  } else {
    dplyr::inner_join(map_clean, data_clean, by = ".mw_join_key", ...)
  }

  result$.mw_join_key <- NULL
  sf::st_as_sf(result)
}

mw_join_suggestions <- function(unmatched, candidates, n = 3) {
  candidates <- unique(as.character(candidates))
  candidates <- candidates[!is.na(candidates)]
  if (length(candidates) == 0L) {
    return(character(0))
  }

  vapply(
    unmatched,
    function(x) {
      suggestions <- mw_suggest_matches(x, candidates, n = n)
      paste0("  ", x, " -> ", paste(suggestions, collapse = ", "))
    },
    character(1)
  )
}

#' Clean Malawi Administrative Names
#'
#' Standardise names for display and backwards-compatible workflows. For joins,
#' `mw_join()` uses a stricter internal key that is robust to punctuation, case,
#' and common suffixes such as "District" and "TA".
#'
#' @param x Character vector of names.
#' @return A character vector.
#' @examples
#' mw_clean_names(c("lilongwe district", "Nkhata Bay", "T/A Mabuka"))
#' @export
mw_clean_names <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("^T/A[[:space:]]+|^TA[[:space:]]+", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+(District|Region|Traditional Authority|TA)$", "",
            x, ignore.case = TRUE)
  x <- tools::toTitleCase(tolower(x))
  x <- gsub("^Nkhata Bay$", "Nkhatabay", x)
  x <- gsub("^Mzuzu$", "Mzuzu City", x)
  x
}

#' Suggest Close Malawi Name Matches
#'
#' @param x Character. Name to match.
#' @param candidates Character vector of valid names.
#' @param n Number of suggestions.
#' @return Character vector of suggested names.
#' @examples
#' mw_suggest_matches("Lilongwee", mw_districts())
#' @importFrom utils adist
#' @export
mw_suggest_matches <- function(x, candidates, n = 3) {
  candidates <- unique(as.character(candidates))
  candidates <- candidates[!is.na(candidates)]
  if (length(candidates) == 0L) {
    return(character(0))
  }

  distances <- utils::adist(mw_clean_key(x), mw_clean_key(candidates))[1, ]
  candidates[order(distances)][seq_len(min(n, length(candidates)))]
}
