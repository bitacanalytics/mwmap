# Core helpers for the mwmap public API.

`%||%` <- function(x, y) if (is.null(x)) y else x

mw_level_key <- function(level) {
  if (is.character(level)) {
    level <- tolower(level)
    aliases <- c(
      country = 0L, national = 0L, region = 1L, regions = 1L,
      district = 2L, districts = 2L, ta = 3L, tas = 3L,
      traditional_authority = 3L, traditional_authorities = 3L
    )
    if (level %in% names(aliases)) {
      level <- aliases[[level]]
    }
  }

  level <- suppressWarnings(as.integer(level))
  if (length(level) != 1L || is.na(level) || !level %in% 0:3) {
    stop(
      "`level` must be one of 0, 1, 2, 3, 'country', 'region', ",
      "'district', or 'ta'.",
      call. = FALSE
    )
  }
  level
}

mw_level_label <- function(level) {
  c("country", "region", "district", "ta")[[mw_level_key(level) + 1L]]
}

mw_map_data <- function(level = 2) {
  switch(
    as.character(mw_level_key(level)),
    "0" = mwmapdata::mw_level_0,
    "1" = mwmapdata::mw_level_1,
    "2" = mwmapdata::mw_level_2,
    "3" = mwmapdata::mw_level_3
  )
}

mw_name_column <- function(level) {
  switch(
    as.character(mw_level_key(level)),
    "0" = "ADM0_EN",
    "1" = "ADM1_EN",
    "2" = "ADM2_EN",
    "3" = "ADM3_EN"
  )
}

mw_default_unit_col <- function(level) {
  switch(
    as.character(mw_level_key(level)),
    "0" = "country",
    "1" = "region",
    "2" = "district",
    "3" = "ta"
  )
}

mw_capture_col <- function(expr, env = parent.frame(), default = NULL) {
  if (missing(expr) || identical(expr, quote(expr)) || is.null(expr)) {
    return(default)
  }
  if (is.character(expr) && length(expr) == 1L) {
    return(expr)
  }
  if (is.symbol(expr)) {
    value <- tryCatch(get(as.character(expr), envir = env), error = function(e) NULL)
    if (is.character(value) && length(value) == 1L) {
      return(value)
    }
    return(as.character(expr))
  }
  as.character(expr)
}

mw_clean_key <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(tolower(x))
  x <- gsub("\\b(district|dist|traditional authority|t a|ta|region)\\b", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

mw_expand_region <- function(region) {
  region_key <- mw_clean_key(region)
  region_map <- c(
    n = "Northern", north = "Northern", northern = "Northern",
    c = "Central", central = "Central",
    s = "Southern", south = "Southern", southern = "Southern"
  )
  unname(region_map[region_key] %||% tools::toTitleCase(region_key))
}

mw_prepare_map <- function(
  level = 2,
  region = NULL,
  districts = NULL,
  tas = NULL,
  projection = "EPSG:4326"
) {
  level <- mw_level_key(level)
  map <- mw_map_data(level)

  if (!is.null(region) && "ADM1_EN" %in% names(map)) {
    wanted <- mw_expand_region(region)
    map <- map[mw_clean_key(map$ADM1_EN) %in% mw_clean_key(wanted), ]
  }

  if (!is.null(districts) && "ADM2_EN" %in% names(map)) {
    map <- map[mw_clean_key(map$ADM2_EN) %in% mw_clean_key(districts), ]
  }

  if (!is.null(tas) && "ADM3_EN" %in% names(map)) {
    map <- map[mw_clean_key(map$ADM3_EN) %in% mw_clean_key(tas), ]
  }

  if (nrow(map) == 0L) {
    stop("No Malawi map features matched the requested filters.", call. = FALSE)
  }

  if (!identical(projection, "EPSG:4326")) {
    map <- sf::st_transform(map, projection)
  }

  map
}

mw_pretty_title <- function(x) {
  x <- gsub("_", " ", x)
  tools::toTitleCase(x)
}

mw_is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

mw_distinct_count <- function(x) {
  length(unique(x[!is.na(x)]))
}
