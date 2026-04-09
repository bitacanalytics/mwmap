#' Add Point Locations to Malawi Map
#'
#' Plot geographic points (e.g., survey clusters, health facilities, schools)
#' on a Malawi map with extensive customization options.
#'
#' @param data Data frame containing coordinates and optional attributes.
#' @param lon Longitude column (unquoted).
#' @param lat Latitude column (unquoted).
#' @param color Point color. Can be a single color or a column name for
#'   color mapping. Default: "red".
#' @param size Point size. Can be a single value or a column name for
#'   size mapping. Default: 2.
#' @param shape Point shape. Default: 19 (filled circle).
#' @param alpha Point transparency (0-1). Default: 0.8.
#' @param stroke Border thickness for points. Default: 0.5.
#' @param mapping Optional aesthetic mapping created with [ggplot2::aes()].
#' @param show.legend Logical. Show legend? Default: TRUE.
#' @param jitter Logical. Add small random noise to points to reduce overplotting.
#'   Default: FALSE.
#' @param jitter_width Numeric. Width of jitter. Default: 0.1.
#' @param jitter_height Numeric. Height of jitter. Default: 0.1.
#' @param label Logical. Add labels to points. Default: FALSE.
#' @param label_column Column for point labels (if `label = TRUE`).
#' @param label_size Size of point labels. Default: 3.
#' @param label_color Color of point labels. Default: "black".
#' @param repel_labels Use ggrepel to avoid overlapping labels. Default: FALSE.
#' @param ... Additional arguments passed to [ggplot2::geom_point()] or
#'   [ggrepel::geom_text_repel()].
#'
#' @return A ggplot2 layer object that can be added to a map.
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' 
#' # Sample health facilities
#' facilities <- data.frame(
#'   name = c("Lilongwe Central Hospital", "Queen Elizabeth Central Hospital",
#'            "Mzuzu Central Hospital", "Zomba Central Hospital"),
#'   lon = c(33.78, 35.00, 34.02, 35.32),
#'   lat = c(-13.98, -15.78, -11.46, -15.38),
#'   type = c("Central", "Central", "Central", "Central"),
#'   beds = c(1200, 1350, 600, 450)
#' )
#' 
#' # Basic points
#' mw_map() + 
#'   mw_points(facilities, lon, lat)
#' 
#' # Colored by type (pass column name as a string)
#' mw_map() + 
#'   mw_points(facilities, lon, lat, color = "type")
#' 
#' # Sized by beds (pass column name as a string)
#' mw_map() + 
#'   mw_points(facilities, lon, lat, size = "beds", alpha = 0.7)
#' 
#' # With labels
#' mw_map() + 
#'   mw_points(facilities, lon, lat,
#'             label = TRUE, label_column = "name")
#' 
#' # Custom styling
#' mw_map() + 
#'   mw_points(facilities, lon, lat,
#'             color = "darkblue", shape = 16, size = 4, alpha = 0.6)
#' }
#'
#' @importFrom rlang ensym as_string sym
#' @importFrom ggplot2 geom_point geom_sf aes
#' @importFrom sf st_as_sf
#' @export
mw_points <- function(
  data,
  lon,
  lat,
  color = "red",
  size = 2,
  shape = 19,
  alpha = 0.8,
  stroke = 0.5,
  mapping = NULL,
  show.legend = TRUE,
  jitter = FALSE,
  jitter_width = 0.1,
  jitter_height = 0.1,
  label = FALSE,
  label_column = NULL,
  label_size = 3,
  label_color = "black",
  repel_labels = FALSE,
  ...
) {
  
  # Capture column names
  lon_name <- rlang::as_string(rlang::ensym(lon))
  lat_name <- rlang::as_string(rlang::ensym(lat))
  
  # Validate coordinates exist
  if (!lon_name %in% names(data)) {
    stop("Column '", lon_name, "' not found in data")
  }
  if (!lat_name %in% names(data)) {
    stop("Column '", lat_name, "' not found in data")
  }
  
  # Check for missing coordinates
  missing_coords <- is.na(data[[lon_name]]) | is.na(data[[lat_name]])
  if (any(missing_coords)) {
    warning(sum(missing_coords), " rows have missing coordinates and will be skipped")
    data <- data[!missing_coords, ]
  }
  
  # Create layers list
  layers <- list()
  
  # Build aesthetic mapping
  if (is.null(mapping)) {
    point_mapping <- ggplot2::aes(
      x = !!rlang::ensym(lon),
      y = !!rlang::ensym(lat)
    )
  } else {
    point_mapping <- mapping
    point_mapping$x <- rlang::ensym(lon)
    point_mapping$y <- rlang::ensym(lat)
  }
  
  # Handle color mapping
  # color is already a string here, so use rlang::sym() not ensym()
  if (is.character(color) && length(color) == 1 && color %in% names(data)) {
    point_mapping$colour <- rlang::sym(color)
    color <- NULL
  }
  
  # Handle size mapping
  if (is.character(size) && length(size) == 1 && size %in% names(data)) {
    point_mapping$size <- rlang::sym(size)
    size <- NULL
  }
  
  # Convert data to sf points so geom_sf is used instead of geom_point.
  # This avoids the "coordinate system already present" warning that occurs
  # when geom_point triggers a second coord_sf on top of mw_map()'s coord_sf.
  data_sf <- sf::st_as_sf(data, coords = c(lon_name, lat_name), crs = 4326,
                           remove = FALSE)

  # Rebuild mapping using only geometry (drop x/y which are for geom_point)
  sf_mapping <- point_mapping
  sf_mapping$x <- NULL
  sf_mapping$y <- NULL

  # Build fixed aesthetics list — only include colour/size when they are NOT
  # already mapped via aes(). Passing NULL explicitly to geom_sf causes the
  # "Ignoring empty aesthetic" warning, so we omit them entirely instead.
  fixed_aes <- list(shape = shape, alpha = alpha, stroke = stroke)
  if (is.null(sf_mapping$colour)) fixed_aes$color  <- color
  if (is.null(sf_mapping$size))   fixed_aes$size   <- size

  # Main point layer
  if (jitter) {
    # geom_sf doesn't support jitter — fall back to geom_point and suppress
    # the duplicate coord_sf message (it is cosmetic only for point layers)
    jitter_aes <- list(shape = shape, alpha = alpha, stroke = stroke)
    if (is.null(point_mapping$colour)) jitter_aes$color <- color
    if (is.null(point_mapping$size))   jitter_aes$size  <- size
    layers[[1]] <- suppressMessages(do.call(ggplot2::geom_jitter, c(
      list(mapping = point_mapping, data = data,
           width = jitter_width, height = jitter_height,
           show.legend = show.legend),
      jitter_aes,
      list(...)
    )))
  } else {
    layers[[1]] <- do.call(ggplot2::geom_sf, c(
      list(mapping = sf_mapping, data = data_sf, show.legend = show.legend),
      fixed_aes,
      list(...)
    ))
  }
  
  # Add labels if requested
  if (label) {
    if (is.null(label_column)) {
      stop("label_column must be specified when label = TRUE")
    }
    
    if (!label_column %in% names(data)) {
      stop("Column '", label_column, "' not found in data")
    }
    
    if (repel_labels && requireNamespace("ggrepel", quietly = TRUE)) {
      layers[[2]] <- ggrepel::geom_text_repel(
        data = data,
        ggplot2::aes(
          x = !!rlang::ensym(lon),
          y = !!rlang::ensym(lat),
          label = .data[[label_column]]
        ),
        size = label_size,
        color = label_color,
        box.padding = 0.5,
        point.padding = 0.3,
        segment.color = "grey50",
        segment.size = 0.2,
        min.segment.length = 0.1
      )
    } else {
      layers[[2]] <- ggplot2::geom_text(
        data = data,
        ggplot2::aes(
          x = !!rlang::ensym(lon),
          y = !!rlang::ensym(lat),
          label = .data[[label_column]]
        ),
        size = label_size,
        color = label_color,
        check_overlap = TRUE
      )
    }
  }
  
  # Return layers
  if (length(layers) == 1) {
    return(layers[[1]])
  } else {
    return(layers)
  }
}