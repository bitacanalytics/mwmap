#' Plot Traditional Authorities Map
#'
#' Create a detailed map of Malawi's Traditional Authorities (TA level).
#' This is the third administrative level, showing subdivisions within districts.
#'
#' @param districts Optional vector of district names to filter TAs.
#'   If NULL, shows all TAs. Default: NULL.
#' @param fill Fill colour for TAs. Can be a single color or a column name
#'   for choropleth mapping. Default: "white".
#' @param border_color Border colour. Default: "black".
#' @param border_size Border line width. Default: 0.2.
#' @param district_border Logical. Show district borders? Default: TRUE.
#' @param district_border_color Color for district borders. Default: "red".
#' @param district_border_size Size for district borders. Default: 0.5.
#' @param alpha Fill transparency. Default: 1.
#' @param labels Logical. Add TA labels. Default: FALSE.
#' @param label_size Size of TA labels. Default: 2.
#' @param label_color Color of TA labels. Default: "black".
#' @param max_labels Maximum number of labels to show (to avoid overcrowding).
#'   Default: 50.
#' @param lakes Show major lakes. Default: TRUE.
#' @param projection Map projection. Default: "EPSG:4326".
#' @param title Map title. Default: NULL.
#' @param data Optional dataset joined with TA map.
#' @param fill_column Column name in `data` used for fill colour.
#' @param ... Additional arguments passed to [ggplot2::geom_sf()].
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' # All TAs
#' mw_ta_map()
#' 
#' # TAs in specific districts
#' mw_ta_map(districts = c("Lilongwe", "Dowa"))
#' 
#' # Custom styling
#' mw_ta_map(border_color = "grey50", 
#'           district_border_color = "blue",
#'           district_border_size = 1)
#' 
#' # With labels (first 30 TAs only)
#' mw_ta_map(labels = TRUE, max_labels = 30)
#' 
#' # Thematic map with data
#' \dontrun{
#' ta_data <- data.frame(
#'   ta_name = c("Chitipa", "Mwaulambia", "Mabuka"),
#'   population = c(45000, 32000, 28000)
#' )
#' mw_ta_map(data = ta_data, fill_column = "population")
#' }
#' }
#'
#' @importFrom dplyr filter
#' @importFrom sf st_centroid st_coordinates st_as_sf st_transform st_make_valid st_is_valid
#' @importFrom ggplot2 ggplot coord_sf geom_sf geom_text aes theme_void labs
#' @importFrom tools toTitleCase
#' @export
mw_ta_map <- function(
  districts = NULL,
  fill = "white",
  border_color = "black",
  border_size = 0.2,
  district_border = TRUE,
  district_border_color = "red",
  district_border_size = 0.5,
  alpha = 1,
  labels = FALSE,
  label_size = 2,
  label_color = "black",
  max_labels = 50,
  lakes = FALSE,
  projection = "EPSG:4326",
  title = NULL,
  data = NULL,
  fill_column = NULL,
  ...
) {
  
  # Start with level 3 data
  ta_data <- mw_level_3
  
  # Repair any invalid geometries (prevents s2 errors on centroid/plot calls)
  if (!all(sf::st_is_valid(ta_data))) {
    ta_data <- sf::st_make_valid(ta_data)
  }
  
  # Filter by districts if specified
  if (!is.null(districts)) {
    districts <- tools::toTitleCase(tolower(districts))
    ta_data <- ta_data[ta_data$ADM2_EN %in% districts, ]
    
    if (nrow(ta_data) == 0) {
      stop("No TAs found in specified districts")
    }
    
    message("Filtered to ", length(unique(ta_data$ADM2_EN)), 
            " districts with ", nrow(ta_data), " TAs")
  }
  
  # Join with external data if provided
  if (!is.null(data) && !is.null(fill_column)) {
    if (!"ADM3_EN" %in% names(data) && !"TA" %in% names(data)) {
      stop("Data must contain TA names in 'ADM3_EN' or 'TA' column")
    }
    
    join_col <- if ("ADM3_EN" %in% names(data)) "ADM3_EN" else "TA"
    join_col <- intersect(join_col, names(data))[1]
    
    data[[join_col]] <- tools::toTitleCase(tolower(data[[join_col]]))
    ta_data <- merge(ta_data, data, by.x = "ADM3_EN", by.y = join_col, all.x = TRUE)
    ta_data <- sf::st_as_sf(ta_data)
  }
  
  # Transform projection if needed
  if (projection != "EPSG:4326") {
    ta_data <- sf::st_transform(ta_data, crs = projection)
    if (lakes && exists("major_lakes")) {
      major_lakes <- sf::st_transform(major_lakes, crs = projection)
    }
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::coord_sf(crs = projection, expand = FALSE)
  
  # Add TA layer
  if (!is.null(fill_column) && fill_column %in% names(ta_data)) {
    p <- p + ggplot2::geom_sf(
      data = ta_data,
      ggplot2::aes(fill = .data[[fill_column]]),
      color = border_color,
      linewidth = border_size,
      alpha = alpha,
      ...
    )
  } else {
    p <- p + ggplot2::geom_sf(
      data = ta_data,
      fill = fill,
      color = border_color,
      linewidth = border_size,
      alpha = alpha,
      ...
    )
  }
  
  # Add district borders (using level 2 data)
  if (district_border) {
    district_data <- mw_level_2
    
    if (!is.null(districts)) {
      district_data <- district_data[district_data$ADM2_EN %in% districts, ]
    }
    
    if (projection != "EPSG:4326") {
      district_data <- sf::st_transform(district_data, crs = projection)
    }
    
    p <- p + ggplot2::geom_sf(
      data = district_data,
      fill = NA,
      color = district_border_color,
      linewidth = district_border_size
    )
  }
  
  # Add lakes
  if (lakes && exists("major_lakes")) {
    p <- p + ggplot2::geom_sf(
      data = major_lakes,
      fill = "#9ecae1",
      color = "#3182bd",
      alpha = alpha
    )
  }
  
  # Add TA labels (limit to avoid overcrowding)
  if (labels) {
    # Calculate centroids
    suppressWarnings({
      centroids <- sf::st_centroid(ta_data)
    })
    
    # Limit number of labels if needed
    if (nrow(centroids) > max_labels) {
      # Random sample
      set.seed(42)  # For reproducibility
      centroids <- centroids[sample(nrow(centroids), max_labels), ]
    }
    
    # Extract coordinates
    coords <- sf::st_coordinates(centroids)
    
    p <- p + ggplot2::geom_text(
      data = centroids,
      ggplot2::aes(
        x = coords[, "X"],
        y = coords[, "Y"],
        label = .data$ADM3_EN
      ),
      size = label_size,
      color = label_color,
      check_overlap = TRUE
    )
  }
  
  # Add title and theme
  p <- p +
    ggplot2::theme_void() +
    ggplot2::labs(title = title)
  
  return(p)
}