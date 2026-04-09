#' Create Malawi Choropleth Map
#'
#' Quickly create publication-ready choropleth maps from district-level data.
#' This function handles data joining, scaling, and visualization in one step.
#'
#' @param data Data frame containing district-level values to map.
#' @param value Column name (unquoted) containing values for choropleth fill.
#'   Can be numeric (continuous scale) or factor/categorical (discrete scale).
#' @param district_col Column name (unquoted) containing district names.
#'   Default: "district". Case insensitive matching is applied.
#' @param palette Character. Name of Malawi palette (see [scale_fill_mw]) or
#'   a custom palette function/vector. Default: "health".
#' @param level Integer. Administrative level: 0=country, 1=regions, 2=districts,
#'   3=traditional authorities. Default: 2.
#' @param lakes Logical. Show major lakes? Default: TRUE.
#' @param border_color Character. Border colour. Default: "black".
#' @param border_size Numeric. Border line width. Default: 0.3.
#' @param alpha Numeric. Fill transparency (0-1). Default: 1.
#' @param title Character. Map title. Default: NULL.
#' @param subtitle Character. Map subtitle. Default: NULL.
#' @param legend_title Character. Legend title. Defaults to `value` name.
#' @param na_color Character. Colour for missing values. Default: "grey50".
#' @param scale_type Character. "continuous", "discrete", or "auto". 
#'   Default: "auto" (detects from data).
#' @param n_breaks Numeric. Number of breaks for continuous scale. Default: 5.
#' @param breaks Optional custom break points.
#' @param labels Optional custom legend labels.
#' @param direction Numeric. Palette direction: 1 = normal, -1 = reversed.
#'   Default: 1.
#' @param interactive Logical. Return interactive plotly object? Default: FALSE.
#' @param quiet Logical. Suppress messages? Default: FALSE.
#' @param ... Additional arguments passed to [mw_map()].
#'
#' @return A ggplot2 object (or plotly object if interactive=TRUE).
#'
#' @examples
#' \donttest{
#' # Example with synthetic health data
#' health_data <- data.frame(
#'   district = c("Lilongwe", "Blantyre", "Mzimba", "Zomba", "Mzuzu City"),
#'   malaria_cases = c(1245, 2890, 987, 1654, 743),
#'   population = c(2300000, 1800000, 940000, 1050000, 220000)
#' )
#' 
#' # Basic choropleth
#' mw_choropleth(health_data, malaria_cases)
#' 
#' # Calculate and map rates
#' health_data$rate <- health_data$malaria_cases / health_data$population * 1000
#' mw_choropleth(health_data, rate, 
#'               palette = "malaria",
#'               title = "Malaria Incidence Rate per 1000")
#' 
#' # Categorical data
#' health_data$risk_level <- cut(health_data$rate, 
#'                               breaks = c(0, 0.5, 1, 2, 5),
#'                               labels = c("Low", "Medium", "High", "Very High"))
#' mw_choropleth(health_data, risk_level, palette = "health")
#' }
#'
#' @importFrom rlang ensym as_string
#' @importFrom ggplot2 waiver scale_fill_gradientn scale_fill_manual
#' @export
mw_choropleth <- function(
  data,
  value,
  district_col = "district",
  palette = "health",
  level = 2,
  lakes = FALSE,
  border_color = "black",
  border_size = 0.3,
  alpha = 1,
  title = NULL,
  subtitle = NULL,
  legend_title = NULL,
  na_color = "grey50",
  scale_type = "auto",
  n_breaks = 5,
  breaks = waiver(),
  labels = waiver(),
  direction = 1,
  interactive = FALSE,
  quiet = FALSE,
  ...
) {
  
  # Validate inputs
  if (missing(data)) {
    stop("Argument 'data' is required")
  }
  
  # Capture value name
  value_name <- rlang::as_string(rlang::ensym(value))
  district_name <- rlang::as_string(rlang::ensym(district_col))
  
  # Check if value column exists
  if (!value_name %in% names(data)) {
    stop("Column '", value_name, "' not found in data")
  }
  
  if (!quiet) {
    message("Creating choropleth for variable: ", value_name)
  }
  
  # Join data with map
  map_data <- mw_join(
    data = data,
    district_col = district_name,
    map = switch(
      as.character(level),
      "0" = mw_level_0,
      "1" = mw_level_1,
      "2" = mw_level_2,
      "3" = mw_level_3,
      stop("level must be 0, 1, 2, or 3")
    ),
    quiet = quiet
  )
  
  # Determine scale type if auto
  if (scale_type == "auto") {
    scale_type <- if (is.numeric(map_data[[value_name]])) "continuous" else "discrete"
    if (!quiet) message("Detected scale type: ", scale_type)
  }
  
  # Create base map
  p <- mw_map(
    data = map_data,
    fill = value_name,
    level = level,
    lakes = lakes,
    border_color = border_color,
    border_size = border_size,
    alpha = alpha,
    title = title,
    subtitle = subtitle,
    grid = FALSE,
    ...
  )
  
  # Apply appropriate fill scale
  if (scale_type == "continuous") {
    if (is.character(palette) && length(palette) == 1 && 
        palette %in% names(malawi_palettes)) {
      # Use Malawi continuous palette
      p <- p + scale_fill_mw(
        palette = palette,
        name = if (!is.null(legend_title)) legend_title else value_name,
        na.value = na_color,
        breaks = breaks,
        labels = labels,
        direction = direction,
        discrete = FALSE
      )
    } else {
      # Custom palette or function
      p <- p + ggplot2::scale_fill_gradientn(
        colours = if (is.function(palette)) palette(100) else palette,
        name = if (!is.null(legend_title)) legend_title else value_name,
        na.value = na_color,
        breaks = breaks,
        labels = labels
      )
    }
  } else {
    # Discrete scale
    if (is.character(palette) && length(palette) == 1 && 
        palette %in% names(malawi_palettes)) {
      p <- p + scale_fill_mw(
        palette = palette,
        name = if (!is.null(legend_title)) legend_title else value_name,
        na.value = na_color,
        discrete = TRUE,
        direction = direction
      )
    } else {
      p <- p + ggplot2::scale_fill_manual(
        values = palette,
        name = if (!is.null(legend_title)) legend_title else value_name,
        na.value = na_color,
        breaks = breaks,
        labels = labels
      )
    }
  }
  
  # Convert to interactive if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("Package 'plotly' is required for interactive maps. Returning static map.")
    } else {
      p <- plotly::ggplotly(p)
    }
  }
  
  return(p)
}