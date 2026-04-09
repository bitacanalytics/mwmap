#' Highlight Selected Districts on Malawi Map
#'
#' Adds emphasis to specific districts by overlaying them with custom fill,
#' border, or label styles. Useful for drawing attention to areas of interest.
#'
#' @param districts Character vector of district names to highlight.
#'   Case insensitive matching is applied.
#' @param fill Fill colour for highlighted districts. Default: "red".
#'   Use NA for transparent fill.
#' @param color Border colour for highlighted districts. Default: "black".
#' @param alpha Transparency level (0-1). Default: 0.8.
#' @param size Border size for highlighted districts. Default: 1.
#' @param linetype Line type for borders. Default: "solid".
#' @param label Logical. Add district labels to highlighted areas? Default: FALSE.
#' @param label_size Numeric. Label text size. Default: 4.
#' @param label_color Character. Label text colour. Default: "black".
#' @param label_fontface Label font style. Default: "bold".
#' @param data Optional sf object. If NULL, uses mw_level_2.
#' @param district_col Column containing district names. Default: "ADM2_EN".
#' @param outline_only Logical. Show only outline without fill? Default: FALSE.
#' @param ... Additional arguments passed to [ggplot2::geom_sf()].
#'
#' @return A ggplot2 layer object that can be added to a map.
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' 
#' # Basic highlighting
#' mw_map() + 
#'   mw_highlight("Lilongwe")
#' 
#' # Multiple districts with custom styling
#' mw_map() + 
#'   mw_highlight(c("Lilongwe", "Blantyre", "Mzuzu City"),
#'                fill = "gold", color = "darkred", alpha = 0.5)
#' 
#' # Outline only (for emphasis without obscuring)
#' mw_map(fill_color = "lightgrey") + 
#'   mw_highlight("Mzimba", outline_only = TRUE, size = 1.2)
#' 
#' # With labels
#' mw_map() + 
#'   mw_highlight("Zomba", label = TRUE, label_size = 5)
#' }
#'
#' @importFrom ggplot2 geom_sf aes geom_text
#' @importFrom sf st_as_sf st_centroid st_coordinates
#' @export
mw_highlight <- function(
  districts,
  fill = "red",
  color = "black",
  alpha = 0.8,
  size = 1,
  linetype = "solid",
  label = FALSE,
  label_size = 4,
  label_color = "black",
  label_fontface = "bold",
  data = NULL,
  district_col = "ADM2_EN",
  outline_only = FALSE,
  ...
) {
  
  # Use default data if not provided
  if (is.null(data)) {
    data <- mw_level_2
  }
  
  # Validate district column exists
  if (!district_col %in% names(data)) {
    stop("Column '", district_col, "' not found in data")
  }
  
  # Normalize district names (case insensitive matching)
  districts_orig <- districts
  districts <- tools::toTitleCase(tolower(districts))
  
  # Find matching districts
  district_names <- data[[district_col]]
  matched <- districts[districts %in% district_names]
  unmatched <- districts[!districts %in% district_names]
  
  if (length(matched) == 0) {
    stop("No matching districts found. Check district names.")
  }
  
  if (length(unmatched) > 0) {
    warning("These districts were not found: ", 
            paste(unmatched, collapse = ", "))
  }
  
  # Filter data to matched districts
  highlight_data <- data[data[[district_col]] %in% matched, ]
  
  # Create layers list
  layers <- list()
  
  # Main highlight layer
  if (outline_only) {
    layers[[1]] <- ggplot2::geom_sf(
      data = highlight_data,
      fill = NA,
      color = color,
      linewidth = size,
      linetype = linetype,
      ...
    )
  } else {
    layers[[1]] <- ggplot2::geom_sf(
      data = highlight_data,
      fill = fill,
      color = color,
      linewidth = size,
      linetype = linetype,
      alpha = alpha,
      ...
    )
  }
  
  # Add labels if requested
  if (label) {
    # Calculate centroids
    suppressWarnings({
      centroid_data <- sf::st_centroid(highlight_data)
    })
    
    # Extract coordinates
    coords <- sf::st_coordinates(centroid_data)
    
    layers[[2]] <- ggplot2::geom_text(
      data = centroid_data,
      ggplot2::aes(
        x = coords[, "X"],
        y = coords[, "Y"],
        label = .data[[district_col]]
      ),
      size = label_size,
      color = label_color,
      fontface = label_fontface,
      check_overlap = TRUE
    )
  }
  
  # Return layers (if multiple, combine with list)
  if (length(layers) == 1) {
    return(layers[[1]])
  } else {
    return(layers)
  }
}

#' Highlight Multiple Districts with Different Colors
#'
#' Highlights different districts with potentially different colors.
#'
#' @param district_list Named list or vector. Names are district names,
#'   values are fill colors. Example: c("Lilongwe" = "red", "Blantyre" = "blue")
#' @param ... Additional arguments passed to [mw_highlight()] for all layers.
#'
#' @return List of ggplot2 layers.
#'
#' @examples
#' \donttest{
#' mw_map() + 
#'   mw_highlight_multi(c("Lilongwe" = "gold", 
#'                        "Blantyre" = "steelblue", 
#'                        "Mzimba" = "forestgreen"))
#' }
#'
#' @export
mw_highlight_multi <- function(district_list, ...) {
  
  if (is.null(names(district_list))) {
    stop("district_list must have names (district names)")
  }
  
  layers <- list()
  
  for (i in seq_along(district_list)) {
    district <- names(district_list)[i]
    color_val <- district_list[i]
    
    layers[[i]] <- mw_highlight(
      districts = district,
      fill = color_val,
      ...
    )
  }
  
  return(layers)
}