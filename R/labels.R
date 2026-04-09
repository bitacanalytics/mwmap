#' Add District Labels to Malawi Map
#'
#' Adds text labels for districts to a Malawi map. Labels are positioned at
#' district centroids with options for customization.
#'
#' @param districts Character vector of district names to label. If NULL,
#'   labels all districts. Default: NULL.
#' @param size Numeric. Text size in points. Default: 3.
#' @param color Character. Text colour. Default: "black".
#' @param fontface Character or numeric. Font style: "plain", "bold", "italic",
#'   "bold.italic". Default: "bold".
#' @param family Character. Font family. Default: "" (system default).
#' @param alpha Numeric. Text transparency (0-1). Default: 1.
#' @param angle Numeric. Text rotation angle in degrees. Default: 0.
#' @param hjust Numeric. Horizontal justification (0-1). Default: 0.5.
#' @param vjust Numeric. Vertical justification (0-1). Default: 0.5.
#' @param check_overlap Logical. If TRUE, prevents overlapping labels. Default: TRUE.
#' @param show.legend Logical. Include in legend? Default: FALSE.
#' @param data Optional sf object. If NULL, uses mw_level_2.
#' @param label_column Character. Column name containing labels. Default: "ADM2_EN".
#' @param ... Additional arguments passed to [ggplot2::geom_sf_text()].
#'
#' @return A ggplot2 layer object that can be added to a map.
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' # Basic usage
#' mw_map() + mw_labels()
#'
#' # Customized labels
#' mw_map() +
#'   mw_labels(size = 4, color = "darkblue", fontface = "italic")
#'
#' # Labels for specific districts only
#' library(dplyr)
#' selected_districts <- mw_level_2 %>%
#'   filter(ADM2_EN %in% c("Lilongwe", "Blantyre", "Mzuzu City"))
#'
#' mw_map() +
#'   mw_labels(data = selected_districts, color = "red", size = 5)
#' }
#'
#' @importFrom ggplot2 geom_sf_text aes
#' @importFrom sf st_centroid st_geometry_type st_is
#' @export
mw_labels <- function(
  districts = NULL,
  size = 3,
  color = "black",
  fontface = "bold",
  family = "",
  alpha = 1,
  angle = 0,
  hjust = 0.5,
  vjust = 0.5,
  check_overlap = TRUE,
  show.legend = FALSE,
  data = NULL,
  label_column = "ADM2_EN",
  ...
) {
  # Use default data if not provided
  if (is.null(data)) {
    data <- mw_level_2
  }

  # Use selected districts if available
  if (!is.null(districts)) {
    data <- mw_level_2[mw_level_2$ADM2_EN %in% districts, ]
  }

  # Validate label column exists
  if (!label_column %in% names(data)) {
    stop(
      "Column '",
      label_column,
      "' not found in data. Available columns: ",
      paste(names(data), collapse = ", ")
    )
  }

  # Calculate centroids for labeling
  # Handle different geometry types safely
  if (any(grepl("MULTIPOLYGON|POLYGON", sf::st_geometry_type(data)))) {
    # Suppress warnings about centroids of longitude/latitude data
    suppressWarnings({
      centroid_data <- sf::st_centroid(data)
    })
  } else {
    centroid_data <- data
  }

  # Create label layer
  ggplot2::geom_sf_text(
    data = centroid_data,
    ggplot2::aes(label = .data[[label_column]]),
    size = size,
    color = color,
    fontface = fontface,
    family = family,
    alpha = alpha,
    angle = angle,
    hjust = hjust,
    vjust = vjust,
    check_overlap = check_overlap,
    show.legend = show.legend,
    ...
  )
}

#' Add Label Repel for Better Placement
#'
#' Alternative to mw_labels that uses ggrepel to prevent overlapping labels.
#' Requires the ggrepel package.
#'
#' @param ... Arguments passed to mw_labels()
#' @param force Numeric. Repulsion force. Default: 1.
#' @param max.overlaps Numeric. Maximum allowed overlaps. Default: 10.
#'
#' @return A ggrepel layer object.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggrepel", quietly = TRUE)) {
#'   mw_map() + mw_label_repel()
#' }
#' }
#'
#' @export
mw_label_repel <- function(
  ...,
  force = 1,
  max.overlaps = 10
) {
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package 'ggrepel' is required for this function. Please install it.")
  }

  # Get base label arguments
  args <- list(...)

  # Default data if not provided
  if (is.null(args$data)) {
    args$data <- mw_level_2
  }

  # Calculate centroids
  suppressWarnings({
    centroid_data <- sf::st_centroid(args$data)
  })

  # Extract coordinates
  coords <- sf::st_coordinates(centroid_data)

  # Create repel layer
  ggrepel::geom_text_repel(
    data = centroid_data,
    ggplot2::aes(
      x = coords[, "X"],
      y = coords[, "Y"],
      label = .data[[args$label_column %||% "ADM2_EN"]]
    ),
    size = args$size %||% 3,
    color = args$color %||% "black",
    fontface = args$fontface %||% "bold",
    force = force,
    max.overlaps = max.overlaps,
    ...
  )
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
