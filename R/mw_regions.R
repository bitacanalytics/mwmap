#' Plot Malawi Regions Map
#'
#' Create a simplified map showing Malawi's three administrative regions:
#' Northern, Central, and Southern. Useful for regional overview visualizations.
#'
#' @param fill Fill colour for regions. Can be a single color or a vector of
#'   3 colors (one per region). Default: "white".
#' @param border_color Border colour. Default: "black".
#' @param border_size Border line width. Default: 0.5.
#' @param alpha Fill transparency (0-1). Default: 1.
#' @param labels Logical. Add region labels. Default: FALSE.
#' @param label_size Size of region labels. Default: 4.
#' @param label_color Color of region labels. Default: "black".
#' @param label_face Font face for labels. Default: "bold".
#' @param region_colors Named vector of colors for specific regions.
#'   Names should be "Northern", "Central", "Southern". Default: NULL.
#' @param lakes Show major lakes. Default: TRUE.
#' @param projection Map projection. Default: "EPSG:4326".
#' @param title Map title. Default: NULL.
#' @param ... Additional arguments passed to [ggplot2::geom_sf()].
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' # Basic regions map
#' mw_regions()
#' 
#' # Colored regions
#' mw_regions(fill = c("Northern" = "#E41A1C", 
#'                     "Central" = "#377EB8", 
#'                     "Southern" = "#4DAF4A"))
#' 
#' # With labels
#' mw_regions(labels = TRUE, label_size = 5)
#' 
#' # Custom styling
#' mw_regions(fill = "lightblue", 
#'            border_color = "darkblue", 
#'            border_size = 1,
#'            alpha = 0.7)
#' }
#'
#' @importFrom dplyr group_by summarise
#' @importFrom sf st_union st_centroid st_coordinates st_transform
#' @importFrom ggplot2 ggplot coord_sf geom_sf geom_text aes scale_fill_manual theme_void labs
#' @export
mw_regions <- function(
  fill = "white",
  border_color = "black",
  border_size = 0.5,
  alpha = 1,
  labels = FALSE,
  label_size = 4,
  label_color = "black",
  label_face = "bold",
  region_colors = NULL,
  lakes = FALSE,
  projection = "EPSG:4326",
  title = NULL,
  ...
) {
  
  # Aggregate districts to regions
  # Use ADM1_EN (the actual region column in the shapefile).
  # Also sort so Northern / Central / Southern always come out in a
  # consistent order — don't rely on group_by ordering.
  regions <- dplyr::group_by(mw_level_2, .data$ADM1_EN)
  regions <- dplyr::summarise(regions, geometry = sf::st_union(geometry))

  # Rename to a clean "region" column and strip " Region" suffix if present
  names(regions)[1] <- "region"
  regions$region <- gsub(" Region$", "", regions$region)
  regions <- regions[order(regions$region), ]  # Alphabetical: Central, Northern, Southern
  
  # Transform projection if needed
  if (projection != "EPSG:4326") {
    regions <- sf::st_transform(regions, crs = projection)
    if (lakes && exists("major_lakes")) {
      major_lakes <- sf::st_transform(major_lakes, crs = projection)
    }
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::coord_sf(crs = projection, expand = FALSE)
  
  # Handle fill colors
  if (!is.null(region_colors)) {
    # Use named colors
    regions$fill_color <- region_colors[regions$region]
    p <- p + ggplot2::geom_sf(
      data = regions,
      ggplot2::aes(fill = .data$region),
      color = border_color,
      linewidth = border_size,
      alpha = alpha,
      ...
    ) +
      ggplot2::scale_fill_manual(values = region_colors, guide = "none")
  } else if (length(fill) == 1) {
    # Single color for all regions
    p <- p + ggplot2::geom_sf(
      data = regions,
      fill = fill,
      color = border_color,
      linewidth = border_size,
      alpha = alpha,
      ...
    )
  } else if (length(fill) == 3) {
    # Vector of 3 colors
    p <- p + ggplot2::geom_sf(
      data = regions,
      ggplot2::aes(fill = .data$region),
      color = border_color,
      linewidth = border_size,
      alpha = alpha,
      ...
    ) +
      ggplot2::scale_fill_manual(
        values = setNames(fill, regions$region),
        guide = "none"
      )
  } else {
    stop("fill must be a single color, a vector of 3 colors, or a named vector via region_colors")
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
  
  # Add region labels
  if (labels) {
    # Calculate centroids
    suppressWarnings({
      centroids <- sf::st_centroid(regions)
    })
    
    # Extract coordinates
    coords <- sf::st_coordinates(centroids)
    
    p <- p + ggplot2::geom_text(
      data = centroids,
      ggplot2::aes(
        x = coords[, "X"],
        y = coords[, "Y"],
        label = .data$region
      ),
      size = label_size,
      color = label_color,
      fontface = label_face,
      check_overlap = TRUE
    )
  }
  
  # Add title and theme
  p <- p +
    ggplot2::theme_void() +
    ggplot2::labs(title = title)
  
  return(p)
}