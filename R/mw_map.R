# ================================
# mw_map.R
# ================================
#' Plot Malawi Map (Professional Edition)
#'
#' Main function for creating publication-ready Malawi maps with extensive
#' customization options, including spatial visualization, custom projections,
#' and professional cartographic elements.
#'
#' @param data Optional dataset joined with the map. Can be a data.frame, sf object, or tibble.
#' @param fill Column name in `data` used for fill colour.
#' @param districts Optional vector of district names to plot (case-insensitive).
#' @param region Optional region name to filter districts: "Northern", "Central", "Southern" (or shortcuts "n", "c", "s").
#' @param level Administrative level: 0 = country, 1 = regions, 2 = districts (default), 3 = traditional authorities.
#' @param lakes Show major lakes. Default: TRUE.
#' @param borders Show administrative borders. Default: TRUE.
#' @param border_color Border colour for main administrative units. Default: "#333333".
#' @param border_size Border line width. Default: 0.3.
#' @param lake_color Fill colour for lakes. Default: "#9ecae1".
#' @param lake_border_color Border colour for lakes. Default: "#3182bd".
#' @param alpha Fill transparency (0-1). Default: 1.
#' @param title Map title. Default: NULL.
#' @param subtitle Map subtitle. Default: NULL.
#' @param caption Map caption. Default: NULL.
#' @param fill_color Default fill colour if `fill` is NULL. Default: "#f0f0f0".
#' @param fill_palette Colour palette for fill variable. Default: NULL.
#' @param fill_scale_type Scale type: "continuous", "discrete", or "auto". Default: "auto".
#' @param fill_breaks Custom breaks for continuous fill. Default: waiver().
#' @param fill_labels Custom labels for fill legend. Default: waiver().
#' @param fill_limits Limits for continuous fill. Default: NULL.
#' @param fill_na Colour for NA values. Default: "#bdbdbd".
#' @param legend_title Title for fill legend. Defaults to `fill` parameter.
#' @param legend_position Legend position: "right", "left", "top", "bottom", "none", or coordinates. Default: "right".
#' @param projection Map projection. Default: "EPSG:4326".
#' @param grid Show coordinate grid. Default: FALSE.
#' @param grid_lines Show grid lines (if `grid = TRUE`). Default: TRUE.
#' @param grid_labels Show grid labels (if `grid = TRUE`). Default: TRUE.
#' @param scale_bar Add scale bar. Default: FALSE.
#' @param scale_bar_position Position of scale bar: "bottomright", "bottomleft", "topright", "topleft". Default: "bottomright".
#' @param north_arrow Add north arrow. Default: FALSE.
#' @param north_arrow_position Position of north arrow. Default: "topright".
#' @param interactive Return interactive plot (TRUE/FALSE) using plotly. Default: FALSE.
#' @param highlight_districts Vector of districts to highlight with different border. Default: NULL.
#' @param highlight_border_color Border colour for highlighted districts. Default: "#E41A1C".
#' @param highlight_border_size Border size for highlighted districts. Default: 1.2.
#' @param highlight_fill Fill colour for highlighted districts (NA for outline only). Default: NA.
#' @param label_districts Show district labels. Default: FALSE.
#' @param label_column Column to use for labels. Default: "ADM2_EN" for level 2.
#' @param label_size Size of district labels. Default: 3.
#' @param label_color Colour of district labels. Default: "#333333".
#' @param label_repel Use ggrepel to avoid overlapping labels. Default: FALSE.
#' @param theme Custom ggplot2 theme (NULL for default). Default: NULL.
#' @param quiet Suppress messages. Default: FALSE.
#' @param ... Additional arguments passed to ggplot2::geom_sf().
#'
#' @return A ggplot2 object (or plotly object if `interactive = TRUE`).
#' @importFrom ggplot2 ggplot coord_sf geom_sf geom_text theme_void labs theme
#' @importFrom ggplot2 scale_fill_gradientn scale_fill_distiller scale_fill_gradient
#' @importFrom ggplot2 scale_fill_discrete scale_fill_manual element_text element_blank
#' @importFrom ggplot2 aes waiver
#' @importFrom sf st_transform st_centroid st_coordinates
#' @importFrom tools toTitleCase
#' @export

mw_map <- function(
  data = NULL,
  fill = NULL,
  districts = NULL,
  region = NULL,
  level = 2,
  lakes = FALSE,
  borders = TRUE,
  border_color = "#333333",
  border_size = 0.3,
  lake_color = "#9ecae1",
  lake_border_color = "#3182bd",
  alpha = 1,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_color = "#f0f0f0",
  fill_palette = NULL,
  fill_scale_type = "auto",
  fill_breaks = waiver(),
  fill_labels = waiver(),
  fill_limits = NULL,
  fill_na = "#bdbdbd",
  legend_title = NULL,
  legend_position = "right",
  projection = "EPSG:4326",
  grid = FALSE,
  grid_lines = TRUE,
  grid_labels = TRUE,
  scale_bar = FALSE,
  scale_bar_position = "bottomright",
  north_arrow = FALSE,
  north_arrow_position = "topright",
  interactive = FALSE,
  highlight_districts = NULL,
  highlight_border_color = "#E41A1C",
  highlight_border_size = 1.2,
  highlight_fill = NA,
  label_districts = FALSE,
  label_column = NULL,
  label_size = 3,
  label_color = "#333333",
  label_repel = FALSE,
  theme = NULL,
  quiet = FALSE,
  ...
) {
  # Fallback operator
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Validate level
  if (!level %in% 0:3) {
    stop("level must be 0, 1, 2, or 3")
  }

  # Validate region
  if (
    !is.null(region) &&
      !tolower(region) %in% c("northern", "central", "southern", "n", "c", "s")
  ) {
    stop(
      "region must be one of: 'Northern', 'Central', 'Southern' (or shortcuts 'n', 'c', 's')"
    )
  }

  # Choose map data
  map_data <- switch(
    as.character(level),
    "0" = mw_level_0,
    "1" = mw_level_1,
    "2" = mw_level_2,
    "3" = mw_level_3,
    stop("Invalid level")
  )

  # Default label column
  label_column <- label_column %||%
    switch(
      as.character(level),
      "0" = "ADM0_EN",
      "1" = "ADM1_EN",
      "2" = "ADM2_EN",
      "3" = "ADM3_EN"
    )

  # Region filter
  if (!is.null(region)) {
    region_expanded <- mw_expand_region(region)
    map_data <- map_data[map_data$ADM1_EN %in% region_expanded, ]
    if (nrow(map_data) == 0) {
      stop(
        "No data found for region: ",
        paste(region_expanded, collapse = ", ")
      )
    }
    if (!quiet) {
      message(
        "Filtered to ",
        paste(region_expanded, collapse = ", "),
        " region (",
        nrow(map_data),
        " features)"
      )
    }
  }

  # District filter
  if (!is.null(districts)) {
    district_col <- switch(
      as.character(level),
      "0" = "ADM0_EN",
      "1" = "ADM1_EN",
      "2" = "ADM2_EN",
      "3" = "ADM3_EN"
    )
    districts_clean <- mw_clean_names(districts)
    map_districts <- mw_clean_names(map_data[[district_col]])
    map_data <- map_data[map_districts %in% districts_clean, ]
    if (nrow(map_data) == 0) {
      stop("No matching districts found")
    }
    if (!quiet) message("Filtered to ", nrow(map_data), " districts")
  }

  # Projection
  if (projection != "EPSG:4326") {
    map_data <- sf::st_transform(map_data, crs = projection)
    if (lakes && exists("major_lakes") && inherits(major_lakes, "sf")) {
      major_lakes <- sf::st_transform(major_lakes, crs = projection)
    }
  }

  # Join external data
  # If data is already an sf object (e.g. pre-joined by mw_choropleth),
  # use it directly as map_data instead of attempting another join.
  if (!is.null(data)) {
    # Join external data
    # If data is already an sf object (e.g. pre-joined by mw_choropleth),
    # use it directly as map_data instead of attempting another join.

    if (!is.null(data)) {
      # HARD VALIDATION FIRST (CRAN SAFE)
      if (!inherits(data, "sf") && !inherits(data, "data.frame")) {
        stop("'data' must be a data.frame or sf object", call. = FALSE)
      }

      # SF OBJECT: use directly
      if (inherits(data, "sf")) {
        map_data <- data
      } else {
        # FORCE SAFE STRUCTURE (important for CRAN)
        data <- as.data.frame(data)

        map_data <- mw_join(
          data = data,
          map = map_data,
          map_district_col = switch(
            as.character(level),
            "0" = "ADM0_EN",
            "1" = "ADM1_EN",
            "2" = "ADM2_EN",
            "3" = "ADM3_EN"
          ),
          quiet = quiet
        )
      }
    }
  }

  # Base plot
  p <- ggplot2::ggplot() +
    ggplot2::coord_sf(
      crs = projection,
      datum = if (grid) projection else NULL,
      expand = FALSE
    )

  # Main map layer
  if (!is.null(fill) && fill %in% names(map_data)) {
    if (fill_scale_type == "auto") {
      fill_scale_type <- if (is.numeric(map_data[[fill]])) {
        "continuous"
      } else {
        "discrete"
      }
    }
    p <- p +
      ggplot2::geom_sf(
        data = map_data,
        ggplot2::aes(fill = .data[[fill]]),
        color = if (borders) border_color else NA,
        linewidth = border_size,
        alpha = alpha,
        ...
      )
    p <- add_fill_scale_mw(
      p,
      fill,
      fill_palette,
      fill_scale_type,
      fill_breaks,
      fill_labels,
      fill_limits,
      fill_na,
      legend_title %||% fill
    )
  } else {
    p <- p +
      ggplot2::geom_sf(
        data = map_data,
        fill = fill_color,
        color = if (borders) border_color else NA,
        linewidth = border_size,
        alpha = alpha,
        ...
      )
    if (!quiet && !is.null(fill)) {
      warning(
        "Column '",
        fill,
        "' not found in data. Using fill_color instead."
      )
    }
  }

  # Lakes
  if (lakes && exists("major_lakes") && inherits(major_lakes, "sf")) {
    p <- p +
      ggplot2::geom_sf(
        data = major_lakes,
        fill = lake_color,
        color = lake_border_color,
        alpha = alpha
      )
  }

  # Highlight districts
  if (!is.null(highlight_districts)) {
    highlight_col <- switch(
      as.character(level),
      "0" = "ADM0_EN",
      "1" = "ADM1_EN",
      "2" = "ADM2_EN",
      "3" = "ADM3_EN"
    )
    highlight_clean <- mw_clean_names(highlight_districts)
    map_highlights <- mw_clean_names(map_data[[highlight_col]])
    highlight_data <- map_data[map_highlights %in% highlight_clean, ]
    if (nrow(highlight_data) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = highlight_data,
          fill = highlight_fill,
          color = highlight_border_color,
          linewidth = highlight_border_size
        )
    }
  }

  # Labels
  if (label_districts && label_column %in% names(map_data)) {
    if (label_repel && requireNamespace("ggrepel", quietly = TRUE)) {
      p <- add_label_repel_mw(
        p,
        map_data,
        label_column,
        label_size,
        label_color
      )
    } else {
      p <- add_labels_mw(p, map_data, label_column, label_size, label_color)
    }
  }

  # Scale bar
  if (scale_bar) {
    p <- add_scale_bar_mw(p, map_data, scale_bar_position)
  }

  # North arrow
  if (north_arrow) {
    p <- add_north_arrow_mw(p, map_data, north_arrow_position)
  }

  # Grid
  if (grid) {
    p <- p + add_grid_mw(grid_lines, grid_labels)
  }

  # Theme
  if (is.null(theme)) {
    p <- p + ggplot2::theme_void()
    if (grid && grid_labels) {
      p <- p +
        ggplot2::theme(
          axis.text = ggplot2::element_text(color = "grey50", size = 8)
        )
    }
  } else {
    p <- p + theme
  }

  # Titles
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill = legend_title
    )
  p <- p + ggplot2::theme(legend.position = legend_position)

  # Interactive
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }

  return(p)
}

# ================================
# Helper functions
# ================================

add_fill_scale_mw <- function(
  p,
  fill_var,
  palette,
  scale_type,
  breaks,
  labels,
  limits,
  na_color,
  legend_title
) {
  if (!exists("malawi_palettes")) {
    malawi_palettes <- list()
  }
  is_mw_palette <- is.character(palette) &&
    length(palette) == 1 &&
    palette %in% names(malawi_palettes)

  if (scale_type == "continuous") {
    if (is_mw_palette) {
      p <- p +
        scale_fill_mw(
          palette = palette,
          name = legend_title,
          na.value = na_color,
          breaks = breaks,
          labels = labels,
          limits = limits,
          discrete = FALSE
        )
    } else if (is.character(palette) && length(palette) == 1) {
      p <- p +
        ggplot2::scale_fill_distiller(
          palette = palette,
          name = legend_title,
          na.value = na_color,
          breaks = breaks,
          labels = labels,
          limits = limits
        )
    } else if (is.function(palette)) {
      p <- p +
        ggplot2::scale_fill_gradientn(
          colors = palette(100),
          name = legend_title,
          na.value = na_color,
          breaks = breaks,
          labels = labels,
          limits = limits
        )
    } else if (is.character(palette) && length(palette) > 1) {
      p <- p +
        ggplot2::scale_fill_gradientn(
          colors = palette,
          name = legend_title,
          na.value = na_color,
          breaks = breaks,
          labels = labels,
          limits = limits
        )
    } else {
      p <- p +
        ggplot2::scale_fill_gradient(
          low = "#f7f7f7",
          high = "#08519c",
          name = legend_title,
          na.value = na_color,
          breaks = breaks,
          labels = labels,
          limits = limits
        )
    }
  } else {
    if (is_mw_palette) {
      p <- p +
        scale_fill_mw(
          palette = palette,
          name = legend_title,
          na.value = na_color,
          discrete = TRUE
        )
    } else if (is.null(palette)) {
      p <- p +
        ggplot2::scale_fill_discrete(
          name = legend_title,
          na.value = na_color,
          breaks = breaks,
          labels = labels
        )
    } else {
      p <- p +
        ggplot2::scale_fill_manual(
          values = palette,
          name = legend_title,
          na.value = na_color,
          breaks = breaks,
          labels = labels
        )
    }
  }
  return(p)
}

add_labels_mw <- function(p, map_data, label_column, size, color) {
  centroids <- suppressWarnings(sf::st_centroid(map_data))
  coords <- sf::st_coordinates(centroids)
  p +
    ggplot2::geom_text(
      data = centroids,
      ggplot2::aes(
        x = coords[, "X"],
        y = coords[, "Y"],
        label = .data[[label_column]]
      ),
      size = size,
      color = color,
      check_overlap = TRUE
    )
}

add_label_repel_mw <- function(p, map_data, label_column, size, color) {
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    return(add_labels_mw(p, map_data, label_column, size, color))
  }
  centroids <- suppressWarnings(sf::st_centroid(map_data))
  coords <- sf::st_coordinates(centroids)
  p +
    ggrepel::geom_text_repel(
      data = centroids,
      ggplot2::aes(
        x = coords[, "X"],
        y = coords[, "Y"],
        label = .data[[label_column]]
      ),
      size = size,
      color = color,
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "grey50",
      segment.size = 0.2,
      min.segment.length = 0.1
    )
}

add_scale_bar_mw <- function(p, map_data, position) {
  if (!requireNamespace("ggspatial", quietly = TRUE)) {
    return(p)
  }
  p +
    ggspatial::annotation_scale(
      location = position,
      width_hint = 0.2,
      pad_x = ggplot2::unit(0.5, "cm"),
      pad_y = ggplot2::unit(0.5, "cm"),
      style = "bar"
    )
}

add_north_arrow_mw <- function(p, map_data, position) {
  if (!requireNamespace("ggspatial", quietly = TRUE)) {
    return(p)
  }
  p +
    ggspatial::annotation_north_arrow(
      location = position,
      which_north = "true",
      pad_x = ggplot2::unit(0.5, "cm"),
      pad_y = ggplot2::unit(0.5, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(
        fill = c("white", "black"),
        line_col = "grey20"
      )
    )
}

add_grid_mw <- function(grid_lines, grid_labels) {
  theme_elems <- list()
  if (grid_lines) {
    theme_elems$panel.grid.major <- ggplot2::element_line(
      color = "grey80",
      linewidth = 0.2
    )
    theme_elems$panel.grid.minor <- ggplot2::element_line(
      color = "grey90",
      linewidth = 0.1
    )
  }
  if (grid_labels) {
    theme_elems$axis.text <- ggplot2::element_text(color = "grey50", size = 8)
    theme_elems$axis.text.x <- ggplot2::element_text(angle = 0, hjust = 0.5)
    theme_elems$axis.text.y <- ggplot2::element_text(angle = 0, hjust = 1)
  }
  ggplot2::theme(!!!theme_elems)
}

mw_expand_region <- function(region) {
  region <- tolower(region)
  region_map <- c(
    "n" = "Northern",
    "northern" = "Northern",
    "c" = "Central",
    "central" = "Central",
    "s" = "Southern",
    "southern" = "Southern"
  )
  if (region %in% names(region_map)) {
    return(region_map[region])
  }
  tools::toTitleCase(region)
}
