#' Get Malawi Boundary Data
#'
#' Return Malawi administrative boundaries from \pkg{mwmapdata}, with optional
#' region, district, TA, and projection filters.
#'
#' @param level Administrative level: `0`/`"country"`, `1`/`"region"`,
#'   `2`/`"district"`, or `3`/`"ta"`.
#' @param region Optional region filter.
#' @param districts Optional district filter.
#' @param tas Optional Traditional Authority filter.
#' @param projection Coordinate reference system. Defaults to `"EPSG:4326"`.
#'
#' @return An sf object.
#' @examples
#' \donttest{
#' mw_get_map("district")
#' mw_get_map("ta", districts = "Lilongwe")
#' }
#' @export
mw_get_map <- function(
  level = 2,
  region = NULL,
  districts = NULL,
  tas = NULL,
  projection = "EPSG:4326"
) {
  mw_prepare_map(
    level = level,
    region = region,
    districts = districts,
    tas = tas,
    projection = projection
  )
}

#' Create a Professional Malawi Map
#'
#' `mw_map()` is the main high-level plotting function in mwmap. It can draw
#' Malawi boundaries at country, region, district, or Traditional Authority
#' level, join your data by name, and choose an appropriate colour scale for
#' numeric or categorical values.
#'
#' @param data Optional data frame or sf object. If a data frame is supplied it
#'   is joined to the selected Malawi boundaries.
#' @param fill Optional column to map to fill colour. May be quoted or unquoted.
#' @param unit_col Column in `data` containing names to join by. May be quoted
#'   or unquoted. Defaults to `country`, `region`, `district`, or `ta`,
#'   depending on `level`.
#' @param level Administrative level: `0`/`"country"`, `1`/`"region"`,
#'   `2`/`"district"`, or `3`/`"ta"`.
#' @param region Optional region filter.
#' @param districts Optional district filter. For `level = "ta"`, this maps TAs
#'   only inside the selected districts.
#' @param tas Optional Traditional Authority filter.
#' @param palette Name of a Malawi palette, a vector of colours, or a palette
#'   function. Defaults to `"health"` for numeric data and `"qualitative_2"` for
#'   categorical data.
#' @param scale_type `"auto"`, `"continuous"`, or `"discrete"`.
#' @param reverse Reverse the fill palette.
#' @param na_color Fill colour for missing values.
#' @param fill_color Fill colour used when `fill` is not supplied.
#' @param border_color Boundary colour.
#' @param border_size Boundary line width.
#' @param alpha Fill opacity.
#' @param lakes Add Lake Malawi.
#' @param lake_color Lake fill colour.
#' @param lake_border_color Lake border colour.
#' @param district_borders Add district outlines on TA maps.
#' @param district_border_color District outline colour on TA maps.
#' @param district_border_size District outline width on TA maps.
#' @param highlight_districts Districts to outline.
#' @param highlight_tas Traditional Authorities to outline.
#' @param highlight_color Highlight outline colour.
#' @param highlight_size Highlight outline width.
#' @param labels Add labels for mapped features.
#' @param label_column Optional label column. Defaults to the level name column.
#' @param label_size Label size.
#' @param label_color Label colour.
#' @param label_repel Use \pkg{ggrepel} for label placement if installed.
#' @param title,subtitle,caption Plot labels.
#' @param legend_title Legend title. Defaults to the fill column.
#' @param legend_position Legend position.
#' @param projection Coordinate reference system.
#' @param scale_bar Add a scale bar if \pkg{ggspatial} is installed.
#' @param north_arrow Add a north arrow if \pkg{ggspatial} is installed.
#' @param interactive Return a plotly object if \pkg{plotly} is installed.
#' @param quiet Suppress join messages.
#' @param ... Additional arguments passed to [ggplot2::geom_sf()].
#'
#' @return A ggplot2 object, or a plotly object when `interactive = TRUE`.
#' @examples
#' \donttest{
#' mw_map()
#'
#' df <- data.frame(
#'   district = c("Lilongwe", "Blantyre", "Mzuzu"),
#'   cases = c(120, 80, 35)
#' )
#' mw_map(df, fill = cases)
#'
#' ta_df <- data.frame(
#'   ta = c("Mabuka", "Mwaulambia"),
#'   coverage = c(72, 64)
#' )
#' mw_map(ta_df, fill = coverage, level = "ta", districts = "Mulanje")
#' }
#' @importFrom ggplot2 aes coord_sf element_blank element_line element_rect
#' @importFrom ggplot2 element_text geom_sf geom_text ggplot labs margin
#' @importFrom ggplot2 scale_fill_gradientn scale_fill_manual scale_fill_discrete
#' @importFrom ggplot2 theme theme_void unit waiver
#' @importFrom rlang ensym as_string
#' @importFrom sf st_centroid st_coordinates st_transform
#' @export
mw_map <- function(
  data = NULL,
  fill,
  unit_col,
  level = 2,
  region = NULL,
  districts = NULL,
  tas = NULL,
  palette = NULL,
  scale_type = c("auto", "continuous", "discrete"),
  reverse = FALSE,
  na_color = "#D7DCE2",
  fill_color = "#F2F4F3",
  border_color = "#252222",
  border_size = 0.25,
  alpha = 1,
  lakes = FALSE,
  lake_color = "#A7D8F0",
  lake_border_color = "#5D9BC2",
  district_borders = NULL,
  district_border_color = "#2F3437",
  district_border_size = 0.45,
  highlight_districts = NULL,
  highlight_tas = NULL,
  highlight_color = "#D7263D",
  highlight_size = 1,
  labels = FALSE,
  label_column = NULL,
  label_size = NULL,
  label_color = "#222222",
  label_repel = FALSE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend_title = NULL,
  legend_position = "right",
  projection = "EPSG:4326",
  scale_bar = FALSE,
  north_arrow = FALSE,
  interactive = FALSE,
  quiet = FALSE,
  ...
) {
  level <- mw_level_key(level)
  scale_type <- match.arg(scale_type)

  fill_col <- if (missing(fill)) NULL else mw_capture_col(substitute(fill), parent.frame())
  unit_col_name <- if (missing(unit_col)) NULL else mw_capture_col(substitute(unit_col), parent.frame())

  map_data <- mw_prepare_map(
    level = level,
    region = region,
    districts = districts,
    tas = tas,
    projection = projection
  )

  if (!is.null(data)) {
    if (inherits(data, "sf")) {
      map_data <- data
      if (!identical(projection, "EPSG:4326")) {
        map_data <- sf::st_transform(map_data, projection)
      }
    } else {
      join_args <- list(
        data = data,
        level = level,
        map = map_data,
        keep_all = TRUE,
        quiet = quiet
      )
      if (!is.null(unit_col_name)) {
        join_args$unit_col <- unit_col_name
      }
      map_data <- do.call(mw_join, join_args)
    }
  }

  if (!is.null(fill_col) && !fill_col %in% names(map_data)) {
    warning(
      "Fill column `", fill_col, "` was not found after joining data. ",
      "Drawing an unfilled map instead.",
      call. = FALSE
    )
    fill_col <- NULL
  }

  if (scale_type == "auto" && !is.null(fill_col)) {
    scale_type <- if (mw_is_discrete(map_data[[fill_col]])) "discrete" else "continuous"
  }
  palette <- palette %||% if (identical(scale_type, "discrete")) "qualitative_2" else "health"

  p <- ggplot2::ggplot()

  if (!is.null(fill_col)) {
    p <- p +
      ggplot2::geom_sf(
        data = map_data,
        ggplot2::aes(fill = .data[[fill_col]]),
        color = border_color,
        linewidth = border_size,
        alpha = alpha,
        ...
      ) +
      mw_fill_scale(
        values = map_data[[fill_col]],
        palette = palette,
        scale_type = scale_type,
        reverse = reverse,
        na_color = na_color,
        name = legend_title %||% mw_pretty_title(fill_col)
      )
  } else {
    p <- p +
      ggplot2::geom_sf(
        data = map_data,
        fill = fill_color,
        color = border_color,
        linewidth = border_size,
        alpha = alpha,
        ...
      )
  }

  if (isTRUE(lakes)) {
    lakes_data <- mwmapdata::major_lakes
    if (!identical(projection, "EPSG:4326")) {
      lakes_data <- sf::st_transform(lakes_data, projection)
    }
    p <- p +
      ggplot2::geom_sf(
        data = lakes_data,
        fill = lake_color,
        color = lake_border_color,
        linewidth = 0.2
      )
  }

  district_borders <- district_borders %||% identical(level, 3L)
  if (isTRUE(district_borders) && level == 3L) {
    district_data <- mw_prepare_map(
      level = 2,
      region = region,
      districts = districts,
      projection = projection
    )
    p <- p +
      ggplot2::geom_sf(
        data = district_data,
        fill = NA,
        color = district_border_color,
        linewidth = district_border_size
      )
  }

  p <- add_highlights(
    p,
    level = level,
    projection = projection,
    region = region,
    districts = highlight_districts,
    tas = highlight_tas,
    color = highlight_color,
    size = highlight_size
  )

  if (isTRUE(labels)) {
    label_column <- label_column %||% mw_name_column(level)
    label_size <- label_size %||% if (level == 3L) 2 else 3
    p <- add_map_labels(
      p,
      map_data,
      label_column = label_column,
      size = label_size,
      color = label_color,
      repel = label_repel
    )
  }

  if (scale_bar) {
    p <- add_scale_bar_mw(p)
  }
  if (north_arrow) {
    p <- add_north_arrow_mw(p)
  }

  p <- p +
    ggplot2::coord_sf(crs = projection, datum = NA, expand = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill = legend_title %||% if (!is.null(fill_col)) mw_pretty_title(fill_col) else NULL
    ) +
    mw_theme(legend_position = legend_position)

  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("Package 'plotly' is required for interactive maps. Returning ggplot.")
    } else {
      p <- plotly::ggplotly(p)
    }
  }

  p
}

mw_fill_scale <- function(
  values,
  palette,
  scale_type,
  reverse = FALSE,
  na_color = "#D7DCE2",
  name = NULL
) {
  if (is.character(palette) && length(palette) == 1L &&
      palette %in% names(malawi_palettes)) {
    return(scale_fill_mw(
      palette = palette,
      reverse = reverse,
      discrete = identical(scale_type, "discrete"),
      na.value = na_color,
      name = name
    ))
  }

  if (is.function(palette)) {
    colors <- palette(if (identical(scale_type, "discrete")) mw_distinct_count(values) else 100)
  } else {
    colors <- palette
  }
  if (reverse) {
    colors <- rev(colors)
  }

  if (identical(scale_type, "continuous")) {
    ggplot2::scale_fill_gradientn(
      colours = colors,
      na.value = na_color,
      name = name
    )
  } else if (is.null(colors)) {
    ggplot2::scale_fill_discrete(
      na.value = na_color,
      name = name
    )
  } else {
    ggplot2::scale_fill_manual(
      values = colors,
      na.value = na_color,
      name = name
    )
  }
}

mw_theme <- function(legend_position = "right") {
  ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 15,
        color = "#1F2933",
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 10.5,
        color = "#4B5563",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = 8.5,
        color = "#68737D",
        margin = ggplot2::margin(t = 8),
        hjust = 1
      ),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold", size = 9.5),
      legend.text = ggplot2::element_text(size = 8.5),
      legend.key.height = ggplot2::unit(0.55, "cm"),
      legend.key.width = ggplot2::unit(0.45, "cm"),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(8, 10, 8, 10)
    )
}

add_highlights <- function(
  p,
  level,
  projection,
  region = NULL,
  districts = NULL,
  tas = NULL,
  color = "#D7263D",
  size = 1
) {
  if (!is.null(districts)) {
    district_data <- mw_prepare_map(
      level = 2,
      region = region,
      districts = districts,
      projection = projection
    )
    p <- p +
      ggplot2::geom_sf(
        data = district_data,
        fill = NA,
        color = color,
        linewidth = size
      )
  }

  if (!is.null(tas)) {
    ta_data <- mw_prepare_map(
      level = 3,
      region = region,
      tas = tas,
      projection = projection
    )
    p <- p +
      ggplot2::geom_sf(
        data = ta_data,
        fill = NA,
        color = color,
        linewidth = size
      )
  }

  p
}

add_map_labels <- function(p, map_data, label_column, size, color, repel = FALSE) {
  if (!label_column %in% names(map_data)) {
    stop("Label column `", label_column, "` was not found.", call. = FALSE)
  }

  suppressWarnings({
    centroids <- sf::st_centroid(map_data)
  })
  coords <- sf::st_coordinates(centroids)
  centroids$.mw_x <- coords[, "X"]
  centroids$.mw_y <- coords[, "Y"]

  if (isTRUE(repel) && requireNamespace("ggrepel", quietly = TRUE)) {
    return(p +
      ggrepel::geom_text_repel(
        data = centroids,
        ggplot2::aes(x = .data$.mw_x, y = .data$.mw_y, label = .data[[label_column]]),
        size = size,
        color = color,
        min.segment.length = 0,
        segment.color = "#9AA3AA",
        segment.linewidth = 0.2,
        box.padding = 0.25
      ))
  }

  p +
    ggplot2::geom_text(
      data = centroids,
      ggplot2::aes(x = .data$.mw_x, y = .data$.mw_y, label = .data[[label_column]]),
      size = size,
      color = color,
      check_overlap = TRUE
    )
}

add_scale_bar_mw <- function(p, location = "br") {
  if (!requireNamespace("ggspatial", quietly = TRUE)) {
    return(p)
  }
  p +
    ggspatial::annotation_scale(
      location = location,
      width_hint = 0.18,
      text_cex = 0.65,
      line_width = 0.35,
      pad_x = ggplot2::unit(0.25, "cm"),
      pad_y = ggplot2::unit(0.25, "cm")
    )
}

add_north_arrow_mw <- function(p, location = "tr") {
  if (!requireNamespace("ggspatial", quietly = TRUE)) {
    return(p)
  }
  p +
    ggspatial::annotation_north_arrow(
      location = location,
      which_north = "true",
      height = ggplot2::unit(0.8, "cm"),
      width = ggplot2::unit(0.8, "cm"),
      pad_x = ggplot2::unit(0.25, "cm"),
      pad_y = ggplot2::unit(0.25, "cm"),
      style = ggspatial::north_arrow_minimal(
        line_col = "#1F2933",
        text_col = "#1F2933"
      )
    )
}
