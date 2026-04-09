#' Malawi Themed Color Palettes
#'
#' A collection of professionally designed color palettes optimized for
#' Malawi maps and data visualization. Palettes are carefully chosen to be
#' colorblind-friendly and print-ready.
#'
#' @format A list of character vectors containing hex color codes.
#' @keywords internal
malawi_palettes <- list(
  # Health & Epidemiology
  health = c("#E8F4F8", "#A7D3E8", "#5EB4E0", "#2C8BC8", "#1A5A9A"),
  malaria = c("#FEEDDE", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603"),
  vaccination = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476"),
  hiv = c(
    "#FCFBFD",
    "#EFEDF5",
    "#DADAEB",
    "#BCBDDC",
    "#9E9AC8",
    "#756BB1",
    "#54278F"
  ),

  # Agriculture & Environment
  agriculture = c("#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#238443"),
  crops = c("#FFFFD4", "#FEE391", "#FEC44F", "#FE9929", "#CC4C02"),
  vegetation = c("#F7F7D9", "#C7E9C0", "#80CDA0", "#35978F", "#01665E"),
  rainfall = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C"),

  # Socioeconomic
  poverty = c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"),
  education = c("#F2F0F7", "#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"),
  population = c("#F7F7F7", "#CCCCCC", "#969696", "#636363", "#252525"),
  development = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#CE1256"),

  # Administrative & Reference
  admin = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6"),
  regions = c("#E41A1C", "#377EB8", "#4DAF4A"), # Northern, Central, Southern
  districts = c(
    "#8DD3C7",
    "#FFFFB3",
    "#BEBADA",
    "#FB8072",
    "#80B1D3",
    "#FDB462",
    "#B3DE69",
    "#FCCDE5",
    "#D9D9D9",
    "#BC80BD"
  ),

  # Sequential (single-hue)
  blues = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C"),
  greens = c("#EDF8E9", "#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#006D2C"),
  oranges = c("#FEEDDE", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603"),
  purples = c("#F2F0F7", "#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"),

  # Diverging
  divergence = c(
    "#2166AC",
    "#4393C3",
    "#92C5DE",
    "#D1E5F0",
    "#F7F7F7",
    "#FDDBC7",
    "#F4A582",
    "#D6604D",
    "#B2182B"
  ),
  temperature = c(
    "#313695",
    "#4575B4",
    "#74ADD1",
    "#ABD9E9",
    "#E0F3F8",
    "#FFFFBF",
    "#FEE090",
    "#FDAE61",
    "#F46D43",
    "#D73027",
    "#A50026"
  ),

  # Qualitative (categorical)
  qualitative_1 = c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#FFFF33",
    "#A65628",
    "#F781BF",
    "#999999"
  ),
  qualitative_2 = c(
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#E5C494",
    "#B3B3B3"
  ),

  # Malawi flag colors
  malawi_flag = c("#000000", "#CE1126", "#339E35"),

  # Lake colors
  lakes = c("#9ECAE1", "#6BAED6", "#3182BD", "#08519C")
)

#' Malawi Fill Scale
#'
#' Apply Malawi-themed color palettes to ggplot2 maps. Supports both continuous
#' and discrete scales with options for colorblind-friendly palettes.
#'
#' @param palette Name of palette. See [get_mw_palettes()] for available options.
#'   Default: "health".
#' @param reverse Logical. Reverse palette order. Default: FALSE.
#' @param discrete Logical. Use discrete scale. Default: FALSE (continuous).
#' @param colorblind_friendly Logical. Use only colorblind-friendly palettes.
#'   Default: FALSE.
#' @param direction Numeric. Direction of palette: 1 = normal, -1 = reversed.
#'   Default: 1.
#' @param ... Additional arguments passed to [ggplot2::scale_fill_gradientn()]
#'   or [ggplot2::discrete_scale()].
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' df <- data.frame(
#'   district = c("Lilongwe", "Blantyre", "Mzimba"),
#'   value = c(10, 20, 30),
#'   category = c("A", "B", "A")
#' )
#' # Continuous scale
#' mw_map(data = df, fill = "value") +
#'   scale_fill_mw("population")
#'
#' # Discrete scale
#' mw_map(data = df, fill = "category") +
#'   scale_fill_mw("qualitative_1", discrete = TRUE)
#'
#' # Reversed
#' mw_map(data = df, fill = "value") +
#'   scale_fill_mw("malaria", reverse = TRUE)
#' }
#'
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @importFrom grDevices colorRampPalette
#' @export
scale_fill_mw <- function(
  palette = "health",
  reverse = FALSE,
  discrete = FALSE,
  colorblind_friendly = FALSE,
  direction = 1,
  ...
) {
  # Get palette
  pal <- malawi_palettes[[palette]]

  if (is.null(pal)) {
    stop(
      "Palette '",
      palette,
      "' not found.\n",
      "Available palettes: ",
      paste(names(malawi_palettes), collapse = ", ")
    )
  }

  # Check colorblind friendly status
  if (colorblind_friendly) {
    cbf_palettes <- c(
      "health",
      "vaccination",
      "blues",
      "greens",
      "oranges",
      "purples",
      "qualitative_2",
      "divergence"
    )

    if (!palette %in% cbf_palettes) {
      warning(
        "Palette '",
        palette,
        "' may not be fully colorblind-friendly. ",
        "Consider using one of: ",
        paste(cbf_palettes, collapse = ", ")
      )
    }
  }

  # Reverse if requested via reverse = TRUE or direction = -1
  if (reverse || identical(direction, -1L) || identical(direction, -1)) {
    pal <- rev(pal)
  }

  # Apply scale
  if (discrete) {
    # Discrete scale
    ggplot2::discrete_scale(
      "fill",
      palette,
      palette = function(n) {
        if (n <= length(pal)) {
          pal[1:n]
        } else {
          grDevices::colorRampPalette(pal)(n)
        }
      },
      ...
    )
  } else {
    # Continuous scale
    ggplot2::scale_fill_gradientn(
      colours = pal,
      ...
    )
  }
}

#' Get Malawi Palette Colors
#'
#' Extract specific colors from Malawi palettes for custom use.
#'
#' @param name Palette name.
#' @param n Number of colors to return. If NULL, returns all colors in palette.
#'   Default: NULL.
#' @param reverse Logical. Reverse palette order. Default: FALSE.
#' @param interpolate Logical. Interpolate to get exactly n colors.
#'   If FALSE, returns first n colors. Default: TRUE.
#'
#' @return A character vector of hex color codes.
#'
#' @examples
#' # Get all health palette colors
#' get_mw_palette("health")
#'
#' # Get 3 colors from malaria palette
#' get_mw_palette("malaria", n = 3)
#'
#' # Get reversed regions palette
#' get_mw_palette("regions", reverse = TRUE)
#'
#' @importFrom grDevices colorRampPalette
#' @export
get_mw_palette <- function(
  name,
  n = NULL,
  reverse = FALSE,
  interpolate = TRUE
) {
  # Get palette
  pal <- malawi_palettes[[name]]

  if (is.null(pal)) {
    stop(
      "Palette '",
      name,
      "' not found.\n",
      "Available palettes: ",
      paste(names(malawi_palettes), collapse = ", ")
    )
  }

  # Reverse if requested
  if (reverse) {
    pal <- rev(pal)
  }

  # Return based on n
  if (is.null(n)) {
    # Return all colors
    return(pal)
  } else {
    # Return n colors
    if (n <= length(pal) && !interpolate) {
      # Take first n
      return(pal[1:n])
    } else {
      # Interpolate
      return(grDevices::colorRampPalette(pal)(n))
    }
  }
}

#' List Available Malawi Palettes
#'
#' Returns a data frame of all available palettes with descriptions.
#'
#' @param category Optional category filter: "health", "agriculture",
#'   "socioeconomic", "sequential", "diverging", "qualitative".
#'
#' @return A data frame with palette names and descriptions.
#'
#' @examples
#' # List all palettes
#' get_mw_palettes()
#'
#' # List only health palettes
#' get_mw_palettes("health")
#'
#' @export
get_mw_palettes <- function(category = NULL) {
  # Palette descriptions
  palette_info <- data.frame(
    name = names(malawi_palettes),
    category = c(
      rep("health", 4),
      rep("agriculture", 4),
      rep("socioeconomic", 4),
      rep("administrative", 3),
      rep("sequential", 4),
      rep("diverging", 2),
      rep("qualitative", 2),
      "administrative",
      "sequential"
    ),
    description = c(
      "Health indicators",
      "Malaria incidence",
      "Vaccination coverage",
      "HIV prevalence",
      "Agricultural productivity",
      "Crop yields",
      "Vegetation indices",
      "Rainfall patterns",
      "Poverty rates",
      "Education levels",
      "Population density",
      "Development indices",
      "Administrative boundaries",
      "Regions (3 colors)",
      "Districts (10 colors)",
      "Sequential blues",
      "Sequential greens",
      "Sequential oranges",
      "Sequential purples",
      "Diverging (blue-red)",
      "Temperature diverging",
      "Qualitative set 1",
      "Qualitative set 2",
      "Malawi flag colors",
      "Lake colors"
    ),
    n_colors = sapply(malawi_palettes, length)
  )

  # Filter by category if specified
  if (!is.null(category)) {
    if (!category %in% unique(palette_info$category)) {
      stop(
        "Invalid category. Available categories: ",
        paste(unique(palette_info$category), collapse = ", ")
      )
    }
    palette_info <- palette_info[palette_info$category == category, ]
  }

  return(palette_info)
}

#' Visualize Malawi Palettes
#'
#' Display all available palettes for visual inspection.
#'
#' @param n Number of colors to show for each palette. Default: NULL (all colors).
#' @param ncol Number of columns in plot. Default: 3.
#'
#' @return A ggplot2 object showing palette swatches.
#'
#' @examples
#' \donttest{
#' # Show all palettes
#' view_mw_palettes()
#'
#' # Show 5 colors from each palette
#' view_mw_palettes(n = 5)
#' }
#'
#' @export
view_mw_palettes <- function(n = NULL, ncol = 3) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.")
  }

  # Get all palettes
  palette_names <- names(malawi_palettes)

  # Prepare data for plotting
  plot_data <- data.frame()

  for (i in seq_along(palette_names)) {
    pname <- palette_names[i]
    pcolors <- malawi_palettes[[pname]]

    if (!is.null(n)) {
      pcolors <- get_mw_palette(pname, n = n)
    }

    plot_data <- rbind(
      plot_data,
      data.frame(
        palette = pname,
        color = pcolors,
        x = seq_along(pcolors),
        y = i
      )
    )
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_tile(ggplot2::aes(
      x = .data$x,
      y = .data$y,
      fill = .data$color
    )) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      breaks = seq_along(palette_names),
      labels = palette_names,
      sec.axis = ggplot2::dup_axis()
    ) +
    ggplot2::labs(
      title = "Malawi Color Palettes",
      x = "Color Index",
      y = "Palette Name"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  return(p)
}
