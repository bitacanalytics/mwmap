#' Apply Consistent Layout Styling to Malawi Maps
#'
#' Adds professionally formatted titles, theme elements, and layout options
#' to Malawi maps. Provides a consistent look and feel across visualizations.
#'
#' @param title Character. Main map title. Default: NULL.
#' @param subtitle Character. Map subtitle. Default: NULL.
#' @param caption Character. Map caption (usually data source). Default: NULL.
#' @param legend_position Character or numeric. Legend position: "right", "left",
#'   "top", "bottom", "none", or coordinates c(x, y). Default: "right".
#' @param legend_title Character. Legend title. If NULL, uses existing legend title.
#'   Default: NULL.
#' @param legend_direction Character. Legend layout: "vertical" or "horizontal".
#'   Default: "vertical".
#' @param theme Character. Base theme: "void", "minimal", "classic", or "custom".
#'   Default: "void".
#' @param font_family Character. Base font family. Default: "" (system default).
#' @param title_size Numeric. Title font size. Default: 16.
#' @param title_face Character. Title font face. Default: "bold".
#' @param subtitle_size Numeric. Subtitle font size. Default: 12.
#' @param subtitle_face Character. Subtitle font face. Default: "plain".
#' @param caption_size Numeric. Caption font size. Default: 9.
#' @param caption_face Character. Caption font face. Default: "italic".
#' @param legend_text_size Numeric. Legend text size. Default: 10.
#' @param legend_title_size Numeric. Legend title size. Default: 11.
#' @param legend_title_face Character. Legend title font face. Default: "bold".
#' @param legend_key_size Numeric. Legend key size (in cm). Default: 1.
#' @param legend_spacing Numeric. Legend spacing (in cm). Default: 0.5.
#' @param margin Numeric vector or unit. Plot margins. Default: unit(c(0.2, 0.2, 0.2, 0.2), "cm").
#' @param panel_border Logical. Add panel border? Default: FALSE.
#' @param panel_border_color Character. Panel border color. Default: "grey50".
#' @param background_color Character. Plot background color. Default: "white".
#' @param grid_color Character. Grid line color. Default: "grey90".
#' @param grid_major Logical. Show major grid lines? Default: FALSE.
#' @param grid_minor Logical. Show minor grid lines? Default: FALSE.
#' @param axis_text Logical. Show axis text? Default: FALSE.
#' @param axis_ticks Logical. Show axis ticks? Default: FALSE.
#' @param ... Additional arguments passed to [ggplot2::theme()].
#'
#' @return A list of ggplot2 theme modifications and labs.
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' 
#' # Basic layout
#' mw_map() + 
#'   mw_layout("Malawi Health Districts")
#' 
#' # Full layout with all elements
#' mw_map(fill = "population") + 
#'   mw_layout(
#'     title = "Population Distribution in Malawi",
#'     subtitle = "Data from 2023 Census",
#'     caption = "Source: National Statistical Office",
#'     legend_position = "bottom",
#'     legend_title = "Population",
#'     panel_border = TRUE,
#'     grid_major = TRUE
#'   )
#' 
#' # Minimal layout
#' mw_map() + 
#'   mw_layout(
#'     theme = "minimal",
#'     legend_position = "none"
#'   )
#' 
#' # Custom colors
#' mw_map() + 
#'   mw_layout(
#'     title = "Malawi Map",
#'     background_color = "#f5f5f5",
#'     panel_border = TRUE,
#'     panel_border_color = "darkblue"
#'   )
#' }
#'
#' @importFrom ggplot2 labs theme element_text element_rect element_line margin
#' @importFrom ggplot2 theme_minimal theme_classic theme_void unit element_blank
#' @importFrom grid unit
#' @export
mw_layout <- function(
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend_position = "right",
  legend_title = NULL,
  legend_direction = "vertical",
  theme = c("void", "minimal", "classic", "custom"),
  font_family = "",
  title_size = 16,
  title_face = "bold",
  subtitle_size = 12,
  subtitle_face = "plain",
  caption_size = 9,
  caption_face = "italic",
  legend_text_size = 10,
  legend_title_size = 11,
  legend_title_face = "bold",
  legend_key_size = 1,
  legend_spacing = 0.5,
  margin = grid::unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
  panel_border = FALSE,
  panel_border_color = "grey50",
  background_color = "white",
  grid_color = "grey90",
  grid_major = FALSE,
  grid_minor = FALSE,
  axis_text = FALSE,
  axis_ticks = FALSE,
  ...
) {
  
  # Match theme argument
  theme <- match.arg(theme)
  
  # Create labs object
  labs_obj <- ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  )
  
  # Modify legend title if specified
  if (!is.null(legend_title)) {
    labs_obj$fill <- legend_title
    labs_obj$color <- legend_title
  }
  
  # Base theme based on selection
  if (theme == "void") {
    base_theme <- ggplot2::theme_void()
  } else if (theme == "minimal") {
    base_theme <- ggplot2::theme_minimal()
  } else if (theme == "classic") {
    base_theme <- ggplot2::theme_classic()
  } else {
    base_theme <- ggplot2::theme()
  }
  
  # Custom theme elements
  theme_custom <- ggplot2::theme(
    # Plot appearance
    plot.background = ggplot2::element_rect(
      fill = background_color,
      color = NA
    ),
    plot.margin = margin,
    
    # Title appearance
    plot.title = ggplot2::element_text(
      size = title_size,
      face = title_face,
      family = font_family,
      hjust = 0,
      vjust = 2,
      margin = ggplot2::margin(b = 5)
    ),
    plot.subtitle = ggplot2::element_text(
      size = subtitle_size,
      face = subtitle_face,
      family = font_family,
      hjust = 0,
      margin = ggplot2::margin(b = 10)
    ),
    plot.caption = ggplot2::element_text(
      size = caption_size,
      face = caption_face,
      family = font_family,
      hjust = 1,
      margin = ggplot2::margin(t = 5)
    ),
    
    # Legend appearance
    legend.position = legend_position,
    legend.direction = legend_direction,
    legend.title = ggplot2::element_text(
      size = legend_title_size,
      face = legend_title_face,
      family = font_family
    ),
    legend.text = ggplot2::element_text(
      size = legend_text_size,
      family = font_family
    ),
    legend.key.size = grid::unit(legend_key_size, "cm"),
    legend.spacing = grid::unit(legend_spacing, "cm"),
    legend.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    legend.box.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    
    # Panel appearance
    panel.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    
    # Grid lines
    panel.grid.major = if (grid_major) {
      ggplot2::element_line(color = grid_color, linewidth = 0.2)
    } else {
      ggplot2::element_blank()
    },
    panel.grid.minor = if (grid_minor) {
      ggplot2::element_line(color = grid_color, linewidth = 0.1)
    } else {
      ggplot2::element_blank()
    },
    
    # Axis appearance
    axis.text = if (axis_text) {
      ggplot2::element_text(
        size = 8,
        family = font_family,
        color = "grey50"
      )
    } else {
      ggplot2::element_blank()
    },
    axis.ticks = if (axis_ticks) {
      ggplot2::element_line(color = "grey50")
    } else {
      ggplot2::element_blank()
    },
    axis.title = ggplot2::element_blank()
  )
  
  # Add panel border if requested
  if (panel_border) {
    theme_custom <- theme_custom +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(
          fill = NA,
          color = panel_border_color,
          linewidth = 0.5
        )
      )
  }
  
  # Combine with any additional theme elements
  theme_custom <- theme_custom + 
    ggplot2::theme(...)
  
  # Return combined layout elements
  structure(
    list(
      labs = labs_obj,
      theme = theme_custom
    ),
    class = "mw_layout"
  )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.mw_layout <- function(object, plot, object_name, ...) {
  plot + object$labs + object$theme
}