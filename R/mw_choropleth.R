#' Create a Malawi Choropleth Map
#'
#' A convenient wrapper around [mw_map()] for mapping numeric or categorical
#' values attached to Malawi administrative units.
#'
#' @param data Data frame containing values to map.
#' @param value Column to map to fill colour. May be quoted or unquoted.
#' @param unit_col Column containing administrative unit names. May be quoted or
#'   unquoted. Defaults by `level`.
#' @param level Administrative level. Use `"ta"` for Traditional Authorities.
#' @param palette Malawi palette name, colour vector, or palette function.
#' @param title,subtitle,caption Plot labels.
#' @param legend_title Legend title.
#' @param ... Passed to [mw_map()].
#'
#' @return A ggplot2 object.
#' @examples
#' \donttest{
#' district_data <- data.frame(
#'   district = c("Lilongwe", "Blantyre", "Mzuzu"),
#'   cases = c(120, 80, 35)
#' )
#' mw_choropleth(district_data, cases)
#'
#' ta_data <- data.frame(
#'   ta = c("Mabuka", "Mwaulambia"),
#'   status = c("On track", "Needs support")
#' )
#' mw_choropleth(ta_data, status, level = "ta")
#' }
#' @importFrom rlang ensym as_string
#' @export
mw_choropleth <- function(
  data,
  value,
  unit_col,
  level = 2,
  palette = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend_title = NULL,
  ...
) {
  if (missing(data)) {
    stop("`data` is required.", call. = FALSE)
  }
  if (missing(value)) {
    stop("`value` is required.", call. = FALSE)
  }

  value_name <- mw_capture_col(substitute(value), parent.frame())
  unit_name <- if (missing(unit_col)) NULL else mw_capture_col(substitute(unit_col), parent.frame())

  map_args <- list(
    data = data,
    fill = value_name,
    level = level,
    palette = palette,
    title = title,
    subtitle = subtitle,
    caption = caption,
    legend_title = legend_title,
    ...
  )
  if (!is.null(unit_name)) {
    map_args$unit_col <- unit_name
  }
  do.call(mw_map, map_args)
}
