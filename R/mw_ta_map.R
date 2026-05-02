#' Create a Traditional Authority Map
#'
#' Map Malawi Traditional Authorities. This wrapper is optimised for TA-level
#' work and supports numeric or categorical fills by TA name.
#'
#' @param data Optional data frame with TA-level values.
#' @param fill Optional column to map to fill colour. May be quoted or unquoted.
#' @param ta_col Column in `data` containing TA names. Defaults to `ta`.
#' @param districts Optional district filter.
#' @param region Optional region filter.
#' @param tas Optional TA filter.
#' @param palette Malawi palette name, colour vector, or palette function.
#' @param labels Add TA labels.
#' @param title,subtitle,caption Plot labels.
#' @param district_borders Add district outlines.
#' @param ... Passed to [mw_map()].
#'
#' @return A ggplot2 object.
#' @examples
#' \donttest{
#' mw_ta_map(districts = "Lilongwe")
#'
#' ta_data <- data.frame(
#'   ta = c("Mabuka", "Mwaulambia"),
#'   coverage = c(72, 64)
#' )
#' mw_ta_map(ta_data, fill = coverage, districts = "Mulanje")
#'
#' ta_status <- data.frame(
#'   ta = c("Mabuka", "Mwaulambia"),
#'   status = c("On track", "Needs support")
#' )
#' mw_ta_map(ta_status, fill = status)
#' }
#' @importFrom rlang ensym as_string
#' @export
mw_ta_map <- function(
  data = NULL,
  fill,
  ta_col,
  districts = NULL,
  region = NULL,
  tas = NULL,
  palette = NULL,
  labels = FALSE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  district_borders = TRUE,
  ...
) {
  fill_name <- if (missing(fill)) NULL else mw_capture_col(substitute(fill), parent.frame())
  ta_name <- if (missing(ta_col)) "ta" else mw_capture_col(substitute(ta_col), parent.frame())

  map_args <- list(
    data = data,
    unit_col = ta_name,
    level = "ta",
    region = region,
    districts = districts,
    tas = tas,
    palette = palette,
    labels = labels,
    title = title,
    subtitle = subtitle,
    caption = caption,
    district_borders = district_borders,
    ...
  )
  if (!is.null(fill_name)) {
    map_args$fill <- fill_name
  }
  do.call(mw_map, map_args)
}
