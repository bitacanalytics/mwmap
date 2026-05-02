#' Create a Malawi Regions Map
#'
#' Draw Malawi's three administrative regions with the same polished defaults
#' used by [mw_map()].
#'
#' @param data Optional region-level data.
#' @param fill Optional fill column. May be quoted or unquoted.
#' @param region_col Region-name column in `data`. Defaults to `region`.
#' @param palette Malawi palette name, colour vector, or palette function.
#' @param labels Add region labels.
#' @param title,subtitle,caption Plot labels.
#' @param ... Passed to [mw_map()].
#'
#' @return A ggplot2 object.
#' @examples
#' \donttest{
#' mw_regions(labels = TRUE)
#'
#' df <- data.frame(region = c("Northern", "Central", "Southern"),
#'                  value = c(1, 2, 3))
#' mw_regions(df, fill = value)
#' }
#' @export
mw_regions <- function(
  data = NULL,
  fill,
  region_col,
  palette = "regions",
  labels = FALSE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  fill_name <- if (missing(fill)) NULL else mw_capture_col(substitute(fill), parent.frame())
  region_name <- if (missing(region_col)) "region" else mw_capture_col(substitute(region_col), parent.frame())

  map_args <- list(
    data = data,
    unit_col = region_name,
    level = "region",
    palette = palette,
    labels = labels,
    title = title,
    subtitle = subtitle,
    caption = caption,
    ...
  )
  if (!is.null(fill_name)) {
    map_args$fill <- fill_name
  }
  do.call(mw_map, map_args)
}
