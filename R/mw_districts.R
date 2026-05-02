#' Get Malawi District Names
#'
#' Return district names from the current \pkg{mwmapdata} boundary data.
#'
#' @param region Optional region filter. Accepts `"Northern"`, `"Central"`,
#'   `"Southern"` and shortcuts such as `"n"`, `"c"`, and `"s"`.
#' @param type `"standard"`, `"admin"`, `"short"`, or `"all"`.
#' @param sorted Sort alphabetically.
#' @param include_ta Deprecated. Use [mw_tas()] for Traditional Authorities.
#' @param quiet Suppress messages.
#'
#' @return A character vector, or a data frame when `type = "all"`.
#' @examples
#' mw_districts()
#' mw_districts("Southern")
#' mw_districts(type = "all")
#' @export
mw_districts <- function(
  region = NULL,
  type = c("standard", "admin", "short", "all"),
  sorted = TRUE,
  include_ta = FALSE,
  quiet = FALSE
) {
  type <- match.arg(type)
  if (isTRUE(include_ta)) {
    if (!quiet) {
      warning("`include_ta` is deprecated. Use `mw_tas()` instead.", call. = FALSE)
    }
    return(mw_tas(region = region, type = type, sorted = sorted))
  }

  x <- mwmapdata::mw_level_2
  if (!is.null(region)) {
    wanted <- mw_expand_region(region)
    x <- x[mw_clean_key(x$ADM1_EN) %in% mw_clean_key(wanted), ]
  }

  out <- data.frame(
    standard = x$ADM2_EN,
    admin = x$ADM2_EN,
    short = gsub(" City$", "", x$ADM2_EN),
    region = x$ADM1_EN,
    pcode = x$ADM2_PCODE,
    stringsAsFactors = FALSE
  )

  out <- out[!duplicated(out$standard), ]
  if (isTRUE(sorted)) {
    out <- out[order(out$standard), ]
  }

  if (type == "all") {
    rownames(out) <- NULL
    return(out)
  }
  out[[type]]
}

#' Get Malawi Traditional Authority Names
#'
#' Return Traditional Authority names, with optional region or district filters.
#'
#' @param region Optional region filter.
#' @param districts Optional district filter.
#' @param type `"standard"`, `"admin"`, `"short"`, or `"all"`.
#' @param sorted Sort alphabetically.
#'
#' @return A character vector, or a data frame when `type = "all"`.
#' @examples
#' mw_tas(districts = "Lilongwe")
#' mw_tas(region = "Southern", type = "all")
#' @export
mw_tas <- function(
  region = NULL,
  districts = NULL,
  type = c("standard", "admin", "short", "all"),
  sorted = TRUE
) {
  type <- match.arg(type)
  x <- mw_prepare_map(level = 3, region = region, districts = districts)

  out <- data.frame(
    standard = x$ADM3_EN,
    admin = x$ADM3_EN,
    short = x$ADM3_EN,
    district = x$ADM2_EN,
    region = x$ADM1_EN,
    pcode = x$ADM3_PCODE,
    stringsAsFactors = FALSE
  )

  out <- out[!duplicated(out$pcode), ]
  if (isTRUE(sorted)) {
    out <- out[order(out$district, out$standard), ]
  }

  if (type == "all") {
    rownames(out) <- NULL
    return(out)
  }
  out[[type]]
}

#' Simple District Name Helper
#'
#' Backwards-compatible alias for [mw_districts()].
#'
#' @param region Optional region filter.
#' @return Character vector of district names.
#' @export
mw_districts_simple <- function(region = NULL) {
  mw_districts(region = region, type = "standard")
}

#' Create fallback district data when package data is unavailable
#'
#' Retained for backwards compatibility. mwmap now reads district names from
#' \pkg{mwmapdata}.
#'
#' @param include_ta Ignored.
#' @return Data frame with current district information.
#' @keywords internal
create_fallback_district_data <- function(include_ta = FALSE) {
  if (isTRUE(include_ta)) {
    return(mw_tas(type = "all"))
  }
  mw_districts(type = "all")
}
