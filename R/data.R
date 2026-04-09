#' Malawi Administrative Boundaries - Level 0 (Country)
#'
#' National boundary of Malawi at administrative level 0. This represents the
#' outermost border of the country, including major lakes.
#'
#' @format An sf object with 1 feature and 3 fields:
#' \describe{
#'   \item{ADM0_EN}{Country name (Malawi)}
#'   \item{ADM0_PCODE}{Country ISO code (MWI)}
#'   \item{geometry}{MULTIPOLYGON geometry for the national boundary}
#' }
#' @source Malawi Spatial Data Platform (MASDAP) / Department of Surveys
#' @keywords datasets
"mw_level_0"

#' Malawi Administrative Boundaries - Level 1 (Regions)
#'
#' Administrative regions of Malawi (Northern, Central, Southern) at level 1.
#' These are the three main administrative regions.
#'
#' @format An sf object with 3 features and 7 fields:
#' \describe{
#'   \item{ADM1_EN}{Region name (Northern, Central, Southern)}
#'   \item{ADM1_PCODE}{Region code (MW001, MW002, MW003)}
#'   \item{ADM0_EN}{Country name}
#'   \item{ADM0_PCODE}{Country code}
#'   \item{Shape_Leng}{Perimeter length}
#'   \item{Shape_Area}{Area in square meters}
#'   \item{geometry}{MULTIPOLYGON geometry for regional boundaries}
#' }
#' @source Malawi Spatial Data Platform (MASDAP) / Department of Surveys
#' @keywords datasets
"mw_level_1"

#' Malawi Administrative Boundaries - Level 2 (Districts)
#'
#' District-level administrative boundaries for all 28 districts of Malawi.
#' This is the most commonly used administrative level for mapping.
#'
#' @format An sf object with 28 features and 11 fields:
#' \describe{
#'   \item{ADM2_EN}{District name (e.g., Lilongwe, Blantyre, Mzimba)}
#'   \item{ADM2_PCODE}{District code (e.g., MW0201)}
#'   \item{ADM1_EN}{Region name}
#'   \item{ADM1_PCODE}{Region code}
#'   \item{ADM0_EN}{Country name}
#'   \item{ADM0_PCODE}{Country code}
#'   \item{Shape_Leng}{Perimeter length}
#'   \item{Shape_Area}{Area in square meters}
#'   \item{REGION}{Region name (short form)}
#'   \item{DISTRICT}{District name (short form)}
#'   \item{geometry}{MULTIPOLYGON geometry for district boundaries}
#' }
#' @source Malawi Spatial Data Platform (MASDAP) / Department of Surveys
#' @keywords datasets
"mw_level_2"

#' Malawi Administrative Boundaries - Level 3 (Traditional Authorities)
#'
#' Traditional Authority (TA) level administrative boundaries. This represents
#' the third-level subdivisions within districts, governed by traditional leaders.
#'
#' @format An sf object with approximately 250 features and 13 fields:
#' \describe{
#'   \item{ADM3_EN}{Traditional Authority name}
#'   \item{ADM3_PCODE}{Traditional Authority code}
#'   \item{ADM2_EN}{District name}
#'   \item{ADM2_PCODE}{District code}
#'   \item{ADM1_EN}{Region name}
#'   \item{ADM1_PCODE}{Region code}
#'   \item{ADM0_EN}{Country name}
#'   \item{ADM0_PCODE}{Country code}
#'   \item{Shape_Leng}{Perimeter length}
#'   \item{Shape_Area}{Area in square meters}
#'   \item{TA}{Traditional Authority (short form)}
#'   \item{DISTRICT}{District (short form)}
#'   \item{geometry}{MULTIPOLYGON geometry for TA boundaries}
#' }
#' @source Malawi Spatial Data Platform (MASDAP) / Department of Surveys
#' @keywords datasets
"mw_level_3"

#' Major Lakes of Malawi
#'
#' Spatial data for the three major lakes in Malawi:
#' Lake Malawi (Lake Nyasa), Lake Malombe, and Lake Chilwa.
#'
#' @format An sf object with 3 features and 3 fields:
#' \describe{
#'   \item{name}{Lake name (Lake Malawi, Lake Malombe, Lake Chilwa)}
#'   \item{area_km2}{Approximate surface area in square kilometers}
#'   \item{max_depth_m}{Maximum depth in meters (where available)}
#'   \item{geometry}{MULTIPOLYGON geometry for lake boundaries}
#' }
#' @source Malawi Spatial Data Platform (MASDAP) / Department of Surveys
#' @keywords datasets
"major_lakes"

#' Legacy Malawi States Dataset
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' Legacy dataset containing Malawi states. Please use `mw_level_2` instead.
#'
#' @format An sf object (deprecated)
#' @keywords internal
"malawi_data"