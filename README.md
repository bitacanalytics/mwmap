# mwmap

**mwmap** provides tools for creating publication-ready maps of Malawi using R and ggplot2. It bundles administrative boundary data at four levels — country, region, district, and traditional authority — along with choropleth mapping, point plotting, and a collection of Malawi-themed colour palettes.

## Installation

Once on CRAN:

``` r
install.packages("mwmap")
```

Development version:

``` r
# install.packages("remotes")
remotes::install_github("bitacanalytics/mwmap")
```

## Quick start

``` r
library(mwmap)

# Basic district map
mw_map()

# Choropleth from your own data
health_data <- data.frame(
  district = c("Lilongwe", "Blantyre", "Mzimba", "Zomba"),
  cases    = c(1245, 2890, 987, 1654)
)

mw_choropleth(health_data, cases, palette = "malaria",
              title = "Malaria cases by district")

# Highlight specific districts
mw_map() +
  mw_highlight(c("Lilongwe", "Blantyre"), fill = "gold")

# Add point locations
facilities <- data.frame(
  name = c("Lilongwe Central Hospital", "Queen Elizabeth Central Hospital"),
  lon  = c(33.78, 35.00),
  lat  = c(-13.98, -15.78)
)
mw_map() + mw_points(facilities, lon, lat)
```

## Available palettes

``` r
view_mw_palettes()   # display all palettes
get_mw_palettes()    # list palette names and categories
```

## Data included

| Dataset       | Description                         | Features |
|---------------|-------------------------------------|----------|
| `mw_level_0`  | National boundary                   | 1        |
| `mw_level_1`  | Regions (Northern/Central/Southern) | 3        |
| `mw_level_2`  | Districts                           | 28       |
| `mw_level_3`  | Traditional Authorities             | \~250    |
| `major_lakes` | Lake Malawi, Malombe, Chilwa        | 3        |

## License

MIT
