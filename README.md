# mwmap

`mwmap` makes polished Malawi maps from ordinary R data frames.

It uses the companion `mwmapdata` package for official Malawi administrative
boundaries and gives you a tidy interface for country, region, district, and
Traditional Authority maps.

## Installation

```r
install.packages("mwmap")
```

## Quick Start

```r
library(mwmap)

district_data <- data.frame(
  district = c("Lilongwe", "Blantyre", "Mzuzu"),
  cases = c(120, 80, 35)
)

mw_map(district_data, fill = cases)
```

## Traditional Authority Maps

```r
ta_data <- data.frame(
  ta = c("Mabuka", "Mwaulambia"),
  coverage = c(72, 64),
  status = c("On track", "Needs support")
)

mw_ta_map(ta_data, fill = coverage, districts = "Mulanje")
mw_ta_map(ta_data, fill = status)
```

## Useful Helpers

```r
mw_districts()
mw_tas(districts = "Lilongwe")
mw_get_map("ta", districts = "Mulanje")
mw_join(ta_data, level = "ta")
```

## Design Goals

- A small, tidy API that works with quoted or unquoted column names.
- Robust matching for common spelling, case, punctuation, and suffix variants.
- Numeric and categorical maps that choose professional fill scales by default.
- Clean ggplot2 outputs ready for reports, dashboards, and publications.

