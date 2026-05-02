# mwmap 1.0.0

- Reworked the package around a tidy, level-aware mapping API.
- `mw_map()` now handles country, region, district, and Traditional Authority maps through one consistent interface.
- `mw_join()` now joins by the correct administrative unit for each level and reports unmatched names with suggestions.
- Added `mw_get_map()` for direct access to filtered boundary data.
- Added `mw_tas()` for listing Traditional Authorities by region or district.
- `mw_ta_map()` now supports numeric and categorical fills joined by TA name.
- Improved default map styling, fills, legends, labels, district outlines, and highlight layers.
- Updated the package to depend on `mwmapdata >= 1.0.0`.

# mwmap 0.1.0

- Initial CRAN release.
- Spatial boundary data has been moved to the companion package `mwmapdata`, which is installed automatically as a hard dependency — no extra steps needed.
- All mapping functions work exactly as before: `mw_map()`, `mw_choropleth()`, `mw_highlight()`, `mw_ta_map()`, `mw_regions()`, and others.
