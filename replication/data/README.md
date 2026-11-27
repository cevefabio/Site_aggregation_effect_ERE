# Data

The data for this project are distributed in a separate archive:

- Archive: `hawaii_bw_replication_data.zip`
- Zenodo record: [https://doi.org/10.5281/zenodo.23811094](https://doi.org/10.5281/zenodo.23811094)
- Replication code: [GitHub repository](https://github.com/cevefabio/Site_aggregation_effect_ERE/tree/main)

The archive contains the complete `data/` directory. Extract it into the
repository's `replication/` folder so `data/` sits alongside `code/`. Original inputs
are kept separate from datasets constructed by the code. File sizes and
SHA-256 checksums are reported in `DATA_INVENTORY.csv`.

## File inventory and sources

| File or directory | Contents and source |
|---|---|
| `data/raw/for_estimation_withbio.dat` | Maui residents' recreation survey used in Fezzi, Ford, and Oleson (2023), including site visits, travel costs, and site characteristics matched to each respondent. |
| `data/external/BWevents.xlsx` | Brown water event records from the Clean Water Branch, Hawaiʻi Department of Health. |
| `data/external/coralPNAS.csv` | Coral-cover data from Asner et al. (2020). |
| `data/external/grid_centroid_sites_routes3.csv` | Travel routes and distances between population-grid centroids and recreation sites, prepared for Fezzi, Ford, and Oleson (2023). |
| `data/external/gridpop_zipcode3.xlsx` | Gridded Maui population data used to aggregate welfare estimates in Fezzi, Ford, and Oleson (2023). |
| `data/external/sites.xlsx` | Complete recreation-site inventory compiled for Fezzi, Ford, and Oleson (2023) from travel guides, the Hawaiʻi Statewide GIS Program, Google Maps, and AllTrails; it includes the coastal attributes used in estimation. |
| `data/external/sites_aggregated.xlsx` | Site inventory after combining non-coastal sites into the Other Outdoor Recreation alternative. It also fixes the alternative ordering used in estimation. |
| `data/external/mappe_GIS/hillmau.tif/hillmau.tif` | Maui hillshade raster used as a map background. |
| `data/external/mappe_GIS/uac15.shp/` | Urban-area shapefile and its required companion files, obtained through the Hawaiʻi Statewide GIS Program and used in the empirical maps. |

Fish biomass and related coastal attributes included in the site inventory are
based on Costa, Kendall, and Winship (2016). Travel speeds were checked against
Google Maps when the route data were prepared.

## Data citations

- Asner, G. P., Vaughn, N. R., Heckler, J., Knapp, D. E., Balzotti, C.,
  Shafron, E., Martin, R. E., Neilson, B. J., and Gove, J. M. (2020).
  Large-scale mapping of live corals to guide reef conservation.
  *Proceedings of the National Academy of Sciences, 117*(52), 33711--33718.
  [https://doi.org/10.1073/pnas.2017628117](https://doi.org/10.1073/pnas.2017628117)
- Costa, B. M., Kendall, M. S., and Winship, A. J. (2016). *Marine
  biogeographic assessment of the main Hawaiian Islands*. NOAA Technical
  Memorandum NOS NCCOS 214 and BOEM OCS Study 2016-035.
  [https://doi.org/10.7289/V5/TM-NOS-NCCOS-214](https://doi.org/10.7289/V5/TM-NOS-NCCOS-214)
- Fezzi, C., Ford, D. J., and Oleson, K. L. L. (2023). The economic value of
  coral reefs: Climate change impacts and spatial targeting of restoration
  measures. *Ecological Economics, 203*, 107628.
  [https://doi.org/10.1016/j.ecolecon.2022.107628](https://doi.org/10.1016/j.ecolecon.2022.107628)
- Hawaiʻi Department of Health, Clean Water Branch. Water-quality and beach
  monitoring information. [https://health.hawaii.gov/cwb/](https://health.hawaii.gov/cwb/)
- Hawaiʻi Statewide GIS Program. Geospatial data portal.
  [https://geoportal.hawaii.gov/](https://geoportal.hawaii.gov/)
- AllTrails. Recreation-site information.
  [https://www.alltrails.com/](https://www.alltrails.com/)
- Google Maps. Recreation-site locations, routes, and travel-time checks.
  [https://maps.google.com/](https://maps.google.com/)

## Constructed data

The scripts in `code/01_data/` create these files in `data/processed/`:

```text
HW_wide_full_unadjusted.rds
HW_wide_full_adjusted.rds
HW_wide_aggregated_unadjusted.rds
HW_wide_aggregated_adjusted.rds
data_for_post_estimation.rds
```

The adjusted datasets assign one visit to each otherwise unchosen alternative,
as required for estimation of the complete set of site fixed effects. The
unadjusted datasets are retained for the respondent-level bootstrap.

Do not place constructed files in `data/raw/` or `data/external/`. Rerunning
the preparation scripts does not overwrite the source data.
