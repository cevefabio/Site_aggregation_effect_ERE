# Data

The data for this project are distributed as a separate Zenodo archive:

- Archive: `hawaii_bw_replication_data.zip`
- Zenodo DOI: **TO BE ADDED**

The archive contains the complete `data/` directory. Users should extract it
in the root of the GitHub repository without changing its internal structure.
The original inputs are kept separate from the datasets constructed by the
code. File sizes and SHA-256 checksums are reported in `DATA_INVENTORY.csv`.

## Original survey data

`data/raw/for_estimation_withbio.dat` contains the Maui residents' recreation
survey data used in Fezzi et al. (2023), including site visits, travel costs,
and site characteristics matched to each respondent.

## Site, environmental, and spatial data

```text
data/external/BWevents.xlsx
data/external/coralPNAS.csv
data/external/grid_centroid_sites_routes3.csv
data/external/gridpop_zipcode3.xlsx
data/external/sites.xlsx
data/external/sites_aggregated.xlsx
data/external/mappe_GIS/hillmau.tif/hillmau.tif
data/external/mappe_GIS/uac15.shp/uac15.cpg
data/external/mappe_GIS/uac15.shp/uac15.dbf
data/external/mappe_GIS/uac15.shp/uac15.prj
data/external/mappe_GIS/uac15.shp/uac15.shp
data/external/mappe_GIS/uac15.shp/uac15.shx
```

The main sources are:

- Brown water event records: Clean Water Branch, Hawaiʻi Department of Health.
- Recreation-site inventory: compiled for Fezzi et al. (2023) from travel
  guides, the Hawaiʻi Statewide GIS Program, Google Maps, and AllTrails.
- Coral cover: Asner et al. (2020).
- Fish biomass and related coastal attributes: Costa and Kendall (2016).
- Road network and geographic information: Hawaiʻi Statewide GIS Program;
  travel speeds were validated using Google Maps.
- Population grid: the gridded Maui population data used in Fezzi et al.
  (2023) for welfare aggregation.
- Urban-area and hillshade layers: geographic inputs used to construct
  Figures 3 and 4.

`sites_aggregated.xlsx` is the site inventory after combining non-coastal
recreation sites into the Other Outdoor Recreation alternative. It is supplied
as an input because it also fixes the alternative ordering used in estimation.

## Constructed data

The two scripts in `code/01_data/` create the following files in
`data/processed/`:

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

Do not place constructed files in `data/raw/` or `data/external/`; rerunning the
preparation scripts should never overwrite the source data.
