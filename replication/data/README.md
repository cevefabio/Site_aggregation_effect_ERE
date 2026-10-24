# Data archive and directory structure

The replication data are distributed separately from the GitHub repository.
Preserve the directory structure below when extracting the archive.

## Archive information

- Permanent archive: **TO BE ADDED**
- DOI: **TO BE ADDED**
- Version used for the paper: **TO BE ADDED**
- Date accessed: **TO BE ADDED**
- Access conditions: **TO BE ADDED**
- Archive checksum: **TO BE ADDED**

A persistent, versioned archive such as Zenodo or OSF is preferable to a
personal cloud-storage link.

## Required original input

Place the anonymized recreation-choice file here:

```text
data/raw/for_estimation_withbio.dat
```

Before public release, confirm that this file is consistent with participant
consent, disclosure-control requirements, and any data-sharing agreement. A
numeric respondent identifier is not, by itself, sufficient evidence that a
dataset is safe to release.

## Required external and site-level inputs

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

The archival release should document the original provider, license, download
URL, and any modifications for each external file.

## Constructed data

Do not place constructed files in the raw-data directory. The two scripts in
`code/01_data/` create the following files in `data/processed/`:

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

## Integrity information to add before release

The final archive should include, for every file:

- filename and relative path;
- file size;
- SHA-256 checksum;
- number of observations and variables, where applicable;
- stable identifiers and merge keys;
- units for travel distance, travel time, population, and monetary variables.
