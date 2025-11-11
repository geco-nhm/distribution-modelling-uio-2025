---
title: "Efficient data access for data reporting"
subtitle: "GBIF Data Access Training, rgbif"
author: "Dag Endresen, https://orcid.org/0000-0002-2352-5497"
date:   "2025-11-11"
output:
  html_document:
    keep_md: true
    toc: true
    toc_depth: 3
---



# Introduction to using SQL with rgbif

## RGBIF SQL


``` r
library(rgbif)
occ_download_sql("SELECT datasetKey, countryCode, COUNT(*) FROM occurrence WHERE continent = 'EUROPE' GROUP BY datasetKey, countryCode")
# https://docs.ropensci.org/rgbif/articles/gbif_sql_downloads.html
```


``` r
# test if your download is set up correctly 
# occ_download_sql_prep("SELECT datasetKey, countryCode, COUNT(*) FROM occurrence WHERE continent = 'EUROPE' GROUP BY datasetKey, countryCode")
occ_download_sql("SELECT datasetKey, countryCode, COUNT(*) FROM occurrence WHERE continent = 'EUROPE' GROUP BY datasetKey, countryCode")
```


``` r
# fetch the download and load the data
occ_download_get("0000967-240425142415019") %>%
  occ_download_import()
```

## Multi-dimension Counts


``` r
sql <-
"
SELECT publishingcountry, specieskey, COUNT(*) as occurrence_count
FROM occurrence
WHERE publishingcountry IS NOT NULL AND specieskey IS NOT NULL
GROUP BY publishingcountry, specieskey
ORDER BY occurrence_count DESC;
"
occ_download_sql(sql)
```
