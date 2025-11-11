---
title:  "GBIF data use with R >> Occurrence data quality"
subtitle: "GBIF Data Access Training, rgbif"
author: "Dag Endresen, https://orcid.org/0000-0002-2352-5497"
date:   "2025-11-11"
output:
  html_document:
    keep_md: true
    toc: true
    toc_depth: 3
---
<!-- data_quality.html is generated from data_quality.Rmd. Please edit that file -->
***
This session includes examples of accessing GBIF data from R using the [rgbif](https://www.gbif.org/tool/81747/rgbif) [package](https://cran.r-project.org/web/packages/rgbif/index.html) from [rOpenSci](https://ropensci.org/). This script (```data_quality```) provides a small set of examples for data quality checks you may want to perform.
***

### Read in some example data
Using [readxl](http://readxl.tidyverse.org/) from tidyverse.

``` r
library(tidyverse) ## tidyverse is without external dependencies such as Java or Pearl
library(readxl) ## readxl is part of tidyverse
##excel_sheets("./data_quality/spp_dq.xlsx") ## List sheets in a xlsx document
##spp_dq <- read_excel("./data_quality/spp_dq.xlsx", sheet=1, range="A1:BQ101") ## example with range
spp_dq <- readxl::read_excel("./data_quality/spp_dq.xlsx", sheet=1) ## occurrences with some introduced errors
head(spp_dq, n=5)
```
![Dummy occurrences for data quality tests, from Excel file](./data_quality/head_spp_dq.png "spp_dq")

***

### Extract a subset of data-columns

``` r
spp_dq_m <- spp_dq[c("name", "decimalLongitude","decimalLatitude", "basisOfRecord", "year", "month", "day", "eventDate", "country", "countryCode", "stateProvince", "county", "municipality", "taxonKey", "species", "scientificName", "catalogNumber", "occurrenceID")] ## Subset columns
xy_dq <- spp_dq[c("decimalLongitude","decimalLatitude")] ## Extract only the coordinates
#head(spp_dq_m, n=5)
```

***

### Make sf points from the example data

``` r
library(sf)
options(digits=8) ## set 8 digits to keep decimals
spp_dq_m$lon <- as.double(spp_dq_m$decimalLongitude)
spp_dq_m$lat <- as.double(spp_dq_m$decimalLatitude)
spp_dq_sf <- st_as_sf(spp_dq_m, coords = c("lon", "lat"), crs = 4326)
```

***

### Explore the occurrence data

``` r
table(spp_dq$name)
table(spp_dq$country)
table(spp_dq$countryCode)
table(spp_dq$county)
#table(toupper(spp_dq$county)) ## Hint: use toupper() or tolower() if you find issues with CasE
```
![Dummy occurrences for data quality tests, per county](./data_quality/table_spp_dq_county.png "table spp_dq county")

***

### Vector data with country polygons (rnaturalearth + sf)
For more detailed borders use a source such as GADM; for quick demos use Natural Earth.

``` r
library(rnaturalearth)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
plot(st_geometry(world), xlim=c(4, 31), ylim=c(54,64), axes=TRUE)
title("Dummy occurrences plotted on Natural Earth")
points(xy_dq, col='red', pch=20, cex=1)
```
![Dummy occurrences for data quality tests](./data_quality/map_spp_dq_wrld_simpl.png "wrld_simpl")

***

Note: For overlay with `sf`, both layers should share the **same CRS**.
```
 st_crs(world)
 st_crs(spp_dq_sf)
```
Hint: You can use `st_transform()` to modify the CRS, eg:
```
 spp_dq_sf <- st_transform(spp_dq_sf, 4326)
```

***

### Overlay dummy occurrence data with spatial vector data (country borders)

``` r
library(dplyr)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
## spatial join: add world attributes to points
spp_join <- sf::st_join(spp_dq_sf, world[, c("name", "iso_a2", "iso_a3")])
## summarise match vs reported country
spp_join <- spp_join %>%
  mutate(couMatch = !is.na(name) & (toupper(name) == toupper(country))) %>%
  mutate(longitude = decimalLongitude, latitude = decimalLatitude)
head(spp_join, n=30)
table(spp_join$couMatch, useNA = "ifany")
```
![Data quality test for point inside reported country](./data_quality/head_spp_dq_over.png "overlay results")

```
table(spp_dq_over$couMatched, useNA = "ifany")

Norway Sweden   <NA> 
    23     48     29
```

***

### Plot with different color for occurrences not matching country (-polygon)
```
spp_dq_over: ISO2, NAME, country, couMatch, couMatched, longitude, latitude, catalogNumber
```


``` r
i1 <- which(is.na(spp_join$name)) ## with issue (outside any polygon or mismatch)
i2 <- which(!is.na(spp_join$name) & toupper(spp_join$name) == toupper(spp_join$country))
plot(st_geometry(world), xlim=c(4, 31), ylim=c(54,64), axes=TRUE, border="#555555")
title("Dummy test occurrences for data quality control demo")
points(xy_dq, col='blue', pch=20, cex=0.9)
points(xy_dq[i1,], col='red', pch='x', cex=1)
legend("bottomright", title=NULL, legend=c(paste("with issue: ", length(i1), " occurrences"), paste("without issue: ", length(i2), " occurrences")), col=c("red", "blue"), pch=c('x'), cex=0.9, box.col="#777777")
```
![Dummy occurrences with data quality test result](./data_quality/map_spp_dq_issues.png "wrld_simpl")

***

### Leaflet map (with zoom) for occurrences WITH overlay issue


``` r
library(leaflet)
library(RColorBrewer)
spp_issue <- dplyr::select(spp_join[i1,], name = species, country, longitude, latitude, catalogNumber)
spp_issue$longitude <- as.numeric(spp_issue$longitude)
spp_issue$latitude <- as.numeric(spp_issue$latitude)
spp_issue$issue <- "country overlay"
myCol <- colorRampPalette(brewer.pal(11,"Spectral"))(length(unique(spp_issue$name)))
leaflet() %>% addTiles() %>% addCircleMarkers(lng = spp_issue$longitude, lat = spp_issue$latitude, color = myCol)
```
![Dummy occurrences with data quality issues](./data_quality/head_spp_dq_over_m.png "head_spp_dq_over_m")

![Dummy occurrences with data quality issues, in a leaflet map](./data_quality/map_spp_dq_leaf.png "leaflet map")


***
***
***

For this dummy test set I initially selected Poaceae occurrences from Scandinavia...


***

![](./images/gbif-norway-full.png "GBIF banner")
