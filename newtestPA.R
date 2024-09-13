#new test based on https://prioritizr.github.io/wdpar/articles/wdpar.html
# load packages
library(wdpar)
library(dplyr)
library(ggmap)
#test from scratch 
countrylist = c("AFG", "ALB", "AND", "ARE", "ARG", "ARM", "AUS", "AUT", "AZE", "BGD", "BGR", "BIH", "BLR", "BOL", "BRA",
                "BWA", "CAN", "CHE", "CHL", "CMR", "COL", "CRI", "CYP", "CZE", "DEU", "DNK", "DZA", "ECU", "EGY", "ESP",
                "EST", "ETH", "FIN", "FRA", "GBR", "GEO", "GHA", "GRC", "GTM", "HRV", "HTI", "HUN", "IDN", "IND", "IRN",
                "IRQ", "ISL", "ISR", "ITA", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KOR", "KWT", "LBN", "LBY", "LKA",
                "LTU", "MAR", "MDA", "MEX", "MKD", "MMR", "MNE", "MWI", "MYS", "NGA", "NIC", "NLD", "NOR", "NZL", "PAK",
                "PER", "PHL", "POL", "PRK", "PRT", "PSE", "QAT", "ROU", "RUS", "RWA", "SAU", "SGP", "SRB", "SUR", "SVK",
                "SVN", "SWE", "THA", "TJK", "TTO", "TUN", "TUR", "TWN", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VEN",
                "VNM", "YEM", "ZAF", "ZWE")
raw_pa_data <-
  c(countrylist) %>%
  lapply(wdpa_fetch, wait = TRUE,
         download_dir = rappdirs::user_data_dir("wdpar")) %>%
  bind_rows()

# clean protected area data (with procedure for erasing overlaps disabled)
#marine list : https://www.marineregions.org/content.php
full_pa_data <- wdpa_clean(raw_pa_data, erase_overlaps = FALSE)

# at this stage, the data could be filtered based on extra criteria (if needed)
## for example, we could subset the data to only include protected areas
## classified as IUCN category Ia or Ib
sub_pa_data <-
  full_pa_data %>%
  filter(IUCN_CAT %in% c("Ia", "Ib"))

# dissolve all geometries together (removing spatial overlaps)
pa_data <- wdpa_dissolve(sub_pa_data)

# preview data
print(pa_data)