################################################################
################################################################
################LIBRAIRIES################################
#https://stackoverflow.com/questions/68211272/push-to-github-stuck-on-rstudio if stuck
require(purrr)
require(tidyverse)
library(here)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(patchwork)
library(ggpmisc)

library(raster)
library(sf)
library(dplyr)
library(exactextractr) # For raster extraction by polygons
library(rasterVis)
library(terra)
################################################################
################################################################
################FUNCTION LOADING################################
functions <- list.files("./rfunctions/", full.names = T) %>%
  purrr::map(source)

################################################################
################################################################
################DATA FORMATING################################
#1 - format governance data
governancedata = formatWGIdata(
  pathgovdata = 'data/wgidataset.xlsx',
  variableWGI = c(
    'VoiceAccount',
    'PoliticalStability',
    'GovEffectiveness',
    'RegulatoryQuality',
    'RuleOfLaw',
    'ControlCorruption'
  )
)

governancedata.managed = governancedata %>%
  filter(year > 2018) %>%
  group_by(Country, isocode) %>%
  dplyr::select(-year) %>%
  summarise_all(mean, na.rm = T) %>%
  ungroup()

#small basic plot - less variation within country than accross countries
ggplot(governancedata, aes(x = year, y = GovEffectiveness, group = Country)) +
  geom_line()

#2 - format landuse data - keep only percentage country and urban percentage
landusedata = read_csv('data/LAND_COVER_08112023114050883.csv') %>%
  filter(MEAS == 'PCNT' & VARIABLE == 'URBAN')

landusedata.managed = landusedata %>%
  filter(Year == '2019') %>%
  dplyr::select(COU, Country, Year, Value)

#3 - format PROTECTED AREAS
mypathnextcloud = '/Users/valentinjourne/Nextcloud/behavioral_climate/prog/protected_areas_formating'
#load rds file name
path.full_pa_data_cleanedpb = list.files(
  paste(mypathnextcloud),
  pattern = 'rds',
  full.names = T
)
#get lust country name
country_list.temp = str_remove(
  path.full_pa_data_cleanedpb,
  pattern = paste0(mypathnextcloud, '/')
)
country_list = str_remove(country_list.temp, pattern = '.rds')
#load files PA
list.pa = list()
#sf_use_s2(FALSE)
for (j in 1:length(path.full_pa_data_cleanedpb)) {
  list.pa[[j]] <- readRDS(paste(path.full_pa_data_cleanedpb[j]))
}
#take some time
#the objective here is to get percentage of Protected Areas - for each countries we have
runing.pa = F
if (runing.pa == T) {
  summarypercentagearea = getPercentagePA_upgraded(
    sfuse = F,
    list.pa,
    country_list
  )
} else {
  summarypercentagearea = qs::qread(here('summarypercentagearea.v082025.qs'))
}

#here we seperate each PA types, but the problem is
#many PA are overlapping so it is overestimating percentage of PA countries (the sum would be more then the previosu dataset)
summarypercentagearea_versionNatRegInter = getPercentagePA(
  sfuse = F,
  list.pa,
  country_list,
  methodrobust = F
)

summarypercentagearea_versionNatRegInter %>%
  mutate(year.factor.status = as_factor(year.factor.status)) %>%
  ggplot(aes(x = percentageByCountry, fill = year.factor.status)) +
  geom_histogram()

##################################################################
#4 - format data of behavior traits
load("./data/covid_gps.RData")


##################################################################
#4 - HDI for me extended - with last data
library(countrycode)
#load and convert to HDI numeric and isocode
HDI = read_excel('data/HDR23-24_Statistical_Annex_HDI_Table.xlsx', skip = 7) %>%
  dplyr::select(2, 3) %>%
  rename(Country = 1, HDIvalue = 2) %>%
  mutate(HDIvalue = as.numeric(as.character(HDIvalue))) %>%
  drop_na() %>%
  mutate(ISOCODE = countrycode(Country, "country.name", "iso3c")) %>%
  drop_na()


################################################################
################################################################
################MAPS 1################################
#get shp world maps
world_sf <- ne_countries(returnclass = "sf", scale = 50) #scale change resoluton maps
world_sf <- ne_countries(returnclass = "sf")

#make boxplot percetange
#use here the initial box version, with information about regional, internation and national PA
# boxplotareas = ggplot(
#   summarypercentagearea_versionNatRegInter %>%
#     dplyr::filter(MARINE == 'terrestrial') %>%
#     filter(DESIG_TYPE != 'Not Applicable'),
#   aes(x = DESIG_TYPE, y = percentageByCountry)
# ) +
#   geom_boxplot() +
#   coord_flip() +
#   ylab('Percentage') +
#   xlab('') +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.title = element_text(size = 15)
#   )

vectorPAcountry = summarypercentagearea %>%
  mutate(per = cum_km2 / sizecountry * 100) %>%
  distinct() %>%
  rename(ISOCODE = country) %>%
  mutate(
    `Protected area surface (%)` = as.numeric(perecentageTOTALovercountry),
    Continent = countrycode::countrycode(
      sourcevar = .[["ISOCODE"]],
      origin = "iso3c",
      destination = "continent"
    )
  )


tt = world_sf %>%
  mutate(ISOCODE = adm0_a3) %>%
  full_join(vectorPAcountry) %>%
  filter(admin != 'Antarctica')

mapsPA = ggplot(data = tt, aes(fill = `Protected area surface (%)`)) +
  geom_sf(col = 'darkred', linewidth = .05) + #plot map of France
  xlab(" ") +
  ylab(" ") +
  coord_sf(crs = "+proj=vandg4") +
  ggpubr::theme_pubr() +
  scale_fill_viridis_c(
    breaks = c(10, 20, 30, 40, 50, 60),
    na.value = "white",
    option = 'magma'
  ) + #rocket ? viridis?
  theme(legend.position = 'bottom') +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(size = .5)
    )
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_blank(), #after update of ggplot ...
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank()
  )

#for results presentation
tt %>%
  group_by(region_wb) %>%
  summarise(
    mean = mean(`Protected area surface (%)`, na.rm = T),
    sd = sd(`Protected area surface (%)`, na.rm = T)
  ) %>%
  arrange(mean)


#new up version maps
# Identify outliers
outliers <- summarypercentagearea_versionNatRegInter %>%
  right_join(
    covid_gps %>%
      dplyr::select(-country) %>%
      rename(country = isocode) %>%
      dplyr::select(country)
  ) %>%
  dplyr::select(DESIG_TYPE, percentageByCountry, country) %>%
  distinct() %>%
  group_by(country, DESIG_TYPE) %>%
  slice_max(percentageByCountry) %>%
  ungroup() %>%
  group_by(DESIG_TYPE) %>%
  drop_na(percentageByCountry) %>%
  mutate(
    is_outlier = percentageByCountry <
      quantile(percentageByCountry, 0.25) - 1.5 * IQR(percentageByCountry) |
      percentageByCountry >
        quantile(percentageByCountry, 0.75) + 1.5 * IQR(percentageByCountry)
  ) %>%
  filter(is_outlier) %>%
  arrange(desc(percentageByCountry)) %>%
  slice_head(n = 5) %>%
  mutate(ISOCODE = country) %>%
  left_join(
    world_sf %>% mutate(ISOCODE = adm0_a3) %>% dplyr::select(ISOCODE, name_en)
  )

boxplotareas.v2 = ggplot(
  summarypercentagearea_versionNatRegInter %>%
    right_join(
      covid_gps %>% dplyr::select(-country) %>% rename(country = isocode)
    ) %>%
    drop_na(percentageByCountry) %>%
    dplyr::select(DESIG_TYPE, percentageByCountry, country) %>%
    distinct() %>%
    group_by(country, DESIG_TYPE) %>%
    slice_max(percentageByCountry) %>%
    ungroup(),
  aes(x = DESIG_TYPE, y = percentageByCountry)
) +
  geom_boxplot() +
  #coord_flip()+
  ylab('Percentage') +
  xlab('') +
  theme_bw() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 13)
  ) +
  ggrepel::geom_text_repel(
    data = outliers,
    aes(label = name_en),
    color = "darkblue",
    nudge_x = 0.3, # Optional: Slight nudge to adjust label positions
    max.overlaps = Inf, # To ensure all labels are displayed
    size = 3.5
  )
boxplotareas.v2
#

#for results summary
summarypercentagearea_versionNatRegInter %>%
  dplyr::filter(MARINE == 'terrestrial') %>%
  filter(DESIG_TYPE != 'Not Applicable') %>%
  group_by(DESIG_TYPE) %>%
  summarise(mean = mean(percentageByCountry), sd = sd(percentageByCountry))

summarypercentagearea_versionNatRegInter %>%
  group_by(IUCN_CAT) %>%
  summarise(mean = mean(percentageByCountry), sd = sd(percentageByCountry))

boxplotareas.iucn = ggplot(
  summarypercentagearea_versionNatRegInter %>%
    right_join(
      covid_gps %>% dplyr::select(-country) %>% rename(country = isocode)
    ) %>%
    dplyr::filter(MARINE == 'terrestrial'),
  aes(x = IUCN_CAT, y = percentageByCountry)
) +
  geom_boxplot() +
  coord_flip() +
  ylab('Percentage') +
  xlab('') +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15)
  )
# #updated jan 2025
cowplot::save_plot(
  "iucnbox.png",
  plot_grid(
    boxplotareas.v2 + coord_flip(),
    boxplotareas.iucn,
    labels = "auto",
    ncol = 1,
    nrow = 2,
    align = "v"
  ),
  ncol = 1,
  nrow = 2,
  dpi = 300
)
# #sup fig PA annexe
vectorPAcountry.PAtype = summarypercentagearea_versionNatRegInter %>%
  right_join(
    covid_gps %>%
      dplyr::select(-country) %>%
      rename(country = isocode) %>%
      dplyr::select(country)
  ) %>%
  dplyr::filter(MARINE == 'terrestrial') %>%
  filter(DESIG_TYPE != 'Not Applicable') %>%
  group_by(DESIG_TYPE, country) %>%
  summarise(sumPAtype = sum(percentageByCountry)) %>%
  rename(ISOCODE = country) %>%
  mutate(`Protected area surface (%)` = as.numeric(sumPAtype))


length(unique(vectorPAcountry.PAtype$ISOCODE))
#71 countries = OK
tt.sub = world_sf %>%
  mutate(ISOCODE = adm0_a3) %>%
  full_join(
    vectorPAcountry.PAtype %>%
      right_join(
        covid_gps %>%
          dplyr::select(-country) %>%
          rename(ISOCODE = isocode) %>%
          dplyr::select(ISOCODE)
      ) %>%
      ungroup() %>%
      dplyr::select(`Protected area surface (%)`, ISOCODE, DESIG_TYPE) %>%
      tidyr::complete(
        ISOCODE,
        DESIG_TYPE,
        fill = list(`Protected area surface (%)` = NA)
      )
  ) %>%
  filter(admin != 'Antarctica') %>%
  drop_na(DESIG_TYPE)

maps.PA.subset = ggplot(
  data = tt.sub,
  aes(fill = `Protected area surface (%)`)
) +
  geom_sf(col = 'darkred', linewidth = .05) + #plot map of France
  xlab(" ") +
  ylab(" ") +
  coord_sf(crs = "+proj=vandg4") +
  ggpubr::theme_pubr() +
  scale_fill_viridis_c(
    breaks = c(10, 20, 30, 40, 50, 60),
    na.value = "grey80",
    option = 'magma'
  ) + #rocket ? viridis?
  theme(legend.position = 'bottom') +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(size = .5)
    )
  ) +
  theme(
    legend.title = element_text(size = 12, family = "Arial Narrow"),
    legend.text = element_text(size = 10, family = "Arial Narrow"),
    axis.text = element_blank(), #after update of ggplot ...
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank()
  ) +
  facet_grid(. ~ DESIG_TYPE)

cowplot::save_plot(
  "figures/typeOfPAs.png",
  maps.PA.subset,
  ncol = 2.5,
  nrow = 1.9,
  dpi = 300
)
################################################################
################################################################
################DATA COMBINATION ################################
################################################################
################################################################
################WITH GPS################################
library(FactoMineR)
library(factoextra)
#selected columns - intiial without robust method
# PAterrestre = summarypercentagearea %>%
#   dplyr::filter(MARINE == 'terrestrial') %>%
#   filter(DESIG_TYPE != 'Not Applicable') %>%
#   rename(isocode = country) %>%
#   group_by(sizecountry, isocode) %>% #DESIG_TYPE
#   summarise_at(vars(area_km, percentageByCountry), lst( sum), na.rm = T)

#alternaive
PAterrestre = summarypercentagearea %>%
  mutate(per = cum_km2 / sizecountry * 100) %>%
  distinct() %>%
  rename(ISOCODE = country) %>%
  mutate(
    `Protected area surface (%)` = as.numeric(perecentageTOTALovercountry),
    Continent = countrycode::countrycode(
      sourcevar = .[["ISOCODE"]],
      origin = "iso3c",
      destination = "continent"
    )
  ) %>%
  rename(country = ISOCODE) %>%
  dplyr::select(
    country,
    sizecountry,
    Continent,
    perecentageTOTALovercountry
  ) %>%
  distinct() %>%
  rename(isocode = country) %>%
  mutate(size.PA.km = perecentageTOTALovercountry / 100 * sizecountry)

# #itiniatlu I got this
# PAterrestre = summarypercentagearea %>%
#   rename(isocode = country) %>%
#   mutate(percentageByCountry_sum = as.numeric(perecentageTOTALovercountry)) %>%
#   dplyr::select(isocode, percentageByCountry_sum) %>%
#   distinct()

select <- dplyr::select
#redo with left join stuff
gps <- covid_gps %>%
  select(
    "isocode",
    "continent",
    "country",
    "population_density",
    "human_development_index",
    "gdp_per_capita",
    "patience", # , "polstab" ,"account","gov_eff","regqual","corrupt","rulelaw",
    "risktaking",
    "posrecip",
    "negrecip",
    "altruism",
    "trust"
  )

PAgps = PAterrestre %>%
  left_join(gps) %>%
  left_join(
    landusedata.managed %>%
      dplyr::select(-Country, -Year) %>%
      rename(isocode = COU, PercentageUrban = Value)
  ) %>%
  left_join(governancedata.managed %>% dplyr::select(-Country)) %>%
  drop_na('gdp_per_capita')

colnames(PAgps)

#count number of PA

#nb.pa = do.call(bind_rows, list.pa)
#subset.pa.nb = nb.pa %>% right_join(PAgps, by = join_by(ISO3 == isocode))

#do a PCA with filled species
PAgps.sub <- PAgps %>%
  ungroup() %>%
  dplyr::select(
    'isocode',
    "human_development_index",
    "gdp_per_capita",
    'population_density',
    'PercentageUrban',
    'VoiceAccount',
    "PoliticalStability",
    "GovEffectiveness",
    "RegulatoryQuality",
    "RuleOfLaw",
    "ControlCorruption"
  ) %>%
  distinct() %>%
  mutate(
    gdp_per_capita = scale(log10(gdp_per_capita), center = TRUE, scale = TRUE),
    human_development_index = scale(
      (human_development_index),
      center = T,
      scale = T
    ),
    PercentageUrban = scale(PercentageUrban, center = T, scale = T),
    population_density = scale(log10(population_density), center = T, scale = T)
  ) %>%
  as.data.frame #%>% dplyr::select(-Species)   #CV, ACF1,
row.names(PAgps.sub) <- PAgps.sub$isocode
PAgps.sub.analysis <- PAgps.sub[c(2:(ncol(PAgps.sub) - 1))] #6 traits variable
PCAnew <- ade4::dudi.pca(
  PAgps.sub.analysis,
  center = FALSE,
  scale = FALSE,
  scannf = F,
  nf = ncol(PAgps.sub.analysis)
)
screeplot(PCAnew, main = "Screeplot - Eigenvalues")
ade4::s.corcircle(PCAnew$co, xax = 1, yax = 2)
ade4::s.corcircle(PCAnew$co, xax = 3, yax = 2)
factoextra::fviz_eig(PCAnew, addlabels = TRUE, ylim = c(0, 100))
factoextra::fviz_pca_var(PCAnew, col.var = "black", axes = c(3, 2))

#after PCA, we'll make some analysis
PAgps.PCA <- cbind(PAgps, PCAnew$li[, 1], PCAnew$li[, 2]) %>%
  tibble() %>%
  rename(PCA1 = 21, PCA2 = 22) %>%
  #mutate(
  #  beta.PA = value / 100, #percentageByCountry_sum / 100,
  #  logit.PA = car::logit(beta.PA)
  #) %>%
  mutate(
    gdp_per_capita = scale(log10(gdp_per_capita), center = TRUE, scale = TRUE)
  ) %>%
  right_join(
    summarypercentagearea %>%
      dplyr::select(-IUCN_CAT, -DESIG_TYPE, -percentageByCountry, -area_km) %>%
      distinct() %>%
      rename(isocode = country)
  )

#DO HERE NEW MAPS
tt.traits = world_sf %>%
  mutate(isocode = adm0_a3) %>%
  dplyr::select(isocode, admin) %>%
  full_join(
    PAgps.PCA %>%
      dplyr::select(altruism, trust, risktaking, patience, isocode) %>%
      rename(Altruism = 1, Trust = 2, Risktaking = 3, Patience = 4) %>%
      pivot_longer(-isocode, names_to = 'variable', values_to = "Value")
  ) %>%
  filter(admin != 'Antarctica')

pivoted_data <- PAgps.PCA %>%
  select(isocode, altruism, trust, risktaking, patience) %>%
  rename(
    Altruism = altruism,
    Trust = trust,
    Risktaking = risktaking,
    Patience = patience
  ) %>%
  pivot_longer(
    cols = c(Altruism, Trust, Risktaking, Patience),
    names_to = "variable",
    values_to = "Value"
  )
vars <- c("Altruism", "Trust", "Risktaking", "Patience")

# Expand grid to get every (isocode, variable) pair
expanded_grid <- expand.grid(
  isocode = unique(world_sf$adm0_a3), # or however your ISO code is stored
  variable = vars,
  stringsAsFactors = FALSE
)
tt.traits <- expanded_grid %>%
  left_join(
    world_sf %>%
      select(isocode = adm0_a3, admin, geometry),
    by = "isocode"
  ) %>%
  # 2) Merge pivoted traits
  left_join(
    pivoted_data,
    by = c("isocode", "variable")
  )

# Convert to sf if needed (only if 'geometry' is a valid sf column)
tt.traits <- st_as_sf(tt.traits) %>%
  filter(admin != 'Antarctica')

traits.maps = ggplot(data = tt.traits, aes(fill = Value)) +
  geom_sf(col = 'grey', linewidth = .05) + #plot map of France
  xlab(" ") +
  ylab(" ") +
  facet_wrap(. ~ variable, nrow = 1) +
  coord_sf(crs = "+proj=vandg4") +
  ggpubr::theme_pubr() +
  scale_fill_viridis_c(
    breaks = c(-.5, 0, .5),
    na.value = "white",
    option = 'viridis',
    direction = -1
  ) + #rocket ? viridis?
  theme(legend.position = 'bottom') +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(size = .5)
    )
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10),
    legend.key.size = unit(.25, 'cm'),
    panel.spacing = unit(-0.2, 'lines'),
    axis.text = element_blank(), #after update of ggplot ...
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank()
  )
traits.maps

#reduce here to the 75 countries used
tt = world_sf %>%
  mutate(ISOCODE = adm0_a3) %>%
  full_join(
    vectorPAcountry %>% right_join(PAgps.PCA %>% rename(ISOCODE = isocode))
  ) %>%
  filter(admin != 'Antarctica') %>%
  rename(`Protected area coverage (%)` = `Protected area surface (%)`)

mapsPA = ggplot(data = tt, aes(fill = `Protected area coverage (%)`)) +
  geom_sf(col = 'grey', linewidth = .05) + #plot map of France
  xlab(" ") +
  ylab(" ") +
  coord_sf(crs = "+proj=vandg4") +
  ggpubr::theme_pubr() +
  scale_fill_viridis_c(
    breaks = c(10, 20, 30, 40, 50, 60),
    na.value = "white",
    option = 'cividis'
  ) + #rocket ? viridis?
  theme(legend.position = 'bottom') +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(size = .5)
    )
  ) +
  theme(
    legend.title = element_text(size = 12, family = "Arial Narrow"),
    legend.text = element_text(size = 12, family = "Arial Narrow"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-20, -20, -20, -20),
    legend.key.size = unit(.25, 'cm'),
    axis.text = element_blank(), #after update of ggplot ...
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank()
  )

mapsPA


#model TMB
library(glmmTMB)
library(brms)
colnames(PAgps.PCA)

PAgps.PCA.final = PAgps.PCA %>%
  mutate(
    ChangePA.percentage = cum_km2 / sizecountry * 100,
    ChangePA.scaled = cum_km2 / sizecountry
  ) %>%
  mutate(
    isocode.factor = as_factor(isocode),
    continent.factor = as_factor(continent),
    year.factor = as_factor(year),
    increment_km2 = if_else(
      is.na(is_same_as_previous),
      NA,
      (cum_km2 - lag(cum_km2, default = 0))
    ),
    increment_rate = increment_km2 / sizecountry
  ) %>%
  drop_na(trust) %>%
  filter(year > 1947) %>%
  filter(year < 2023) %>%
  mutate(increment_rate_betar = y.transf.betareg(increment_rate)) #does not really matter, just remove those country that does not match trait data set

ggplot(
  PAgps.PCA.final,
  aes(y = increment_rate_betar, x = year, group = country)
) +
  geom_line()

mod.inc.test = glmmTMB(
  increment_rate_betar ~
    PCA1 +
      PCA2 +
      altruism +
      trust +
      risktaking +
      patience +
      (1 | year.factor) +
      (1 | isocode),
  data = PAgps.PCA.final,
  family = beta_family()
)
summary(mod.inc.test)

last_vals <- PAgps.PCA.final %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  summarize(
    last_year = max(year, na.rm = TRUE),
    last_val = ChangePA.scaled[which.max(year)],
    isocode = dplyr::last(na.omit(isocode)),
    .groups = "drop"
  )

topN <- 10
top_countries <- last_vals %>%
  slice_max(last_val, n = topN, with_ties = FALSE) %>%
  pull(country)

label_pts <- PAgps.PCA.final %>%
  semi_join(tibble(country = top_countries), by = "country") %>%
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

library(ggrepel)
plot.change.pa.time = ggplot(
  PAgps.PCA.final,
  aes(x = year, y = ChangePA.scaled, group = country)
) +
  geom_line(color = "grey80", linewidth = 0.4) +
  geom_line(
    data = ~ dplyr::filter(.x, country %in% top_countries),
    aes(color = country),
    linewidth = 0.9,
    alpha = 0.9,
    show.legend = FALSE
  ) +
  geom_point(
    data = label_pts,
    aes(color = country),
    size = 1.8,
    show.legend = FALSE
  ) +
  # geom_vline(xintercept = 1992, linetype = "dashed") +
  # annotate(
  #   geom = "text",
  #   x = 1992 - 1.2,
  #   y = 0.6,
  #   label = "Rio",
  #   family = "Arial Narrow",
  #   colour = 'black',
  #   angle = 90
  # ) +
  # geom_vline(xintercept = 1972, linetype = "dashed") +
  # annotate(
  #   geom = "text",
  #   x = 1972 - 1.2,
  #   y = 0.6,
  #   label = "Stockholm",
  #   family = "Arial Narrow",
  #   colour = 'black',
  #   angle = 90
  # ) +
  # geom_vline(xintercept = 2022, linetype = "dashed") +
  # annotate(
  #   geom = "text",
  #   x = 2022 - 1,
  #   y = 0.6,
  #   label = "Montreal",
  #   family = "Arial Narrow",
  #   colour = 'black',
  #   angle = 90
  # ) +
  # geom_vline(xintercept = 2010, linetype = "dashed") +
  # annotate(
  #   geom = "text",
  #   x = 2010 - 1.2,
  #   y = 0.6,
  #   label = "Aichi",
  #   family = "Arial Narrow",
  #   colour = 'black',
  #   angle = 90
  # ) +
  geom_label_repel(
    data = label_pts,
    aes(label = country, color = country),
    size = 3,
    label.size = 0.15,
    box.padding = 0.25,
    point.padding = 0.15,
    direction = "y",
    segment.size = 0.2,
    xlim = c(2023, 2050),
    nudge_x = 1,
    show.legend = FALSE
  ) +
  scale_color_brewer(palette = "PuOr") +
  labs(x = "Year", y = "Cumulative protected area coverage (%)") +
  ylim(0, 0.6) +
  scale_y_continuous(labels = scales::percent) +
  hrbrthemes::theme_ipsum(base_size = 14, axis_title_size = 16) +
  coord_cartesian(clip = "off", xlim = c(1950, 2025)) + # Allow drawing outside the panel
  theme(
    plot.margin = unit(c(1, 4, 1, 1), "lines") # Increase right margin
  )

plot.change.pa.time

p_hist <- ggplot(PAgps.PCA.final, aes(x = ChangePA.scaled)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black", size = .1) +
  coord_flip() + # flip so it aligns vertically
  hrbrthemes::theme_ipsum(base_size = 14, axis_title_size = 16) +
  xlim(0, 0.6) +
  theme(
    axis.text.y = element_blank(),
    axis.text.y.left = element_blank()
  ) +
  labs(x = NULL, y = "Count")

plotmaps.v2 = mapsPA +
  plot.change.pa.time +
  plot_layout(widths = c(6, 2)) +
  plot_annotation(tag_levels = "a")

cowplot::save_plot(
  "figures/plotmaps.png",
  plotmaps.v2,
  ncol = 2,
  nrow = 1.5,
  dpi = 300
)


#########################################################################################################
#########################################################################################################
#########################################################################################################
##########################################
#####################
#it is not related to final increment, speed
#https://www.nature.com/articles/nmeth.3137
#https://peerj.com/articles/616/
#https://www.andrewheiss.com/blog/2021/12/01/multilevel-models-panel-data-guide/
#Country-level random intercepts absorb unobserved, time-invariant heterogeneity (e.g., geographic or cultural factors), while year-level random effects capture global temporal dynamics (e.g., policy shifts or data reporting changes)
unique.cont = PAgps.PCA.final %>%
  dplyr::select(isocode, continent) %>%
  distinct()
table(unique.cont$continent)

13 / 54 #africa
1 / 16 #oceania
19 / 48

m.gps.up = glmmTMB(
  ChangePA.scaled ~
    altruism +
      trust +
      patience +
      risktaking +
      PCA1 +
      PCA2 +
      (1 | isocode.factor) +
      (1 | year.factor),
  data = PAgps.PCA.final, #%>% filter(!country %in%c("Cambodia", "Venezuela")) %>% filter(year > 1980)
  #ziformula = ~1,
  family = beta_family()
)
summary(m.gps.up)

library(broom.mixed)
parameters.std = parameters::standardize_parameters(m.gps.up, method = "refit")
parameters.std %>%
  as_tibble() %>%
  filter(Parameter != "(Intercept)") %>%
  left_join(
    broom.mixed::tidy(m.gps.up) %>%
      rename(Parameter = term) %>%
      mutate(significnace = ifelse(p.value < .05, 'y', 'ns'))
  ) %>%
  ggplot(aes(
    x = Std_Coefficient,
    y = Parameter,
    col = significnace,
    fill = significnace
  )) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(aes(
    x = CI_low,
    y = Parameter,
    xend = CI_high,
    yend = Parameter
  )) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  xlab('Coefficient value') +
  theme(legend.position = 'none') +
  ylab('')

simulation.m.gps <- simulateResiduals(
  fittedModel = m.gps.up,
  quantreg = T,
  n = 1000
)
plot(simulation.m.gps)
performance::check_model(m.gps.up)
performance::model_performance(m.gps.up)

summary(m.gps.up)
parameters.std = parameters::standardize_parameters(m.gps.up, method = "refit")


m.gps.1 = glmmTMB(
  ChangePA.scaled ~
    altruism +
      trust +
      patience +
      risktaking +
      (1 | isocode.factor) +
      (1 | year.factor),
  data = PAgps.PCA.final, #%>% filter(!country %in%c("Cambodia", "Venezuela")) %>% filter(year > 1980)
  #ziformula = ~1,
  family = beta_family()
)


m.pca.1 = glmmTMB(
  ChangePA.scaled ~
    PCA1 +
      PCA2 +
      (1 | isocode.factor) +
      (1 | year.factor),
  data = PAgps.PCA.final, #%>% filter(!country %in%c("Cambodia", "Venezuela")) %>% filter(year > 1980)
  #ziformula = ~1,
  family = beta_family()
)
m.gps.gdp.1 = glmmTMB(
  ChangePA.scaled ~
    altruism +
      trust +
      patience +
      risktaking +
      gdp_per_capita +
      (1 | isocode.factor) +
      (1 | year.factor),
  data = PAgps.PCA.final, #%>% filter(!country %in%c("Cambodia", "Venezuela")) %>% filter(year > 1980)
  #ziformula = ~1,
  family = beta_family()
)

m.gdp.1 = glmmTMB(
  ChangePA.scaled ~
    gdp_per_capita +
      (1 | isocode.factor) +
      (1 | year.factor),
  data = PAgps.PCA.final, #%>% filter(!country %in%c("Cambodia", "Venezuela")) %>% filter(year > 1980)
  #ziformula = ~1,
  family = beta_family()
)

m.gps.hdi.1 = glmmTMB(
  ChangePA.scaled ~
    altruism +
      trust +
      patience +
      risktaking +
      human_development_index +
      (1 | isocode.factor) +
      (1 | year.factor),
  data = PAgps.PCA.final, #%>% filter(!country %in%c("Cambodia", "Venezuela")) %>% filter(year > 1980)
  #ziformula = ~1,
  family = beta_family()
)

m.hdi.1 = glmmTMB(
  ChangePA.scaled ~
    human_development_index +
      (1 | isocode.factor) +
      (1 | year.factor),
  data = PAgps.PCA.final, #%>% filter(!country %in%c("Cambodia", "Venezuela")) %>% filter(year > 1980)
  #ziformula = ~1,
  family = beta_family()
)


MuMIn::AICc(m.pca.1, m.gps.gdp.1, m.gps.hdi.1, m.gdp.1, m.hdi.1, m.gps.up)
cand.set <- model.sel(
  m.pca.1,
  m.gps.gdp.1,
  m.gps.hdi.1,
  m.gdp.1,
  m.hdi.1,
  m.gps.up
)

performance::model_performance(m.gps.up)
performance::model_performance(m.pca.1)
performance::model_performance(m.hdi.1)
performance::model_performance(m.gdp.1)

cor(qlogis(PAgps.PCA.final$ChangePA.scaled), predict(m.pca.1, type = "link"))^2
cor(qlogis(PAgps.PCA.final$ChangePA.scaled), predict(m.hdi.1, type = "link"))^2
cor(qlogis(PAgps.PCA.final$ChangePA.scaled), predict(m.gdp.1, type = "link"))^2
cor(qlogis(PAgps.PCA.final$ChangePA.scaled), predict(m.gps.up, type = "link"))^2

#now testing changing year threshold

summary(PAgps.PCA.final$year)

#I want more or less 20 years to make more fair comparison
vector.time = seq(1948, 2000, 1)

datalist.out.slope.list = vector("list", length = length(vector.time))


for (i in 1:length(vector.time)) {
  year.selection = vector.time[i]
  PAgps.PCA.final.subseted = PAgps.PCA.final %>% filter(year > year.selection)

  m.gps.timing.year = glmmTMB(
    ChangePA.scaled ~
      altruism +
        trust +
        patience +
        risktaking +
        PCA1 +
        PCA2 +
        (1 | isocode.factor) +
        (1 | year.factor),
    data = PAgps.PCA.final.subseted,
    family = beta_family()
  )

  broomout = broom::tidy(m.gps.timing.year) %>%
    filter(
      term %in% c("altruism", "trust", "patience", "risktaking", "PCA1", "PCA2")
    )
  parameters.std.time = parameters::standardize_parameters(
    m.gps.timing.year,
    method = "refit"
  ) %>%
    as_tibble()

  datalist.out.slope.list[[i]] = broomout %>%
    left_join(parameters.std.time %>% rename(term = Parameter)) %>%
    mutate(year.subset = year.selection)
}

datalist.out.slope <- dplyr::bind_rows(datalist.out.slope.list) %>%
  mutate(adj.p = p.adjust(p.value, method = "fdr"))

trends.slope.time = datalist.out.slope %>%
  mutate(
    term = dplyr::recode(
      term,
      "altruism" = "Altruism",
      'trust' = "Trust",
      "patience" = "Patience",
      "risktaking" = "Risktaking",
      'PCA1' = 'Governance and \nEconomic axis',
      'PCA2' = 'Population and \nUrbanisation axis'
    )
  ) %>%
  mutate(
    term = factor(
      term,
      levels = c(
        "Trust",
        "Altruism",
        "Patience",
        "Risktaking",
        'Governance and \nEconomic axis',
        'Population and \nUrbanisation axis'
      )
    )
  ) %>% #filter(term %in% c("altruism", "trust", "patience", "risktaking")) %>%
  ggplot(aes(x = year.subset, y = Std_Coefficient, col = (adj.p))) +
  geom_pointrange(aes(ymin = CI_low, ymax = CI_high), alpha = .7) +
  geom_point(shape = 21, col = "black") +
  facet_wrap(. ~ term, ncol = 3) +
  scale_color_viridis_c(name = "Adj. p-value", option = "mako") +
  labs(x = "Starting year for GLMMs analysis", y = "Standardised coefficient") +
  hrbrthemes::theme_ipsum(base_size = 14, axis_title_size = 16) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")

cowplot::save_plot(
  "figures/trends.slope.time.png",
  trends.slope.time,
  ncol = 2.1,
  nrow = 2,
  dpi = 300
)


#made a function based on previous code

p_trust <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "trust",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Trust (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .22))

p_risk <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "risktaking",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Risktaking (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .22))


p_altruism <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "altruism",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Altruism (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .22))


p_patience <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "patience",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Patience (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .22))


p_PCA1 <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "PCA1",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Governance and economic axis",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB",
  clip01 = F
)

p_PCA2 <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "PCA2",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Population and urbanization axis",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB",
  clip01 = F
)

traits.prediction <- (p_trust) /
  (p_patience + ylab("")) /
  (p_risk + ylab("")) /
  (p_altruism + ylab("")) +
  #(p_PCA1 + ylab("")) /
  #(p_PCA2 + ylab("")) +
  plot_layout(heights = c(1, 1, 1, 1), guides = "collect") &
  theme(plot.margin = margin(-0, -0, -0, -0))


cowplot::save_plot(
  "figures/traits.prediction.png",
  traits.prediction,
  ncol = .9,
  nrow = 5,
  dpi = 300
)


#figure in supplement with all predictions
p_trust_full <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "trust",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Trust (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent)

p_risk_full <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "risktaking",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Risktaking (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent)


p_altruism_full <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "altruism",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Altruism (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent)


p_patience_full <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "patience",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Patience (dimensionless)",
  ylab = "Protected area coverage",
  line_col = "#F7B1AB"
) +
  scale_y_continuous(labels = scales::percent)


p_PCA1_full <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "PCA1",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Governance and economic axis",
  ylab = "Protected area coverage",
  line_col = "#ADD8E6",
  clip01 = F
) +
  scale_y_continuous(labels = scales::percent)

p_PCA2_full <- plot_partial_effect(
  model = m.gps.up,
  data = PAgps.PCA.final,
  focal = "PCA2",
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = "Population and urbanization axis",
  ylab = "Protected area coverage",
  line_col = "#ADD8E6",
  clip01 = F
) +
  scale_y_continuous(labels = scales::percent)

traits.prediction.pca.full <- (p_trust_full +
  (p_patience_full + theme(axis.text.y = element_blank()) + ylab("")) +
  (p_PCA1_full + theme(axis.text.y = element_blank()) + ylab(""))) /
  (p_risk_full +
    (p_altruism_full + ylab("") + theme(axis.text.y = element_blank())) +
    (p_PCA2_full + theme(axis.text.y = element_blank()) + ylab(""))) &
  theme(plot.margin = margin(-0, -0, -0, -0))


cowplot::save_plot(
  "figures/traits.prediction.full.png",
  traits.prediction.pca.full,
  ncol = 1.8,
  nrow = 1.8,
  dpi = 300
)

#now for the maps trait ans pca
pivoted_data <- PAgps.PCA.final %>%
  select(isocode, altruism, trust, risktaking, patience, PCA1, PCA2) %>%
  distinct() %>%
  rename(
    Altruism = altruism,
    Trust = trust,
    Risktaking = risktaking,
    Patience = patience,
    `Governance and economic` = PCA1,
    `Population and uban` = PCA2
  ) %>%
  pivot_longer(
    cols = c(
      Altruism,
      Trust,
      Risktaking,
      Patience,
      `Governance and economic`,
      `Population and uban`
    ),
    names_to = "variable",
    values_to = "Value"
  )
vars <- c(
  "Altruism",
  "Trust",
  "Risktaking",
  "Patience",
  "Governance and economic",
  "Population and uban"
)

# Expand grid to get every (isocode, variable) pair
expanded_grid <- expand.grid(
  isocode = unique(world_sf$adm0_a3), # or however your ISO code is stored
  variable = vars,
  stringsAsFactors = FALSE
)
tt.traits <- expanded_grid %>%
  left_join(
    world_sf %>%
      select(isocode = adm0_a3, admin, geometry),
    by = "isocode"
  ) %>%
  left_join(
    pivoted_data,
    by = c("isocode", "variable")
  )

# Convert to sf if needed (only if 'geometry' is a valid sf column)
tt.traits <- st_as_sf(tt.traits) %>%
  filter(admin != 'Antarctica')

plot.traits.pca = list()
for (i in unique(tt.traits$variable)) {
  #sub
  sub = tt.traits[tt.traits$variable == i, ]

  plot.traits.pca[[i]] =
    ggplot(data = sub, aes(fill = Value)) +
    geom_sf(col = 'grey', linewidth = .05) + #plot map of France
    xlab(" ") +
    ylab(" ") +
    facet_wrap(. ~ variable, nrow = 1) +
    coord_sf(crs = "+proj=vandg4") +
    ggpubr::theme_pubr() +
    scale_fill_viridis_c(
      na.value = "white",
      option = 'plasma',
      direction = -1
    ) +
    theme(legend.position = 'right') +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(size = .5)
      )
    ) +
    #hrbrthemes::theme_ipsum(base_size = 14, axis_title_size = 16) +
    theme(
      strip.background = element_blank(),
      legend.key.size = unit(.25, 'cm'),
      legend.key.spacing.y = unit(0.05, "cm"),
      #new arg after ggplot tidy uypdate (coord_ function)
      axis.text = element_blank(), #after update of ggplot ...
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      # Move closer:
      #legend.position = "right", # still to the right
      #legend.justification = c(0, 0.5), # anchor to left-middle
      legend.box.margin = margin(0, -5, 0, -5), # pull legend box inward
      legend.margin = margin(0, 0, 0, 0),

      #legend.margin = margin(0, 0, 0, 0),
      #legend.box.margin = margin(-10, -10, -10, -10),
      panel.spacing = unit(-0.5, 'lines'),
      plot.margin = margin(2, 2, 2, 2), # no big external margin
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.title = element_text(size = 10, family = "Arial Narrow"),
      legend.text = element_text(size = 8, family = "Arial Narrow"),
      legend.position.inside = c(0.9, 0.2), # x=90% across, y=20% up
      legend.justification = c(0, 0)
    )
}


traits.maps <- (plot.traits.pca$Trust) /
  (plot.traits.pca$Patience) /
  plot.traits.pca$Risktaking /
  (plot.traits.pca$Altruism) /
  #(plot.traits.pca$`Governance and economic`) /
  #(plot.traits.pca$`Population and uban`) +
  plot_layout(heights = rep(1, 4)) &
  theme(
    #plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(4, "mm") # reduce spacing between rows
  )


cowplot::save_plot(
  "figures/traits.prediction.subset.png",
  plot_grid(
    traits.maps,
    traits.prediction,
    ncol = 2,
    rel_widths = c(2, 1),
    labels = "auto",
    label_x = 0,
    hjust = .2
  ),
  ncol = 2,
  nrow = 4,
  dpi = 300
)

# focals <- c("trust", "altruism", "patience", "risktaking", "PCA1", "PCA2")
#
# p_all <- plot_partial_effects_facet(
#   model = m.gps.up,
#   data = PAgps.PCA.final,
#   focals = focals,
#   re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
#   n = 120,
#   xlab_map = list(
#     trust = "Trust (z)",
#     altruism = "Altruism (z)",
#     patience = "Patience (z)",
#     risktaking = "Risk-taking (z)",
#     PCA1 = "Socio-governance (PC1)",
#     PCA2 = "Urbanization/Density (PC2)"
#   ),
#   ylab = "Protected area coverage",
#   ribbon_alpha = 0.25,
#   line_col = "black", # neutral color for all facets
#   clip01 = TRUE,
#   show_raw = TRUE,
#   raw_alpha = 0.06
# )
#
# p_all
