################################################################
################################################################
################LIBRAIRIES################################
require(purrr)
require(tidyverse)
library(here)
library(rnaturalearth)
library(tidyverse)
library(sf)

################################################################
################################################################
################FUNCTION LOADING################################
functions <- list.files("./rfunctions/", full.names = T) %>%
  purrr::map(source)

################################################################
################################################################
################DATA FORMATING################################
#1 - format governance data
governancedata = formatWGIdata(pathgovdata = 'data/wgidataset.xlsx',
              variableWGI = c('VoiceAccount', 'PoliticalStability', 'GovEffectiveness', 'RegulatoryQuality','RuleOfLaw', 'ControlCorruption'))

governancedata.managed = governancedata %>% 
  filter(year > 2018) %>% 
  group_by(Country, isocode) %>% 
  dplyr::select(-year) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ungroup()

#small basic plot - less variation within country than accross countries 
ggplot(governancedata, aes(x=year, y = GovEffectiveness, group = Country))+geom_line()

#2 - format landuse data - keep only percentage country and urban percentage
landusedata = read_csv('data/LAND_COVER_08112023114050883.csv') %>% 
  filter(MEAS == 'PCNT' & VARIABLE == 'URBAN')

landusedata.managed=  landusedata %>% 
  filter(Year == '2019') %>% 
  dplyr::select(COU, Country, Year, Value)
  # group_by(COU, Country) %>% 
  # summarise(minyear = min(Year),
  #        maxyear = max(Year),
  #        yearInterval = maxyear-minyear,
  #        meanPercentage = mean(Value),
  #        growtherArtificial = meanPercentage/yearInterval)

#3 - format PROTECTED AREAS
mypathnextcloud = '/Users/vjourne/Nextcloud/behavioral_climate/prog/protected_areas_formating'
#load rds file name
path.full_pa_data_cleanedpb= list.files(paste(mypathnextcloud), pattern = 'rds', full.names = T)
#get lust country name 
country_list.temp = str_remove(path.full_pa_data_cleanedpb, pattern = paste0(mypathnextcloud, '/'))
country_list = str_remove(country_list.temp, pattern = '.rds')
#load files PA
list.pa = list()
#sf_use_s2(FALSE)
for(j in 1:length(path.full_pa_data_cleanedpb)){
  list.pa[[j]] <- readRDS(paste(path.full_pa_data_cleanedpb[j]))
}
#take some time 
#the objective here is to get percentage of Protected Areas 
summarypercentagearea = getPercentagePA(sfuse = F,
                           list.pa,
                           country_list,
                           methodrobust = T )
#save it 
#qs::qsave(summarypercentagearea, 'summarypercentagearea.qs')

summarypercentagearea_versionNatRegInter = getPercentagePA(sfuse = F,
                                        list.pa,
                                        country_list,
                                        methodrobust = F )
  
##################################################################
#4 - format data of behavior traits
load("./data/covid_pays_wvs60.RData")
load("./data/covid_gps.RData")
load("./data/covid_evws_80.RData")

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
boxplotareas = ggplot(summarypercentagearea_versionNatRegInter %>% 
                        dplyr::filter(MARINE == 'terrestrial') %>% 
                        filter(DESIG_TYPE != 'Not Applicable'), aes(x = DESIG_TYPE, 
                                                                    y = percentageByCountry))+
  geom_boxplot()+
  coord_flip()+
  ylab('Percentage')+
  xlab('')+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15))

vectorPAcountry = summarypercentagearea %>% 
  dplyr::select(country, perecentageTOTALovercountry) %>% 
  distinct() %>% 
  rename(ISOCODE = country) %>% 
  mutate(`Protected area surface (%)` = as.numeric(perecentageTOTALovercountry))

tt = world_sf %>% mutate(ISOCODE = adm0_a3) %>% 
  full_join(vectorPAcountry) %>% 
  filter(admin != 'Antarctica')

mapsPA = ggplot(data=tt, aes(fill = `Protected area surface (%)`))+ 
  geom_sf(col = 'darkred', linewidth = .05)+     #plot map of France
  xlab(" ")+ ylab(" ")+
  coord_sf(crs= "+proj=vandg4")+
  ggpubr::theme_pubr()+
  scale_fill_viridis_c(breaks=c(10, 20, 30, 40, 50, 60), na.value = "grey80", option = 'magma')+ #rocket ? viridis?
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             override.aes = list(size = .5)))+
  theme(legend.title = element_text(size = 12), 
        legend.text  = element_text(size = 10))

#for results presentation 
tt %>% 
  group_by(region_wb) %>% 
  summarise(mean = mean(`Protected area surface (%)`, na.rm = T),
            sd = sd(`Protected area surface (%)`, na.rm = T)) %>% 
  arrange(mean)

#combine map and boxplot 
plotmaps = ggdraw() +
  draw_plot(mapsPA) +
  draw_plot(boxplotareas, x = 0.05, y = 0.0, width = .3, height = .25)
plotmaps

cowplot::save_plot("plotmaps.png",plotmaps, 
                   ncol = 2.4, nrow = 1.9, dpi = 300)

#new up version maps
# Identify outliers
outliers <- summarypercentagearea_versionNatRegInter%>% 
  dplyr::filter(MARINE == 'terrestrial') %>% 
  filter(DESIG_TYPE != 'Not Applicable') %>% 
  group_by(DESIG_TYPE) %>%
  mutate(is_outlier = percentageByCountry < quantile(percentageByCountry, 0.25) - 1.5 * IQR(percentageByCountry) |
           percentageByCountry > quantile(percentageByCountry, 0.75) + 1.5 * IQR(percentageByCountry)) %>%
  filter(is_outlier) %>%
  arrange(desc(percentageByCountry)) %>%
  slice_head(n = 5) %>% 
  mutate(ISOCODE = country) %>% 
  left_join( world_sf %>% mutate(ISOCODE = adm0_a3) %>% dplyr::select(ISOCODE, name_en))

boxplotareas.v2 = ggplot(summarypercentagearea_versionNatRegInter %>% 
                        dplyr::filter(MARINE == 'terrestrial') %>% 
                        filter(DESIG_TYPE != 'Not Applicable'), aes(x = DESIG_TYPE, 
                                                                    y = percentageByCountry))+
  geom_boxplot()+
  #coord_flip()+
  ylab('Percentage')+
  xlab('')+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))+ 
  ggrepel::geom_text_repel(data = outliers, aes(label = name_en), 
                  color = "darkblue", 
                  nudge_x = 0.3, # Optional: Slight nudge to adjust label positions
                  max.overlaps = Inf, # To ensure all labels are displayed
                  size = 3.5)
boxplotareas.v2

plotmaps = plot_grid(mapsPA, boxplotareas.v2, rel_widths = c(1, .3), labels = 'auto')                  
plotmaps
cowplot::save_plot("plotmaps.png",plotmaps, 
                   ncol = 2.4, nrow = 1.9, dpi = 300)



boxplotareas.iucn = ggplot(summarypercentagearea_versionNatRegInter %>% 
                        dplyr::filter(MARINE == 'terrestrial'), aes(x = IUCN_CAT, 
                                                                    y = percentageByCountry))+
  geom_boxplot()+
  coord_flip()+
  ylab('Percentage')+
  xlab('')+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15))
cowplot::save_plot("iucnbox.png",boxplotareas.iucn, 
                   ncol = 1.3, nrow = 1.9, dpi = 300)

#sup fig PA annexe 
vectorPAcountry.PAtype = summarypercentagearea_versionNatRegInter %>% 
  dplyr::filter(MARINE == 'terrestrial') %>% 
  filter(DESIG_TYPE != 'Not Applicable') %>% 
  group_by(DESIG_TYPE, country) %>% 
  summarise(sumPAtype = sum(percentageByCountry)) %>% 
  rename(ISOCODE = country) %>% 
  mutate(`Protected area surface (%)` = as.numeric(sumPAtype))

tt.sub = world_sf %>% mutate(ISOCODE = adm0_a3) %>% 
  full_join(vectorPAcountry.PAtype %>% 
              ungroup() %>% 
              dplyr::select(`Protected area surface (%)`, ISOCODE, DESIG_TYPE) %>% 
              tidyr::complete(ISOCODE, DESIG_TYPE, fill = list(`Protected area surface (%)` = NA)))  %>% 
  filter(admin != 'Antarctica') %>% 
  drop_na(DESIG_TYPE)

maps.PA.subset = ggplot(data=tt.sub, aes(fill = `Protected area surface (%)`))+ 
  geom_sf(col = 'darkred', linewidth = .05)+     #plot map of France
  xlab(" ")+ ylab(" ")+
  coord_sf(crs= "+proj=vandg4")+
  ggpubr::theme_pubr()+
  scale_fill_viridis_c(breaks=c(10, 20, 30, 40, 50, 60), na.value = "grey80", option = 'magma')+ #rocket ? viridis?
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             override.aes = list(size = .5)))+
  theme(legend.title = element_text(size = 12), 
        legend.text  = element_text(size = 10))+
  facet_grid(.~DESIG_TYPE)

cowplot::save_plot("typeOfPAs.png",maps.PA.subset, 
                   ncol = 2.5, nrow = 1.9, dpi = 300)

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
  rename(isocode = country) %>% 
  mutate(percentageByCountry_sum =as.numeric(perecentageTOTALovercountry)) %>% 
  dplyr::select(isocode, percentageByCountry_sum) %>% 
  distinct()

#redo with left join stuff
gps <- covid_gps %>%
  select("isocode","continent","country","population_density","human_development_index", "gdp_per_capita", 
         "patience"   ,# , "polstab" ,"account","gov_eff","regqual","corrupt","rulelaw",                      
         "risktaking"         ,                "posrecip"     ,                     
         "negrecip"         ,                  "altruism"    ,                      
         "trust"     ) 

PAgps = PAterrestre %>% 
  left_join(gps) %>% 
  left_join(landusedata.managed %>% dplyr::select(-Country, -Year) %>% rename(isocode = COU,
                                                                              PercentageUrban = Value)) %>% 
  left_join(governancedata.managed %>% dplyr::select(-Country)) %>% 
  drop_na('gdp_per_capita')

colnames(PAgps)

#do a PCA with filled species 
PAgps.sub <- PAgps %>% ungroup() %>% dplyr::select('isocode', "human_development_index","gdp_per_capita", 'population_density', 
                                                   'PercentageUrban',
                                                   'VoiceAccount', "PoliticalStability" ,"GovEffectiveness","RegulatoryQuality","RuleOfLaw","ControlCorruption"
) %>% mutate(gdp_per_capita = scale(log10(gdp_per_capita), center = TRUE, scale = TRUE),
             human_development_index = scale(log10(human_development_index), center = T, scale = T),
             PercentageUrban = scale(PercentageUrban, center = T, scale = T),
             population_density = scale(log10(population_density), center = T, scale = T)) %>% 
  as.data.frame #%>% dplyr::select(-Species)   #CV, ACF1, 
row.names(PAgps.sub) <- PAgps.sub$isocode
PAgps.sub.analysis <- PAgps.sub[c(2:(ncol(PAgps.sub)-1))] #6 traits variable
PCAnew <- ade4::dudi.pca(PAgps.sub.analysis, center = FALSE, scale = FALSE, scannf = F, nf = ncol(PAgps.sub.analysis))
screeplot(PCAnew, main = "Screeplot - Eigenvalues")
ade4::s.corcircle(PCAnew$co, xax = 1, yax = 2)
ade4::s.corcircle(PCAnew$co, xax = 3, yax = 2)
factoextra::fviz_eig(PCAnew, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(PCAnew, col.var = "black", axes = c(3, 2))

#after PCA, we'll make some analysis 
PAgps.PCA <- cbind(PAgps, PCAnew$li[,1], PCAnew$li[,2]) %>% 
  tibble() %>% 
  rename(PCA1 = 21, PCA2 = 22) %>% 
  mutate(beta.PA = percentageByCountry_sum/100,
         logit.PA = car::logit(beta.PA))%>%
  mutate(gdp_per_capita = scale(log10(gdp_per_capita), center = TRUE, scale = TRUE),
         negrecip = scale(negrecip, center = T, scale = T),
         altruism = scale(altruism, center = T, scale = T),
         trust = scale(trust, center = T, scale = T),
         patience = scale(patience, center = T, scale = T),
         posrecip = scale(posrecip, center = T, scale = T),
         risktaking = scale(risktaking, center = T, scale = T)) #%>% 
  #left_join(HDI %>% dplyr::select(ISOCODE, HDIvalue) %>% rename(isocode = ISOCODE))

#model TMB 
library(glmmTMB)
library(brms)
colnames(PAgps.PCA)
formula.gps1 = formula(beta.PA~negrecip+altruism+trust+patience+posrecip+risktaking+PCA1+PCA2)
# formula.gps1.scale = formula(beta.PA~negrecip.scale+
#                                altruism.scale+
#                                trust.scale+
#                                patience.scale+
#                                posrecip.scale+
#                                risktaking.scale+PCA1+PCA2)


m.gps <- glmmTMB(formula.gps1, data = PAgps.PCA, family=beta_family())
#m.gps.scale <- glmmTMB(formula.gps1.scale, data = PAgps.PCA, family=beta_family())

summary(m.gps)
#summary(m.gps.scale)
simulation.m.gps <- simulateResiduals(fittedModel = m.gps, quantreg=T, n = 500)
plot(simulation.m.gps)
coef.gps <-broom.mixed::tidy(m.gps,conf.int =TRUE)
dw <-dotwhisker::dwplot(m.gps, by_2sd = T)

#do some cv - estimate of model coefficient 
library(rsample)

# Create resamples (e.g., 10-fold cross-validation)
set.seed(123)
cv_splits <- vfold_cv(PAgps.PCA, v = 10, repeats = 5)

# Function to fit the model and compute performance metrics
cv_model <- function(split) {
  train_data <- training(split)
  test_data <- testing(split)
  
  # Fit the model
  model <- glmmTMB(formula.gps1, data = train_data, family = beta_family())
  # Calculate performance metrics (here pseudo r2 and rmse, just for myself)
  actuals <- test_data$beta.PA
  rmse_val <- RMSE(qlogis(test_data$beta.PA), predict(model, newdata = test_data, type = "link"))
  pseudo_r2 <- cor(qlogis(test_data$beta.PA), predict(model, newdata = test_data, type = "link"))^2 
  
  return(pseudo_r2)
}

# run fun 
cv_results <- map_dbl(cv_splits$splits, cv_model)
summary(cv_results) #average pseudor2 = 0.2 median = .15 




formula.gps.traits = formula(beta.PA~negrecip+altruism+trust+patience+posrecip+risktaking)
formula.gps.pca = formula(beta.PA~PCA1+PCA2)
formula.gps.gdp = formula(beta.PA~negrecip+altruism+trust+patience+posrecip+risktaking+gdp_per_capita)
formula.gdp = formula(beta.PA~gdp_per_capita)
formula.gps.hdi = formula(beta.PA~negrecip+altruism+trust+patience+posrecip+risktaking+human_development_index)
formula.hdi = formula(beta.PA~human_development_index)

m.gps.traits <- glmmTMB(formula.gps.traits, data = PAgps.PCA, family=beta_family())
m.gps.pca <- glmmTMB(formula.gps.pca, data = PAgps.PCA, family=beta_family())
m.gps.gdp <- glmmTMB(formula.gps.gdp, data = PAgps.PCA, family=beta_family())
m.gdp <- glmmTMB(formula.gdp, data = PAgps.PCA, family=beta_family())
m.gps.hdi <- glmmTMB(formula.gps.hdi, data = PAgps.PCA, family=beta_family())
m.hdi <- glmmTMB(formula.hdi, data = PAgps.PCA, family=beta_family())

#null model for pseudo r2 HC 
m.null <- glmmTMB(beta.PA~1, data = PAgps.PCA, family=beta_family())

performance::model_performance(m.gps.traits, metrics = c("AIC", "AICc", "BIC", "ICC", "RMSE"))
performance::model_performance(m.gps.pca, metrics = c("AIC", "AICc", "BIC", "ICC", "RMSE"))
performance::model_performance(m.gps, metrics = c("AIC", "AICc", "BIC", "ICC", "RMSE"))
performance::model_performance(m.gdp, metrics = c("AIC", "AICc", "BIC", "ICC", "RMSE"))

lmtest::lrtest(m.gps.traits, m.gps.pca, m.gps, m.gdp, m.hdi)

#https://cran.r-project.org/web/packages/PerMat/vignettes/PerMat.html
library(PerMat)
RMSE(qlogis(PAgps.PCA$beta.PA), predict(m.gps, type = "link"))
RMSE(qlogis(PAgps.PCA$beta.PA), predict(m.gps.traits, type = "link"))
RMSE(qlogis(PAgps.PCA$beta.PA), predict(m.gps.pca, type = "link"))
RMSE(qlogis(PAgps.PCA$beta.PA), predict(m.gps.gdp, type = "link"))

# m.gps.betaregR <- betareg(formula.gps1, data = PAgps.PCA)
#summary(m.gps.betaregR)
# reducedModel <- StepBeta::StepBeta(m.gps.betaregR)
# summary(reducedModel)
# 
# options(na.action = "na.fail")
# model_set <- dredge(m.gps.betaregR)
# best_model <- get.models(model_set, subset = 1)[[1]]
# summary(best_model)


cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps, type = "link"))^2 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps.traits, type = "link"))^2 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps.pca, type = "link"))^2 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps.gdp, type = "link"))^2 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gdp, type = "link"))^2 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps.hdi, type = "link"))^2 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.hdi, type = "link"))^2 

priors <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept")
)

m.gps.bayes = brm(formula.gps1,
                  data = PAgps.PCA,
                  family=beta_family(),
                  chains = 6,
                  iter = 20000,
                  warmup = 1000,
                  backend = 'cmdstanr',
                  prior = priors)
summary(m.gps.bayes)
coef.gps.bayes <-broom.mixed::tidy(m.gps.bayes,conf.int =TRUE);coef.gps.bayes
# bayestestR::p_map(m.gps.bayes)
#check r2 and rmse 
#will not report r2
#https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/bmsp.12289
#compute the pseudo r2 for beta rege 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps, type = "link"))^2 
#with another r2 
## McFadden's pseudo-R-squared
1- as.vector(logLik(m.null)/logLik(m.gps))
1- as.vector(logLik(m.null)/logLik(m.gps.traits))
1- as.vector(logLik(m.null)/logLik(m.gps.pca))
1- as.vector(logLik(m.null)/logLik(m.gps.gdp))
1- as.vector(logLik(m.null)/logLik(m.gdp))

#now check with surrogate r2 
#https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/bmsp.12289
#library(SurrogateRsq) #useful only for ranking - for me to keep in mind 



dwup = dw$data %>% 
  mutate(significnace = ifelse(p.value < .05, 'y', 'ns')) %>% 
  mutate( term = dplyr::recode(term, "negrecip" = "Reciprocity (-)" ,  
                               "posrecip" = "Reciprocity (+)",
                               "altruism" = "Altruism",
                               'trust' = "Trust",
                               "patience" = "Patience",
                               "risktaking" = "Risktaking")) %>% 
  mutate(term = factor(term, levels = rev(c("Reciprocity (-)",
                                            "Reciprocity (+)",
                                            "Altruism",
                                            "Trust", 
                                            "Patience",
                                            "Risktaking",
                                            'PCA1',
                                            'PCA2')))) %>% 
  ggplot()+
  geom_point(aes(x=estimate,y=term,col=significnace))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_segment(aes(x=conf.low,y=term,xend=conf.high,
                   yend=term,col=significnace))+
  geom_vline(xintercept = 0, linetype = 'dotted')+
  xlab('Coefficient value')+
  theme(legend.position = 'none')+
  ylab('')
dwup


#try to fir a brms model 
library(ggdist)
library(distributional)

plotv2 = m.gps %>%
  tidy() %>%
mutate(significnace = ifelse(p.value < .05, 'y', 'ns')) %>% 
  mutate( term = dplyr::recode(term, "negrecip" = "Reciprocity (-)" ,  
                               "posrecip" = "Reciprocity (+)",
                               "altruism" = "Altruism",
                               'trust' = "Trust",
                               "patience" = "Patience",
                               "risktaking" = "Risktaking",
                               'gdp_per_capita' = 'GDP')) %>% 
  mutate(term = factor(term, levels = rev(c("Reciprocity (-)",
                                            "Reciprocity (+)",
                                            "Altruism",
                                            "Trust", 
                                            "Patience",
                                            "Risktaking",
                                            'PCA1',
                                            'PCA2',
                                            'GDP')))) %>% 
  drop_na(term) %>% 
  mutate(tt = dist_student_t(df = df.residual(m.gps), mu = estimate, sigma = std.error)) %>% 
ggplot(aes(y = reorder(term, estimate)))+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_pointinterval(aes(xdist = tt))
  stat_halfeye(
    aes(xdist = dist_student_t(df = df.residual(m.gps), 
                               mu = estimate, sigma = std.error)), alpha = .88
  )+
  #stat_gradientinterval(aes(xdist = dist_student_t(df = df.residual(m.gps), mu = estimate, sigma = std.error)))+
  xlab('Coefficient value')+
  theme(legend.position = 'none')+
  ylab('')
plotv2

################################################################
################################################################
################DATA COMBINATION ################################
################################################################
################################################################
################WITH EVWS ################################
#extract evws data 
evws <- covid_evws_C %>%
  rename(country = Country) %>% 
  select("isocode","continent","country","population_density","human_development_index",
         "gdp_per_capita", 
         "wvs_altruism" ,"wvs_trust_global", 'wvs_patience') 

#format with PA and other datasets
PAevws = PAterrestre %>% 
  left_join(evws) %>% 
  left_join(landusedata.managed %>% dplyr::select(-Country, -Year) %>% rename(isocode = COU,
                                                                              PercentageUrban = Value)) %>% 
  left_join(governancedata.managed %>% dplyr::select(-Country)) %>% 
  drop_na('gdp_per_capita')

PAevws.sub <- PAevws %>% 
  ungroup() %>% 
  dplyr::select('isocode', "human_development_index","gdp_per_capita", 'population_density', 
                'PercentageUrban',
                'VoiceAccount', "PoliticalStability" ,"GovEffectiveness","RegulatoryQuality","RuleOfLaw","ControlCorruption"
  ) %>% mutate(gdp_per_capita = scale(log10(gdp_per_capita), center = TRUE, scale = TRUE),
               human_development_index = scale(log10(human_development_index), center = T, scale = T),
               PercentageUrban = scale(PercentageUrban, center = T, scale = T),
               population_density = scale(log10(population_density), center = T, scale = T))%>% 
  as.data.frame 
row.names(PAevws.sub) <- PAevws.sub$isocode
PAevws.sub.analysis <- PAevws.sub[c(2:(ncol(PAevws.sub)-1))] #6 traits variable
PCA.evws <- ade4::dudi.pca(PAevws.sub.analysis, center = FALSE, scale = FALSE, scannf = F, nf = ncol(PAevws.sub))
PCA.evws.pp <- rotate_dudi.pca(pca = PCA.evws, ncomp = 2)
fviz_pca_var(PCA.evws)
factoextra::fviz_eig(PCA.evws, addlabels = TRUE, ylim = c(0, 100))

ade4::s.corcircle(PCA.evws.pp$co, xax = 1, yax = 2)
# ade4::s.corcircle(PCA.evws$co, xax = 1, yax = 2)
# factoextra::fviz_eig(PCA.evws, addlabels = TRUE, ylim = c(0, 100))
# fviz_pca_var(PCA.evws, col.var = "black", axes = c(1, 2))
# library(ggfortify)
# PCA.evws = prcomp(PAevws.sub.analysis, center = FALSE, scale = FALSE)
# PCAnew <- prcomp(PAgps.sub.analysis, center = FALSE, scale = FALSE)
# 
# ggplot2::autoplot(PCA.evws,loadings.label=TRUE,loadings=TRUE)
# ggplot2::autoplot(PCAnew,loadings.label=TRUE,loadings=TRUE)


#after PCA, we'll make some analysis 
#i am rotating here PCA axis, to make them similar to the first analysis 
PAevws.PCA <- cbind(PAevws, PCA.evws$li[,1], PCA.evws$li[,2]) %>% 
  tibble() %>% 
  rename(PCA1 = 18, PCA2 = 19) %>%
  mutate(PCA1 = -PCA1,
         PCA2 = -PCA2) %>% 
  mutate(beta.PA = percentageByCountry_sum/100,
         logit.PA = car::logit(beta.PA)) %>% 
  mutate(gdp_per_capita = scale(log10(gdp_per_capita), center = TRUE, scale = TRUE),
         wvs_altruism = scale(wvs_altruism, center = T, scale = T),
         wvs_trust_global = scale(wvs_trust_global, center = T, scale = T),
         wvs_patience = scale(wvs_patience, center = T, scale = T))

formula.evws1 = formula(beta.PA~wvs_altruism+wvs_trust_global+wvs_patience+PCA1+PCA2)
m.evws <- glmmTMB(formula.evws1, data = PAevws.PCA, family=beta_family())
summary(m.evws)
simulation.m.evws <- simulateResiduals(fittedModel = m.evws, quantreg=T, n = 500)
plot(simulation.m.evws)
coef.evws <-broom.mixed::tidy(m.evws,conf.int =TRUE)
dw.evws <-dotwhisker::dwplot(coef.evws, by_2sd = T)
#compute the pseudo r2 for beta rege 
cor(qlogis(PAevws.PCA$beta.PA), predict(m.evws, type = "link"))^2 
dw.evwsup = dw.evws$data %>% 
  mutate(significnace = ifelse(p.value < .05, 'y', 'ns')) %>% 
  mutate( term = dplyr::recode(term, "wvs_altruism" = "Altruism" ,  
                               "wvs_trust_global" = "Trust (global)",
                               "wvs_patience" = "Patience")) %>% 
  mutate(term = factor(term, levels = rev(c("Altruism",
                                            "Trust (global)", 
                                            "Patience",
                                            'PCA1',
                                            'PCA2')))) %>% 
  ggplot()+
  geom_point(aes(x=estimate,y=term,col=significnace))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_segment(aes(x=conf.low,y=term,xend=conf.high,
                   yend=term,col=significnace))+
  geom_vline(xintercept = 0, linetype = 'dotted')+
  xlab('Coefficient value')+
  theme(legend.position = 'none')+
  ylab('')
dw.evwsup

plotv2evws = m.evws %>%
  tidy() %>%
  mutate(significnace = ifelse(p.value < .05, 'y', 'ns')) %>% 
  mutate( term = dplyr::recode(term, "wvs_altruism" = "Altruism" ,  
                               "wvs_trust_global" = "Trust (global)",
                               "wvs_patience" = "Patience")) %>% 
  mutate(term = factor(term, levels = rev(c("Altruism",
                                            "Trust (global)", 
                                            "Patience",
                                            'PCA1',
                                            'PCA2')))) %>% 
  drop_na(term) %>% 
  mutate(tt = dist_student_t(df = df.residual(m.evws), mu = estimate, sigma = std.error)) %>% 
  ggplot(aes(y = reorder(term, estimate)))+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_pointinterval(aes(xdist = tt))
  stat_halfeye(
    aes(xdist = dist_student_t(df = df.residual(m.evws), mu = estimate, sigma = std.error)), alpha = .88
  )+
  xlab('Coefficient value')+
  theme(legend.position = 'none')+
  ylab('')


library(patchwork)
mainresult_regression = dwup+dw.evwsup+plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
mainresult_regression
#with the V2
mainresult_regression = plotv2+plotv2evws+plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
mainresult_regression

cowplot::save_plot("main.res.regression.png",mainresult_regression, 
                   ncol = 1.5, nrow = 1.1, dpi = 300)

#BONUS 
#library(relaimpo)
#rel_importance <- calc.relimp(m.gps, type = c("lmg"), rela = TRUE)
domingpstrait = dominanceanalysis::dominanceAnalysis(betareg(formula.gps1, data = PAgps.PCA))
domingpstrait$contribution.average$r2.pseudo %>% as.data.frame()
#with ews 
domingpstrait.ews = dominanceanalysis::dominanceAnalysis(betareg(formula.evws1, data = PAevws.PCA))
domingpstrait.ews$contribution.average$r2.pseudo %>% as.data.frame()
#domingpstrait = dominanceanalysis::dominanceAnalysis(betareg(formula.gps.gdp, data = PAgps.PCA))

#dominanceanalysis::dominanceAnalysis(m.gps)
(domingpstrait$contribution.average)
#if beta regression used 
#betareg Provides pseudo-r2, Cox and Snell(1989), McFadden (1974), and Estrella (1998). You could set the link function using link.betareg if automatic detection of link function doesn't work.
combined_matrix <- cbind(domingpstrait$contribution.average$r2.cs, 
                         domingpstrait$contribution.average$r2.pseudo, 
                         domingpstrait$contribution.average$r2.m)

contributionR2 = domingpstrait$contribution.average$r2.pseudo %>% as.data.frame() %>% 
  rownames_to_column('term') %>% 
  rename(average.contrib = 2) %>% 
  arrange(average.contrib) %>% 
  mutate( term = dplyr::recode(term, "negrecip" = "Reciprocity (-)" ,  
                               "posrecip" = "Reciprocity (+)",
                               "altruism" = "Altruism",
                               'trust' = "Trust",
                               "patience" = "Patience",
                               "risktaking" = "Risktaking",
                               'gdp_per_capita' = 'GDP')) %>% 
  mutate(term = factor(term, levels = rev(c("Reciprocity (-)",
                                            "Reciprocity (+)",
                                            "Altruism",
                                            "Trust", 
                                            "Patience",
                                            "Risktaking",
                                            'PCA1',
                                            'PCA2',
                                            'GDP')))) %>% 
ggplot(aes(x = reorder(term, average.contrib), y = average.contrib)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  ylab("Contribution") + xlab('')+
  coord_flip()+
  ggpubr::theme_cleveland()

contributionR2

#report a summary table 
coef.evws <-broom.mixed::tidy(m.evws,conf.int =TRUE) %>% 
  mutate(mod = 'evs')
coef.gps <-broom.mixed::tidy(m.gps,conf.int =TRUE)%>% 
  mutate(mod = 'gps')

library(kableExtra)  
kbl( rbind(coef.gps, coef.evws) %>%  
       dplyr::select(-c(effect, component)) %>% 
       relocate(c(conf.low , conf.high), .after = std.error) %>% 
       relocate(mod, .before = term) %>% 
       dplyr::select(-statistic)%>% 
       mutate(across(3:6, \(x) signif(round(x,3), digits = 2))), 
     "latex", 
     booktabs = T, 
     caption = paste("Summary model"),
     col.names = c("Dataset",
                     "Predictor",
                     "Estimate",
                     "SD",
                     "CI025",
                     "CI975",
                   'p.value'), escape = F) %>% 
  kable_styling(latex_options = c("hold_position")) %>% 
  kable_styling() %>%
  row_spec(0, bold = FALSE, italic = T)

#take some more time :D 
#just to get some more details about confidence of estimates -neeedde for table certainly 
bootstrap = T
if(bootstrap == T){
  bootstrap.gps = parameters::bootstrap_parameters(m.gps, iterations = 100)
  qs::qsave(bootstrap.gps, 'bootstrap.gps.qs')
  bootstrap.evs = parameters::bootstrap_parameters(m.evws, iterations = 100)
  qs::qsave(bootstrap.evs, 'bootstrap.evs.qs')
  
  
  kbl( bootstrap.gps %>% mutate(data = 'gps', .before = Parameter) %>% 
         bind_rows(bootstrap.evs %>% mutate(data = 'evs', .before = Parameter)) %>% 
         as_tibble(), 
       "latex", 
       booktabs = T, 
       caption = paste("Summary model with bootstrap procedure"),
       col.names = c("Dataset",
                     "Parameter",
                     "Coefficient",
                     "CIlow",
                     'CIhigh',
                     'p.value'), escape = F) %>% 
    kable_styling(latex_options = c("hold_position")) %>% 
    kable_styling() %>%
    row_spec(0, bold = FALSE, italic = T)
}
######################
#try dredge function
#https://stats.stackexchange.com/questions/473569/model-averaging-predictor-significance-vs-importance
#https://www.sciencedirect.com/science/article/pii/S0169534703003458
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cales.arizona.edu/classes/wfsc578/Symonds%20and%20Moussali%202011.%20A%20brief%20guide%20to%20model%20selection.pdf
#for GPS -----------------
m.gps.dre <- glmmTMB(formula.gps1, data = PAgps.PCA, family=beta_family(), na.action = "na.fail")
dd.gps = dredge(m.gps.dre)
summary(model.avg(dd.gps))
inf_mod <- subset(dd.gps, delta < 3)
#Calculate average parameter estimate across all informative models
avg_mod <- model.avg(inf_mod)
#Extract coefficients
coef <- coefficients(avg_mod, full = TRUE)
ci1 <- confint(avg_mod, full=TRUE)

#now the same for EVWS-----------------
m.evws.dre <- glmmTMB(formula.evws1, data = PAevws.PCA, family=beta_family(), na.action = "na.fail")
dd.evws = dredge(m.evws.dre)
summary(model.avg(dd.evws))
inf_mod.evws <- subset(dd.evws, delta < 3)
#Calculate average parameter estimate across all informative models
avg_mod.evws <- model.avg(inf_mod.evws)
#Extract coefficients
coef.evws.dr <- coefficients(avg_mod.evws, full = TRUE)
ci1.evws <- confint(avg_mod.evws, full=TRUE)

summary.dredge = cbind(coef, ci1) %>% 
  as.data.frame() %>% 
  mutate(dataset = 'gps') %>% 
  bind_rows(cbind(coef.evws.dr, ci1.evws) %>% 
              as.data.frame() %>% 
              mutate(dataset = 'evws') %>% 
              rename(coef = coef.evws.dr)) 
  

kbl( summary.dredge %>% 
       rownames_to_column('variable') %>% 
       relocate(dataset, .before = variable) %>% 
       mutate(across(3:5, \(x) signif(round(x,3), digits = 2))), 
     "latex", 
     booktabs = T, 
     caption = paste("Summary model dredge"),
     col.names = c("Dataset",
                   "Predictor",
                   "Estimate",
                   "CI025",
                   "CI975"), escape = F) %>% 
  kable_styling(latex_options = c("hold_position")) %>% 
  kable_styling() %>%
  row_spec(0, bold = FALSE, italic = T)

checkoverlap = PAgps.PCA %>% 
  dplyr::select(isocode) %>% 
  mutate(d = 0) %>% 
  left_join(PAevws.PCA %>% dplyr::select(isocode) %>% mutate(d1 = 1))
  
table(is.na(checkoverlap$d1))
52/75 


###########################
#correlation matrix model 
###########################
library(RColorBrewer)
#check correlation and make PCA 
dfcor.gps = PAgps[c("human_development_index","gdp_per_capita", 'population_density', 
                'PercentageUrban',
                'VoiceAccount', "PoliticalStability" ,"GovEffectiveness","RegulatoryQuality","RuleOfLaw","ControlCorruption",
                "negrecip", "altruism" ,"trust", 'patience', 'posrecip', "risktaking")] %>% 
  mutate(gdp_per_capita = log10(gdp_per_capita),
         population_density = log10(population_density))%>% 
  as.data.frame()
dfcor.evs = PAevws.PCA[c("human_development_index","gdp_per_capita", 'population_density', 
                    'PercentageUrban',
                    'VoiceAccount', "PoliticalStability" ,"GovEffectiveness","RegulatoryQuality","RuleOfLaw","ControlCorruption",
                    "wvs_altruism", "wvs_trust_global" ,"wvs_patience")] %>% 
  mutate(gdp_per_capita = log10(gdp_per_capita),
         population_density = log10(population_density))%>% 
  as.data.frame()

#plotPairs(data = dfcor, save = T, ncol = 2.6 , nrow = 3.6)
Matrix.cor.gps<-cor(dfcor.gps)
Matrix.cor.evws<-cor(dfcor.evs)

p1cor = corrplot(Matrix.cor.gps, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"),
         tl.col="black")
p2cor = corrplot(Matrix.cor.evws, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"),
         tl.col="black")
library(ggcorrplot)
p1cor = ggcorrplot(Matrix.cor.gps, hc.order = TRUE, type = "lower",
                   outline.col = "white",
                   #method = "circle",
                   lab = TRUE,
                   p.mat = cor.mtest(Matrix.cor.gps),
                   insig = "blank",
                   colors = rev(c("#542788", "white", "#B35806")))
p2cor = ggcorrplot(Matrix.cor.evws, hc.order = TRUE, type = "lower",
           outline.col = "white",
           lab = TRUE,
           p.mat = cor.mtest(Matrix.cor.evws),
           insig = "blank",
           #method = "circle",
           colors = rev(c("#542788", "white", "#B35806")))
correlationplot = p1cor+p2cor+plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))


cowplot::save_plot("correlation.plot.png",correlationplot, 
                   ncol = 2.9, nrow = 2.1, dpi = 300)

###########################
#Joint model 
###########################
#joint model part 
xdata.gps <- PAgps.PCA %>% dplyr::select(human_development_index, gdp_per_capita,population_density,PercentageUrban, 
                                         VoiceAccount, PoliticalStability, GovEffectiveness, RegulatoryQuality, RuleOfLaw, ControlCorruption) %>% 
  mutate(gdp_per_capita = scale(gdp_per_capita, center = TRUE, scale = TRUE),
         human_development_index = scale(human_development_index, center = T, scale = T),
         population_density = scale(population_density, center = T, scale = T),
         PercentageUrban = scale(PercentageUrban, center = T, scale = T)) %>% 
  mutate(across(everything(), as.vector)) %>% 
  data.frame()
#response
ydata.gps  <- as.data.frame(PAgps.PCA  %>% 
                              dplyr::select(logit.PA, negrecip, altruism, trust, patience, posrecip, risktaking))  
# #associate row names with species name - in case, but here same number of row matching, 
# row.names(ydata) <- ydata$Species
# ydata<- ydata[-c(1:2)]
# row.names(xdata) <- xdata$Species
# xdata<- xdata[-c(3)] 

# #gjam specification 
types.gps <- c("CON" , "CON", 'CON',"CON" , "CON", 'CON', 'CON')
ml.gps  <- list(ng = 10000, burnin = 2000, typeNames = types.gps) #to increase
# ml$random <- 'genusNewFactor'
# #gjam fit 
names(xdata.gps)
names(xdata.gps) <- gsub("_", ".", names(xdata.gps))
outgjam.gps <- gjam(~ human.development.index + gdp.per.capita  + population.density + PercentageUrban + VoiceAccount + PoliticalStability + GovEffectiveness + RegulatoryQuality + RuleOfLaw + ControlCorruption, 
                    xdata = xdata.gps, 
                    ydata = ydata.gps, 
                    modelList = ml.gps)

#just plot the model, see sensitivity, different response, chaines, etc 
gjamPlot(outgjam.gps)

plotGJAMinit(outgjam.gps)
cowplot::save_plot("gjam.allresponses.png",plotGJAMinit(outgjam.gps), 
                   ncol = 3, nrow = 2, dpi = 300)
#summary of the model
summary(outgjam.gps)
#summary of standardized coeffciient - what we want 
outgjam.gps$parameters$betaStandXWTable
#to check models fit (DIC, r2, etc )
outgjam.gps$fit

#now we can do the conditioning - to get the direct effect of trait on Protected area
names(ydata.gps)
condgjam.gps <- gjamConditionalParameters( outgjam.gps, conditionOn = c('negrecip', 'posrecip', 'risktaking', "patience", "trust", "altruism") , nsim = 10000)

#pay attention to number of ccolum here 2:5
cond.gps.summary <- condgjam.gps$Amu %>% as.data.frame() %>% rownames_to_column("response") %>% 
  pivot_longer(2:ncol(.), names_to = "covariates", values_to = "Estimate") %>% 
  left_join(condgjam.gps$Atab %>% as.data.frame()) %>% 
  mutate(sigSens = ifelse(CI_025 > 0 & CI_975 > 0, "POS", ifelse(CI_025 < 0 & CI_975 < 0, "NEG", "NO"))) %>% 
  mutate(signV2 =  ifelse(CI_025 > 0 & CI_975 > 0, "strongPOS", 
                          ifelse(CI_025 < 0 & CI_975 < 0, "strongNEG",
                                 ifelse(CI_100 < 0 & CI_900 < 0, "medNeg",
                                        ifelse(CI_100 > 0 & CI_900 > 0, "medPo", "NO")))))

#and make the plot now !
plot.cond.gjam.gps = cond.gps.summary %>% 
  mutate( covariates = dplyr::recode(covariates, "negrecip" = "Reciprocity (-)" ,  
                                     "posrecip" = "Reciprocity (+)",
                                     "altruism" = "Altruism",
                                     'trust' = "Trust",
                                     "patience" = "Patience",
                                     "risktaking" = "Risktaking")) %>% 
  mutate(covariates = factor(covariates, levels = rev(c("Reciprocity (-)",
                                                        "Reciprocity (+)",
                                                        "Altruism",
                                                        "Trust", 
                                                        "Patience",
                                                        "Risktaking")))) %>% 
  ggplot(aes( y = covariates)) + 
  #geom_errorbarh(aes(x = Estimate, 
  #                   xmin = CI_025,xmax = CI_975, col = sigSens, fill = sigSens), height=.0, size =1) + 
  #geom_point(aes(x = Estimate, col = sigSens, fill = sigSens), size = 5, shape = 108)+
  geom_boxplot(aes(x = Estimate, 
                   xmin = CI_025, 
                   xlower = CI_100, 
                   #group = covariates, 
                   xmiddle = Estimate, 
                   xupper = CI_900, 
                   xmax = CI_975), stat = "identity",
               position = position_dodge2(preserve = "total"), 
               alpha = .9 , width = .2, col = "grey30", size = 0.3)+
  geom_vline(aes(xintercept = 0), col = "darkred",linetype = "dashed", size = 0.5)+
  theme(legend.position = "none")+
  ylab('')

cowplot::save_plot("plot.cond.gjam.gps.png",plot.cond.gjam.gps, 
                   ncol = 1.4, nrow = .8, dpi = 300)

#joint model part - EVWS ----------
xdata.evws <- PAevws.PCA %>% dplyr::select(human_development_index, gdp_per_capita,population_density,PercentageUrban, 
                                         VoiceAccount, PoliticalStability, GovEffectiveness, RegulatoryQuality, RuleOfLaw, ControlCorruption) %>% 
  mutate(gdp_per_capita = scale(gdp_per_capita, center = TRUE, scale = TRUE),
         human_development_index = scale(human_development_index, center = T, scale = T),
         population_density = scale(population_density, center = T, scale = T),
         PercentageUrban = scale(PercentageUrban, center = T, scale = T)) %>% 
  mutate(across(everything(), as.vector)) %>% 
  data.frame()
#response
ydata.evws  <- as.data.frame(PAevws.PCA  %>% 
                              dplyr::select(logit.PA, wvs_altruism, wvs_trust_global, wvs_patience))  
# #gjam specification 
types.evws<- c("CON" , "CON", 'CON',"CON" )
ml.evws  <- list(ng = 10000, burnin = 2000, typeNames = types.evws) #to increase
names(xdata.evws) <- gsub("_", ".", names(xdata.evws))
names(ydata.evws) <- gsub("_", ".", names(ydata.evws))

outgjam.evws <- gjam(~ human.development.index + gdp.per.capita  + population.density + PercentageUrban + VoiceAccount + PoliticalStability + GovEffectiveness + RegulatoryQuality + RuleOfLaw + ControlCorruption, 
                    xdata = xdata.evws, 
                    ydata = ydata.evws, 
                    modelList = ml.evws)
gjamPlot(outgjam.evws)

cowplot::save_plot("gjam.allresponses.evws.png",plotGJAMinit(outgjam.evws), 
                   ncol = 3, nrow = 2, dpi = 300)
condgjam.evws <- gjamConditionalParameters( outgjam.evws, conditionOn = c('wvs.altruism', 'wvs.trust.global', 'wvs.patience') , nsim = 10000)
cond.evws.summary <- condgjam.evws$Amu %>% as.data.frame() %>% rownames_to_column("response") %>% 
  pivot_longer(2:ncol(.), names_to = "covariates", values_to = "Estimate") %>% 
  left_join(condgjam.evws$Atab %>% as.data.frame()) %>% 
  mutate(sigSens = ifelse(CI_025 > 0 & CI_975 > 0, "POS", ifelse(CI_025 < 0 & CI_975 < 0, "NEG", "NO"))) %>% 
  mutate(signV2 =  ifelse(CI_025 > 0 & CI_975 > 0, "strongPOS", 
                          ifelse(CI_025 < 0 & CI_975 < 0, "strongNEG",
                                 ifelse(CI_100 < 0 & CI_900 < 0, "medNeg",
                                        ifelse(CI_100 > 0 & CI_900 > 0, "medPo", "NO")))))

plot.cond.gjam.evws = cond.evws.summary %>% 
  mutate( covariates = dplyr::recode(covariates,
                                     "wvs.altruism" = "Altruism",
                                     'wvs.trust.global' = "Trust",
                                     "wvs.patience" = "Patience")) %>% 
  mutate(covariates = factor(covariates, levels = rev(c("Altruism",
                                                        "Trust", 
                                                        "Patience")))) %>% 
  ggplot(aes( y = covariates)) + 
  #geom_errorbarh(aes(x = Estimate, 
  #                   xmin = CI_025,xmax = CI_975, col = sigSens, fill = sigSens), height=.0, size =1) + 
  #geom_point(aes(x = Estimate, col = sigSens, fill = sigSens), size = 5, shape = 108)+
  geom_boxplot(aes(x = Estimate, 
                   xmin = CI_025, 
                   xlower = CI_100, 
                   #group = covariates, 
                   xmiddle = Estimate, 
                   xupper = CI_900, 
                   xmax = CI_975), stat = "identity",
               position = position_dodge2(preserve = "total"), 
               alpha = .9 , width = .2, col = "grey30", size = 0.3)+
  geom_vline(aes(xintercept = 0), col = "darkred",linetype = "dashed", size = 0.5)+
  theme(legend.position = "none")+
  ylab('')

cowplot::save_plot("plot.cond.gjam.png",plot.cond.gjam.gps+plot.cond.gjam.evws, 
                   ncol = 1.6, nrow = .8, dpi = 300)

#############################################
#SUPP FIGURE 
#check contribution for first and second axis 

Contribution.gps <- rbind(giveAxisContrib(PCAres = PCAnew, axisID = 1),
                          giveAxisContrib(PCAres = PCAnew, axisID = 2)) %>% 
  mutate(axisID = paste0("Axis.",axisID)) 

Contribgraph.gps <- ggpubr::ggbarplot(Contribution.gps, x = "name", y = "contrib", 
                                      xlab = FALSE, ylab = "Contributions (%)",
                                      col = 'grey', fill = 'black')+
  facet_grid(.~axisID)+
  coord_flip()+
  xlab("")+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14))+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Set2")

varPCAnewMM.gps <- get_pca_var(PCAnew)

LoadingsGraph.gps <- varPCAnewMM.gps$cor %>% 
  rownames_to_column("name") %>% 
  dplyr::select(1:3) %>% 
  rename("Axis.1"=2,"Axis.2"=3) %>% 
  pivot_longer(cols = -c("name"), names_to = "axisID", values_to = "Correlation") %>% 
  ggpubr::ggbarplot(x = "name", y = "Correlation", fill = "black", 
                    color = "grey", 
                    xlab = FALSE, ylab = "Contributions (%)")+
  facet_grid(.~axisID)+
  coord_flip()+
  geom_hline(aes(yintercept=0), linetype = 2, color = "grey", linewidth = 1)+
  xlab("")+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14))+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Set2")


Contribution.evws <- rbind(giveAxisContrib(PCAres = PCA.evws, axisID = 1),
                          giveAxisContrib(PCAres = PCA.evws, axisID = 2)) %>% 
  mutate(axisID = paste0("Axis.",axisID)) 

Contribgraph.evws <- ggpubr::ggbarplot(Contribution.evws, x = "name", y = "contrib", 
                                      xlab = FALSE, ylab = "Contributions (%)",
                                      col = 'grey', fill = 'black')+
  facet_grid(.~axisID)+
  coord_flip()+
  xlab("")+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14))+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Set2")

varPCAnewMM.evws <- get_pca_var(PCA.evws)


LoadingsGraph.evws <- varPCAnewMM.evws$cor %>% 
  mutate(Dim.1 = -Dim.1) %>% 
  rownames_to_column("name") %>% 
  dplyr::select(1:3) %>% 
  rename("Axis.1"=2,"Axis.2"=3) %>% 
  pivot_longer(cols = -c("name"), names_to = "axisID", values_to = "Correlation") %>% 
  ggpubr::ggbarplot(x = "name", y = "Correlation", fill = "black", 
                    color = "grey", 
                    xlab = FALSE, ylab = "Contributions (%)")+
  facet_grid(.~axisID)+
  coord_flip()+
  geom_hline(aes(yintercept=0), linetype = 2, color = "grey", linewidth = 1)+
  xlab("")+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14))+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Set2")

pcasummary = (Contribgraph.gps+Contribgraph.evws)/(LoadingsGraph.gps+LoadingsGraph.evws)+plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))


cowplot::save_plot("pcasum.plot.png",pcasummary, 
                   ncol = 2.6, nrow = 1.6, dpi = 300)

#now coefficient without pca
formula.gps.no = formula(beta.PA~negrecip+altruism+trust+patience+posrecip+risktaking)
m.gps.no <- glmmTMB(formula.gps.no, data = PAgps.PCA, family=beta_family())
formula.evws.no = formula(beta.PA~wvs_altruism+wvs_trust_global+wvs_patience)
m.evws.no <- glmmTMB(formula.evws.no, data = PAevws.PCA, family=beta_family())
summary(m.evws.no)
summary(m.gps.no)

#r2 speudo
cor(qlogis(PAevws.PCA$beta.PA), predict(m.evws.no, type = "link"))^2 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps.no, type = "link"))^2 

quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble(x = quantile(x, q), q = q)
}
quibble(PAterrestre$percentageByCountry_sum, c(0.025, 0.5, 0.975))
summary(PAterrestre$percentageByCountry_sum)

plotv2.NO = m.gps.no %>%
  tidy() %>%
  mutate(significnace = ifelse(p.value < .05, 'y', 'ns')) %>% 
  mutate( term = dplyr::recode(term, "negrecip" = "Reciprocity (-)" ,  
                               "posrecip" = "Reciprocity (+)",
                               "altruism" = "Altruism",
                               'trust' = "Trust",
                               "patience" = "Patience",
                               "risktaking" = "Risktaking")) %>% 
  mutate(term = factor(term, levels = rev(c("Reciprocity (-)",
                                            "Reciprocity (+)",
                                            "Altruism",
                                            "Trust", 
                                            "Patience",
                                            "Risktaking",
                                            'PCA1',
                                            'PCA2')))) %>% 
  drop_na(term) %>% 
  mutate(tt = dist_student_t(df = df.residual(m.gps), mu = estimate, sigma = std.error)) %>% 
  ggplot(aes(y = term))+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_pointinterval(aes(xdist = tt))
  stat_halfeye(
    aes(xdist = dist_student_t(df = df.residual(m.gps), mu = estimate, sigma = std.error)), alpha = .88
  )+
  #stat_gradientinterval(aes(xdist = dist_student_t(df = df.residual(m.gps), mu = estimate, sigma = std.error)))+
  xlab('Coefficient value')+
  theme(legend.position = 'none')+
  ylab('')

plotv2evws.NO = m.evws.no %>%
  tidy() %>%
  mutate(significnace = ifelse(p.value < .05, 'y', 'ns')) %>% 
  mutate( term = dplyr::recode(term, "wvs_altruism" = "Altruism" ,  
                               "wvs_trust_global" = "Trust (global)",
                               "wvs_patience" = "Patience")) %>% 
  mutate(term = factor(term, levels = rev(c("Altruism",
                                            "Trust (global)", 
                                            "Patience",
                                            'PCA1',
                                            'PCA2')))) %>% 
  drop_na(term) %>% 
  mutate(tt = dist_student_t(df = df.residual(m.evws), mu = estimate, sigma = std.error)) %>% 
  ggplot(aes(y = term))+
  geom_vline(xintercept=0, linetype="dashed")+
  stat_halfeye(
    aes(xdist = dist_student_t(df = df.residual(m.evws), mu = estimate, sigma = std.error)), alpha = .88
  )+
  xlab('Coefficient value')+
  theme(legend.position = 'none')+
  ylab('')

mainresult_regression.NOpca = plotv2.NO+plotv2evws.NO+plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12))
mainresult_regression.NOpca

cowplot::save_plot("main.res.regression.nopca.png",mainresult_regression.NOpca, 
                   ncol = 1.5, nrow = 1.1, dpi = 300)


  #test with region data set shit for gps 
data.region.gps =  summarypercentagearea_versionNatRegInter %>% 
  filter(MARINE == 'terrestrial') %>% 
  ungroup() %>% 
  group_by(country, DESIG_TYPE) %>% 
  summarise(sumPA = sum(percentageByCountry)) %>% 
  rename(isocode = country) %>% 
  left_join(gps) %>% 
  left_join(landusedata.managed %>% dplyr::select(-Country, -Year) %>% rename(isocode = COU,
                                                                              PercentageUrban = Value)) %>% 
  left_join(governancedata.managed %>% dplyr::select(-Country)) %>% 
  drop_na('gdp_per_capita') %>% 
  left_join(PAgps.PCA %>% dplyr::select(isocode, PCA1, PCA2)) %>% 
  mutate(beta.sumPA = sumPA/100)

data.region.evws =  summarypercentagearea_versionNatRegInter %>% 
  filter(MARINE == 'terrestrial') %>% 
  ungroup() %>% 
  group_by(country, DESIG_TYPE) %>% 
  summarise(sumPA = sum(percentageByCountry)) %>% 
  rename(isocode = country) %>% 
  left_join(evws) %>% 
  left_join(landusedata.managed %>% dplyr::select(-Country, -Year) %>% rename(isocode = COU,
                                                                              PercentageUrban = Value)) %>% 
  left_join(governancedata.managed %>% dplyr::select(-Country)) %>% 
  drop_na('gdp_per_capita') %>% 
  left_join(PAevws.PCA %>% dplyr::select(isocode, PCA1, PCA2)) %>% 
  mutate(beta.sumPA = sumPA/100)

m.gps.regintnat <- glmmTMB(beta.sumPA~DESIG_TYPE+negrecip+altruism+trust+patience+posrecip+risktaking+PCA1 +PCA2, data = data.region.gps, family=beta_family())
m.evws.regintnat <- glmmTMB(beta.sumPA~DESIG_TYPE+wvs_altruism+wvs_trust_global+wvs_patience+PCA1 +PCA2, data = data.region.evws, family=beta_family())
#check r2
cor(qlogis(data.region.gps$beta.sumPA), predict(m.gps.regintnat, type = "link"))^2 
cor(qlogis(data.region.evws$beta.sumPA), predict(m.evws.regintnat, type = "link"))^2 


summary(m.gps.regintnat)
summary(m.evws.regintnat)
visreg(m.gps.regintnat)
library(ggeffects)
  

data.region.gps =  summarypercentagearea_versionNatRegInter %>% 
  filter(MARINE == 'terrestrial') %>% 
  ungroup() %>% 
  group_by(country, IUCN_CAT) %>% 
  summarise(sumPA = sum(percentageByCountry)) %>% 
  rename(isocode = country) %>% 
  left_join(gps) %>% 
  left_join(landusedata.managed %>% dplyr::select(-Country, -Year) %>% rename(isocode = COU,
                                                                              PercentageUrban = Value)) %>% 
  left_join(governancedata.managed %>% dplyr::select(-Country)) %>% 
  drop_na('gdp_per_capita') %>% 
  left_join(PAgps.PCA %>% dplyr::select(isocode, PCA1, PCA2)) %>% 
  mutate(beta.sumPA = sumPA/100)
m.gps.iucn <- glmmTMB(beta.sumPA~IUCN_CAT+negrecip+altruism+trust+patience+posrecip+risktaking+PCA1 +PCA2, data = data.region.gps, family=beta_family())
summary(m.gps.iucn)

data.iucn =  summarypercentagearea_versionNatRegInter %>% 
  filter(MARINE == 'terrestrial') %>% 
  ungroup() %>% 
  group_by(country, IUCN_CAT) %>% 
  summarise(sumPA = sum(percentageByCountry)) %>% 
  rename(isocode = country) %>% 
  left_join(evws) %>% 
  left_join(landusedata.managed %>% dplyr::select(-Country, -Year) %>% rename(isocode = COU,
                                                                              PercentageUrban = Value)) %>% 
  left_join(governancedata.managed %>% dplyr::select(-Country)) %>% 
  drop_na('gdp_per_capita') %>% 
  left_join(PAevws.PCA %>% dplyr::select(isocode, PCA1, PCA2)) %>% 
  mutate(beta.sumPA = sumPA/100)

m.evws.iuscn <- glmmTMB(beta.sumPA~IUCN_CAT+wvs_altruism+wvs_trust_global+wvs_patience+PCA1 +PCA2, data = data.iucn, family=beta_family())
summary(m.evws.iuscn)
cor(qlogis(data.iucn$beta.sumPA), predict(m.evws.iuscn, type = "link"))^2 

#now mae the plot 
plot.factor.type = ggpredict(m.gps.regintnat, terms = "DESIG_TYPE") %>% 
  as_tibble() %>% 
  arrange(x) %>% 
  ggplot(aes(x = x,
             y = predicted))+
  geom_point()+ 
  geom_pointrange(aes(ymin=conf.low, ymax= conf.high))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(labels = scales::percent, limits = c(0, .2))+
  ylab('Protected area (%)')+
  xlab('')


plot.factor.typeiucn = ggpredict(m.gps.iucn, terms = "IUCN_CAT") %>% 
  as_tibble() %>% 
  arrange(x) %>% 
  ggplot(aes(x = x,
             y = predicted))+
  geom_point()+ 
  geom_pointrange(aes(ymin=conf.low, ymax= conf.high))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  ylab('Protected area (%)')+
  xlab('')+
  ylim(0, .2)+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

cowplot::save_plot("regionalnational.plot.png",plot.factor.type+plot.factor.typeiucn, 
                   ncol = 1.2, nrow = 1.2, dpi = 300)
