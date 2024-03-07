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
summarypercentagearea = getPercentagePA(sfuse = F,
                           list.pa,
                           country_list,
                           methodrobust = T )
qs::qsave(summarypercentagearea, 'summarypercentagearea.qs')

summarypercentagearea_versionNatRegInter = getPercentagePA(sfuse = F,
                                        list.pa,
                                        country_list,
                                        methodrobust = F )
  
##################################################################
#4 - format data of behavior traits
load("./data/covid_pays_wvs60.RData")
load("./data/covid_gps.RData")
load("./data/covid_evws_80.RData")

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

#combine map and boxplot 
plotmaps = ggdraw() +
  draw_plot(mapsPA) +
  draw_plot(boxplotareas, x = 0.05, y = 0.0, width = .3, height = .25)
plotmaps

cowplot::save_plot("plotmaps.png",plotmaps, 
                   ncol = 2.4, nrow = 1.9, dpi = 300)


################################################################
################################################################
################DATA COMBINATION ################################
################################################################
################################################################
################WITH GPS################################

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
#check correlation and make PCA 
dfcor = PAgps[c("human_development_index","gdp_per_capita", 'population_density', 
                'PercentageUrban',
                'VoiceAccount', "PoliticalStability" ,"GovEffectiveness","RegulatoryQuality","RuleOfLaw","ControlCorruption",
                "negrecip", "altruism" ,"trust", 'patience', 'posrecip', "risktaking")] %>% 
  mutate(gdp_per_capita = log10(gdp_per_capita))%>% 
  as.data.frame()
plotPairs(data = dfcor, save = T, ncol = 2.6 , nrow = 3.6)
Matrix.cor.gps<-cor(dfcor)
corrplot(Matrix.cor.gps, type="upper")
library(RColorBrewer)
corrplot(Matrix.cor.gps, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"),
         tl.col="black")

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

#check contribution for first and second axis 
giveAxisContrib(PCAres = PCAnew, axisID = 1)
giveAxisContrib(PCAres = PCAnew, axisID = 2)

#after PCA, we'll make some analysis 
PAgps.PCA <- cbind(PAgps, PCAnew$li[,1], PCAnew$li[,2]) %>% 
  tibble() %>% 
  rename(PCA1 = 21, PCA2 = 22) %>% 
  mutate(beta.PA = percentageByCountry_sum/100,
         logit.PA = car::logit(beta.PA))

#model TMB 
library(glmmTMB)
colnames(PAgps.PCA)
formula.gps1 = formula(beta.PA~negrecip+altruism+trust+patience+posrecip+risktaking+PCA1+PCA2)
m.gps <- glmmTMB(formula.gps1, data = PAgps.PCA, family=beta_family())
summary(m.gps)
simulation.m.gps <- simulateResiduals(fittedModel = m.gps, quantreg=T, n = 500)
plot(simulation.m.gps)
coef.gps <-broom.mixed::tidy(m.gps,conf.int =TRUE)
dw <-dotwhisker::dwplot(coef.gps, by_2sd = T)
#check r2 and rmse 
#will not report r2
#https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/bmsp.12289
#compute the pseudo r2 for beta rege 
cor(qlogis(PAgps.PCA$beta.PA), predict(m.gps, type = "link"))^2 


bootstrap = F
if(bootstrap == T){
  bootstrap.gps = parameters::bootstrap_parameters(m.gps, iterations = 1000)
  qs::qsave(bootstrap.gps, 'bootstrap.gps.qs')
}

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
         logit.PA = car::logit(beta.PA))

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
  ggplot(aes(y = term))+
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


######################
#try dredge function
#https://stats.stackexchange.com/questions/473569/model-averaging-predictor-significance-vs-importance
#https://www.sciencedirect.com/science/article/pii/S0169534703003458
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cales.arizona.edu/classes/wfsc578/Symonds%20and%20Moussali%202011.%20A%20brief%20guide%20to%20model%20selection.pdf
#for GPS -----------------
m.gps.dre <- glmmTMB(formula.gps1, data = PAgps.PCA, family=beta_family(), na.action = "na.fail")
dd.gps = dredge(m.gps.dre)
summary(model.avg(dd.gps))
inf_mod <- subset(dd.gps, delta < 7)
#Calculate average parameter estimate across all informative models
avg_mod <- model.avg(inf_mod)
#Extract coefficients
coef <- coefficients(avg_mod, full = TRUE)
ci1 <- confint(avg_mod, full=TRUE)

#now the same for EVWS-----------------
m.evws.dre <- glmmTMB(formula.evws1, data = PAevws.PCA, family=beta_family(), na.action = "na.fail")
dd.evws = dredge(m.evws.dre)
summary(model.avg(dd.evws))
inf_mod.evws <- subset(dd.evws, delta < 7)
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



###########################
#Joint model 
###########################
#joint model part 
xdata.gps <- PAgps.PCA %>% dplyr::select(human_development_index, gdp_per_capita, 
                                         polstab, account, gov_eff, regqual, corrupt, rulelaw) %>% 
  mutate(gdp_per_capita = scale(gdp_per_capita, center = TRUE, scale = TRUE),
         human_development_index = scale(human_development_index, center = T, scale = T)) %>% 
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
outgjam.gps <- gjam(~ human.development.index + gdp.per.capita + polstab + account + gov.eff + regqual + corrupt + rulelaw, 
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
  geom_errorbarh(aes(x = Estimate, 
                     xmin = CI_025,xmax = CI_975, col = sigSens, fill = sigSens), height=.0, size =1) + 
  geom_point(aes(x = Estimate, col = sigSens, fill = sigSens), size = 5, shape = 108)+
  geom_vline(aes(xintercept = 0), col = "darkred",linetype = "dashed", size = 0.5)+
  theme(legend.position = "none")
cowplot::save_plot("plot.cond.gjam.gps.png",plot.cond.gjam.gps, 
                   ncol = 1.4, nrow = .8, dpi = 300)


