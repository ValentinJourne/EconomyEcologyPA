require(purrr)
require(tidyverse)
library(here)
functions <- list.files("./rfunctions/", full.names = T) %>%
  purrr::map(source)

library(wdpar)
raw_pa_data <-
  c("CYP") %>%
  lapply(wdpa_fetch, wait = TRUE,
         download_dir = rappdirs::user_data_dir("wdpar")) %>%
  bind_rows()
full_pa_data <- wdpa_clean(raw_pa_data, erase_overlaps = FALSE)

full_pa_data %>% 
  mutate(sizecountry = 5719.953) %>% 
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(IUCN_CAT, DESIG_TYPE, sizecountry, MARINE) %>%
  #group_by(MARINE) %>%
  summarize(area_km = sum(AREA_KM2)) %>%
  ungroup() %>%
  mutate(percentageByCountry = (area_km / sizecountry) * 100) %>%
  arrange(desc(area_km)) %>% 
  mutate(country = country_list[a])


##################################################################
load("./data/covid_gps.RData")
load("./data/covid_evws_80.RData")
load("./data/covid_pays_wvs60.RData")

#selected columns
gps <- covid_gps %>%
  select("isocode"  ,                          "continent"       ,                   "location"   ,
         "population_density", "life_expectancy"        ,            "human_development_index" ,
         "aged_70_older",                      "gdp_per_capita",
         "median_age" , "polstab" ,                           "account"     ,                       "gov_eff"   ,                         "regqual",
         "corrupt"   ,                         "rulelaw"    ,                        "code"      ,                         "ncode",
         "country"  ,                          "patience"   ,                        "risktaking" ,                        "posrecip",
         "negrecip"   ,                        "altruism"    ,                       "trust")

#load and merge gps dataset 
pa_terrest <- read_csv("./data/PROTECTED_AREAS_terrestrial.csv") %>% 
  dplyr::select(COU, Country, Value, YEA)

#merge both 
pa_terrest_gps  <-  pa_terrest %>%  
  group_by(COU) %>% 
  filter(YEA == max(YEA)) %>%
  ungroup() %>% 
  mutate(beta.analy.01 = trans.beta(Value, .001, 0.999)) %>% 
  right_join(gps, by = c("COU" = "isocode" )) %>% 
  mutate(sqrtvalue = sqrt(Value),
         lgdp = log(gdp_per_capita),
         lpopulationdensity= log(population_density),
         lmedianage = log(median_age),
         scale.gdp_per_capita = scale(gdp_per_capita),
         scale.population_density = scale(population_density),
         scale.median_age = scale(median_age)) %>% 
  rename(populationdensity = population_density,
         gdppercapita = gdp_per_capita,
         medianage = median_age) %>% 
  drop_na(lgdp, lmedianage, lpopulationdensity, sqrtvalue)

colnames(pa_terrest_gps)

#between zero and one
hist(pa_terrest_gps$beta.analy.01)
#for gjam
hist(pa_terrest_gps$sqrtvalue)


#by default , brms use normal
#define all model and submodel (direct/indirect)
df =  pa_terrest_gps %>% ungroup()  %>%  dplyr::select("populationdensity"  ,    "life_expectancy"  ,      
                                                       "human_development_index", "aged_70_older"        ,   "gdppercapita"      ,    "medianage"  ,           
                                                       "polstab"          ,       "account"             ,    "gov_eff"    ,             "regqual" ,               
                                                       "corrupt"        ,         "rulelaw"                 ,                                   
                                                       "patience"          ,      "risktaking"          ,    "posrecip"      ,         
                                                       "negrecip"            ,    "altruism"         ,       "trust"   ) %>% as.data.frame()   %>% drop_na() %>% 
  mutate_if(is.numeric, scale)



cormat  = cor(df)
corrplot(cormat, method = 'circle') # colorful number

#plotPairs(df, save = F, ncol = 2.6 , nrow = 3.6, log10SCALE = F)
#small beta regression - full model 
betamod = glmmTMB(beta.analy.01 ~ risktaking + patience +  trust + altruism + negrecip + gdppercapita + populationdensity + medianage, data = pa_terrest_gps, family=beta_family(link="logit"))
summary(betamod)
#small model evarulation
simulationOutput <- simulateResiduals(fittedModel = betamod, plot = T) #model looks ok 
#now do bootstrap to know how much variation we got in regression coefficient 
#take some time - > 30 min 
#see here for more details https://easystats.github.io/parameters/
#out1 = parameters::bootstrap_model(betamod, iterations = 1000)


#do some dredge model selection - to see if I missed an much better model, see variation in parameters etc 
#here is the part bellow 
oop <- 
  options(na.action = "na.fail")
#TAKE SOME TIME 
dd <- dredge(betamod)
subset(dd, delta < 4)
#models with delta.aicc < 4
summary(model.avg(dd, subset = delta < 4))

#test with brms
#need to scale covariates, too much differences, gdp, popdens, etc
# betamod.brms<- brm(beta.analy.01 ~ risktaking + patience +  
#                      trust + altruism + negrecip + 
#                      scale.gdp_per_capita + scale.population_density + scale.median_age, data = pa_terrest_gps,
#                    family = Beta(),backend = "cmdstanr",
#                    cores = 4, seed = 1234,
#                    chains = 4, iter = 6000, warmup = 1000)
# summary(betamod.brms)
# broom.mixed::tidy(betamod.brms, effects = "fixed")

##################################################################################################################################################################################################################
##########################################################################################
############################################################
#up feb 2024
library(rnaturalearth)
world_sf <- ne_countries(returnclass = "sf", scale = 50)
library(tidyverse)
library(sf)

#path data
path.full_pa_data_cleanedpb= list.files('/Users/vjourne/Nextcloud/behavioral_climate/prog/protected_areas_formating', pattern = 'rds', full.names = T)
#get lust country name 
country_list.temp = str_remove(path.full_pa_data_cleanedpb, pattern = '/Users/vjourne/Nextcloud/behavioral_climate/prog/protected_areas_formating/')
country_list = str_remove(country_list.temp, pattern = '.rds')
#load files PA
list.pa = list()
#sf_use_s2(FALSE)
for(j in 1:length(path.full_pa_data_cleanedpb)){
  list.pa[[j]] <- readRDS(paste(path.full_pa_data_cleanedpb[j]))
}

#get summary percentage 
summarypercentagearea = NULL
sf_use_s2(FALSE)
#a = 23 #(CYP)
for(a in 1:length(list.pa)){
  
  # summarypercentagearea.temp = list.pa[[a]] %>% 
  #   #filter(GEOMETRY_TYPE == 'POLYGON') %>% 
  #   as.data.frame() %>%
  #   select(-geometry) %>%
  #   group_by(IUCN_CAT, DESIG_TYPE, sizecountry, MARINE) %>%
  #   #group_by(MARINE) %>%
  #   summarize(area_km = sum(AREA_KM2)) %>%
  #   ungroup() %>%
  #   mutate(percentageByCountry = (area_km / sizecountry) * 100) %>%
  #   arrange(desc(area_km)) %>% 
  #   mutate(country = country_list[a]) 
  # summarypercentagearea = rbind(summarypercentagearea, summarypercentagearea.temp)
  
  tt = list.pa[[a]] %>% 
    filter(GEOMETRY_TYPE == 'POLYGON') %>% 
    filter(MARINE == 'terrestrial'& DESIG_TYPE == 'National') %>% 
    st_union() 
  #plot(tt)
  totpacountry = st_area(tt)
  
  summarypercentagearea.temp = list.pa[[a]] %>% 
    #filter(GEOMETRY_TYPE == 'POLYGON') %>% 
    as.data.frame() %>%
    select(-geometry) %>%
    group_by(IUCN_CAT, DESIG_TYPE, sizecountry, MARINE) %>%
    #group_by(MARINE) %>%
    summarize(area_km = sum(AREA_KM2)) %>%
    ungroup() %>%
    mutate(percentageByCountry = (area_km / sizecountry) * 100) %>%
    arrange(desc(area_km)) %>% 
    mutate(country = country_list[a],
           TOTAL_PA = totpacountry[2]/1e+6,
           perecentageTOTALovercountry = TOTAL_PA/sizecountry) 
  summarypercentagearea = rbind(summarypercentagearea, summarypercentagearea.temp)
  
  
  
}

tt = list.pa[[a]] %>% 
  filter(MARINE == 'terrestrial' & DESIG_TYPE == 'National')

plot(tt[9])

cc = summarypercentagearea %>% 
  dplyr::filter(MARINE == 'terrestrial') %>% 
  filter(DESIG_TYPE != 'Not Applicable')
boxplotareas = ggplot(summarypercentagearea %>% 
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

cowplot::save_plot("boxplotareas.png",
          boxplotareas, dpi = 300, nrow = 1.1, ncol = 1.2)


#selected columns
PAterrestre = summarypercentagearea %>% 
  dplyr::filter(MARINE == 'terrestrial') %>% 
  filter(DESIG_TYPE != 'Not Applicable') %>% 
  rename(isocode = country) %>% 
  group_by(sizecountry, isocode) %>% #DESIG_TYPE
  summarise_at(vars(area_km, percentageByCountry), lst( sum), na.rm = T) 

#FOR GPS DATA
########################################################################################
load("./data/covid_gps.RData")
load("./data/covid_evws_80.RData")

#redo with left join stuff
gps <- covid_gps %>%
  select("isocode","continent","country","population_density","human_development_index", "gdp_per_capita", 
         "patience"    ,                      
         "risktaking"         ,                "posrecip"     ,                     
         "negrecip"         ,                  "altruism"    ,                      
         "trust"     ) 

PAgps = PAterrestre %>% 
  left_join(gps) %>% 
  drop_na('gdp_per_capita')


ggplot(data = PAgps,
       mapping = aes(y = car::logit(percentageByCountry_sum),
                     x = posrecip))+geom_point()+
  geom_smooth()

#load("./data/covid_pays_wvs60.RData")
#check correlation and make PCA 
dfcor = PAgps[c("human_development_index",
                "gdp_per_capita", "polstab" ,"account","gov_eff","regqual","corrupt","rulelaw",
                "negrecip", "altruism" ,"trust", 'patience', 'posrecip', "risktaking")] %>% 
  mutate(gdp_per_capita = log10(gdp_per_capita))%>% 
  as.data.frame()
plotPairs(data = dfcor, save = T, ncol = 2.6 , nrow = 3.6)

#do a PCA with filled species 
PAgps.sub <- PAgps %>% ungroup() %>% dplyr::select('isocode', "human_development_index",
                                     "gdp_per_capita", "polstab" ,"account","gov_eff","regqual","corrupt","rulelaw"#,
                                     #"negrecip", "altruism" ,"trust", 'patience', 'posrecip', "risktaking"
                                     ) %>% mutate(gdp_per_capita = scale(gdp_per_capita, center = TRUE, scale = TRUE),
                                                  human_development_index = scale(human_development_index, center = T, scale = T)) %>% as.data.frame #%>% dplyr::select(-Species)   #CV, ACF1, 
row.names(PAgps.sub) <- PAgps.sub$isocode
PAgps.sub.analysis <- PAgps.sub[c(2:(ncol(PAgps.sub)-1))] #6 traits variable
PCAnew <- ade4::dudi.pca(PAgps.sub.analysis, center = FALSE, scale = FALSE, scannf = F, nf = ncol(PAgps.sub.analysis))
screeplot(PCAnew, main = "Screeplot - Eigenvalues")
ade4::s.corcircle(PCAnew$co, xax = 1, yax = 2)
ade4::s.corcircle(PCAnew$co, xax = 3, yax = 2)
factoextra::fviz_eig(PCAnew, addlabels = TRUE, ylim = c(0, 100))

#check contribution for first and second axis 
giveAxisContrib(PCAres = PCAnew, axisID = 1)
giveAxisContrib(PCAres = PCAnew, axisID = 2)

#after PCA, we'll make some analysis 
PAgps.PCA <- cbind(PAgps, PCAnew$li[,1], PCAnew$li[,2]) %>% 
  tibble() %>% 
  rename(PCA1 = 23, PCA2 = 24) %>% 
  mutate(beta.PA = percentageByCountry_sum/100,
         logit.PA = car::logit(beta.PA))
plot(y=PAgps.PCA$logit.PA, PAgps.PCA$beta.PA)

ggplot(PAgps.PCA, aes(y = logit.PA,
                      x = PCA1))+geom_point()
ggplot(PAgps.PCA, aes(y = logit.PA,
                      x = PCA2))+geom_point()

library(glmmTMB)
formula.gps1 = formula(beta.PA~negrecip+altruism+trust+patience+posrecip+risktaking+PCA1+PCA2)
m.gps <- glmmTMB(formula.gps1, data = PAgps.PCA, family=beta_family())
summary(m.gps)
simulation.m.gps <- simulateResiduals(fittedModel = m.gps, quantreg=T)
plot(simulation.m.gps)
coef.gps <-broom.mixed::tidy(m.gps,conf.int =TRUE)
dw <-dotwhisker::dwplot(coef.gps, by_2sd = T)
#check r2 and rmse 
#will not report r2
#https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/bmsp.12289
performance::model_performance(m.gps)
performance::r2_efron(m.gps)
nagelker.beta.gps <- rcompanion::nagelkerke(fit = m.gps,
                             null = glmmTMB(beta.PA~1, data = PAgps.PCA, family=beta_family()),
                             restrictNobs= FALSE)
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
  theme(legend.position = 'none')
dwup
cowplot::save_plot("betareg.png",dwup, 
                    ncol = 1.4, nrow = .8, dpi = 300)

#try dredge function
#https://stats.stackexchange.com/questions/473569/model-averaging-predictor-significance-vs-importance
#https://www.sciencedirect.com/science/article/pii/S0169534703003458
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cales.arizona.edu/classes/wfsc578/Symonds%20and%20Moussali%202011.%20A%20brief%20guide%20to%20model%20selection.pdf
m.gps.dre <- glmmTMB(formula.gps1, data = PAgps.PCA, family=beta_family(), na.action = "na.fail")
dd.gps = dredge(m.gps.dre)
summary(model.avg(dd.gps))
inf_mod <- subset(dd.gps, delta < 7)
#Calculate average parameter estimate across all informative models
avg_mod <- model.avg(inf_mod)
#Extract coefficients
coef <- coefficients(avg_mod, full = TRUE)
ci1 <- confint(avg_mod, full=TRUE)

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



######################
######################
#TEST WVS80 shit 
library(readxl)


variableWGI = c('VoiceAccount', 'PoliticalStability', 'GovEffectiveness', 'RegulatoryQuality','RuleOfLaw', 'ControlCorruption')
listout = list()

for(i in 1:length(variableWGI)){
  #load file and correct sheet nb 
  sheet2. =read_excel('data/wgidataset.xlsx', sheet = i+1, skip = 13)
  
  vectorIwant = paste0(c(1996, 1998, 2000,2002:2022), '...', seq(3, 146, by = 6))
  
  tt = cbind(sheet2.[1:2], select(sheet2., matches(vectorIwant))) %>% 
    rename(Country = 1,
           isocode = 2)
  tt = tt[-c(1),]
  colnames(tt)[-c(1,2)] = c(1996, 1998, 2000,2002:2022)
  ttfin = tt %>% pivot_longer('1996':'2022', names_to = 'year') %>% 
    mutate(value = as.numeric(as.character(value)),
           year = as.numeric(as.character(year)))
  colnames(ttfin)[4] = variableWGI[i]
  
  listout[[i]] = ttfin
}

governancedata = listout %>% reduce(left_join) %>% 
  mutate(isocode = ifelse(isocode == 'ROM', 'ROU', 
                          ifelse(isocode == 'ADO', 'AND', isocode)))

ggplot(governancedata, aes(x=year, y = GovEffectiveness, group = Country))+geom_line()



evws <- covid_evws_C %>%
  rename(country = Country) %>% 
  select("isocode","continent","country","population_density","human_development_index",
         "gdp_per_capita", 
         "wvs_altruism" ,"wvs_trust_global", 'wvs_patience') %>% 
  left_join(governancedata %>% filter(year == 2020) %>% dplyr::select(-year))

PAevws = PAterrestre %>% 
  left_join(evws) %>% 
  drop_na('gdp_per_capita') %>% 
  filter(percentageByCountry_sum<100)

PAevws.sub <- PAevws %>% ungroup() %>% dplyr::select('isocode', "human_development_index",
                                                   "gdp_per_capita", "VoiceAccount" ,"PoliticalStability","GovEffectiveness","RegulatoryQuality","RuleOfLaw","ControlCorruption"#,
) %>% mutate(gdp_per_capita = scale(gdp_per_capita, center = TRUE, scale = TRUE),
             human_development_index = scale(human_development_index, center = T, scale = T)) %>% as.data.frame #%>% dplyr::select(-Species)   #CV, ACF1, 
row.names(PAevws.sub) <- PAevws.sub$isocode
PAevws.sub.analysis <- PAevws.sub[c(2:(ncol(PAevws.sub)-1))] #6 traits variable
PCA.evws <- ade4::dudi.pca(PAevws.sub.analysis, center = FALSE, scale = FALSE, scannf = F, nf = ncol(PAevws.sub.analysis))
ade4::s.corcircle(PCA.evws$co, xax = 1, yax = 2)
#after PCA, we'll make some analysis 
PAevws.PCA <- cbind(PAevws, PCA.evws$li[,1], PCA.evws$li[,2]) %>% 
  tibble() %>% 
  rename(PCA1 = 20, PCA2 = 21) %>% 
  mutate(beta.PA = percentageByCountry_sum/100,
         logit.PA = car::logit(beta.PA))

formula.evws1 = formula(beta.PA~wvs_altruism+wvs_trust_global+wvs_patience+PCA1+PCA2)
m.evws <- glmmTMB(formula.evws1, data = PAevws.PCA, family=beta_family())
summary(m.evws)
simulation.m.evws <- simulateResiduals(fittedModel = m.evws, quantreg=T)
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
  theme(legend.position = 'none')
dw.evwsup
cowplot::save_plot("betareg.evws.png", dw.evwsup, 
                   ncol = 1.4, nrow = .8, dpi = 300)














# repair any geometry issues, dissolve the border, reproject to same
# coordinate system as the protected area data, and repair the geometry again
sf_use_s2(FALSE)

#known issue with summarise sf object 
# https://github.com/r-spatial/sf/issues/131

#to test to do and then another loop for each country 
test = pa.all.sf %>%
  filter(MARINE == 'terrestrial') %>% 
  group_by( DESIG_TYPE, PARENT_ISO, ISO3) %>% 
  summarize(geometry = st_union(geometry))

ggplot() +
 geom_sf(data = test , aes(fill = DESIG_TYPE), inherit.aes = FALSE) +
 theme(axis.title = element_blank())

sf_use_s2(FALSE)
fincountry = list()
for(k in 1:length(list.pa)){
  fincountry[[k]] = list.pa[[k]]%>%
    dplyr::filter(MARINE == 'terrestrial') %>% 
    filter(DESIG_TYPE != 'Not Applicable') %>% 
    group_by(IUCN_CAT, DESIG_TYPE, sizecountry, MARINE) %>%
    summarize(geometry = st_union(geometry))
  
}

#qs::qsave(fincountry, 'fincountry.qs')

allshptog <- bind_rows(fincountry)
sf::st_write(allshptog, "all_PA_United.gpkg")

mv_simpl <- st_simplify(allshptog, preserveTopology = FALSE, dTolerance = 0)
plot(mv_simpl)

mv_simpl = st_transform(allshptog, crs = 'ESRI:54032') #azimuthal equidistant
states_simple = st_simplify(mv_simpl)
states = st_transform(states_simple, crs = 4326) 
sf::st_write(states, "all_PA_United_simpleshape.gpkg")

#https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html for simpl shape 
#https://stackoverflow.com/questions/53507133/combining-multiple-shapefiles-in-r
ggplot(data=world_sf)+ 
  geom_sf(color = "black",
          fill = "#FAEBD7",
          size = 0.9,
          alpha = .2)+
  geom_sf(data = mv_simpl, aes(fill = DESIG_TYPE), inherit.aes = FALSE) 

plot (st_geometry(fincountry[[103]]))
