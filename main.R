require(purrr)
require(tidyverse)
library(here)
functions <- list.files("./rfunctions/", full.names = T) %>%
  purrr::map(source)

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
         lmedianage = log(median_age)) %>% 
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
#small beta regression 
betamod = glmmTMB(beta.analy.01 ~ risktaking + patience +  trust + altruism + negrecip + gdppercapita + populationdensity + medianage, data = pa_terrest_gps, family=beta_family(link="logit"))
summary(betamod)
#small model evarulation
simulationOutput <- simulateResiduals(fittedModel = betamod, plot = T)


###########################
#Joint model 
###########################
#joint model part 
xdata <- pa_terrest_gps %>% dplyr::select(medianage, populationdensity, gdppercapita) %>% as.data.frame()
#response
ydata  <- as.data.frame(pa_terrest_gps  %>% 
                          dplyr::select(sqrtvalue, risktaking, patience, trust, altruism))  
# #associate row names with species name - in case, but here same number of row matching, 
# row.names(ydata) <- ydata$Species
# ydata<- ydata[-c(1:2)]
# row.names(xdata) <- xdata$Species
# xdata<- xdata[-c(3)] 

# #gjam specification 
types <- c("CON" , "CON", 'CON',"CON" , "CON") #, "CON", "CON", "CON", "CON", "CON", "CON"
ml  <- list(ng = 6000, burnin = 2000, typeNames = types) #to increase
# ml$random <- 'genusNewFactor'
# 
# #gjam fit 
outgjam1 <- gjam(~ gdppercapita + medianage + populationdensity, 
                 xdata = xdata, ydata = ydata, modelList = ml)

#just plot the model, see sensitivity, different response, chaines, etc 
gjamPlot(outgjam1)

#summary of the model
summary(outgjam1)
#summary of standardized coeffciient - what we want 
outgjam1$parameters$betaStandXWTable

#now we can do the conditioning - to get the direct effect of trait on Protected area
condgjam <- gjamConditionalParameters( outgjam1, conditionOn = c('risktaking', "patience", "trust", "altruism") , nsim = 10000)

#pay attention to number of ccolum here 2:5
condP1V2 <- condgjam$Amu %>% as.data.frame() %>% rownames_to_column("response") %>% 
  pivot_longer(2:ncol(.), names_to = "covariates", values_to = "Estimate") %>% 
  left_join(condgjam$Atab %>% as.data.frame()) %>% 
  mutate(sigSens = ifelse(CI_025 > 0 & CI_975 > 0, "POS", ifelse(CI_025 < 0 & CI_975 < 0, "NEG", "NO"))) %>% 
  mutate(signV2 =  ifelse(CI_025 > 0 & CI_975 > 0, "strongPOS", 
                          ifelse(CI_025 < 0 & CI_975 < 0, "strongNEG",
                                 ifelse(CI_100 < 0 & CI_900 < 0, "medNeg",
                                        ifelse(CI_100 > 0 & CI_900 > 0, "medPo", "NO")))))

#and make the plot now !
ggplot(condP1V2, aes( y = covariates)) + 
  geom_errorbarh(aes(x = Estimate, 
                     xmin = CI_025,xmax = CI_975, col = sigSens, fill = sigSens), height=.0, size =1) + 
  geom_point(aes(x = Estimate, col = sigSens, fill = sigSens), size = 5, shape = 108)+
  geom_vline(aes(xintercept = 0), col = "darkred",linetype = "dashed", size = 0.5)+
  theme(legend.position = "none")

# formula_sem <- '
# patience ~ gdp_per_capita
# Value ~ patience
# '
# 
# sem_run <- sem(formula_sem, data = pa_terrest_gps)
# graph_sem(model = sem_run)
# get_edges(sem_run)
# summary(sem_run)


#missingval  = pa_terrest_gps %>% filter(is.na(Value))
#COUmissin = c(missingval$COU)
#percentage 
#Valuemissin = c(0, 4.6, 4.6, 0, 0, )
#Yearmissin = c(2013, 2020, 2014, 2013, 2013, )

# fit3way <- brm(count ~ zAge * zBase * Trt, data = epilepsy,  backend = "cmdstanr")
# conditions <- make_conditions(fit3way, "zAge")
# conditional_effects(fit3way, "zBase:Trt", conditions = conditions)
# conditional_effects(fit3way, "zBase:Trt")
# 
# response <- bf(Value ~ risktaking + patience +  trust + altruism + gdplog + popden + medAge)
# bfz <- bf(risktaking ~  gdplog + popden + medAge)
# bfz1 <- bf(patience ~  gdplog + popden + medAge )
# bfz2 <- bf(trust ~ gdplog + popden + medAge)
# bfz3 <- bf(altruism ~ gdplog + popden + medAge )
# 
# 
# path1 <- bf(CV ~ pollibi+dispersalbi+ sMAT*sMAP ) #+ (1 | genusNewFactor)
# path2 <-  bf(pollibi ~ sMAT*sMAP, family = "binomial")
# path3<- bf(dispersalbi ~ sMAT*sMAP, family = "binomial")
# 
# mod.po.di.cl <- brm(
#   path1 + path2 + path3 + 
#     set_rescor(FALSE), #set_rescor(FALSE) ensures that no residual correlation between y and z is estimated since we already have z as predictor for y
#   warmup = 1000, 
#   iter = 8000,  backend = "cmdstanr", 
#   data=MastreeSpeciesGJAMmissing,
#   sample_prior = 'yes',
#   cores=detectCores()-2, chains = 4)
# 
