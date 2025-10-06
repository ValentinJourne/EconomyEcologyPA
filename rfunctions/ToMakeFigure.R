#plot output from gjam 
plotGJAMinit <- function(gjammodel = gjammodel){
  
  formatFig <- gjammodel$parameters$betaStandXWTable %>% 
    tibble::rownames_to_column("variable") %>% 
    separate(variable , into=c("response", "covariates"), 
             sep="_") %>% 
    mutate(sigSens = ifelse(CI_025 > 0 & CI_975 > 0, "POS", ifelse(CI_025 < 0 & CI_975 < 0, "NEG", "NO"))) %>% 
    mutate(signV2 =  ifelse(CI_025 > 0 & CI_975 > 0, "strongPOS", 
                            ifelse(CI_025 < 0 & CI_975 < 0, "strongNEG",
                                   ifelse(CI_100 < 0 & CI_900 < 0, "medNeg",
                                          ifelse(CI_100 > 0 & CI_900 > 0, "medPo", "NO"))))) %>% 
    mutate(colors =  ifelse(CI_025 > 0 & CI_975 > 0, "#1F78B4", 
                            ifelse(CI_025 < 0 & CI_975 < 0, "#FF7F00",
                                   ifelse(CI_100 < 0 & CI_900 < 0, "#FDBF6F",
                                          ifelse(CI_100 > 0 & CI_900 > 0, "#A6CEE3", "grey70")))))
  
  colors <- distinct(formatFig, signV2, colors)
  pal <- colors$colors
  names(pal) <- colors$signV2
  
  coefPlot <- ggplot(formatFig, aes( x = covariates)) + 
    facet_grid(.~ response)+ 
    geom_boxplot(aes(x = covariates, ymin = CI_025, lower = CI_100, group = covariates, middle = Estimate, 
                     upper = CI_900, ymax = CI_975, fill = signV2), stat = "identity",
                 position = position_dodge2(preserve = "total"), 
                 alpha = .9 , width = .5, col = "grey30", size = 0.3)+
    coord_flip()+
    geom_hline(aes(yintercept = 0), col = "black",linetype = "dashed", size = 0.5)+
    scale_color_manual(values = pal, name = "")+
    scale_fill_manual(values = pal, name = "")+
    xlab("")+
    ylab("Coefficient value")+
    theme(legend.position = "none",
          strip.background = element_blank(), 
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.text.x = element_text(size = 12, face = "bold"))
  
  return(coefPlot)
  
}