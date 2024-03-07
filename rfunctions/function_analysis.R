#some other function for pairs plot with ggplot 
#myPalette <- colorRampPalette(rev(brewer.pal(9, "Blues")))
colmin <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[2]
colmax <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[9]
#general for ggplot size axis
sizeAxisgg <- theme(axis.text=element_text(size=18),
                    axis.title=element_text(size=20),legend.title=element_text(size=18), legend.text = element_text(size = 17))

y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}
## scale between 0.001 and 0.999 for beta reg
trans.beta<- function(x, first, last) {
  (last - first) * ((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) + first
}


my_fn <- function(data, mapping, N=100, ...){
  
  get_density <- function(x, y, n ) {
    dens <- MASS::kde2d(x = x, y = y, n = n)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  data$density <- get_density(x=x, y=y, n = N)
  
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(aes(colour= density), ...)  + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    #viridis::scale_color_viridis(option = "magma")
    scale_colour_gradient(low=colmin, high = colmax)+
    #geom_smooth(method=lm, fill="blue", color="blue", ...)
    theme(axis.text.x = element_text(angle=90, hjust = 1))+
    sizeAxisgg
  p
}

my_fn2 <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d(aes(fill=..density..), geom="tile", contour = FALSE) +
    #stat_density_2d(aes(fill=stat(level)), geom="polygon", bins = 10) +
    scale_fill_viridis_c(option = "magma")
  p
}

my_histdens <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_histogram(aes(y = ..density..),color = "black", fill = "white") + 
    geom_density(alpha = 0.2, fill = "#FF6666") + 
    theme_minimal()+
    theme(axis.text.x = element_text(angle=90, hjust = 1),
          axis.text = element_text(colour = "black"), 
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black"))+
    sizeAxisgg
  
  p
}

cor_fun <- function(data, mapping, method="pearson", ndp=2, sz=7, stars=TRUE, ...){
  
  x <- eval_data_col(data, mapping$x) 
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method=method)
  est <- corr$estimate
  lb.size <- sz* abs(est) 
  
  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
    lbl <- paste0(round(est, ndp), stars)
  }else{
    lbl <- round(est, ndp)
  }
  
  ggplot(data=data, mapping=mapping) + 
    annotate("text", x=mean(x, na.rm=TRUE), y=mean(y, na.rm=TRUE), label=lbl,..., size = 8)+
    theme(panel.grid = element_blank())+sizeAxisgg
}


#pairs of histogram 
plotPairs <- function(data, save = T, ncol , nrow){
  
  ggpairs<- ggpairs(as.data.frame(data),
                    lower = list(continuous = my_fn),
                    upper = list(continuous = cor_fun), 
                    diag = list(continuous = my_histdens))
  
  if(save == T){
    save_plot("PairsTraits.png",ggpairs, ncol = ncol, nrow = nrow, dpi = 300) 
  }
  
  ggpairs
  
}

#for gjam to get higher credible interval for chains
.chain2tab <- function (chain, snames = NULL, xnn = NULL, wF = NULL, sigfig = 3) 
{
  mu <- colMeans(chain)
  SE <- apply(chain, 2, sd)
  CI <- apply(chain, 2, quantile, c(0.025, 0.975)) #5%
  CIb2 <- apply(chain, 2, quantile, c(0.1, 0.9)) #20%
  
  splus <- rep("", length = length(SE))
  splus[CI[1, ] > 0 | CI[2, ] < 0] <- "*"
  tab <- cbind(mu, SE, t(CI), t(CIb2))
  tab <- signif(tab, sigfig)
  colnames(tab) <- c("Estimate", "SE", "CI_025", "CI_975", "CI_100", "CI_900")
  tab <- as.data.frame(tab)
  tab$sig95 <- splus
  attr(tab, "note") <- "* indicates that zero is outside the 95% CI"
  mmat <- smat <- NULL
  if (!is.null(snames)) {
    Q <- length(xnn)
    S <- length(snames)
    mmat <- matrix(NA, Q, S)
    colnames(mmat) <- snames
    rownames(mmat) <- xnn
    smat <- mmat
    if (is.null(wF)) {
      wF <- 1:length(mmat)
    }
    mmat[wF] <- signif(mu, sigfig)
    smat[wF] <- signif(SE, sigfig)
    ww <- which(rowSums(mmat, na.rm = T) != 0)
    mmat <- mmat[drop = FALSE, ww, ]
    smat <- smat[drop = FALSE, ww, ]
  }
  list(mu = mmat, se = smat, tab = tab)
}
#to assign new enviroment 
environment(.chain2tab) <- asNamespace('gjam')
assignInNamespace(".chain2tab", .chain2tab, ns = "gjam")

giveAxisContrib <- function(PCAres = PCAres, axisID = axisID){
  library(factoextra)
  dd <- facto_summarize(PCAres, element = "var", result = "contrib", 
                        axes = axisID)
  contrib <- dd$contrib
  theo_contrib <- 100/length(contrib)
  dd$theo = theo_contrib
  dd$axisID = axisID
  row.names(dd) <- NULL
  return(dd)
}

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


formatWGIdata = function(pathgovdata,variableWGI ){
  library(readxl)
  require(tidyverse)
  listout = list()
  for(i in 1:length(variableWGI)){
    #load file and correct sheet nb 
    sheet2. =read_excel(pathgovdata, sheet = i+1, skip = 13)
    
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
  
  return(governancedata)
}

#get three argument function
#function to extract percentage PA - I made some assumption, for example removing marine PA
getPercentagePA = function(sfuse = F,
                           list.pa,
                           country_list,
                           methodrobust = T ){
  if(sfuse == T){
    sf_use_s2(T)
  }else(sf_use_s2(F))
  #get summary percentage 
  summarypercentagearea = NULL
  totpacountry = NULL
  if(methodrobust == T){
    for(a in 1:length(list.pa)){
      #&DESIG_TYPE %in% c('National', 'International', 'Regional')
      tt = list.pa[[a]] %>% 
        #filter(GEOMETRY_TYPE == 'POLYGON') %>% 
        filter(MARINE == 'terrestrial' ) %>% 
        st_union() 
      
      totpacountry[[a]] = as.numeric(st_area(tt) / 1000000 )
      #(totpacountry[[a]]/unique(list.pa[[a]]$sizecountry))/1e+6
      
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
               TOTAL_PA = totpacountry[[a]],
               perecentageTOTALovercountry = TOTAL_PA*100/sizecountry) 
      summarypercentagearea = rbind(summarypercentagearea, summarypercentagearea.temp)
      
    }
  }else{
    for(a in 1:length(list.pa)){
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
        mutate(country = country_list[a]) 
      summarypercentagearea = rbind(summarypercentagearea, summarypercentagearea.temp)
    }
  }
  return(summarypercentagearea)
}


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


rotate_dudi.pca <- function(pca, ncomp = 2) {
  
  rawLoadings <- as.matrix(pca$c1[,1:ncomp]) %*% diag(sqrt(pca$eig), ncomp, ncomp)
  pca$c1 <- rawLoadings
  pca$li <- scale(pca$li[,1:ncomp]) %*% varimax(rawLoadings)$rotmat
  
  return(pca)
} 
