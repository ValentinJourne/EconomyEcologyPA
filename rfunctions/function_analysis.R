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
plotPairs <- function(data, save = T, ncol , nrow, log10SCALE = F){
  
  ggpairs<- ggpairs(as.data.frame(data),
                    lower = list(continuous = my_fn),
                    upper = list(continuous = cor_fun), 
                    diag = list(continuous = my_histdens))
  
  if(log10SCALE == T){
    ggpairs <- ggpairs+scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                     labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))
  }
  
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

