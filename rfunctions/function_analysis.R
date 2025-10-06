#some other function for pairs plot with ggplot
#myPalette <- colorRampPalette(rev(brewer.pal(9, "Blues")))
colmin <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[2]
colmax <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[9]
#general for ggplot size axis
sizeAxisgg <- theme(
  axis.text = element_text(size = 18),
  axis.title = element_text(size = 20),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 17)
)

y.transf.betareg <- function(y) {
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}
## scale between 0.001 and 0.999 for beta reg
trans.beta <- function(x, first, last) {
  (last - first) *
    ((x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) +
    first
}

#FUNCTION FOR PAIR PLOTS
my_fn <- function(data, mapping, N = 100, ...) {
  get_density <- function(x, y, n) {
    dens <- MASS::kde2d(x = x, y = y, n = n)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }

  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  data$density <- get_density(x = x, y = y, n = N)

  p <- ggplot(data = data, mapping = mapping) +
    geom_point(aes(colour = density), ...) +
    geom_smooth(method = loess, fill = "red", color = "red", ...) +
    #viridis::scale_color_viridis(option = "magma")
    scale_colour_gradient(low = colmin, high = colmax) +
    #geom_smooth(method=lm, fill="blue", color="blue", ...)
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    sizeAxisgg
  p
}

my_fn2 <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    stat_density2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
    #stat_density_2d(aes(fill=stat(level)), geom="polygon", bins = 10) +
    scale_fill_viridis_c(option = "magma")
  p
}

my_histdens <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
    geom_density(alpha = 0.2, fill = "#FF6666") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text = element_text(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_line(colour = "black")
    ) +
    sizeAxisgg

  p
}

cor_fun <- function(
  data,
  mapping,
  method = "pearson",
  ndp = 2,
  sz = 7,
  stars = TRUE,
  ...
) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  corr <- cor.test(x, y, method = method)
  est <- corr$estimate
  lb.size <- sz * abs(est)

  if (stars) {
    stars <- c("***", "**", "*", "")[findInterval(
      corr$p.value,
      c(0, 0.001, 0.01, 0.05, 1)
    )]
    lbl <- paste0(round(est, ndp), stars)
  } else {
    lbl <- round(est, ndp)
  }

  ggplot(data = data, mapping = mapping) +
    annotate(
      "text",
      x = mean(x, na.rm = TRUE),
      y = mean(y, na.rm = TRUE),
      label = lbl,
      ...,
      size = 8
    ) +
    theme(panel.grid = element_blank()) +
    sizeAxisgg
}


#pairs of histogram
plotPairs <- function(data, name = "tttt.png", save = T, ncol, nrow) {
  ggpairs <- ggpairs(
    as.data.frame(data),
    lower = list(continuous = my_fn),
    upper = list(continuous = cor_fun),
    diag = list(continuous = my_histdens)
  )

  if (save == T) {
    save_plot(here(name), ggpairs, ncol = ncol, nrow = nrow, dpi = 300)
  }

  ggpairs
}

#for gjam to get higher credible interval for chains
.chain2tab <- function(
  chain,
  snames = NULL,
  xnn = NULL,
  wF = NULL,
  sigfig = 3
) {
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

giveAxisContrib <- function(PCAres = PCAres, axisID = axisID) {
  library(factoextra)
  dd <- facto_summarize(
    PCAres,
    element = "var",
    result = "contrib",
    axes = axisID
  )
  contrib <- dd$contrib
  theo_contrib <- 100 / length(contrib)
  dd$theo = theo_contrib
  dd$axisID = axisID
  row.names(dd) <- NULL
  return(dd)
}


#function to format updated governance data
formatWGIdata = function(pathgovdata, variableWGI) {
  library(readxl)
  require(tidyverse)
  listout = list()
  for (i in 1:length(variableWGI)) {
    #load file and correct sheet nb
    sheet2. = read_excel(pathgovdata, sheet = i + 1, skip = 13)

    vectorIwant = paste0(
      c(1996, 1998, 2000, 2002:2022),
      '...',
      seq(3, 146, by = 6)
    )

    tt = cbind(sheet2.[1:2], dplyr::select(sheet2., matches(vectorIwant))) %>%
      rename(Country = 1, isocode = 2)
    tt = tt[-c(1), ]
    colnames(tt)[-c(1, 2)] = c(1996, 1998, 2000, 2002:2022)
    ttfin = tt %>%
      pivot_longer('1996':'2022', names_to = 'year') %>%
      mutate(
        value = as.numeric(as.character(value)),
        year = as.numeric(as.character(year))
      )
    colnames(ttfin)[4] = variableWGI[i]

    listout[[i]] = ttfin
  }

  governancedata = listout %>%
    reduce(left_join) %>%
    mutate(
      isocode = ifelse(
        isocode == 'ROM',
        'ROU',
        ifelse(isocode == 'ADO', 'AND', isocode)
      )
    )

  return(governancedata)
}

#for later
safe_val <- function(x) ifelse(length(x) == 0, 0, x)

#to get area in km
safe_area_km2 <- function(x) {
  if (inherits(x, "sf") && nrow(x) == 0) return(0)
  a <- suppressWarnings(sf::st_area(x))
  if (length(a) == 0) return(0)
  as.numeric(units::set_units(a, km^2))
}

#get three argument function
#function to extract percentage PA - I made some assumption, for example removing marine PA
#not the best code I have made but it is working
getPercentagePA = function(
  sfuse = F,
  list.pa,
  country_list,
  methodrobust = T,
  sub.year = T,
  threshold_year = 1948
) {
  if (sfuse == T) {
    sf_use_s2(T)
  } else (sf_use_s2(F))
  #get summary percentage
  summarypercentagearea = NULL
  totpacountry = NULL
  tot.pa.before = NULL
  tot.pa.after = NULL
  tot.pa.missing.year = NULL
  if (methodrobust == T) {
    for (a in 1:length(list.pa)) {
      #&DESIG_TYPE %in% c('National', 'International', 'Regional')

      list.pa[[a]] = list.pa[[a]] %>%
        filter(IUCN_CAT %in% c("I", "II", "III", "IV", "V", "VI"))

      if (sub.year == T) {
        tt = list.pa[[a]] %>%
          filter(MARINE == 'terrestrial') %>%
          #filter(STATUS_YR > 1950) %>%
          st_union()

        #totpacountry[[a]] = as.numeric(st_area(tt) / 1000000)
        totpacountry[[a]] <- safe_area_km2(tt)

        tt.time.series <- list.pa[[a]] %>%
          #filter(STATUS_YR > 1950) %>%
          filter(MARINE == 'terrestrial') %>%
          mutate(
            year.factor.status = case_when(
              is.na(STATUS_YR) ~ NA_character_,
              STATUS_YR <= threshold_year ~ "Before",
              TRUE ~ "After"
            ) |>
              fct_explicit_na(na_level = "Missing"),
            MARINE = as.character(MARINE)
          )

        before. = tt.time.series %>%
          filter(year.factor.status == "Before") %>%
          st_union()

        tot.pa.before[[a]] = safe_area_km2(before.) #as.numeric(st_area(before.) / 1000000)

        after. = tt.time.series %>%
          filter(year.factor.status == "After") %>%
          st_union()

        tot.pa.after[[a]] = safe_area_km2(after.) #as.numeric(st_area(after.) / 1000000)

        missing.year.PA = tt.time.series %>%
          filter(is.na(year.factor.status)) %>%
          st_union()

        tot.pa.missing.year[[a]] = safe_area_km2(missing.year.PA) #as.numeric(
        #st_area(missing.year.PA) / 1000000
        #)

        summarypercentagearea.temp = list.pa[[a]] %>%
          mutate(
            year.factor.status = as_factor(if_else(
              STATUS_YR <= threshold_year,
              "Before",
              "After"
            ))
          ) %>%
          as.data.frame() %>%
          dplyr::select(-geometry) %>%
          group_by(
            IUCN_CAT,
            DESIG_TYPE,
            sizecountry,
            MARINE,
            year.factor.status
          ) %>%
          summarize(area_km = sum(AREA_KM2)) %>%
          ungroup() %>%
          mutate(percentageByCountry = (area_km / sizecountry) * 100) %>%
          arrange(desc(area_km)) %>%
          mutate(
            country = country_list[a],
            TOTAL_PA = totpacountry[[a]],
            perecentageTOTALovercountry = TOTAL_PA * 100 / sizecountry,
            percentageTotal.before = safe_val(tot.pa.before[[a]]) *
              100 /
              sizecountry,
            percentageTotal.after = safe_val(tot.pa.after[[a]]) *
              100 /
              sizecountry,
            percentageTotal.missingYear = safe_val(tot.pa.missing.year[[a]]) *
              100 /
              sizecountry
          )
      } else {
        tt = list.pa[[a]] %>%
          #filter(GEOMETRY_TYPE == 'POLYGON') %>%
          filter(MARINE == 'terrestrial') %>%
          filter(IUCN_CAT %in% c("I", "II", "III", "IV", "V", "VI")) %>%
          st_union()

        totpacountry[[a]] = as.numeric(st_area(tt) / 1000000)
        #here it does not matter to much, because after I'll do a subset focus only on terrestrial
        #so pay attention that percentage for Marine PA are wrong or does not really make sense XX
        summarypercentagearea.temp = list.pa[[a]] %>%
          mutate(
            year.factor.status = as_factor(if_else(
              STATUS_YR <= threshold_year,
              "Before",
              "After"
            ))
          ) %>%
          as.data.frame() %>%
          dplyr::select(-geometry) %>%
          group_by(
            IUCN_CAT,
            DESIG_TYPE,
            sizecountry,
            MARINE,
            year.factor.status
          ) %>%
          summarize(area_km = sum(AREA_KM2)) %>%
          ungroup() %>%
          mutate(percentageByCountry = (area_km / sizecountry) * 100) %>%
          arrange(desc(area_km)) %>%
          mutate(
            country = country_list[a],
            TOTAL_PA = totpacountry[[a]],
            perecentageTOTALovercountry = TOTAL_PA * 100 / sizecountry
          )
      }

      #(totpacountry[[a]]/unique(list.pa[[a]]$sizecountry))/1e+6

      summarypercentagearea = rbind(
        summarypercentagearea,
        summarypercentagearea.temp
      )
    }
  } else {
    for (a in 1:length(list.pa)) {
      summarypercentagearea.temp = list.pa[[a]] %>%
        #filter(GEOMETRY_TYPE == 'POLYGON') %>%
        as.data.frame() %>%
        dplyr::select(-geometry) %>%
        filter(MARINE == 'terrestrial') %>%
        filter(IUCN_CAT %in% c("I", "II", "III", "IV", "V", "VI")) %>%
        mutate(
          year.factor.status = if_else(
            STATUS_YR <= threshold_year,
            "Before",
            "After"
          )
        ) %>%
        group_by(
          IUCN_CAT,
          DESIG_TYPE,
          sizecountry,
          MARINE,
          year.factor.status
        ) %>%
        summarize(area_km = sum(AREA_KM2)) %>%
        ungroup() %>%
        mutate(percentageByCountry = (area_km / sizecountry) * 100) %>%
        arrange(desc(area_km)) %>%
        mutate(country = country_list[a])

      summarypercentagearea = rbind(
        summarypercentagearea,
        summarypercentagearea.temp
      )
    }
  }
  return(summarypercentagearea)
}


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
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

#function to rotate PCA axis
rotate_dudi.pca <- function(pca, ncomp = 2) {
  rawLoadings <- as.matrix(pca$c1[, 1:ncomp]) %*%
    diag(sqrt(pca$eig), ncomp, ncomp)
  pca$c1 <- rawLoadings
  pca$li <- scale(pca$li[, 1:ncomp]) %*% varimax(rawLoadings)$rotmat

  return(pca)
}


getPercentagePA_upgraded <- function(
  sfuse = FALSE,
  list.pa,
  country_list
) {
  if (sfuse) sf::sf_use_s2(TRUE) else sf::sf_use_s2(FALSE)

  summarypercentagearea <- NULL

  for (a in seq_along(list.pa)) {
    # subset for this country
    list.pa.sub <- list.pa[[a]] %>%
      dplyr::filter(
        IUCN_CAT %in% c("I", "II", "III", "IV", "V", "VI"),
        MARINE == "terrestrial"
      )

    country_list_sub <- country_list[a]

    # skip if empty after filter
    if (nrow(list.pa.sub) == 0) next

    # total PA footprint (union) in km²
    tt <- list.pa.sub %>% sf::st_union()
    totpacountry <- safe_area_km2(tt)

    # year range for this country
    min.year <- min(list.pa.sub$STATUS_YR, na.rm = TRUE)
    max.year <- max(list.pa.sub$STATUS_YR, na.rm = TRUE)
    n.year <- if (!is.finite(min.year)) 2000:2020 else
      seq(min.year, max.year, by = 1)

    # mark before/after the first year
    tt.time.series <- list.pa.sub %>%
      dplyr::mutate(
        year.factor.status = dplyr::case_when(
          is.na(STATUS_YR) ~ NA_character_,
          STATUS_YR <= min.year ~ "Before",
          TRUE ~ "After"
        ) %>%
          forcats::fct_explicit_na(na_level = "Missing")
      )

    # area before the initial year
    before. <- tt.time.series %>%
      dplyr::filter(year.factor.status == "Before") %>%
      sf::st_union()
    tot.pa.before <- safe_area_km2(before.)

    # build per-year totals from attributes (your approach), then cumulative
    out <- vector("list", length(n.year))
    for (y in seq_along(n.year)) {
      y_df <- tt.time.series %>%
        dplyr::filter(STATUS_YR <= n.year[y]) %>%
        st_union() #%>%
      #sf::st_drop_geometry()

      #cum_km2 <- if (nrow(y_df) == 0) 0 else
      #  sum(y_df[["REP_AREA"]], na.rm = TRUE)

      cum_km2 = safe_area_km2(y_df)

      out[[y]] <- dplyr::tibble(
        country = country_list_sub,
        year = n.year[y],
        cum_km2 = cum_km2
      )
    }

    tot.pa.after <- dplyr::bind_rows(out) %>%
      mutate(is_same_as_previous = (cum_km2 == lag(cum_km2)))

    # summary by category (one row per category), then join single-row country summary
    summarypercentagearea.temp <- list.pa.sub %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(IUCN_CAT, DESIG_TYPE, sizecountry, MARINE) %>%
      dplyr::summarize(
        area_km = sum(AREA_KM2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        percentageByCountry = (area_km / sizecountry) * 100,
        country = country_list_sub,
        TOTAL_PA = totpacountry,
        perecentageTOTALovercountry = TOTAL_PA * 100 / sizecountry,
        percentageTotal.Initial.Year = safe_val(tot.pa.before) *
          100 /
          sizecountry
      ) %>%
      dplyr::left_join(tot.pa.after, by = "country")

    # append
    summarypercentagearea <- rbind(
      summarypercentagearea,
      summarypercentagearea.temp
    )
  }

  # make sure no accidental exact duplicates remain
  summarypercentagearea <- dplyr::distinct(summarypercentagearea)

  return(summarypercentagearea)
}

plot_partial_effect <- function(
  model,
  data,
  focal,
  re_levels = list(isocode.factor = "FRA", year.factor = "1970"),
  n = 100,
  xlab = NULL,
  ylab = "Protected area coverage",
  ribbon_alpha = 0.25,
  line_col = "blue",
  clip01 = TRUE
) {
  stopifnot(focal %in% names(data))

  # what are the terms here
  tt <- stats::terms(model)
  fe_terms <- attr(tt, "term.labels")
  # remove interactions; expand later if you need
  fe_terms <- fe_terms[!grepl(":", fe_terms)]
  fe_terms <- fe_terms[fe_terms %in% names(data)]

  # future X predictors
  x_seq <- seq(
    min(data[[focal]], na.rm = TRUE),
    max(data[[focal]], na.rm = TRUE),
    length.out = n
  )

  newdata <- data.frame(rep(NA_real_, n))
  names(newdata) <- focal
  newdata[[focal]] <- x_seq

  # set other covariates: numeric -> mean; factor/character -> first level
  for (v in setdiff(fe_terms, focal)) {
    if (is.numeric(data[[v]])) {
      newdata[[v]] <- mean(data[[v]], na.rm = TRUE)
    } else {
      newdata[[v]] <- if (is.factor(data[[v]])) levels(data[[v]])[1] else
        unique(data[[v]])[1]
    }
  }

  # take from re_levels if provided; else use first level in data
  # try to detect grouping factors from formula
  re_labs <- character(0)
  if (!is.null(attr(tt, "predvars.random")) || !is.null(attr(tt, "specials"))) {
    # generic: user passes re_levels; we include any provided names
    re_labs <- intersect(names(re_levels), names(data))
  } else {
    # still include any provided in re_levels that exist in data
    re_labs <- intersect(names(re_levels), names(data))
  }
  for (g in re_labs) newdata[[g]] <- re_levels[[g]]
  # if a grouping factor exists in data but not in re_levels, set to first level
  maybe_groups <- setdiff(
    names(data)[sapply(data, function(x) is.factor(x) || is.character(x))],
    c(names(newdata), fe_terms)
  )
  for (g in maybe_groups)
    if (g %in% names(model.frame(model))) {
      if (!g %in% names(newdata)) {
        newdata[[g]] <- if (is.factor(data[[g]])) levels(data[[g]])[1] else
          unique(data[[g]])[1]
      }
    }

  #but dont do this as it is beta regression models, it does not assume gaussian
  #pr <- predict(model, newdata = newdata, type = "response", se.fit = TRUE)
  #newdata$fit <- as.numeric(pr$fit)
  #newdata$se <- as.numeric(pr$se.fit)
  #newdata$upper <- newdata$fit + 1.96 * newdata$se
  #newdata$lower <- newdata$fit - 1.96 * newdata$se
  linkinv <- NULL
  fam_try <- try(stats::family(model), silent = TRUE)
  if (!inherits(fam_try, "try-error") && !is.null(fam_try$linkinv))
    linkinv <- fam_try$linkinv
  if (is.null(linkinv)) {
    # glmmTMB sometimes stores family in $family
    fam2 <- try(model$family, silent = TRUE)
    if (!inherits(fam2, "try-error") && !is.null(fam2$linkinv))
      linkinv <- fam2$linkinv
  }
  if (is.null(linkinv)) stop("Could not retrieve link inverse from model.")

  pr <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
  newdata$fit_link <- as.numeric(pr$fit)
  newdata$lower_link <- newdata$fit_link - 1.96 * as.numeric(pr$se.fit)
  newdata$upper_link <- newdata$fit_link + 1.96 * as.numeric(pr$se.fit)

  newdata$fit <- linkinv(newdata$fit_link)
  newdata$lower <- linkinv(newdata$lower_link)
  newdata$upper <- linkinv(newdata$upper_link)

  if (clip01) {
    newdata$fit <- pmin(pmax(newdata$fit, 0), 1)
    newdata$lower <- pmin(pmax(newdata$lower, 0), 1)
    newdata$upper <- pmin(pmax(newdata$upper, 0), 1)
  }

  # check sign of the predictors
  pval <- NA_real_
  est <- NA_real_

  p_try <- try(
    {
      if (requireNamespace("broom.mixed", quietly = TRUE)) {
        tb <- broom.mixed::tidy(model, effects = "fixed")
        row <- tb$term == focal
        if (!any(row)) row <- grepl(paste0("^", focal, "$"), tb$term)
        if (any(row)) {
          pval <- tb$p.value[row][1]
          est <- tb$estimate[row][1]
        }
      }
    },
    silent = TRUE
  )

  if (is.na(pval)) {
    # fallback for glmmTMB or GLM: extract from summary
    s <- summary(model)
    cf <- try(s$coefficients$cond, silent = TRUE)
    if (inherits(cf, "try-error") || is.null(cf))
      cf <- try(coef(s), silent = TRUE)
    if (!inherits(cf, "try-error") && !is.null(cf)) {
      rn <- rownames(cf)
      hit <- which(rn == focal)
      if (length(hit) == 0) hit <- grep(paste0("^", focal, "$"), rn)
      if (length(hit) > 0) {
        est <- cf[hit[1], 1]
        # Wald p available for many models:
        pcol <- which(colnames(cf) %in% c("Pr(>|z|)", "Pr(>|t|)"))
        if (length(pcol)) pval <- cf[hit[1], pcol[1]]
      }
    }
  }

  sig <- !is.na(pval) && (pval < 0.05)
  lt <- if (sig) "solid" else "dotted"

  #add obs data aggregates (median and IQR )
  agg <- data %>%
    dplyr::summarise(
      mean_y = median(ChangePA.scaled, na.rm = TRUE),
      sd_y = plotrix::std.error(ChangePA.scaled, na.rm = TRUE),
      high.q = as.numeric(quantile(ChangePA.scaled, 0.75, na.rm = TRUE)),
      low.q = as.numeric(quantile(ChangePA.scaled, 0.25, na.rm = TRUE)),
      .by = focal
    )

  # the ggplot re4g
  ggplot() +
    geom_point(data = agg, aes_string(x = focal, y = "mean_y"), alpha = 0.05) +
    geom_pointrange(
      data = agg,
      aes_string(
        x = focal,
        y = "mean_y",
        ymin = "low.q", #"mean_y - sd_y",
        ymax = "high.q" #"mean_y + sd_y"
      ),
      alpha = 0.05
    ) +
    geom_ribbon(
      data = newdata,
      aes_string(x = focal, y = "fit", ymin = "lower", ymax = "upper"),
      alpha = ribbon_alpha,
      fill = line_col,
      inherit.aes = FALSE
    ) +
    geom_line(
      data = newdata,
      aes_string(x = focal, y = "fit"),
      color = line_col,
      linewidth = 0.9,
      linetype = lt
    ) +
    labs(
      x = xlab %||% focal,
      y = ylab,
      #subtitle = if (!is.na(pval))
      #  sprintf("%s: estimate = %.3f, p = %.3f", focal, est, pval) else NULL
    ) +
    hrbrthemes::theme_ipsum(base_size = 14, axis_title_size = 16)
}

# helper
`%||%` <- function(a, b) if (!is.null(a)) a else b


#contribution
temp_contrib_glmmTMB <- function(
  model,
  drivers = NULL,
  group,
  timevar = "year",
  plot = TRUE
) {
  stopifnot(inherits(model, "glmmTMB"))
  if (missing(group))
    stop("Please provide the grouping variable name (e.g., 'isocode.factor').")

  # -- Extract model data robustly
  df <- try(model.frame(model), silent = TRUE)
  if (inherits(df, "try-error") || is.null(df) || nrow(df) == 0) {
    stop(
      "Could not retrieve model.frame(model). Did you fit with na.action=... ?"
    )
  }

  # enforce timevar exists
  if (!timevar %in% names(df))
    stop(sprintf("timevar '%s' not found in model data.", timevar))
  if (!group %in% names(df))
    stop(sprintf("group '%s' not found in model data.", group))
  df[[timevar]] <- as.numeric(df[[timevar]])

  # -- response name detection (more robust)
  resp <- NULL
  if (requireNamespace("insight", quietly = TRUE)) {
    resp <- insight::find_response(model)[1]
  }
  if (is.null(resp)) {
    # fallback
    form_cond <- stats::formula(model)$cond
    resp <- all.vars(form_cond[[2]])[1]
  }
  if (!resp %in% names(df)) {
    stop(sprintf(
      "Detected response '%s' not found in model data. Available: %s",
      resp,
      paste(names(df), collapse = ", ")
    ))
  }

  # -- Identify drivers if not given
  if (is.null(drivers)) {
    form_cond <- stats::formula(model)$cond
    tt <- terms(form_cond)
    all_terms <- attr(tt, "term.labels")
    # drop interactions and the timevar itself if present
    all_terms <- all_terms[!grepl(":", all_terms)]
    drivers <- setdiff(all_terms, timevar)
    drivers <- intersect(drivers, names(df)) # keep only those in data
  }

  # helper: weighted trend of yearly means
  year_trend <- function(x_year, x_value) {
    # Basic guards
    if (length(x_year) == 0 || length(x_value) == 0) {
      return(c(est = NA_real_, se = NA_real_, p = NA_real_))
    }
    if (length(x_year) != length(x_value)) {
      # Try to coerce to equal length or bail out
      stop(sprintf(
        "year_trend: lengths differ (year=%d, value=%d).",
        length(x_year),
        length(x_value)
      ))
    }

    d <- data.frame(year = x_year, value = x_value)
    d <- d[is.finite(d$year) & is.finite(d$value), , drop = FALSE]
    if (nrow(d) < 3) return(c(est = NA_real_, se = NA_real_, p = NA_real_))

    # aggregate to yearly mean and weight by counts per year
    d_agg <- aggregate(value ~ year, data = d, FUN = mean)
    d_n <- aggregate(value ~ year, data = d, FUN = length)
    d_agg$n <- d_n$value

    # if still too few points
    if (nrow(d_agg) < 3) return(c(est = NA_real_, se = NA_real_, p = NA_real_))

    fit <- try(lm(value ~ year, data = d_agg, weights = n), silent = TRUE)
    if (inherits(fit, "try-error")) return(c(est = NA, se = NA, p = NA))
    s <- summary(fit)$coefficients
    if (!"year" %in% rownames(s)) return(c(est = NA, se = NA, p = NA))
    c(
      est = s["year", "Estimate"],
      se = s["year", "Std. Error"],
      p = s["year", "Pr(>|t|)"]
    )
  }

  # -- 1) Observed trend
  obs_tr <- year_trend(df[[timevar]], df[[resp]])

  # -- 2) Full-model predicted trend
  pred_full <- try(
    predict(model, type = "response", allow.new.levels = TRUE),
    silent = TRUE
  )
  if (inherits(pred_full, "try-error") || length(pred_full) != nrow(df)) {
    stop("predict(model, type='response') failed or returned incorrect length.")
  }
  df$pred_full <- as.numeric(pred_full)
  full_tr <- year_trend(df[[timevar]], df$pred_full)

  if (plot) {
    obs_agg <- aggregate(
      df[[resp]],
      list(year = df[[timevar]]),
      mean,
      na.rm = TRUE
    )
    pred_agg <- aggregate(
      df$pred_full,
      list(year = df[[timevar]]),
      mean,
      na.rm = TRUE
    )
    plot(
      obs_agg$year,
      obs_agg$x,
      type = "l",
      lwd = 2,
      xlab = timevar,
      ylab = resp,
      main = "Observed vs Predicted (full)"
    )
    lines(pred_agg$year, pred_agg$x, col = "blue", lwd = 2)
    legend(
      "topleft",
      legend = c("Observed", "Predicted (full)"),
      col = c("black", "blue"),
      lwd = 2,
      bty = "n"
    )
  }

  # -- Precompute per-group held (median for numeric, mode for factors)
  held_vals <- list()
  for (dr in drivers) {
    if (is.numeric(df[[dr]])) {
      # median per group
      tmp <- tapply(df[[dr]], df[[group]], function(z) median(z, na.rm = TRUE))
    } else {
      # mode per group
      tmp <- tapply(df[[dr]], df[[group]], function(z) {
        z <- z[!is.na(z)]
        if (length(z) == 0) return(NA)
        ux <- unique(z)
        ux[which.max(tabulate(match(z, ux)))]
      })
    }
    held_vals[[dr]] <- tmp
  }

  # -- 3) Loop over drivers
  out_list <- vector("list", length(drivers))
  n_year_unique <- length(unique(df[[timevar]]))
  df_a <- max(2 * n_year_unique - 4, 1) # avoid negative df

  for (i in seq_along(drivers)) {
    dr <- drivers[i]
    newdat <- df

    # map held values to each row without merge
    hv <- held_vals[[dr]]
    # ensure names of hv are the levels or values of the group column
    if (is.null(names(hv))) {
      stop(sprintf(
        "Held-values for driver '%s' are not named by '%s'.",
        dr,
        group
      ))
    }
    # assign by matching group
    idx <- match(newdat[[group]], names(hv))
    newdat[[dr]] <- hv[idx]

    # re-predict under held scenario
    pred_held <- try(
      predict(
        model,
        newdata = newdat,
        type = "response",
        allow.new.levels = TRUE
      ),
      silent = TRUE
    )
    if (inherits(pred_held, "try-error") || length(pred_held) != nrow(newdat)) {
      warning(sprintf(
        "Prediction failed or mismatched length for driver '%s'; skipping.",
        dr
      ))
      out_list[[i]] <- data.frame(
        driver = dr,
        obs_slope = obs_tr["est"],
        obs_slope_se = obs_tr["se"],
        obs_slope_p = obs_tr["p"],
        full_slope = full_tr["est"],
        full_slope_se = full_tr["se"],
        full_slope_p = full_tr["p"],
        held_slope = NA,
        held_slope_se = NA,
        held_slope_p = NA,
        contribution = NA,
        contribution_se = NA,
        contribution_t = NA,
        contribution_p = NA,
        pred_trend = NA,
        pred_trend_se = NA,
        sensitivity = NA,
        sensitivity_se = NA,
        sensitivity_t = NA,
        sensitivity_p = NA,
        stringsAsFactors = FALSE
      )
      next
    }
    newdat$pred_held <- as.numeric(pred_held)

    # slope (held)
    held_tr <- year_trend(newdat[[timevar]], newdat$pred_held)

    # contribution = full - held
    a <- as.numeric(full_tr["est"] - held_tr["est"])
    se_a <- sqrt((full_tr["se"]^2) + (held_tr["se"]^2))
    t_a <- a / se_a
    p_a <- 1 - pt(abs(t_a), df = df_a)

    # trend of the driver itself
    dr_tr <- year_trend(df[[timevar]], df[[dr]])
    d <- as.numeric(dr_tr["est"])
    se_d <- as.numeric(dr_tr["se"])

    # sensitivity
    if (is.finite(a) && is.finite(d) && abs(d) > .Machine$double.eps) {
      sens <- a / d
      sens_se <- (a / d) * sqrt((se_d / d)^2 + (se_a / a)^2)
      sens_t <- sens / sens_se
      sens_p <- 1 - pt(abs(sens_t), df = df_a)
    } else {
      sens <- sens_se <- sens_t <- sens_p <- NA_real_
    }

    out_list[[i]] <- data.frame(
      driver = dr,
      obs_slope = obs_tr["est"],
      obs_slope_se = obs_tr["se"],
      obs_slope_p = obs_tr["p"],

      full_slope = full_tr["est"],
      full_slope_se = full_tr["se"],
      full_slope_p = full_tr["p"],

      held_slope = held_tr["est"],
      held_slope_se = held_tr["se"],
      held_slope_p = held_tr["p"],

      contribution = a,
      contribution_se = se_a,
      contribution_t = t_a,
      contribution_p = p_a,

      pred_trend = d,
      pred_trend_se = se_d,

      sensitivity = sens,
      sensitivity_se = sens_se,
      sensitivity_t = sens_t,
      sensitivity_p = sens_p,
      stringsAsFactors = FALSE
    )
  }

  res <- do.call(rbind, out_list)
  rownames(res) <- NULL
  return(res)
}


temp_contrib_glmmTMB_simple <- function(
  model,
  drivers = NULL,
  group,
  timevar = "year"
) {
  stopifnot(inherits(model, "glmmTMB"))
  if (missing(group))
    stop("Please provide `group` (panel id, e.g., 'isocode.factor').")

  # --- Get data & response
  df <- model.frame(model)
  form <- stats::formula(model)$cond
  resp <- all.vars(form)[1]

  if (!timevar %in% names(df)) stop("`timevar` not found in model data.")
  if (!group %in% names(df)) stop("`group` not found in model data.")
  df[[timevar]] <- as.numeric(df[[timevar]])
  df[[group]] <- as.factor(df[[group]])

  # --- Drivers (fixed effects without interactions & timevar)
  if (is.null(drivers)) {
    tt <- terms(form)
    trms <- setdiff(attr(tt, "term.labels"), timevar)
    drivers <- trms[!grepl(":", trms)]
    drivers <- intersect(drivers, names(df))
  }

  # --- Small helpers --------------------------------------------------------
  # slope of yearly mean (weighted by counts per year)
  slope_yearly <- function(year, value) {
    ok <- is.finite(year) & is.finite(value)
    if (!any(ok)) return(c(est = NA, se = NA, p = NA))
    d <- data.frame(year = year[ok], value = value[ok])
    if (nrow(d) < 3) return(c(est = NA, se = NA, p = NA))
    agg <- aggregate(value ~ year, data = d, FUN = mean)
    cnt <- aggregate(value ~ year, data = d, FUN = length)
    agg$n <- cnt$value
    if (nrow(agg) < 3) return(c(est = NA, se = NA, p = NA))
    fit <- lm(value ~ year, data = agg, weights = n)
    s <- summary(fit)$coefficients
    if (!"year" %in% rownames(s)) return(c(est = NA, se = NA, p = NA))
    c(
      est = s["year", "Estimate"],
      se = s["year", "Std. Error"],
      p = s["year", "Pr(>|t|)"]
    )
  }

  # per-group "held constant" (median for numeric, mode for factor)
  held_by_group <- function(x, g) {
    tapply(x, g, function(z) {
      z <- z[!is.na(z)]
      if (!length(z)) return(NA)
      if (is.numeric(z)) stats::median(z) else {
        ux <- unique(z)
        ux[which.max(tabulate(match(z, ux)))]
      }
    })
  }

  # predict on response scale (safe)
  predict_resp <- function(mod, newdata) {
    pr <- try(
      predict(
        mod,
        newdata = newdata,
        type = "response",
        allow.new.levels = TRUE
      ),
      silent = TRUE
    )
    if (!inherits(pr, "try-error")) return(as.numeric(pr))
    fam <- try(stats::family(mod), silent = TRUE)
    linkinv <- if (!inherits(fam, "try-error")) fam$linkinv else
      mod$family$linkinv
    eta <- predict(
      mod,
      newdata = newdata,
      type = "link",
      allow.new.levels = TRUE
    )
    as.numeric(linkinv(eta))
  }

  # --- 1) Observed slope
  obs_tr <- slope_yearly(df[[timevar]], df[[resp]])

  # --- 2) Full predicted slope
  df$pred_full <- predict_resp(model, df)
  full_tr <- slope_yearly(df[[timevar]], df$pred_full)

  # --- 3) Loop over drivers
  out <- lapply(drivers, function(dr) {
    # held constant per group
    hv <- held_by_group(df[[dr]], df[[group]])
    idx <- match(df[[group]], names(hv))
    newdat <- df
    newdat[[dr]] <- hv[idx]

    # slope when 'dr' is held constant
    newdat$pred_held <- predict_resp(model, newdat)
    held_tr <- slope_yearly(newdat[[timevar]], newdat$pred_held)

    # contribution (full - held)
    a <- full_tr["est"] - held_tr["est"]
    se_a <- sqrt(full_tr["se"]^2 + held_tr["se"]^2)
    t_a <- a / se_a
    # use normal approx for p (simple & robust here)
    p_a <- 2 * pnorm(-abs(t_a))

    # trend of the driver itself (for sensitivity)
    dr_tr <- slope_yearly(df[[timevar]], df[[dr]])
    d <- dr_tr["est"]
    se_d <- dr_tr["se"]

    # sensitivity = contribution / driver trend
    if (is.finite(a) && is.finite(d) && abs(d) > .Machine$double.eps) {
      sens <- a / d
      sens_se <- abs(sens) * sqrt((se_a / a)^2 + (se_d / d)^2)
      sens_t <- sens / sens_se
      sens_p <- 2 * pnorm(-abs(sens_t))
    } else {
      sens <- sens_se <- sens_t <- sens_p <- NA
    }

    data.frame(
      driver = dr,
      obs_slope = obs_tr["est"],
      obs_slope_se = obs_tr["se"],
      obs_slope_p = obs_tr["p"],
      full_slope = full_tr["est"],
      full_slope_se = full_tr["se"],
      full_slope_p = full_tr["p"],
      held_slope = held_tr["est"],
      held_slope_se = held_tr["se"],
      held_slope_p = held_tr["p"],
      contribution = a,
      contribution_se = se_a,
      contribution_t = t_a,
      contribution_p = p_a,
      pred_trend = d,
      pred_trend_se = se_d,
      sensitivity = sens,
      sensitivity_se = sens_se,
      sensitivity_t = sens_t,
      sensitivity_p = sens_p,
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, out)
  rownames(res) <- NULL

  # OPTIONAL: add "Unknown" contribution (Observed − sum of contributions)
  # unknown <- sum(res$contribution, na.rm = TRUE)
  # res <- rbind(
  #   res,
  #   data.frame(
  #     driver = "Unknown",
  #     obs_slope = obs_tr["est"],
  #     obs_slope_se = obs_tr["se"],
  #     obs_slope_p = obs_tr["p"],
  #     full_slope = full_tr["est"],
  #     full_slope_se = full_tr["se"],
  #     full_slope_p = full_tr["p"],
  #     held_slope = NA,
  #     held_slope_se = NA,
  #     held_slope_p = NA,
  #     contribution = obs_tr["est"] - unknown,
  #     contribution_se = obs_tr["se"],
  #     contribution_t = (obs_tr["est"] - unknown) / obs_tr["se"],
  #     contribution_p = 2 *
  #       pnorm(-abs((obs_tr["est"] - unknown) / obs_tr["se"])),
  #     pred_trend = NA,
  #     pred_trend_se = NA,
  #     sensitivity = NA,
  #     sensitivity_se = NA,
  #     sensitivity_t = NA,
  #     sensitivity_p = NA
  #   )
  # )

  res
}

simple_upgraded_beta_model <- function(
  model,
  drivers = NULL,
  group, # grouping var name in model data, e.g., "isocode.factor"
  timevar = "year", # time variable name (numeric or integer)
  plot = TRUE # optional quick plot of observed vs predicted
) {
  stopifnot(inherits(model, "glmmTMB"))
  if (missing(group))
    stop("Please provide the grouping factor name via `group`.")

  # --- Extract model data
  df <- try(model.frame(model), silent = TRUE)
  if (inherits(df, "try-error") || is.null(df) || nrow(df) == 0) {
    stop(
      "Could not retrieve model.frame(model). Try refitting with model.frame available."
    )
  }
  if (!timevar %in% names(df))
    stop(sprintf("timevar '%s' not in model data.", timevar))
  if (!group %in% names(df))
    stop(sprintf("group '%s' not in model data.", group))
  df[[timevar]] <- as.numeric(df[[timevar]])

  # --- Detect response
  resp <- NULL
  if (requireNamespace("insight", quietly = TRUE)) {
    resp <- insight::find_response(model)[1]
  }
  if (is.null(resp)) {
    form_cond <- stats::formula(model)$cond
    resp <- all.vars(form_cond[[2]])[1]
  }
  if (!resp %in% names(df))
    stop(sprintf("Response '%s' not in model data.", resp))

  # --- Identify drivers (fixed effects) if not supplied
  if (is.null(drivers)) {
    form_cond <- stats::formula(model)$cond
    tt <- terms(form_cond)
    all_terms <- attr(tt, "term.labels")
    all_terms <- all_terms[!grepl(":", all_terms)]
    drivers <- setdiff(all_terms, timevar)
    drivers <- intersect(drivers, names(df))
  }

  # --- Helper: safe logit transform
  logit_safe <- function(p, eps = 1e-6) {
    p <- pmin(pmax(p, eps), 1 - eps)
    qlogis(p)
  }

  # --- Helper: linear trend (weighted by counts per year)
  year_trend <- function(x_year, x_value) {
    if (length(x_year) == 0 || length(x_value) == 0)
      return(c(est = NA_real_, se = NA_real_, p = NA_real_))
    if (length(x_year) != length(x_value))
      stop("year_trend: year and value lengths differ.")

    d <- data.frame(year = as.numeric(x_year), value = as.numeric(x_value))
    d <- d[is.finite(d$year) & is.finite(d$value), , drop = FALSE]
    if (nrow(d) < 3) return(c(est = NA, se = NA, p = NA))

    d_agg <- aggregate(value ~ year, data = d, FUN = mean)
    d_n <- aggregate(value ~ year, data = d, FUN = length)
    d_agg$n <- d_n$value

    if (nrow(d_agg) < 3) return(c(est = NA, se = NA, p = NA))

    fit <- try(lm(value ~ year, data = d_agg, weights = n), silent = TRUE)
    if (inherits(fit, "try-error")) return(c(est = NA, se = NA, p = NA))
    s <- summary(fit)$coefficients
    if (!"year" %in% rownames(s)) return(c(est = NA, se = NA, p = NA))

    c(
      est = s["year", "Estimate"],
      se = s["year", "Std. Error"],
      p = s["year", "Pr(>|t|)"]
    )
  }

  # --- 1) Observed trend (on *logit* scale of Y)
  obs_tr <- year_trend(df[[timevar]], logit_safe(df[[resp]]))

  # --- 2) Full-model predicted trend (on response scale)
  pred_full <- try(
    predict(model, type = "response", allow.new.levels = TRUE),
    silent = TRUE
  )
  if (inherits(pred_full, "try-error") || length(pred_full) != nrow(df)) {
    stop("predict(type='response') failed or wrong length.")
  }
  df$pred_full <- as.numeric(pred_full)
  full_tr <- year_trend(df[[timevar]], df$pred_full)

  # Optional quick plot
  if (plot) {
    obs_agg <- aggregate(
      df[[resp]],
      list(year = df[[timevar]]),
      mean,
      na.rm = TRUE
    )
    pred_agg <- aggregate(
      df$pred_full,
      list(year = df[[timevar]]),
      mean,
      na.rm = TRUE
    )
    plot(
      obs_agg$year,
      obs_agg$x,
      type = "l",
      lwd = 2,
      xlab = timevar,
      ylab = resp,
      main = "Observed vs Predicted (full)"
    )
    lines(pred_agg$year, pred_agg$x, col = "blue", lwd = 2)
    legend(
      "topleft",
      legend = c("Observed", "Predicted (full)"),
      col = c("black", "blue"),
      lwd = 2,
      bty = "n"
    )
  }

  # --- Precompute per-group held values: median (numeric) or mode (factor)
  held_vals <- lapply(drivers, function(dr) {
    if (is.numeric(df[[dr]])) {
      tapply(df[[dr]], df[[group]], function(z) median(z, na.rm = TRUE))
    } else {
      tapply(df[[dr]], df[[group]], function(z) {
        z <- z[!is.na(z)]
        if (length(z) == 0) return(NA)
        ux <- unique(z)
        ux[which.max(tabulate(match(z, ux)))]
      })
    }
  })
  names(held_vals) <- drivers

  # --- 3) Loop over drivers and compute contribution & sensitivity
  out_list <- vector("list", length(drivers))
  df_a <- max(2 * length(unique(df[[timevar]])) - 4, 1) # for t-tests

  for (i in seq_along(drivers)) {
    dr <- drivers[i]
    newdat <- df

    hv <- held_vals[[dr]]
    if (is.null(names(hv))) {
      warning(sprintf("Held values for '%s' had no names; skipping.", dr))
      next
    }
    idx <- match(newdat[[group]], names(hv))
    newdat[[dr]] <- hv[idx]

    pred_held <- try(
      predict(
        model,
        newdata = newdat,
        type = "response",
        allow.new.levels = TRUE
      ),
      silent = TRUE
    )
    if (inherits(pred_held, "try-error") || length(pred_held) != nrow(newdat)) {
      warning(sprintf("Prediction failed for driver '%s'; skipping.", dr))
      next
    }
    newdat$pred_held <- as.numeric(pred_held)

    held_tr <- year_trend(newdat[[timevar]], newdat$pred_held)

    # Contribution = full_slope - held_slope
    a <- as.numeric(full_tr["est"] - held_tr["est"])
    se_a <- sqrt((full_tr["se"]^2) + (held_tr["se"]^2))
    t_a <- a / se_a
    p_a <- 1 - pt(abs(t_a), df = df_a)

    # Driver trend (on *raw* driver scale)
    dr_tr <- year_trend(df[[timevar]], df[[dr]])
    d <- as.numeric(dr_tr["est"])
    se_d <- as.numeric(dr_tr["se"])

    # Sensitivity = contribution / driver_trend
    if (is.finite(a) && is.finite(d) && abs(d) > .Machine$double.eps) {
      sens <- a / d
      sens_se <- (a / d) * sqrt((se_d / d)^2 + (se_a / a)^2)
      sens_t <- sens / sens_se
      sens_p <- 1 - pt(abs(sens_t), df = df_a)
    } else {
      sens <- sens_se <- sens_t <- sens_p <- NA_real_
    }

    out_list[[i]] <- data.frame(
      driver = dr,
      obs_slope = obs_tr["est"], # on logit(Y) scale
      obs_slope_se = obs_tr["se"],
      obs_slope_p = obs_tr["p"],
      full_slope = full_tr["est"], # on response scale
      full_slope_se = full_tr["se"],
      full_slope_p = full_tr["p"],
      held_slope = held_tr["est"],
      held_slope_se = held_tr["se"],
      held_slope_p = held_tr["p"],
      contribution = a,
      contribution_se = se_a,
      contribution_t = t_a,
      contribution_p = p_a,
      pred_trend = d, # trend of the driver
      pred_trend_se = se_d,
      sensitivity = sens,
      sensitivity_se = sens_se,
      sensitivity_t = sens_t,
      sensitivity_p = sens_p,
      stringsAsFactors = FALSE
    )
  }

  res <- do.call(rbind, out_list)
  rownames(res) <- NULL
  res
}
