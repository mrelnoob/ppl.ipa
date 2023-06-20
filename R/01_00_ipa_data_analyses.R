######## *------------------------------------------------------------* ########
####################### PRELIMINARY ANALYSES FOR ppl.ipa #######################
######## *------------------------------------------------------------* ########

# Note that I manually added this project and its files into my Github account (cf. '_devhistory.R' at the
# root of this project) that can be found at: https://github.com/mrelnoob/ppl.ipa

# ---------------------- #
##### 0. Data import #####
# ---------------------- #

##### * 0.1. Import and initial formatting -------------------------------------
# ---------------------------------------------------------------------------- #
### ** 0.1.1. Import ----
# _______________________

library(magrittr)

rmetrics <- readr::read_csv2(file = here::here("data/ipa_urban_metrics.csv"), col_names = TRUE, na = "",
                             col_types = readr::cols(id_ipa = readr::col_factor(),
                                              site_name = readr::col_factor(),
                                              neighbourhood = readr::col_factor(),
                                              urban_type = readr::col_factor(),
                                              urban_type_2 = readr::col_factor(),
                                              possible_outlier = readr::col_factor(),
                                              evenness_class = readr::col_factor(
                                                ordered = TRUE,
                                                levels = c("A", "B", "C", "D")),
                                              dom_sp_1 = readr::col_factor(),
                                              dom_sp_2 = readr::col_factor(),
                                              dom_sp_3 = readr::col_factor(),
                                              lcz = readr::col_factor(),
                                              vmanag = readr::col_factor(
                                                ordered = TRUE,
                                                levels = c("natural_evol", "extensive",
                                                           "medium", "intesive")))) # Note, as I use "read_csv2",
# my decimal separator should be a "," and not a "." (as used by default by most softwares). I thus corrected
# that manually on my CSV file!
rdata <- readr::read_csv2(file = here::here("data/ppl_ipa_data_20192022.csv"), col_names = TRUE, na = "",
                         col_types = readr::cols(id_ipa = readr::col_factor(),
                                                 id_site = readr::col_factor(),
                                                 site_name = readr::col_factor(),
                                                 observator = readr::col_factor(),
                                                 year = readr::col_factor(),
                                                 date = readr::col_factor()))
rtraits <- readr::read_csv2(file = here::here("data/ppl_ipa_species_traits.csv"), col_names = TRUE, na = "",
                            col_select = -comment, col_types = readr::cols(
                              sp_id = readr::col_factor(),
                              order = readr::col_factor(),
                              family = readr::col_factor(),
                              genus = readr::col_factor(),
                              species = readr::col_factor(),
                              nesting_pref = readr::col_factor(),
                              development_mode = readr::col_factor(),
                              trophic_level = readr::col_factor(),
                              trophic_niche = readr::col_factor(),
                              foraging_behaviour = readr::col_factor(),
                              foraging_strata = readr::col_factor(),
                              habitat = readr::col_factor(),
                              hab_density = readr::col_factor(),
                              prim_lifestyle = readr::col_factor(),
                              urban_tolerance = readr::col_factor(),
                              social_behaviour = readr::col_factor(),
                              migratory = readr::col_factor(),
                              iucn_status = readr::col_factor()))



### ** 0.1.2. Data preparation and cleaning ----
# ______________________________________________

## Removing useless columns, converting number of couples into number of contacted individuals, and changing
# NAs by zeros (i.e. the species was not observed):
rdata %>% dplyr::select(-id_site, -site_name, -date) %>%
  dplyr::mutate_if(is.numeric, ~ . * 2) -> rdata
rdata[is.na(rdata)] <- 0

## Removing useless columns, dealing with NAs and other slight changes:
rmetrics %>% dplyr::select(-area, -vmanag) %>%
  dplyr::rename(buffer_radius = rayon) %>%
  dplyr::rename(coord_x = x) %>%
  dplyr::rename(coord_y = y) -> rmetrics
rmetrics[which(is.na(rmetrics$bh_m)), "bh_m"] <- 0
rmetrics[which(is.na(rmetrics$bh_iqr)), "bh_iqr"] <- 0
rmetrics[which(is.na(rmetrics$barea_m)), "barea_m"] <- 0
rmetrics[which(is.na(rmetrics$barea_iqr)), "barea_iqr"] <- 0
rmetrics[which(is.na(rmetrics$rdens)), "rdens"] <- 0
rmetrics[which(is.na(rmetrics$rortho_m)), "rortho_m"] <- 0
# Note that I do not attribute 0 to the NAs of "bconti" as it would not make sense!

rmetrics %>% dplyr::mutate(urban_type = stats::relevel(x = urban_type, ref = 7), # Assigning "urban parks" as
                           # reference level.
                           urban_type_2 = stats::relevel(x = urban_type_2, ref = 8), # Same here.
                           lcz = stats::relevel(x = lcz, ref = 5)) %>% # Assigning "forested areas" as
  # reference level.
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>% # Jitter coordinates a little bit.
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> rmetrics

## Removing useless columns:
rtraits %>% dplyr::select(-maturity_age, -longevity) -> rtraits

## To check the matching of species names among tables:
sum(!rtraits$sp_id %in% colnames(rdata)) # Initially yielded 2 errors (typos) that I manually corrected in the
# original CSV files to be cleaner (but less reproducible, shame on me :O)!



### ** 0.1.3. Choice of the buffer radius size ----
# _________________________________________________
# For now, we will only work with the data extracted from the 150m radius buffers, so we select them:
rmetrics %>% dplyr::filter(buffer_radius == 150) -> ipa_metrics





########################## ************************************************* ###############################
# --------------------------------------------------------- #
##### 1. Specific and functional diversity computations #####
# --------------------------------------------------------- #

##### * 1.1. Birds specific diversity ------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.1.1. Species richness and abundances ----
# ________________________________________________

ipa_data <- rdata
ipa_data$sp_richness <- apply(rdata[,4:ncol(rdata)] > 0, 1, sum) # Total number of bird species per site.
ipa_data$sp_abund <- apply(rdata[,4:ncol(rdata)], 1, sum) # Total abundance of birds per site.



### ** 1.1.2. Species diversity indices ----
# __________________________________________

### *** 2.1.2.1. Per site diversity indices ----
ipa_data$sp_shannon <- vegan::diversity(x = rdata[,4:ncol(rdata)], index = "shannon") # Shannon-Wiever index.
ipa_data$sp_simpson <- vegan::diversity(x = rdata[,4:ncol(rdata)], index = "invsimpson") # Inverse Simpson index.
ipa_data$sp_evenness <- (ipa_data$sp_shannon/log(ipa_data$sp_richness)) # Pielou's evenness index.
# We preferred the Shannon index over the Simpson one because we wanted to give importance to rare species, but
# we calculate both anyway, notably because Pielou's J is known to be sensitive to sample size (likely not a
# problem here) and to rare species.
pairs(ipa_data[,58:62], pch ="+", col = "blue")


### *** 2.1.2.1. Per urban form diversity levels ----
alpha_uf <- with(ipa_metrics, tapply(vegan::specnumber(rdata[,4:ncol(rdata)]), urban_type_2, mean))
gamma_uf <- with(ipa_metrics, vegan::specnumber(rdata[,4:ncol(rdata)], urban_type_2))
beta_uf <- gamma_uf/alpha_uf - 1
# NOTE: the above computations are from the {vegan} package help. The definition of beta diversity might differ
# from that of others.





##### * 1.2. Birds functional diversity ----------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.2.1. Functional groups richness and abundances ----
# __________________________________________________________

### *** 1.2.1.1. Trophic guild richness and abundances ----

j <- 1

ipa_data %>% dplyr::select(-sp_richness, -sp_abund, -sp_shannon, -sp_simpson, -sp_evenness) %>%
  as.data.frame() -> birds
nb_gr <- length(unique(rtraits$trophic_level))

birds_subsets <- NULL


for (j in 1:nb_gr){
  sp_list <- as.character(as.matrix(rtraits[which(rtraits$trophic_level == levels(rtraits$trophic_level)[j]),
                                            "sp_id"]))


  birds_subsets[[j]] <- birds[, which(colnames(birds) %in% sp_list)]

}

carnibirds <- birds_subsets[[1]]





summary(rtraits)
summary(ipa_metrics)
summary(ipa_data)
# STOP!



### ** 1.2.1. Functional diversity indices ----
# _____________________________________________

# airpoumpoum::super_distriplot(MYVARIABLES = ipa_data[,58:62], GROUPS = ipa_metrics$evenness_class, breaks = 5)














########################## ************************************************* ###############################
# ------------------------------------------ #
##### 2. Exploratory data analyses (EDA) #####
# ------------------------------------------ #

##### * 1.1. Outliers detection ------------------------------------------------
# ---------------------------------------------------------------------------- #
.pardefault <- par()

ppl.tits::uni.boxplots(srich_arthro)
ppl.tits::uni.dotplots(srich_arthro)
ppl.tits::uni.boxplots(srich_polli)
ppl.tits::uni.dotplots(srich_polli)
# It appears that:
#  - There are some extreme values for the B_metrics.
#  - There are values above 1 for the distance to city-centre, but it's normal.



##### * 1.2. Distribution, skewness and kurtosis -------------------------------
# ---------------------------------------------------------------------------- #
ppl.tits::uni.histograms(srich_arthro)
ppl.tits::uni.histograms(srich_polli)

arthro.num <- srich_arthro[, sapply(srich_arthro, is.numeric)]
tab <- data.frame(moments::skewness(x = arthro.num), moments::kurtosis(x = arthro.num)-3)
arthro_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
polli.num <- srich_polli[, sapply(srich_polli, is.numeric)]
tab <- data.frame(moments::skewness(x = polli.num), moments::kurtosis(x = polli.num)-3)
polli_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# It appears that:
#  - There is excess skewness for BH, BAM, BASTD, and especially BHW.
#  - There is highly excessive kurtosis for BN, BD, and especially BH, BAM, BASTD, and BHW!
rm(tab)



##### * 1.3. Bivariate relationships -------------------------------------------
# ---------------------------------------------------------------------------- #
# To compute the correlation matrix:
res.cor.spipoll <- round(stats::cor(arthro.num, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.spipoll <- ggcorrplot::cor_pmat(x = arthro.num, method = "spearman")

spipoll.corplot <- ggcorrplot::ggcorrplot(res.cor.spipoll, type = "upper",
                                          outline.col = "white",
                                          ggtheme = ggplot2::theme_gray,
                                          colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.spipoll,
                                          insig = "blank")
# We can see that:
# - Taxonomic COUNTS are quite strongly positively correlated among themselves but only weakly with the other
#   other variables.
# - The DISTANCE variable is negatively correlated with BD, BH, BAM, BASTD, BHW, OSO_F1 and UA_F1!
# - BN, BD and BH are strongly positively correlated with each other and with BHW, OSO_F1 and UA_F1! They are
#   also negatively correlated with BW.
# - BW is also negatively correlated with BF, BHW and OSO_F1!
# - UA_F1 is also positively correlated with BHW and OSO_F1!
## I suggest removing BD and BASTD and perhaps BN and BHW.

spipoll.pairplot <- GGally::ggpairs(arthro.num)
spipoll.pairplot <- GGally::ggpairs(polli.num)
# We find globally the same patterns:
# - BN and BF, BF and BW, BF and BC, or UA_F1 and OSO_F1 indeed seem highly collinear.
# - It is also true for BW but to a lesser extent and in a curvilinear way.
# - The BAM metric shows diverging patterns (e.g. with BN and BH), BHW also but to a lesser extent!
# - BF and BHW extreme values may be multivariate outliers! For BHW, they come from Paris so they're likely
#   not true outliers but are characteristic from a specific urban form.
# - Many relationships are likely highly collinear on the log scale: e.g. BN, BH and BHW with UA_F1 or OSO_F1!
## I suggest removing BF, BW and the F1 factors.



##### * 1.4. Final formatting --------------------------------------------------
# ---------------------------------------------------------------------------- #

## As suggested, I remove BD, BASTD, BF, BW and the F1 factors (or at least, I won't use them):
srich_arthro %>% dplyr::mutate(log_bn = log10(bn),
                               sqrt_bf = sqrt(bf),
                               sqrt_bh = sqrt(bh),
                               log_bam = log10(bam),
                               log_bhw = log10(bhw)) %>%
  dplyr::relocate(log_bn, .after = bn) %>%
  dplyr::relocate(sqrt_bf, .after = log_bn) %>%
  dplyr::relocate(sqrt_bh, .after = bh) %>%
  dplyr::relocate(log_bam, .after = bam) %>%
  dplyr::relocate(log_bhw, .after = bhw) %>%
  dplyr::rename(dist_centre = part_dist_centre,
                city = ville) %>%
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>%
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> srich_arthro2

srich_polli %>% dplyr::mutate(log_bn = log10(bn),
                              sqrt_bf = sqrt(bf),
                              sqrt_bh = sqrt(bh),
                              log_bam = log10(bam),
                              log_bhw = log10(bhw)) %>%
  dplyr::relocate(log_bn, .after = bn) %>%
  dplyr::relocate(sqrt_bf, .after = log_bn) %>%
  dplyr::relocate(sqrt_bh, .after = bh) %>%
  dplyr::relocate(log_bam, .after = bam) %>%
  dplyr::relocate(log_bhw, .after = bhw) %>%
  dplyr::rename(dist_centre = part_dist_centre,
                city = ville) %>%
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>%
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> srich_polli2

summary(srich_arthro2)
summary(srich_polli2)
colnames(srich_arthro2)
colnames(srich_polli2)





########################## ************************************************* ###############################
# --------------------------------------------------- #
##### 2. Modelling the BIRDYDYDYDY family richness #####
# --------------------------------------------------- #

##### * 2.1. Genus richness models ---------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 2.1.1. Poisson or negative binomial GL(M)M ----
# ____________________________________________________

srich_arthro2 %>% dplyr::filter(genus_richness > 0) %>%
  dplyr::mutate(longitude = scale(coord_x),
                latitude = scale(coord_y),
                c.log_bn = scale(log_bn, scale = FALSE),
                c.sqrt_bf = scale(sqrt_bf, scale = FALSE),
                c.bd = scale(bd, scale = FALSE),
                c.bh = scale(bh, scale = FALSE),
                c.sqrt_bh = scale(sqrt_bh, scale = FALSE),
                c.bc = scale(bc, scale = FALSE),
                c.bf = scale(bf, scale = FALSE),
                c.bam = scale(bam, scale = FALSE),
                c.log_bam = scale(log_bam, scale = FALSE),
                c.bw = scale(bw, scale = FALSE),
                c.bhw = scale(bhw, scale = FALSE),
                c.log_bhw = scale(log_bhw, scale = FALSE))-> srich_arthro3 # Rescaling to avoid convergence issues.
summary(srich_arthro3)
ppl.tits::uni.dotplots(srich_arthro3)


## Fitting a regular Poisson regression:
sppGNa_glm1 <- stats::glm(genus_richness ~ dist_centre + log_bn + sqrt_bh + bc + log_bam + log_bhw +
                            oso_f2 + oso_f3 + oso_f4 + flower_sp + longitude + latitude,
                          data = srich_arthro3, family = "poisson")

## Fitting a regular Poisson GLMM:
sppGNa_glmm1 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + sqrt_bh + bc + log_bam + log_bhw +
                                   oso_f2 + oso_f3 + oso_f4 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = "poisson")
summary(sppGNa_glm1) # AIC = 11791
summary(sppGNa_glmm1) # AIC = 11555.2 (or 11510.5 if "flower_sp" is used as a fixed effect).
# The use of CITY as a random effect (RE) seems warranted by the data!
## UPDATE: diagnostics turned out very problematic (overdispersion, multicollinearity, spatial
# autocorrelation etc.). I will thus try with a negative binomial distribution and altered model specifications.

## Fitting negative binomial GLMMs:
sppGNa_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm2) # AIC = 10887.1
sppGNa_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm3) # AIC = 10886.1
# So the use of a negative binomial (NB) model seems to help the model and so does the interaction.





### ** 2.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.1.2.1. Residuals extraction, autocorrelation and collinearity ----
par(.pardefault)

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = sppGNa_glmm2, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Significant deviation and outliers detected!
# DHARMa::testOutliers(simu.resid, type = 'bootstrap', nBoot = 500) # Long.
DHARMa::outliers(simu.resid) # Six outliers detected!
srich_arthro3[c(510),] # Not sure why...

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = srich_arthro3$coord_x, y = srich_arthro3$coord_y, plot = TRUE) # Nope!!!
performance::check_autocorrelation(sppGNa_glmm2) # Ok.
performance::check_collinearity(sppGNa_glmm2) # Ok-ish.
stats::vcov(sppGNa_glmm2) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$flower_sp)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$city)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$dist_centre)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$log_bn)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$bf)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$sqrt_bh)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$bc)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$log_bam)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$bw)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$log_bhw)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f1)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f2)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f3)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f4)
# Nothing's fine!!!



### *** 2.1.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
AER::dispersiontest(object = sppGNa_glm1, alternative = c("greater")) # Significant overdispersion!
DHARMa::testDispersion(simu.resid, alternative = "greater") # Almost significant overdispersion.

# ## Theoretical count distribution:
# theo_count <- COMPoissonReg::rcmp(n = nrow(srich_arthro3), lambda = mean(srich_arthro3$clutch_size), nu = 1.05) # The 'nu'
# # parameter should be chosen by trial-and-errors.
# tc_df <- data.frame(theo_count)
#
# ggplot2::ggplot(srich_arthro3, ggplot2::aes(clutch_size)) +
#   ggplot2::geom_bar(fill = "#1E90FF") +
#   ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# # This plot suggests that clutch_size could be following a COM-Poisson distribution of parameter nu~1.1!
#
# ## Distribution of the predicted counts:
# pred_counts <- stats::predict(object = sppGNa_glmm2, type = "response") # Extract the predicted counts.
# par(mfrow= c(1,2))
# hist(pred_counts, main = "Predicted counts", xlab = "Number of laid eggs")
# hist(srich_arthro3$clutch_size, main = "Observed counts", xlab = "Number of laid eggs") # The models' predictions
# # are very similar and relatively acceptable (although too narrow).
#
#
#
# ### *** 2.1.2.3. Linearity ----
# # For the sake of further exploration, I also plot variants of our predictors:
# srich_arthro3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
#                          log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
#                          log_F_metric_d2b1,
#                          Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
#                          urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
#                          light_pollution, noise_m,
#                          cumdd_30, laying_day, min_t_before) -> mydata
# predictors <- colnames(mydata)
# # Bind log(Y) and tidying the data for plot (ggplot2, so long format):
# mydata <- mydata %>%
#   dplyr::mutate(log_y = log(srich_arthro3$clutch_size)) %>%
#   tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# # Create scatterplot
# ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
#   ggplot2::geom_point(size = 0.5, alpha = 0.5) +
#   ggplot2::geom_smooth(method = "loess") +
#   ggplot2::theme_bw() +
#   ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected.
#
#
#
# ### *** 2.1.2.4. Model goodness-of-fit (GOF) and performances ----
# # GOF test of Pearson's Chi2 residuals:
# dat.resid <- sum(stats::resid(sppGNa_glmm2, type = "pearson")^2)
# 1 - stats::pchisq(dat.resid, stats::df.residual(sppGNa_glmm2)) # p = 0.83, indicating that there is no
# # significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely complicated
# # topic and interpretations are not straightforward.
#
# # Computing a pseudo-R2:
# performance::r2_nakagawa(sppGNa_glmm2) # [Additive model]: Marg_R2_glmm = 0.1; Cond_R2_glmm = 0.11.
# # Does not work if we tune the dispersion parameter of the COM-Poisson model.
#
# ## Likelihood-based evaluation of effects inclusion:
# # For the "site" random-effects (RE):
# sppGNa_glmm2ac <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
#                                       urban_intensity + manag_intensity +
#                                       light_pollution + noise_m + traffic +
#                                       cumdd_30 + laying_day + year + (1|id_nestbox) + (1|site),
#                                     data = srich_arthro3, family = glmmTMB::compois(link = "log"),
#                                     dispformula = ~1) # Rather long to fit (~3-4 min)!
# summary(sppGNa_glmm2ac) # AIC = 1390.6.
# # The non-mixed model gives AIC = 1391.7, so similar to the mixed-model (AIC = 1391.3) with only
# # "id_nestbox" as RE. The one with both RE gives AIC = 1390.6., so it seems like the use of a mixed model
# # is not truly supported by the data.
#
# ## For the whole model:
# ttCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ 1 + (1|id_nestbox),
#                                   data = srich_arthro3, family = glmmTMB::compois(link = "log"),
#                                   dispformula = ~1)
# res.LRT_null <- stats::anova(object = ttCy_comglmm0, sppGNa_glmm2, test = "LRT")
# # The test is significant, confirming that the model is useful to explain the data.
#
#
#
# ### *** 2.1.2.5. Posterior predictive simulations ----
# # Predicted counts:
# par(.pardefault)
# obsprop <- prop.table(table(srich_arthro3$clutch_size))
# sims <- stats::simulate(sppGNa_glmm2, nsim = 1000)
# nsim4 <- colSums(sims == 4) # Number of fours (min obs value)
# par(las=4,bty="l")
# plot(pt <- prop.table(table(nsim4)),
#      ylab="Probability", xlab="Number of fours (true == 1)")
# (obs4 <- sum(srich_arthro3$clutch_size == 4))
# points(obs4, 0.002, col="red", pch=16, cex=2) # See y values in obsprop!
#
# nsim9 <- colSums(sims == 9) # Number of nines (modal obs value).
# par(las=1,bty="l")
# plot(pt <- prop.table(table(nsim9)),
#      ylab="Probability", xlab="Number of nines (true == 86)")
# (obs9 <- sum(srich_arthro3$clutch_size == 9))
# points(obs9, 0.22, col="red", pch=16, cex=2)
#
# nsim14 <- colSums(sims == 14) # Number of fourteens (max obs value).
# par(las=1,bty="l")
# plot(pt <- prop.table(table(nsim14)),
#      ylab="Probability", xlab="Number of fourteens (true == 5)")
# (obs14 <- sum(srich_arthro3$clutch_size == 14))
# points(obs14, 0.013, col="red", pch=16, cex=2)
# # These three examples confirm that the model tends to overpredict a bit.





### ** 2.1.3. Conclusions ----
# ____________________________

# There are strong GENUS richness variations across regions.
# Methodologically challenging to account for many urban metrics at the same time as they are highly correlated
# (which causes multicollinearity issues).
# This model works relatively fine:
sppGNa_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm2) # AIC = 10887.1
# It shows:
#   - A fairly strong negative effect of building patches mean area!
#   - A strong positive effect of building gaps proportion!
#   - A weak negative effect of UA_F3!
sppGNa_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm3) # AIC = 10886.1

sjPlot::plot_model(model = sppGNa_glmm3, type = "int", terms = c("c.log_bn", "c.bc", "c.log_bam"),
                   mdrt.values = "quart", # Can also be "minmax" or otherwise.
                   title = "Marginal effects of the three-way interaction: BN * BC * BAM
                   Note that metrics are log-transformed and mean-centred",
                   legend.title = "Building contiguity (BC)",
                   axis.title = c("Building numbers (BN)", "Predicted number of arthropod genera"),
                   show.p = TRUE, show.values = TRUE)
# Interesting, but very high variability, suggesting that some variable combinations are likely rare.







########### *-----------------------------------------------------* ############
############################ Main Git commits ##################################
# ---------------------------------------------------------------------------- #
usethis::use_git(message = ":boom: First generated results!")
usethis::use_git(message = ":metal: Created a new function")
usethis::use_git(message = ":zap: Ignoring something")
usethis::use_git(message = ":pencil: Documented a function or wrote something")
usethis::use_git(message = ":hammer: Ongoing programming!")
usethis::use_git(message = ":white_check_mark: Updated the {target} pipeline")
usethis::use_git(message = ":x: Problem detected!")
#system("git push") # Or using a CLI!
# Don't forget to push your commits once you're sure you made no mistakes.
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
########### *-----------------------------------------------------* ############
