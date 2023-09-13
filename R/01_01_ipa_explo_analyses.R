############################ ************************************************* ###############################
# ------------------------------------------ #
##### 2. Exploratory data analyses (EDA) #####
# ------------------------------------------ #

# summary(wtraits)
# summary(ipa_metrics)
# summary(ipa_data)
#
# # First, let's reduce the dataset:
# wdata <- cbind(ipa_data[,-c(4:57)],
#                ipa_metrics[, c(3:29,37,48,50,64,66)])
# wdata %>% dplyr::select(-neighbourhood) -> wdata
# summary(wdata)
#
#
# # *--------------------------------* #####
# # ---------------------------------------------------------------------------- #
# ##### * 2.1. Outliers detection ------------------------------------------------
# # ---------------------------------------------------------------------------- #
#
# ppl.tits::uni.boxplots(wdata)
# ppl.tits::uni.dotplots(wdata)
# # It appears that:
# #  - There are some extreme values for the various abundances, or the Ff metric.
# #  - Some distributions might be problematic, e.g.: the abundance variables, carni_simpson, bh_m, barea_m.
#
#
#
# # *----------------------------------------------------------------------* #####
# # ---------------------------------------------------------------------------- #
# ##### * 2.2. Distribution, skewness and kurtosis -------------------------------
# # ---------------------------------------------------------------------------- #
# ppl.tits::uni.histograms(wdata)
#
# wdata.num <- wdata[, sapply(wdata, is.numeric)]
# tab <- data.frame(moments::skewness(x = wdata.num), moments::kurtosis(x = wdata.num)-3)
# wdata_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# # It appears that:
# #  - There is excess skewness for sp_abund, carni_abund, herbi_abund, barea_m, bhgaps, fprop.
# #  - There is highly excessive kurtosis for sp_abund, sp_evenness, carni_abund, herbi_abund, bh_m, bh_iqr,
# #    barea_m, rdens, fprop, ff_d1b1_euc and cff_d1b1_euc, with some extremely high values!
# rm(tab)
#
#
#
# # *--------------------------------------* #####
# # ---------------------------------------------------------------------------- #
# ##### * 2.3. Bivariate relationships -------------------------------------------
# # ---------------------------------------------------------------------------- #
#
# # To exclude the response variables:
# wdata.num <- wdata.num[,-c(1:14)]
# # To compute the correlation matrix:
# res.cor.ipa <- round(stats::cor(wdata.num, use = "complete.obs", method = "spearman"), 2)
# # To compute a matrix of correlation p-values:
# res.pcor.ipa <- ggcorrplot::cor_pmat(x = wdata.num, method = "spearman")
#
# ipa.corplot <- ggcorrplot::ggcorrplot(res.cor.ipa, type = "upper",
#                                           outline.col = "white",
#                                           ggtheme = ggplot2::theme_gray,
#                                           colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.ipa,
#                                           insig = "blank")
# # We can see that:
# # - Most metrics are quite strongly correlated among each other.
# # - The proportion of herbaceous areas is negatively correlated with the proportion of buildings or their
# #   height, but it is far less true for the proportion of woody areas!
# # - The crossing orthogonality metric is not really correlated with anything!
# # - The F metrics seem like good candidates to approximate herbaceous/woody proportions as well as
# #   connectivity!
# # - The bcount and bfreq metrics could be interesting covariates!
#
# ipa.pairplot <- GGally::ggpairs(wdata.num)
# # We find globally the same patterns, but also:
# # - Interestingly, there seems to be a negative relationship between woody_richness and crossing density!
# # - Bcount and Bfreq seem quite interchangeable.
#
#
#
# # *----------------------------------------------------------------------* #####
# # ---------------------------------------------------------------------------- #
# ##### * 2.4. Homoscedasticity and conditional distributions --------------------
# # ---------------------------------------------------------------------------- #
#
# cond.boxplots <- function(dataset, Y, Xs, outlier, N = 6,
#                           MAR = c(2.2,2.1,0.5,1.7), CEX.LAB = 0.9, FONT.LAB = 2, BTY = "n", FG = "gray35",
#                           COL.AXIS = "gray35", COL.LAB = "gray20", CEX.PAR = 0.8, TCL = -0.3,
#                           MGP = c(1.2, 0.4, 0.2), OMA = c(1, 0, 0, 0),
#                           TYPE = "n", BORDER = "moccasin", COL = "gray50", LTY = 1, STAPLEWEX = 0,
#                           WHISKLWD = 2, BOXWEX = 0.5, BOXLWD = 0.1, MEDLWD = 2.6, PCH = 19, ...){
#   mydata <- as.data.frame(dataset)
#
#   if(!is.numeric(mydata[,Y])){
#     stop("The Y variable must be numeric! Please check the class of your columns before proceeding.")
#   }
#   if(!any(sapply(mydata, is.numeric)) && !any(sapply(mydata, is.factor))){
#     stop("Input dataset should only contain numeric or factor variables!  Please check the class of your
#          columns before proceeding.")
#   }
#   if(missing(Y)){
#     stop("Please specify the Y and X(s) variables to be used and respect the required format. See
#          ?cond.boxplots for details.")
#   }
#
#   nam <- names(mydata)
#   ncol.data <- ncol(mydata[,Xs])
#   ncol.adjust <- ceiling(x = ncol.data/4) # Round to the next integer (e.g. ceiling(x = 7.12) returns 8)!
#   minX <- min(Xs)
#   maxX <- max(Xs)
#   yyy <- as.matrix(mydata[,Y])
#
#   graphics::par(mfrow= c(ncol.adjust,4), mar=MAR, cex.lab=CEX.LAB, font.lab=FONT.LAB, bty=BTY, fg=FG,
#                 col.axis=COL.AXIS, col.lab=COL.LAB, cex=CEX.PAR, tcl=TCL, mgp=MGP, oma=OMA)
#   for (i in c(minX:maxX)) {
#
#     if (is.numeric(mydata[,i])) {
#       fields::bplot.xy(y = mydata[,Y], x = mydata[,i], outlier=outlier, N=N,
#                        xlab=(nam[i]), type=TYPE, border=BORDER, col=COL,lty=LTY, staplewex=STAPLEWEX,
#                        whisklwd=WHISKLWD, boxwex=BOXWEX, boxlwd=BOXLWD, medlwd=MEDLWD, pch=PCH, cex=0.7, ...)
#     }else if (is.factor(mydata[,i])) {
#       xxx <- as.matrix(mydata[,i])
#       graphics::boxplot(yyy~xxx, outline=outlier, ylab="",
#                         xlab=(nam[i]), type=TYPE, border=BORDER, col=COL, lty=LTY, staplewex=STAPLEWEX,
#                         whisklwd=WHISKLWD, boxwex=BOXWEX, boxlwd=BOXLWD,medlwd=MEDLWD, pch=PCH, cex=0.7, ...)
#     }
#   }
# }
#
# wdata %>% dplyr::filter(sp_abund < 150) -> wdata2 # Removing the outlier!
# # For the 5 specific diversity metrics:
# cond.boxplots(dataset = wdata2, Y = 4, Xs = 19:20, outlier = TRUE) # Ok-ish?
# stats::bartlett.test(sp_richness~urban_type_2, data = wdata2) # Ok
# cond.boxplots(dataset = wdata2, Y = 5, Xs = 19:20, outlier = TRUE) # Ok-ish?
# stats::bartlett.test(sp_abund~urban_type_2, data = wdata2) # Nope!
# cond.boxplots(dataset = wdata2, Y = 6, Xs = 19:20, outlier = TRUE) # Ok
# stats::bartlett.test(sp_shannon~urban_type_2, data = wdata2) # Ok
# cond.boxplots(dataset = wdata2, Y = 7, Xs = 19:20, outlier = TRUE) # Ok
# stats::bartlett.test(sp_simpson~urban_type_2, data = wdata2) # Ok
# cond.boxplots(dataset = wdata2, Y = 8, Xs = 19:20, outlier = TRUE) # Nope!
# stats::bartlett.test(sp_evenness~urban_type_2, data = wdata2) # Definitely not!
# # For the carnivore diversity metrics:
# cond.boxplots(dataset = wdata2, Y = 9, Xs = 19:20, outlier = TRUE) # Ok-ish?
# stats::bartlett.test(carni_richness~urban_type_2, data = wdata2) # Definitely not!
# cond.boxplots(dataset = wdata2, Y = 10, Xs = 19:20, outlier = TRUE) # Nope!
# stats::bartlett.test(carni_abund~urban_type_2, data = wdata2) # Definitely not!
# # For the herbivore diversity metrics:
# cond.boxplots(dataset = wdata2, Y = 12, Xs = 19:20, outlier = TRUE) # Ok
# stats::bartlett.test(herbi_richness~urban_type_2, data = wdata2) # Ok
# cond.boxplots(dataset = wdata2, Y = 13, Xs = 19:20, outlier = TRUE) # Ok-ish?
# stats::bartlett.test(herbi_abund~urban_type_2, data = wdata2) # Definitely not!
# # For the omnivore diversity metrics:
# cond.boxplots(dataset = wdata2, Y = 15, Xs = 19:20, outlier = TRUE) # Ok
# stats::bartlett.test(omni_richness~urban_type_2, data = wdata2) # Ok
# cond.boxplots(dataset = wdata2, Y = 16, Xs = 19:20, outlier = TRUE) # Ok-ish
# stats::bartlett.test(omni_abund~urban_type_2, data = wdata2) # Ok-ish
#
#
#
# # *-----------------------------* #####
# # ---------------------------------------------------------------------------- #
# ##### * 2.5. Final formatting --------------------------------------------------
# # ---------------------------------------------------------------------------- #
#
# ## As suggested, I remove BD, BASTD, BF, BW and the F1 factors (or at least, I won't use them):
# wdata2 %>% dplyr::mutate(log_bcount = log10(bcount+1),
#                                log_fh = log10(fh_d1b1_euc),
#                                log_ff = log10(ff_d1b1_euc+1)) -> wdata3
#
# summary(wdata3)
# ppl.tits::uni.dotplots(wdata3)





############################ ************************************************* ###############################
# ----------------------------------------------- #
##### 3. Modelling the bird species diversity #####
# ----------------------------------------------- #
# *------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 3.1. Species richness models -------------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 3.1.1. Poisson or negative binomial GL(M)M ----
# ______________________________________________________

# colnames(wdata3)
#
# ## Fitting a regular Poisson regression:
# ipaSR_glm1 <- stats::glm(sp_richness ~ urban_type_2 + bfreq + log_fh + log_ff,
#                           data = wdata3, family = "poisson")
#
# summary(ipaSR_glm1) # AIC = 644.01
# ## UPDATE: diagnostics turned out quite problematic (underdispersion, potential multicollinearity, and some
# # quantile deviations). I will thus try with a COM-Poisson distribution.
#
# ## Fitting a COM-Poisson regression:
# ipaSR_comglm1 <- glmmTMB::glmmTMB(sp_richness ~ urban_type_2 + bfreq + log_fh + log_ff,
#                                  data = wdata3, family = glmmTMB::compois(link = "log"),
#                                  dispformula = ~1) # Intercept only 'nu' (default).
# summary(ipaSR_comglm1) # AIC = 625.7
# ## UPDATE: as diagnistics are globally fine for this model, we can try a post-hoc test:
# ttt <- dunn.test::dunn.test(x = wdata3$sp_richness, g = wdata3$urban_type_2, method = "holm") # Ok, but does
# # not account for the covariates!
# # stats::TukeyHSD(x = ipaSR_comglm1) # Does not work for {glmmTMB} models!
#
# #AFINIR§§§§§
# #AFINIR§§§§§
# #AFINIR§§§§§
# #AFINIR§§§§§
# #AFINIR§§§§§
# #AFINIR§§§§§
# #AFINIR§§§§§
# #AFINIR§§§§§
# #AFINIR§§§§§
#
# contrasts <- c(
#   "urban_park - industrial_commercial = 0",
#   "industrial_commercial - residential_buildings = 0",
#   "urban_park - intersticial_zones = 0"
#   )
# ttt2 <- multcomp::glht(model = ipaSR_comglm1, linfct = multcomp::mcp(urban_type_2 = contrasts))
# summary(ttt2)
#
#
#
#
# # ## Fitting negative binomial GLMMs:
# # ipaSR_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
# #                                    ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
# #                                  data = wdata3, family = glmmTMB::nbinom2(link = "log"))
# # summary(ipaSR_glmm2) # AIC = 10887.1
# # ipaSR_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
# #                                    ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
# #                                  data = wdata3, family = glmmTMB::nbinom2(link = "log"))
# # summary(ipaSR_glmm3) # AIC = 10886.1
# # # So the use of a negative binomial (NB) model seems to help the model and so does the interaction.
#
#
#
#
#
# ##### ** 3.1.2. Diagnostics and assumption checks ----
# # ____________________________________________________
#
# ##### *** 3.1.2.1. Residuals extraction, autocorrelation and collinearity ----
# par(.pardefault)
#
# ## Simulation-based scaled residuals computation ({DHARMa} method):
# simu.resid <- DHARMa::simulateResiduals(fittedModel = ipaSR_comglm1, n = 1000, re.form = NULL) # The
# # 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# # for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
# plot(simu.resid) # Significant deviation and outliers detected!
# # DHARMa::testOutliers(simu.resid, type = 'bootstrap', nBoot = 500) # Long.
# DHARMa::outliers(simu.resid) # Six outliers detected!
# wdata3[c(79),] # I think I know why...
#
# ## Autocorrelation and collinearity:
# DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
#                                    x = wdata3$coord_x, y = wdata3$coord_y, plot = TRUE) # Ok.
# performance::check_autocorrelation(ipaSR_comglm1) # Ok.
# performance::check_collinearity(ipaSR_comglm1) # High VIF for "urban_type_2" and high correlation for "bfreq"!
# stats::vcov(ipaSR_comglm1) # Ok.
#
# ## Heteroscedasticity and possible model misspecifications:
# par(.pardefault)
# DHARMa::plotResiduals(simu.resid, form = wdata3$urban_type_2)
# DHARMa::plotResiduals(simu.resid, form = wdata3$species_richness) # Not ok (missing covariate)?
# DHARMa::plotResiduals(simu.resid, form = wdata3$bcount)
# DHARMa::plotResiduals(simu.resid, form = wdata3$log_bcount)
# DHARMa::plotResiduals(simu.resid, form = wdata3$bfreq)
# DHARMa::plotResiduals(simu.resid, form = wdata3$log_fh)
# DHARMa::plotResiduals(simu.resid, form = wdata3$log_ff)
# # Some deviations!
#
#
#
# ##### *** 3.1.2.2. Distribution (family, ZI, dispersion) ----
# ## Assessing over or under-dispersion:
# AER::dispersiontest(object = ipaSR_comglm1) # Underdispersion?
# DHARMa::testDispersion(simu.resid, alternative = "less") # Significant underdispersion.
#
# # ## Theoretical count distribution:
# # theo_count <- COMPoissonReg::rcmp(n = nrow(wdata3), lambda = mean(wdata3$clutch_size), nu = 1.05)
# # The 'nu'
# # # parameter should be chosen by trial-and-errors.
# # tc_df <- data.frame(theo_count)
# #
# # ggplot2::ggplot(wdata3, ggplot2::aes(clutch_size)) +
# #   ggplot2::geom_bar(fill = "#1E90FF") +
# #   ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
# #   ggplot2::theme_classic() +
# #   ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# # # This plot suggests that clutch_size could be following a COM-Poisson distribution of parameter nu~1.1!
# #
# # ## Distribution of the predicted counts:
# # pred_counts <- stats::predict(object = ipaSR_glmm2, type = "response") # Extract the predicted counts.
# # par(mfrow= c(1,2))
# # hist(pred_counts, main = "Predicted counts", xlab = "Number of laid eggs")
# # hist(wdata3$clutch_size, main = "Observed counts", xlab = "Number of laid eggs") # The models' predictions
# # # are very similar and relatively acceptable (although too narrow).
# #
# #
# #
# # ##### *** 3.1.2.3. Linearity ----
# # # For the sake of further exploration, I also plot variants of our predictors:
# # wdata3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
# #                          log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
# #                          log_F_metric_d2b1,
# #                          Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
# #                          urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
# #                          light_pollution, noise_m,
# #                          cumdd_30, laying_day, min_t_before) -> mydata
# # predictors <- colnames(mydata)
# # # Bind log(Y) and tidying the data for plot (ggplot2, so long format):
# # mydata <- mydata %>%
# #   dplyr::mutate(log_y = log(wdata3$clutch_size)) %>%
# #   tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# # # Create scatterplot
# # ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
# #   ggplot2::geom_point(size = 0.5, alpha = 0.5) +
# #   ggplot2::geom_smooth(method = "loess") +
# #   ggplot2::theme_bw() +
# #   ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected.
# #
# #
# #
# # ##### *** 3.1.2.4. Model goodness-of-fit (GOF) and performances ----
# # # GOF test of Pearson's Chi2 residuals:
# # dat.resid <- sum(stats::resid(ipaSR_glmm2, type = "pearson")^2)
# # 1 - stats::pchisq(dat.resid, stats::df.residual(ipaSR_glmm2)) # p = 0.83, indicating that there is no
# # # significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely
# # # complicated topic and interpretations are not straightforward.
# #
# # # Computing a pseudo-R2:
# # performance::r2_nakagawa(ipaSR_glmm2) # [Additive model]: Marg_R2_glmm = 0.1; Cond_R2_glmm = 0.11.
# # # Does not work if we tune the dispersion parameter of the COM-Poisson model.
# #
# # ## Likelihood-based evaluation of effects inclusion:
# # # For the "site" random-effects (RE):
# # ipaSR_glmm2ac <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
# #                                       urban_intensity + manag_intensity +
# #                                       light_pollution + noise_m + traffic +
# #                                       cumdd_30 + laying_day + year + (1|id_nestbox) + (1|site),
# #                                     data = wdata3, family = glmmTMB::compois(link = "log"),
# #                                     dispformula = ~1) # Rather long to fit (~3-4 min)!
# # summary(ipaSR_glmm2ac) # AIC = 1390.6.
# # # The non-mixed model gives AIC = 1391.7, so similar to the mixed-model (AIC = 1391.3) with only
# # # "id_nestbox" as RE. The one with both RE gives AIC = 1390.6., so it seems like the use of a mixed model
# # # is not truly supported by the data.
# #
# # ## For the whole model:
# # ttCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ 1 + (1|id_nestbox),
# #                                   data = wdata3, family = glmmTMB::compois(link = "log"),
# #                                   dispformula = ~1)
# # res.LRT_null <- stats::anova(object = ttCy_comglmm0, ipaSR_glmm2, test = "LRT")
# # # The test is significant, confirming that the model is useful to explain the data.
# #
# #
# #
# # ##### *** 3.1.2.5. Posterior predictive simulations ----
# # # Predicted counts:
# # par(.pardefault)
# # obsprop <- prop.table(table(wdata3$clutch_size))
# # sims <- stats::simulate(ipaSR_glmm2, nsim = 1000)
# # nsim4 <- colSums(sims == 4) # Number of fours (min obs value)
# # par(las=4,bty="l")
# # plot(pt <- prop.table(table(nsim4)),
# #      ylab="Probability", xlab="Number of fours (true == 1)")
# # (obs4 <- sum(wdata3$clutch_size == 4))
# # points(obs4, 0.002, col="red", pch=16, cex=2) # See y values in obsprop!
# #
# # nsim9 <- colSums(sims == 9) # Number of nines (modal obs value).
# # par(las=1,bty="l")
# # plot(pt <- prop.table(table(nsim9)),
# #      ylab="Probability", xlab="Number of nines (true == 86)")
# # (obs9 <- sum(wdata3$clutch_size == 9))
# # points(obs9, 0.22, col="red", pch=16, cex=2)
# #
# # nsim14 <- colSums(sims == 14) # Number of fourteens (max obs value).
# # par(las=1,bty="l")
# # plot(pt <- prop.table(table(nsim14)),
# #      ylab="Probability", xlab="Number of fourteens (true == 5)")
# # (obs14 <- sum(wdata3$clutch_size == 14))
# # points(obs14, 0.013, col="red", pch=16, cex=2)
# # # These three examples confirm that the model tends to overpredict a bit.
#
#
#
#
#
# ##### ** 3.1.3. Conclusions ----
# # ______________________________
#
# # There are strong GENUS richness variations across regions.
# # Methodologically challenging to account for many urban metrics at the same time as they are highly
# # correlated (which causes multicollinearity issues).
# # This model works relatively fine:
# ipaSR_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
#                                    ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
#                                  data = wdata3, family = glmmTMB::nbinom2(link = "log"))
# summary(ipaSR_glmm2) # AIC = 10887.1
# # It shows:
# #   - A fairly strong negative effect of building patches mean area!
# #   - A strong positive effect of building gaps proportion!
# #   - A weak negative effect of UA_F3!
# ipaSR_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
#                                    ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
#                                  data = wdata3, family = glmmTMB::nbinom2(link = "log"))
# summary(ipaSR_glmm3) # AIC = 10886.1
#
# sjPlot::plot_model(model = ipaSR_glmm3, type = "int", terms = c("c.log_bn", "c.bc", "c.log_bam"),
#                    mdrt.values = "quart", # Can also be "minmax" or otherwise.
#                    title = "Marginal effects of the three-way interaction: BN * BC * BAM
#                    Note that metrics are log-transformed and mean-centred",
#                    legend.title = "Building contiguity (BC)",
#                    axis.title = c("Building numbers (BN)", "Predicted number of arthropod genera"),
#                    show.p = TRUE, show.values = TRUE)
# # Interesting, but very high variability, suggesting that some variable combinations are likely rare.


