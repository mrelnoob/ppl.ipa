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
.pardefault <- par()

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
sum(!rtraits$sp_id %in% colnames(rdata)) # Counts the number of elements of the first vector that is not
# included in the second vector!



### ** 0.1.3. Choice of the buffer radius size ----
# _________________________________________________
# For now, we will only work with the data extracted from the 150m radius buffers, so we select them:
rmetrics %>% dplyr::filter(buffer_radius == 150) -> ipa_metrics



### ** 0.1.4. Missing data imputation ----
# ________________________________________

rtraits %>% dplyr::select(-sp_id, -genus, -species, -hwi, -iucn_status) %>%
  as.data.frame() -> rtraits_mis # missForest only accepts data.frames or matrices (not tibbles).
# Variables must also be only numeric or factors (no character, date, etc.)!

## Actual data imputation using Random Forests:
set.seed(19)
imput <- missForest::missForest(xmis = rtraits_mis, maxiter = 300, ntree = 300, variablewise = TRUE)
rtraits_imp <- imput$ximp

# To create a summary table for the OOB errors (for each imputed variable):
rtraits_imp_error <- data.frame(cbind(
  sapply(rtraits_mis, function(y) sum(length(which(is.na(y))))), # Number of imputed values (i.e. NAs).
  sqrt(imput$OOBerror)), # When 'variablewise' is set to TRUE, missForest() returns MSE (Mean
  # Squared Error) values instead of NRMSE (Normalized Root Mean Square Error). Therefore, I use
  # the square root of the out-of-bag (OOB) values to convert MSE into RMSE.
  row.names = colnames(rtraits_mis)) # To get the name of the variables
rtraits_imp_error %>% dplyr::rename(Nb_imputed_values = 'X1', Oob_RMSE = 'X2') %>%
  dplyr::filter(Nb_imputed_values > 0) -> sub_errtab_rtraits

rtraits$brain_mass <- rtraits_imp$brain_mass
rm(imput, rtraits_mis, rtraits_imp_error, sub_errtab_rtraits, rtraits_imp)





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

### *** 1.2.1.1. Creating a function to create functional groups subset data ----
functional_groups <- function(community_table, grouping_factor){

  nb_gr <- length(unique(grouping_factor))
  data_subsets <- NULL

  for (i in 1:nb_gr){
    sp_list <- as.character(as.matrix(rtraits[which(grouping_factor == levels(grouping_factor)[i]),
                                              "sp_id"]))
    data_subsets[[i]] <- community_table[, which(colnames(community_table) %in% sp_list)]
  }
  return(data_subsets) # Must be within the function but NOT in the for-loop (otherwise it will obviously only
  # return the first element of the list)!
}



### *** 1.2.1.2. Trophic guild richness, abundances and diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = rtraits$trophic_level)

rcarnibirds <- ttt[[1]]
rherbibirds <- ttt[[2]]
romnibirds <- ttt[[3]]

## For carnivore species:
ipa_data$carni_richness <- apply(rcarnibirds > 0, 1, sum) # No need for [,4:ncol...] because the table only
# contains species columns.
ipa_data$carni_abund <- apply(rcarnibirds, 1, sum) # Same.
ipa_data$carni_simpson <- vegan::diversity(x = rcarnibirds, index = "invsimpson")
# IMPORTANT NOTE: when there is only one species, the Shannon index = 0 and as a consequence, Pielou's evenness
# is an NA (because log(0) = 0, so 0/0 = NaN)!


## For herbivore species:
ipa_data$herbi_richness <- apply(rherbibirds > 0, 1, sum) # No need for [,4:ncol...] because the table only
# contains species columns.
ipa_data$herbi_abund <- apply(rherbibirds, 1, sum) # Same.
ipa_data$herbi_simpson <- vegan::diversity(x = rherbibirds, index = "invsimpson")


## For omnivore species:
ipa_data$omni_richness <- apply(romnibirds > 0, 1, sum) # No need for [,4:ncol...] because the table only
# contains species columns.
ipa_data$omni_abund <- apply(romnibirds, 1, sum) # Same.
ipa_data$omni_simpson <- vegan::diversity(x = romnibirds, index = "invsimpson")
rm(ttt)



### *** 1.2.1.3. Nesting guild richness, abundances and diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = rtraits$nesting_pref)

rnest_cavity <- ttt[[5]] # Cavity nesters.
rnest_tree <- cbind(ttt[[1]], ttt[[6]], ttt[[7]]) # Treetop or tree branch nesters.
rnest_shrub <- cbind(ttt[[10]], ttt[[12]], ttt[[13]]) # Shrub or partial shrub nesters.
rnest_open <- cbind(ttt[[2]], ttt[[3]], ttt[[8]], ttt[[9]], ttt[[11]], ttt[[12]]) # Species that nest on open
# surfaces or almost (e.g. ground, riverbanks, water).


## For cavity nesters:
ipa_data$ncav_richness <- apply(rnest_cavity > 0, 1, sum)
ipa_data$ncav_abund <- apply(rnest_cavity, 1, sum)
ipa_data$ncav_simpson <- vegan::diversity(x = rnest_cavity, index = "invsimpson")
# IMPORTANT NOTE: when there is only one species, the Shannon index = 0 and as a consequence, Pielou's evenness
# is an NA (because log(0) = 0, so 0/0 = NaN)!

## For tree nesters:
ipa_data$ntree_richness <- apply(rnest_tree > 0, 1, sum)
ipa_data$ntree_abund <- apply(rnest_tree, 1, sum) # Same.
ipa_data$ntree_simpson <- vegan::diversity(x = rnest_tree, index = "invsimpson")

## For shrub nesters:
ipa_data$nshrub_richness <- apply(rnest_shrub > 0, 1, sum)
ipa_data$nshrub_abund <- apply(rnest_shrub, 1, sum) # Same.
ipa_data$nshrub_simpson <- vegan::diversity(x = rnest_shrub, index = "invsimpson")

## For open nesters:
ipa_data$nopen_richness <- apply(rnest_open > 0, 1, sum)
ipa_data$nopen_abund <- apply(rnest_open, 1, sum) # Same.
ipa_data$nopen_simpson <- vegan::diversity(x = rnest_open, index = "invsimpson")
rm(ttt)



### ** 1.2.2. Functional diversity indices ----
# _____________________________________________

### *** 1.2.2.1. Rao's entropy and functionnal redundancy ----
ipa_data[,c(4:57)] -> wdata # Species only matrix.
rtraits %>%
  dplyr::select(-sp_id, -order, -family, -genus, -species, -hwi, -max_longevity,
                -migratory, -iucn_status) %>%
  as.data.frame() -> wtraits # Simpler traits.
rownames(wtraits) <- rtraits$sp_id # Required by the 'rao.diversity()' function.

# The 'rao.diversity()' function enables the creation of groups of trait to assign valid weights:
traits_grp <- list(c("nesting_pref", "habitat", "hab_density"),
                   c("prim_lifestyle", "urban_tolerance", "social_behaviour", "brain_mass"),
                   c("latitude_cent", "longitude_cent", "range_size"),
                   c("trophic_level", "trophic_niche", "foraging_behaviour", "foraging_strata"),
                   c("beak_length", "beak_width", "beak_depth"),
                   c("body_mass", "tarsus_length", "wing_length", "kipps_distance", "tail_length"),
                   c("clutch_size", "clutch_nb", "egg_mass", "fledging_age", "development_mode")) # The list
# must have the same length as the trait table!

ttt <- SYNCSA::rao.diversity(comm = wdata, traits = wtraits, put.together = traits_grp)
ipa_data$gini_simpson <- ttt$Simpson
ipa_data$rao_q <- ttt$FunRao
ipa_data$fun_redund <- ttt$FunRedundancy
rm(ttt)





########################## ************************************************* ###############################
# ------------------------------------------ #
##### 2. Exploratory data analyses (EDA) #####
# ------------------------------------------ #

summary(rtraits)
summary(ipa_metrics)
summary(ipa_data)

# First, let's reduce the dataset:
wdata <- cbind(ipa_data[,-c(4:57)],
               ipa_metrics[, c(3:29,37,48,50,64,66)])
wdata %>% dplyr::select(-neighbourhood) -> wdata
summary(wdata)



##### * 2.1. Outliers detection ------------------------------------------------
# ---------------------------------------------------------------------------- #

ppl.tits::uni.boxplots(wdata)
ppl.tits::uni.dotplots(wdata)
# It appears that:
#  - There are some extreme values for the various abundances, or the Ff metric.
#  - Some distributions might be problematic, e.g.: the abundance variables, carni_simpson, bh_m, barea_m.



##### * 2.2. Distribution, skewness and kurtosis -------------------------------
# ---------------------------------------------------------------------------- #
ppl.tits::uni.histograms(wdata)

wdata.num <- wdata[, sapply(wdata, is.numeric)]
tab <- data.frame(moments::skewness(x = wdata.num), moments::kurtosis(x = wdata.num)-3)
wdata_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# It appears that:
#  - There is excess skewness for sp_abund, carni_abund, herbi_abund, barea_m, bhgaps, fprop.
#  - There is highly excessive kurtosis for sp_abund, sp_evenness, carni_abund, herbi_abund, bh_m, bh_iqr,
#    barea_m, rdens, fprop, ff_d1b1_euc and cff_d1b1_euc, with some extremely high values!
rm(tab)



##### * 2.3. Bivariate relationships -------------------------------------------
# ---------------------------------------------------------------------------- #

# To exclude the response variables:
wdata.num <- wdata.num[,-c(1:14)]
# To compute the correlation matrix:
res.cor.ipa <- round(stats::cor(wdata.num, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.ipa <- ggcorrplot::cor_pmat(x = wdata.num, method = "spearman")

ipa.corplot <- ggcorrplot::ggcorrplot(res.cor.ipa, type = "upper",
                                          outline.col = "white",
                                          ggtheme = ggplot2::theme_gray,
                                          colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.ipa,
                                          insig = "blank")
# We can see that:
# - Most metrics are quite strongly correlated among each other.
# - The proportion of herbaceous areas is negatively correlated with the proportion of buildings or their height,
#   but it is far less true for the proportion of woody areas!
# - The crossing orthogonality metric is not really correlated with anything!
# - The F metrics seem like good candidates to approximate herbaceous/woody proportions as well as connectivity!
# - The bcount and bfreq metrics could be interesting covariates!

ipa.pairplot <- GGally::ggpairs(wdata.num)
# We find globally the same patterns, but also:
# - Interestingly, there seems to be a negative relationship between woody_richness and crossing density!
# - Bcount and Bfreq seem quite interchangeable.



##### * 2.4. Homoscedasticity and conditional distributions --------------------
# ---------------------------------------------------------------------------- #

cond.boxplots <- function(dataset, Y, Xs, outlier, N = 6,
                          MAR = c(2.2,2.1,0.5,1.7), CEX.LAB = 0.9, FONT.LAB = 2, BTY = "n", FG = "gray35",
                          COL.AXIS = "gray35", COL.LAB = "gray20", CEX.PAR = 0.8, TCL = -0.3,
                          MGP = c(1.2, 0.4, 0.2), OMA = c(1, 0, 0, 0),
                          TYPE = "n", BORDER = "moccasin", COL = "gray50", LTY = 1, STAPLEWEX = 0,
                          WHISKLWD = 2, BOXWEX = 0.5, BOXLWD = 0.1, MEDLWD = 2.6, PCH = 19, ...){
  mydata <- as.data.frame(dataset)

  if(!is.numeric(mydata[,Y])){
    stop("The Y variable must be numeric! Please check the class of your columns before proceeding.")
  }
  if(!any(sapply(mydata, is.numeric)) && !any(sapply(mydata, is.factor))){
    stop("Input dataset should only contain numeric or factor variables!  Please check the class of your
         columns before proceeding.")
  }
  if(missing(Y)){
    stop("Please specify the Y and X(s) variables to be used and respect the required format. See
         ?cond.boxplots for details.")
  }

  nam <- names(mydata)
  ncol.data <- ncol(mydata[,Xs])
  ncol.adjust <- ceiling(x = ncol.data/4) # Round to the next integer (e.g. ceiling(x = 7.12) returns 8)!
  minX <- min(Xs)
  maxX <- max(Xs)
  yyy <- as.matrix(mydata[,Y])

  graphics::par(mfrow= c(ncol.adjust,4), mar=MAR, cex.lab=CEX.LAB, font.lab=FONT.LAB, bty=BTY, fg=FG,
                col.axis=COL.AXIS, col.lab=COL.LAB, cex=CEX.PAR, tcl=TCL, mgp=MGP, oma=OMA)
  for (i in c(minX:maxX)) {

    if (is.numeric(mydata[,i])) {
      fields::bplot.xy(y = mydata[,Y], x = mydata[,i], outlier=outlier, N=N,
                       xlab=(nam[i]), type=TYPE, border=BORDER, col=COL,lty=LTY, staplewex=STAPLEWEX,
                       whisklwd=WHISKLWD, boxwex=BOXWEX, boxlwd=BOXLWD, medlwd=MEDLWD, pch=PCH, cex=0.7, ...)
    }else if (is.factor(mydata[,i])) {
      xxx <- as.matrix(mydata[,i])
      graphics::boxplot(yyy~xxx, outline=outlier, ylab="",
                        xlab=(nam[i]), type=TYPE, border=BORDER, col=COL, lty=LTY, staplewex=STAPLEWEX,
                        whisklwd=WHISKLWD, boxwex=BOXWEX, boxlwd=BOXLWD,medlwd=MEDLWD, pch=PCH, cex=0.7, ...)
    }
  }
}

wdata %>% dplyr::filter(sp_abund < 150) -> wdata2 # Removing the outlier!
# For the 5 specific diversity metrics:
cond.boxplots(dataset = wdata2, Y = 4, Xs = 19:20, outlier = TRUE) # Ok-ish?
stats::bartlett.test(sp_richness~urban_type_2, data = wdata2) # Ok
cond.boxplots(dataset = wdata2, Y = 5, Xs = 19:20, outlier = TRUE) # Ok-ish?
stats::bartlett.test(sp_abund~urban_type_2, data = wdata2) # Nope!
cond.boxplots(dataset = wdata2, Y = 6, Xs = 19:20, outlier = TRUE) # Ok
stats::bartlett.test(sp_shannon~urban_type_2, data = wdata2) # Ok
cond.boxplots(dataset = wdata2, Y = 7, Xs = 19:20, outlier = TRUE) # Ok
stats::bartlett.test(sp_simpson~urban_type_2, data = wdata2) # Ok
cond.boxplots(dataset = wdata2, Y = 8, Xs = 19:20, outlier = TRUE) # Nope!
stats::bartlett.test(sp_evenness~urban_type_2, data = wdata2) # Definitely not!
# For the carnivore diversity metrics:
cond.boxplots(dataset = wdata2, Y = 9, Xs = 19:20, outlier = TRUE) # Ok-ish?
stats::bartlett.test(carni_richness~urban_type_2, data = wdata2) # Definitely not!
cond.boxplots(dataset = wdata2, Y = 10, Xs = 19:20, outlier = TRUE) # Nope!
stats::bartlett.test(carni_abund~urban_type_2, data = wdata2) # Definitely not!
# For the herbivore diversity metrics:
cond.boxplots(dataset = wdata2, Y = 12, Xs = 19:20, outlier = TRUE) # Ok
stats::bartlett.test(herbi_richness~urban_type_2, data = wdata2) # Ok
cond.boxplots(dataset = wdata2, Y = 13, Xs = 19:20, outlier = TRUE) # Ok-ish?
stats::bartlett.test(herbi_abund~urban_type_2, data = wdata2) # Definitely not!
# For the omnivore diversity metrics:
cond.boxplots(dataset = wdata2, Y = 15, Xs = 19:20, outlier = TRUE) # Ok
stats::bartlett.test(omni_richness~urban_type_2, data = wdata2) # Ok
cond.boxplots(dataset = wdata2, Y = 16, Xs = 19:20, outlier = TRUE) # Ok-ish
stats::bartlett.test(omni_abund~urban_type_2, data = wdata2) # Ok-ish



##### * 2.5. Final formatting --------------------------------------------------
# ---------------------------------------------------------------------------- #

## As suggested, I remove BD, BASTD, BF, BW and the F1 factors (or at least, I won't use them):
wdata2 %>% dplyr::mutate(log_bcount = log10(bcount+1),
                               log_fh = log10(fh_d1b1_euc),
                               log_ff = log10(ff_d1b1_euc+1)) -> wdata3

summary(wdata3)
ppl.tits::uni.dotplots(wdata3)





########################## ************************************************* ###############################
# ----------------------------------------------- #
##### 3. Modelling the bird species diversity #####
# ----------------------------------------------- #

##### * 3.1. Species richness models -------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 3.1.1. Poisson or negative binomial GL(M)M ----
# ____________________________________________________

colnames(wdata3)

## Fitting a regular Poisson regression:
ipaSR_glm1 <- stats::glm(sp_richness ~ urban_type_2 + bfreq + log_fh + log_ff,
                          data = wdata3, family = "poisson")

summary(ipaSR_glm1) # AIC = 644.01
## UPDATE: diagnostics turned out quite problematic (underdispersion, potential multicollinearity, and some
# quantile deviations). I will thus try with a COM-Poisson distribution.

## Fitting a COM-Poisson regression:
ipaSR_comglm1 <- glmmTMB::glmmTMB(sp_richness ~ urban_type_2 + bfreq + log_fh + log_ff,
                                 data = wdata3, family = glmmTMB::compois(link = "log"),
                                 dispformula = ~1) # Intercept only 'nu' (default).
summary(ipaSR_comglm1) # AIC = 625.7
## UPDATE: as diagnistics are globally fine for this model, we can try a post-hoc test:
ttt <- dunn.test::dunn.test(x = wdata3$sp_richness, g = wdata3$urban_type_2, method = "holm") # Ok, but does
# not account for the covariates!
# stats::TukeyHSD(x = ipaSR_comglm1) # Does not work for {glmmTMB} models!

#AFINIR§§§§§
#AFINIR§§§§§
#AFINIR§§§§§
#AFINIR§§§§§
#AFINIR§§§§§
#AFINIR§§§§§
#AFINIR§§§§§
#AFINIR§§§§§
#AFINIR§§§§§

contrasts <- c(
  "urban_park - industrial_commercial = 0",
  "industrial_commercial - residential_buildings = 0",
  "urban_park - intersticial_zones = 0"
  )
ttt2 <- multcomp::glht(model = ipaSR_comglm1, linfct = multcomp::mcp(urban_type_2 = contrasts))
summary(ttt2)




# ## Fitting negative binomial GLMMs:
# ipaSR_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
#                                    ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
#                                  data = wdata3, family = glmmTMB::nbinom2(link = "log"))
# summary(ipaSR_glmm2) # AIC = 10887.1
# ipaSR_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
#                                    ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
#                                  data = wdata3, family = glmmTMB::nbinom2(link = "log"))
# summary(ipaSR_glmm3) # AIC = 10886.1
# # So the use of a negative binomial (NB) model seems to help the model and so does the interaction.





### ** 3.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 3.1.2.1. Residuals extraction, autocorrelation and collinearity ----
par(.pardefault)

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ipaSR_comglm1, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Significant deviation and outliers detected!
# DHARMa::testOutliers(simu.resid, type = 'bootstrap', nBoot = 500) # Long.
DHARMa::outliers(simu.resid) # Six outliers detected!
wdata3[c(79),] # I think I know why...

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = wdata3$coord_x, y = wdata3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(ipaSR_comglm1) # Ok.
performance::check_collinearity(ipaSR_comglm1) # High VIF for "urban_type_2" and high correlation for "bfreq"!
stats::vcov(ipaSR_comglm1) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = wdata3$urban_type_2)
DHARMa::plotResiduals(simu.resid, form = wdata3$species_richness) # Not ok (missing covariate)?
DHARMa::plotResiduals(simu.resid, form = wdata3$bcount)
DHARMa::plotResiduals(simu.resid, form = wdata3$log_bcount)
DHARMa::plotResiduals(simu.resid, form = wdata3$bfreq)
DHARMa::plotResiduals(simu.resid, form = wdata3$log_fh)
DHARMa::plotResiduals(simu.resid, form = wdata3$log_ff)
# Some deviations!



### *** 3.1.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
AER::dispersiontest(object = ipaSR_comglm1) # Underdispersion?
DHARMa::testDispersion(simu.resid, alternative = "less") # Significant underdispersion.

# ## Theoretical count distribution:
# theo_count <- COMPoissonReg::rcmp(n = nrow(wdata3), lambda = mean(wdata3$clutch_size), nu = 1.05)
# The 'nu'
# # parameter should be chosen by trial-and-errors.
# tc_df <- data.frame(theo_count)
#
# ggplot2::ggplot(wdata3, ggplot2::aes(clutch_size)) +
#   ggplot2::geom_bar(fill = "#1E90FF") +
#   ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# # This plot suggests that clutch_size could be following a COM-Poisson distribution of parameter nu~1.1!
#
# ## Distribution of the predicted counts:
# pred_counts <- stats::predict(object = ipaSR_glmm2, type = "response") # Extract the predicted counts.
# par(mfrow= c(1,2))
# hist(pred_counts, main = "Predicted counts", xlab = "Number of laid eggs")
# hist(wdata3$clutch_size, main = "Observed counts", xlab = "Number of laid eggs") # The models' predictions
# # are very similar and relatively acceptable (although too narrow).
#
#
#
# ### *** 3.1.2.3. Linearity ----
# # For the sake of further exploration, I also plot variants of our predictors:
# wdata3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
#                          log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
#                          log_F_metric_d2b1,
#                          Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
#                          urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
#                          light_pollution, noise_m,
#                          cumdd_30, laying_day, min_t_before) -> mydata
# predictors <- colnames(mydata)
# # Bind log(Y) and tidying the data for plot (ggplot2, so long format):
# mydata <- mydata %>%
#   dplyr::mutate(log_y = log(wdata3$clutch_size)) %>%
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
# ### *** 3.1.2.4. Model goodness-of-fit (GOF) and performances ----
# # GOF test of Pearson's Chi2 residuals:
# dat.resid <- sum(stats::resid(ipaSR_glmm2, type = "pearson")^2)
# 1 - stats::pchisq(dat.resid, stats::df.residual(ipaSR_glmm2)) # p = 0.83, indicating that there is no
# # significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely complicated
# # topic and interpretations are not straightforward.
#
# # Computing a pseudo-R2:
# performance::r2_nakagawa(ipaSR_glmm2) # [Additive model]: Marg_R2_glmm = 0.1; Cond_R2_glmm = 0.11.
# # Does not work if we tune the dispersion parameter of the COM-Poisson model.
#
# ## Likelihood-based evaluation of effects inclusion:
# # For the "site" random-effects (RE):
# ipaSR_glmm2ac <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
#                                       urban_intensity + manag_intensity +
#                                       light_pollution + noise_m + traffic +
#                                       cumdd_30 + laying_day + year + (1|id_nestbox) + (1|site),
#                                     data = wdata3, family = glmmTMB::compois(link = "log"),
#                                     dispformula = ~1) # Rather long to fit (~3-4 min)!
# summary(ipaSR_glmm2ac) # AIC = 1390.6.
# # The non-mixed model gives AIC = 1391.7, so similar to the mixed-model (AIC = 1391.3) with only
# # "id_nestbox" as RE. The one with both RE gives AIC = 1390.6., so it seems like the use of a mixed model
# # is not truly supported by the data.
#
# ## For the whole model:
# ttCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ 1 + (1|id_nestbox),
#                                   data = wdata3, family = glmmTMB::compois(link = "log"),
#                                   dispformula = ~1)
# res.LRT_null <- stats::anova(object = ttCy_comglmm0, ipaSR_glmm2, test = "LRT")
# # The test is significant, confirming that the model is useful to explain the data.
#
#
#
# ### *** 3.1.2.5. Posterior predictive simulations ----
# # Predicted counts:
# par(.pardefault)
# obsprop <- prop.table(table(wdata3$clutch_size))
# sims <- stats::simulate(ipaSR_glmm2, nsim = 1000)
# nsim4 <- colSums(sims == 4) # Number of fours (min obs value)
# par(las=4,bty="l")
# plot(pt <- prop.table(table(nsim4)),
#      ylab="Probability", xlab="Number of fours (true == 1)")
# (obs4 <- sum(wdata3$clutch_size == 4))
# points(obs4, 0.002, col="red", pch=16, cex=2) # See y values in obsprop!
#
# nsim9 <- colSums(sims == 9) # Number of nines (modal obs value).
# par(las=1,bty="l")
# plot(pt <- prop.table(table(nsim9)),
#      ylab="Probability", xlab="Number of nines (true == 86)")
# (obs9 <- sum(wdata3$clutch_size == 9))
# points(obs9, 0.22, col="red", pch=16, cex=2)
#
# nsim14 <- colSums(sims == 14) # Number of fourteens (max obs value).
# par(las=1,bty="l")
# plot(pt <- prop.table(table(nsim14)),
#      ylab="Probability", xlab="Number of fourteens (true == 5)")
# (obs14 <- sum(wdata3$clutch_size == 14))
# points(obs14, 0.013, col="red", pch=16, cex=2)
# # These three examples confirm that the model tends to overpredict a bit.





### ** 3.1.3. Conclusions ----
# ____________________________

# There are strong GENUS richness variations across regions.
# Methodologically challenging to account for many urban metrics at the same time as they are highly correlated
# (which causes multicollinearity issues).
# This model works relatively fine:
ipaSR_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = wdata3, family = glmmTMB::nbinom2(link = "log"))
summary(ipaSR_glmm2) # AIC = 10887.1
# It shows:
#   - A fairly strong negative effect of building patches mean area!
#   - A strong positive effect of building gaps proportion!
#   - A weak negative effect of UA_F3!
ipaSR_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = wdata3, family = glmmTMB::nbinom2(link = "log"))
summary(ipaSR_glmm3) # AIC = 10886.1

sjPlot::plot_model(model = ipaSR_glmm3, type = "int", terms = c("c.log_bn", "c.bc", "c.log_bam"),
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
