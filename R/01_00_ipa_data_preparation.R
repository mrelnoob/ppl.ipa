######## *------------------------------------------------------------* ########
####################### PRELIMINARY ANALYSES FOR ppl.ipa #######################
######## *------------------------------------------------------------* ########

# Note that I manually added this project and its files into my Github account (cf. '_devhistory_ppl.ipa.R'
# at the root of this project) that can be found at: https://github.com/mrelnoob/ppl.ipa

# ---------------------- #
##### 0. Data import #####
# ---------------------- #
# *----------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 0.1. Import and initial formatting -------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 0.1.1. Import ----
# _________________________

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
                                                           "medium", "intesive")))) # Note, as I use
# "read_csv2", my decimal separator should be a "," and not a "." (as used by default by most softwares). I
# thus corrected that manually on my CSV file!
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



##### ** 0.1.2. Data preparation and cleaning ----
# ________________________________________________

##### *** 0.1.2.1. Tables formatting ----
## Removing useless columns, converting number of couples into number of contacted individuals, and changing
# NAs by zeros (i.e. the species was not observed):
rdata %>% dplyr::select(-id_site, -site_name, -date) %>%
  dplyr::mutate_if(is.numeric, ~ . * 2) -> rdata
rdata[is.na(rdata)] <- 0
rdata$pica_pica <- round(rdata$pica_pica)

## Removing useless columns, dealing with NAs and other slight changes:
rmetrics %>% dplyr::select(-area, -vmanag) %>%
  dplyr::rename(buffer_radius = rayon,
                coord_x = x,
                coord_y = y) -> rmetrics
rmetrics[which(is.na(rmetrics$bh_m)), "bh_m"] <- 0
rmetrics[which(is.na(rmetrics$bh_iqr)), "bh_iqr"] <- 0
rmetrics[which(is.na(rmetrics$barea_m)), "barea_m"] <- 0
rmetrics[which(is.na(rmetrics$barea_iqr)), "barea_iqr"] <- 0
rmetrics[which(is.na(rmetrics$rdens)), "rdens"] <- 0
rmetrics[which(is.na(rmetrics$rortho_m)), "rortho_m"] <- 0
# Note that I do not attribute 0 to the NAs of "bconti" as it would not make sense!

rmetrics %>% dplyr::mutate(lcz = stats::relevel(x = lcz, ref = 5)) %>% # Assigning "forested areas" as
  # reference level.
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>% # Jitter coordinates a little bit.
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> rmetrics

## Removing useless columns:
rtraits %>% dplyr::select(-maturity_age, -longevity) -> rtraits

## To check the matching of species names among tables:
sum(!rtraits$sp_id %in% colnames(rdata)) # Counts the number of elements of the first vector that is not
# included in the second vector!



##### *** 0.1.2.2. Choice of the buffer radius size ----
# For now, we will only work with the data extracted from the 150m radius buffers, so we select them:
rmetrics %>% dplyr::filter(buffer_radius == 150) -> ipa_metrics



##### *** 0.1.2.3. Computing a synthetic light pollution variable ----
ipa_metrics[,44:47] -> ttt

## Normed-PCA for general light pollution proxies:
res.pca <- FactoMineR::PCA(X = ttt, scale.unit = TRUE, graph = FALSE)
# To plot results:
landscape.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              repel = TRUE)
landscape.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
# gridExtra::grid.arrange(landscape.varplot, landscape.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (79.5%)
# of my four variables, we can use the coordinates of observations on this axis as a synthetic variable:
ttt <- res.pca$ind$coord[,1]
ipa_metrics$lpoll <- ttt # This variable opposes sites with high light pollution with those with low pollution.
ipa_metrics <- ipa_metrics[,-c(44:47)]
ipa_metrics %>% dplyr::relocate(lpoll, .after = npoll) -> ipa_metrics



##### *** 0.1.2.4. Excluding aquatic species ----
taxa <- as.character(rtraits$sp_id)[which(rtraits$habitat %in% c("wetland", "riverine"))]

## Removing the columns or rows belonging to the 8 wetland or riverine species:
ipa_data <- as.data.frame(rdata[, !c(colnames(rdata) %in% taxa)])
ipa_traits <- as.data.frame(rtraits[!rtraits$sp_id %in% taxa, ])
# I decided to remove these species as they will bias analyses.



##### ** 0.1.3. Missing data imputation ----
# __________________________________________

ipa_traits %>% dplyr::select(-sp_id, -genus, -species, -iucn_status) %>%
  as.data.frame() -> rtraits_mis # missForest only accepts data.frames or matrices (not tibbles).
# Variables must also be only numeric or factors (no character, date, etc.)!

## Actual data imputation using Random Forests:
set.seed(402)
imput <- missForest::missForest(xmis = rtraits_mis, maxiter = 300, ntree = 300, variablewise = TRUE)
wtraits_imp <- imput$ximp

# To create a summary table for the OOB errors (for each imputed variable):
wtraits_imp_error <- data.frame(cbind(
  sapply(rtraits_mis, function(y) sum(length(which(is.na(y))))), # Number of imputed values (i.e. NAs).
  sqrt(imput$OOBerror)), # When 'variablewise' is set to TRUE, missForest() returns MSE (Mean
  # Squared Error) values instead of NRMSE (Normalized Root Mean Square Error). Therefore, I use
  # the square root of the out-of-bag (OOB) values to convert MSE into RMSE.
  row.names = colnames(rtraits_mis)) # To get the name of the variables
wtraits_imp_error %>% dplyr::rename(Nb_imputed_values = 'X1', Oob_RMSE = 'X2') %>%
  dplyr::filter(Nb_imputed_values > 0) -> sub_errtab_rtraits

ipa_traits$brain_mass <- wtraits_imp$brain_mass
rm(imput, rtraits_mis, wtraits_imp_error, sub_errtab_rtraits, wtraits_imp, res.pca, ttt, taxa)

## Computing a brain proportion variable:
ipa_traits %>% dplyr::mutate(brain_ratio = brain_mass/body_mass) %>%
  dplyr::relocate(brain_ratio, .after = brain_mass) -> ipa_traits



##### ** 0.1.4. Creating an urban form (UF) morphology typology through cluster analysis ----
# ___________________________________________________________________________________________

## Preparing data:
ipa_metrics %>% dplyr::select(-buffer_radius) -> wmetrics
wmetrics[,14:43] -> wmetrics
wmetrics %>% dplyr::select(-bprop, -bh, -bh_iqr, -bfreq, -barea_iqr, -bhgaps,
                           -hcount, -harea_m, -harea_iqr, -scount, -sarea_m, -sarea_iqr,
                           -fcount, -farea_m, -farea_iqr) %>%
  as.data.frame() -> wmetrics
rownames(wmetrics) <- ipa_metrics$id_ipa
# I only select metrics related to the intrinsic urban characteristics of the sampled urban tissues and not
# those related to their location, their topology within habitat networks, or their vegetation diversity as
# these variables are to be considered as extrinsic factors (e.g. meaning that two distinct urban forms could
# share the same vegetation diversity or connectivity, it is not what defines them. Vegetation amount could,
# but not its diversity or large-scale connectivity)! Similarly, I did not include variables related to
# water surfaces as any UF could include water area and it is thus not a defining factor.

## Computing a distance matrix among sites (using Manhattan distance):
ruf_dist <- cluster::daisy(x = wmetrics, metric = "gower", weights = c(1,1,2, rep(x = 1, 12)))

## Computing a dendrogram using Ward method:
tree <- stats::hclust(d = ruf_dist, method = "ward.D2")
inertia <- sort(tree$height, decreasing = TRUE)
plot(inertia[1:20], type = "s", xlab = "Number of classes", ylab = "Inertia")
# The loss of inertia with increasing number of classes suggest that retaining seven classes could be a
# starting point to prune the tree.
ggplot2::ggplot(dendextend::color_branches(tree, k = 7), labels = TRUE) # To colour the tree branches with
# a 7 class pruning.
# We can see that a 7 class typology makes sense. However, we can also see other clearly defined subgroups.
# So we have the choice and could even retain several solutions for exploratory analyses.

## Extracting the chosen typologies:
ipa_metrics$urban_type_3 <- as.factor(stats::cutree(tree = tree, k = 7))
ipa_metrics %>%
  dplyr::relocate(urban_type_3, .after = urban_type_2) %>%
  as.data.frame()-> ipa_metrics

## Interpreting the created typology:
ipa_metrics %>% dplyr::group_by(urban_type_3) %>%
  dplyr::summarise(building_nb = stats::median(bcount, na.rm = TRUE),
                   building_height = stats::median(bh_m, na.rm = TRUE),
                   building_contig = stats::median(bconti, na.rm = TRUE),
                   building_area = stats::median(barea_m, na.rm = TRUE),
                   gap_prop = stats::median(bgaps, na.rm = TRUE),
                   street_length = stats::median(rlength, na.rm = TRUE),
                   street_density = stats::median(rdens, na.rm = TRUE),
                   street_orthog = stats::median(rortho_m, na.rm = TRUE),
                   herbaceous_prop = stats::median(hprop, na.rm = TRUE),
                   shrub_prop = stats::median(sprop, na.rm = TRUE),
                   forest_prop = stats::median(fprop, na.rm = TRUE),
                   water_prop = stats::median(wprop, na.rm = TRUE),
                   noise_pollution = stats::median(npoll, na.rm = TRUE),
                   light_pollution = stats::median(lpoll, na.rm = TRUE)) -> ttt
# ** Class 1 = High building density   + low buildings    + discontiguous        + small surface  +
#              few gaps     + high street density + orthogonal      +
#              mainly grass and shrubs       + noisy      + high light pollution
#              [= DENSE RESIDENTIAL DEVELOPMENT]!
# ** Class 2 = Medium building density + medium buildings + discontiguous        + medium surface +
#              average gaps + high street density + less orthogonal +
#              diversified vegetation cover  + very noisy + average light pollution
#              [= LARGE MIXED HOUSING (small blocks)]!
# ** Class 3 = High building density   + high buildings   + contiguous           + large surface  +
#              few gaps     + high street density + less orthogonal +
#              low vegetation cover          + less noisy + high light pollution
#              [= CITY CENTRE]!
# ** Class 4 = Low building density    + medium buildings + discontiguous        + large surface  +
#              average gaps + low street density  +  less orthogonal +
#              mainly herbaceous cover       + very noisy + low light pollution
#              [= INDUSTRIAL OR COMMERCIAL AREAS]!
# ** Class 5 = Medium building density + medium buildings + highly discontiguous + medium surface +
#              average gaps + low street density  + less orthogonal +
#              mainly grass and shrubs       + less noisy + average light pollution
#              [= SMALL MIXED HOUSING (large blocks)]!
# ** Class 6 = High building density   + high buildings   + contiguous           + medium surface +
#              few gaps     + high street density + less orthogonal +
#              mainly grass and shrubs (low) + very noisy + average light pollution
#              [= HIGH CONTIGUOUS BUILDINGS]!
# ** Class 7 = Low building density    + low buildings    + highly discontiguous + small surface  +
#              many gaps    + low street density  + less orthogonal +
#              high vegetation cover         + less noisy + low light pollution
#              [= URBAN PARKS]!

## Renaming and reordering factor levels:
ipa_metrics$urban_type_3 <- factor(ipa_metrics$urban_type_3,
                                   levels=c("7", "4", "3", "6", "2", "5", "1")) # This code reorders the
# factor levels in the desired order.
levels(ipa_metrics$urban_type_3) <- c("urban_parks",
                                      "industrial_commercial",
                                      "city_centre",
                                      "high_contig_buildings",
                                      "large_mixed_housing",
                                      "small_mixed_housing",
                                      "dense_resid_housing_dev")
## I also do it for the first classification (expert-based):
ipa_metrics$urban_type <- factor(ipa_metrics$urban_type,
                                   levels=c("urban_park", "industrial_commercial", "old_city",
                                            "housing_buildings", "mixed_housing", "mixed_surrounding_park",
                                            "resid_housing_park", "housing_development"))
levels(ipa_metrics$urban_type) <- c("urban_parks",
                                    "industrial_commercial",
                                    "city_centre",
                                    "housing_buildings",
                                    "mixed_housing",
                                    "mixed_housing_park",
                                    "resid_housing_dev_park",
                                    "resid_housing_dev")





############################ ************************************************* ###############################
# -------------------------------------------------------------- #
##### 1. Computing specific and functional diversity indices #####
# -------------------------------------------------------------- #
# *---------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.1. Birds specific diversity ------------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 1.1.1. Species richness and abundances ----
# __________________________________________________

wdata <- ipa_data
ipa_data$sp_richness <- apply(wdata[,4:ncol(wdata)] > 0, 1, sum) # Total number of bird species per site.
ipa_data$sp_abund <- apply(wdata[,4:ncol(wdata)], 1, sum) # Total abundance of birds per site.
# To compute relative abundance of species, I could also make use of the following function:
# funrar::make_relative()



##### ** 1.1.2. Species diversity indices ----
# ____________________________________________

##### *** 2.1.2.1. Per site diversity indices ----
ipa_data$sp_shannon <- vegan::diversity(x = wdata[,4:ncol(wdata)], index = "shannon") # Shannon-Wiever index.
ipa_data$sp_simpson <- vegan::diversity(x = wdata[,4:ncol(wdata)], index = "simpson") # Simpson index.
ipa_data$sp_evenness <- (ipa_data$sp_shannon/log(ipa_data$sp_richness)) # Pielou's evenness index.
# We preferred the Shannon index over the Simpson one because we wanted to give importance to rare species, but
# we calculate both anyway, notably because Pielou's J is known to be sensitive to sample size (likely not a
# problem here) and to rare species.
pairs(ipa_data[,50:54], pch ="+", col = "blue")


##### *** 2.1.2.1. Per urban form (UF) diversity levels ----
## For the first UF typology:
alpha_uf <- with(ipa_metrics, tapply(vegan::specnumber(wdata[,4:ncol(wdata)]), urban_type, mean))
gamma_uf <- with(ipa_metrics, vegan::specnumber(wdata[,4:ncol(wdata)], urban_type))
beta_uf <- gamma_uf/alpha_uf - 1
# NOTE: the above computations are from the {vegan} package help. The definition of beta diversity might differ
# from that of others. I could also use the 'betdiver' function (e.g.
# https://nhcooper123.github.io/macro-module-2020/diversity-indices-in-r.html#beta-diversity).

## For the second UF typology:
alpha_uf_2 <- with(ipa_metrics, tapply(vegan::specnumber(wdata[,4:ncol(wdata)]), urban_type_2, mean))
gamma_uf_2 <- with(ipa_metrics, vegan::specnumber(wdata[,4:ncol(wdata)], urban_type_2))
beta_uf_2 <- gamma_uf_2/alpha_uf_2 - 1

## For the third UF typology (clustering-based):
alpha_uf_3 <- with(ipa_metrics, tapply(vegan::specnumber(wdata[,4:ncol(wdata)]), urban_type_3, mean))
gamma_uf_3 <- with(ipa_metrics, vegan::specnumber(wdata[,4:ncol(wdata)], urban_type_3))
beta_uf_3 <- gamma_uf_3/alpha_uf_3 - 1





# *------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.2. Birds functional diversity ----------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 1.2.1. Functional and phylogenetic diversity indices ----
# ________________________________________________________________

# To compute some diversity metrics while accounting for phylogenetic signal, I first need to compute
# phylogenetic distances. To do so, I'll have to create a phylogenetic tree. This tree might also be useful
# later on.

##### *** 1.2.1.1. Computing phylogenetic distances ----
## Create a phylogenetic tree from the species list:
taxa <- paste(ipa_traits$genus, ipa_traits$species, sep = "_")

taxa_m <- rotl::tnrs_match_names(names = taxa) # To check whether the input names match those from the
# "Open Tree of Life" database.
tree <- rotl::tol_induced_subtree(ott_ids = rotl::ott_id(taxa_m), label_format = "name")
plot(tree, cex = .8, label.offset = .1, no.margin = TRUE)

## Compute the phylogenetic distance between species:
rphylo_dist <- as.matrix(ape::cophenetic.phylo(x = tree)) # Sometimes, this function doesn't return the
# expected values (too many 0), and the code chunk should thus be run again. So be careful!



##### *** 1.2.1.2. Computing synthetic trait variables ----
## Data preparation and simplification:
droplevels(as.data.frame(ipa_data[,c(4:49)])) -> wdata # Species only matrix...
rownames(wdata) <- ipa_data$id_ipa # ... but keeping site IDs as row names.

ipa_traits %>%
  dplyr::select(-sp_id, -order, -family, -genus, -species,
                -max_longevity, -migratory, -iucn_status,
                -latitude_cent, -longitude_cent, -range_size) %>%
  as.data.frame() %>%
  droplevels() -> wtraits # Simpler traits.
rownames(wtraits) <- ipa_traits$sp_id # Names should be set as row names.

## Computing synthetic morphometric or reproductive variables:
wtraits %>% dplyr::select(body_mass, tarsus_length, wing_length, tail_length,
                          beak_length, beak_width, beak_depth,
                          clutch_size, clutch_nb, egg_mass, fledging_age) -> ttt

## Normed-PCA for general morphology variables:
res.pca <- FactoMineR::PCA(X = ttt[, 1:4], scale.unit = TRUE, graph = FALSE)
# To plot results:
landscape.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              repel = TRUE)
landscape.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
# gridExtra::grid.arrange(landscape.varplot, landscape.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (86.7%)
# of my four variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
wtraits$morpho <- zzz # This variable opposes big birds (high values - with large body size and long wings,
# tarsus and tails) to small birds (small values).

## Normed-PCA for beak morphology variables:
res.pca <- FactoMineR::PCA(X = ttt[, 5:7], scale.unit = TRUE, graph = FALSE)
# To plot results:
landscape.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              repel = TRUE)
landscape.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
# gridExtra::grid.arrange(landscape.varplot, landscape.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (92.5%)
# of my three variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
wtraits$beak_size <- zzz # This variable opposes large beak birds (high values) to birds with small and thin
# beaks (small values).

## Normed-PCA for reproductive variables:
res.pca <- FactoMineR::PCA(X = ttt[, c(8,10,11)], scale.unit = TRUE, graph = FALSE)
# To plot results:
landscape.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              repel = TRUE)
landscape.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
# gridExtra::grid.arrange(landscape.varplot, landscape.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (67.3%)
# of my three variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
wtraits$repro <- zzz # This variable opposes birds that lay few large eggs and fledge late (large values) vs
# birds that lay many small eggs with early fledging juveniles (~slow vs quick POLS)!

wtraits %>%
  dplyr::relocate(morpho, .after = body_mass) %>%
  dplyr::relocate(beak_size, .after = brain_ratio) %>%
  dplyr::relocate(repro, .after = clutch_nb) %>%
  dplyr::select(-body_mass, -tarsus_length, -wing_length, -tail_length,
                -beak_length, -beak_width, -beak_depth,
                -clutch_size, -egg_mass, -fledging_age) -> wtraits



##### *** 1.2.1.3. Rao's entropy and redundancy indices ----
## As the 'rao.diversity()' function requires matching species name lists, I need to harmonize the order
# and names of the species included in the tables:
rphylo_dist -> wphylo_dist
found <- match(colnames(wphylo_dist), table = taxa, nomatch = 0) # This functions enables me to match each
# name of column of "wphylo_dist" with the index of its match in "taxa".
# Now, the following lines of code enable me to assign the matching code names for columns and rows in the
# order found in "found" (= conditional renaming).
colnames(wphylo_dist) <- as.character(ipa_traits$sp_id)[found]
rownames(wphylo_dist) <- as.character(ipa_traits$sp_id)[found]

# Now our three tables share the same code names (names ID - e.g. "pass_dome" for "Passer domesticus"), but
# not in the same order (because the tables were not built this way).
all(rownames(wtraits) == colnames(wdata)) # FALSE!
all(rownames(wtraits) == colnames(wphylo_dist)) # FALSE!
# So we need to re-order them:
wmeta_com <- SYNCSA::organize.syncsa(comm = wdata, traits = wtraits, phylodist = wphylo_dist)
all(rownames(wmeta_com$traits) == colnames(wmeta_com$community)) # TRUE!
all(rownames(wmeta_com$traits) == colnames(wmeta_com$phylodist)) # TRUE!

## Rao's indices computations:
# Creating groups of trait to assign valid weights:
traits_grp <- list(c("nesting_pref", "habitat", "hab_density"),
                   c("prim_lifestyle", "urban_tolerance", "social_behaviour", "brain_mass", "brain_ratio"),
                   c("trophic_level", "trophic_niche", "foraging_behaviour", "foraging_strata"),
                   c("morpho", "beak_size"),
                   c("kipps_distance", "hwi"),
                   c("clutch_nb", "repro", "development_mode")) # The list
# must have the same length as the trait table!

wmetrics <- SYNCSA::rao.diversity(comm = wmeta_com$community, traits = wmeta_com$traits,
                             phylodist = wmeta_com$phylodist, put.together = traits_grp)
ipa_data$gini_simpson <- wmetrics$Simpson # Gini Simpson index.
ipa_data$rao_q <- wmetrics$FunRao # Rao's quadratic entropy index (based on trait distances).
ipa_data$fun_redund <- wmetrics$FunRedundancy # Functional redundancy index.
ipa_data$rao_phy <- wmetrics$PhyRao # Rao's quadratic entropy index (based on phylogenetic distances).
ipa_data$phy_redund <- wmetrics$PhyRedundancy # Phylogenetic redundancy index.
rm(taxa_m, taxa, traits_grp, tree, found, zzz, ttt,
   ruf_dist, inertia, res.pca, landscape.indplot, landscape.varplot)



##### *** 1.2.1.4. FD indices and CWM traits ----
## Computing additional functional diversity (FD) indices and Community Weighted Mean traits (might be long
# to run):
wmetrics <- FD::dbFD(x = wmeta_com$traits, a = wmeta_com$community, w.abun = TRUE,
                     calc.FRic = TRUE, stand.FRic = TRUE, # To calculate functional richness (i.e. the
                     # convex hull volume of the PCA of the trait space).
                     m = "min", # To reduce the dimensionality of the trait matrix.
                     calc.CWM = TRUE, CWM.type = "all", # To calculate CWM with the abundance of each
                     # individual class being returned for categorical variables.
                     calc.FDiv = TRUE, # To calculate functional richness.
                     print.pco = TRUE) # To print the PCA axes eigenvalues.
ipa_cwm <- wmetrics$CWM
ipa_data$fun_richness <- wmetrics$FRic
ipa_data$fun_evenness <- wmetrics$FEve
ipa_data$fun_diversity <- wmetrics$FDiv
ipa_data$fun_dispersion <- wmetrics$FDis



##### ** 1.2.2. Functional groups richness and abundances ----
# ____________________________________________________________

##### *** 1.2.2.1. Creating a function to create functional groups subset data ----
functional_groups <- function(community_table, grouping_factor){

  nb_gr <- length(unique(grouping_factor))
  data_subsets <- NULL

  for (i in 1:nb_gr){
    sp_list <- as.character(as.matrix(ipa_traits[which(grouping_factor == levels(grouping_factor)[i]),
                                              "sp_id"]))
    data_subsets[[i]] <- community_table[, which(colnames(community_table) %in% sp_list), drop = FALSE] # I
    # have to use the 'drop = FALSE' in this case because otherwise, if a single column is extracted, it will
    # coerced it into a numeric vector (not a data.frame) and thus drop its (col)name!
  }
  return(data_subsets) # Must be within the function but NOT in the for-loop (otherwise it will obviously
  # only return the first element of the list)!
}
ipa_fun_groups <- NULL



##### *** 1.2.2.2. Trophic guild diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = wtraits$trophic_level)

ipa_fun_groups[[1]] <- ttt[[1]]
names(ipa_fun_groups)[1] <- "fg_troph_carni"
ipa_fun_groups[[2]] <- ttt[[2]]
names(ipa_fun_groups)[2] <- "fg_troph_herbi"
ipa_fun_groups[[3]] <- ttt[[3]]
names(ipa_fun_groups)[3] <- "fg_troph_omni"

## For carnivore species:
ipa_data$carni_richness <- apply(ipa_fun_groups$fg_troph_carni > 0, 1, sum)
ipa_data$carni_abund <- apply(ipa_fun_groups$fg_troph_carni, 1, sum) # Same.
ipa_data$carni_simpson <- vegan::diversity(x = ipa_fun_groups$fg_troph_carni, index = "simpson")


## For herbivore species:
ipa_data$herbi_richness <- apply(ipa_fun_groups$fg_troph_herbi > 0, 1, sum)
ipa_data$herbi_abund <- apply(ipa_fun_groups$fg_troph_herbi, 1, sum) # Same.
ipa_data$herbi_simpson <- vegan::diversity(x = ipa_fun_groups$fg_troph_herbi, index = "simpson")

## For omnivore species:
ipa_data$omni_richness <- apply(ipa_fun_groups$fg_troph_omni > 0, 1, sum)
ipa_data$omni_abund <- apply(ipa_fun_groups$fg_troph_omni, 1, sum) # Same.
ipa_data$omni_simpson <- vegan::diversity(x = ipa_fun_groups$fg_troph_omni, index = "simpson")



##### *** 1.2.2.3. Nesting guild diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = wtraits$nesting_pref)

ipa_fun_groups[[4]] <- ttt[[4]] # Cavity nesters.
names(ipa_fun_groups)[4] <- "fg_nest_cavity"
ipa_fun_groups[[5]] <- cbind(ttt[[1]], ttt[[5]], ttt[[6]]) # Treetop or tree branch nesters.
names(ipa_fun_groups)[5] <- "fg_nest_tree"
ipa_fun_groups[[6]] <- cbind(ttt[[5]], ttt[[7]], ttt[[8]], ttt[[9]]) # Shrub or partial shrub nesters.
names(ipa_fun_groups)[6] <- "fg_nest_shrub"
ipa_fun_groups[[7]] <- cbind(ttt[[2]], ttt[[8]]) # Species that nest on the ground.
names(ipa_fun_groups)[7] <- "fg_nest_ground"
ipa_fun_groups[[8]] <- cbind(ttt[[3]], ttt[[9]]) # Species that nest on buildings (or cliffs).
names(ipa_fun_groups)[8] <- "fg_nest_build"

## For cavity nesters:
ipa_data$ncav_richness <- apply(ipa_fun_groups$fg_nest_cavity > 0, 1, sum)
ipa_data$ncav_abund <- apply(ipa_fun_groups$fg_nest_cavity, 1, sum)
ipa_data$ncav_simpson <- vegan::diversity(x = ipa_fun_groups$fg_nest_cavity, index = "simpson")

## For tree nesters:
ipa_data$ntree_richness <- apply(ipa_fun_groups$fg_nest_tree > 0, 1, sum)
ipa_data$ntree_abund <- apply(ipa_fun_groups$fg_nest_tree, 1, sum) # Same.
ipa_data$ntree_simpson <- vegan::diversity(x = ipa_fun_groups$fg_nest_tree, index = "simpson")

## For shrub nesters:
ipa_data$nshrub_richness <- apply(ipa_fun_groups$fg_nest_shrub > 0, 1, sum)
ipa_data$nshrub_abund <- apply(ipa_fun_groups$fg_nest_shrub, 1, sum) # Same.
ipa_data$nshrub_simpson <- vegan::diversity(x = ipa_fun_groups$fg_nest_shrub, index = "simpson")

## For ground nesters:
ipa_data$nground_richness <- apply(ipa_fun_groups$fg_nest_ground > 0, 1, sum)
ipa_data$nground_abund <- apply(ipa_fun_groups$fg_nest_ground, 1, sum) # Same.
ipa_data$nground_simpson <- vegan::diversity(x = ipa_fun_groups$fg_nest_ground, index = "simpson")

## For building nesters:
ipa_data$nbuild_richness <- apply(ipa_fun_groups$fg_nest_build > 0, 1, sum)
ipa_data$nbuild_abund <- apply(ipa_fun_groups$fg_nest_build, 1, sum) # Same.
ipa_data$nbuild_simpson <- vegan::diversity(x = ipa_fun_groups$fg_nest_build, index = "simpson")



##### *** 1.2.2.4. Foraging guild diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = wtraits$foraging_strata)

ipa_fun_groups[[9]] <- ttt[[3]] # Ground foragers (exclusive).
names(ipa_fun_groups)[9] <- "fg_forag_exclground"
ipa_fun_groups[[10]] <- cbind(ttt[[3]], ttt[[4]]) # Ground foragers (partial or exclusive).
names(ipa_fun_groups)[10] <- "fg_forag_ground"
ipa_fun_groups[[11]] <- cbind(ttt[[4]], ttt[[5]]) # Lower strata foragers (partial or exclusive).
names(ipa_fun_groups)[11] <- "fg_forag_lower"
ipa_fun_groups[[12]] <- cbind(ttt[[6]], ttt[[7]]) # Mid and upper strata foragers (exclusive).
names(ipa_fun_groups)[12] <- "fg_forag_upper"
# I disregard aquatic species as it is obvious that their foraging habits depends on the presence of water,
# which is rather unrelated to urban forms. I also disregard the four "air" foraging species as they also
# are building nesters and are thus already accounted for in the nesting guild diversity variables.

## For exclusive ground foragers:
ipa_data$fexground_richness <- apply(ipa_fun_groups$fg_forag_exclground > 0, 1, sum)
ipa_data$fexground_abund <- apply(ipa_fun_groups$fg_forag_exclground, 1, sum)
ipa_data$fexground_simpson <- vegan::diversity(x = ipa_fun_groups$fg_forag_exclground, index = "simpson")

## For non-exclusive ground foragers:
ipa_data$fground_richness <- apply(ipa_fun_groups$fg_forag_ground > 0, 1, sum)
ipa_data$fground_abund <- apply(ipa_fun_groups$fg_forag_ground, 1, sum)
ipa_data$fground_simpson <- vegan::diversity(x = ipa_fun_groups$fg_forag_ground, index = "simpson")

## For lower strata foragers:
ipa_data$flower_richness <- apply(ipa_fun_groups$fg_forag_lower > 0, 1, sum)
ipa_data$flower_abund <- apply(ipa_fun_groups$fg_forag_lower, 1, sum)
ipa_data$flower_simpson <- vegan::diversity(x = ipa_fun_groups$fg_forag_lower, index = "simpson")

## For mid and upper strata foragers:
ipa_data$fupper_richness <- apply(ipa_fun_groups$fg_forag_upper > 0, 1, sum)
ipa_data$fupper_abund <- apply(ipa_fun_groups$fg_forag_upper, 1, sum)
ipa_data$fupper_simpson <- vegan::diversity(x = ipa_fun_groups$fg_forag_upper, index = "simpson")



##### *** 1.2.2.5. Habitat preference group diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = wtraits$habitat)

ipa_fun_groups[[13]] <- cbind(ttt[[1]], ttt[[3]]) # Forest or woodland species.
names(ipa_fun_groups)[13] <- "fg_hab_forest"
ipa_fun_groups[[14]] <- cbind(ttt[[4]], ttt[[5]]) # Shrubland or grassland species.
names(ipa_fun_groups)[14] <- "fg_hab_shrubopen"
ipa_fun_groups[[15]] <- ttt[[2]] # Rupestrian species.
names(ipa_fun_groups)[15] <- "fg_hab_rupest"

## For forest or woodland species:
ipa_data$hab_forest_richness <- apply(ipa_fun_groups$fg_hab_forest > 0, 1, sum)
ipa_data$hab_forest_abund <- apply(ipa_fun_groups$fg_hab_forest, 1, sum)
ipa_data$hab_forest_simpson <- vegan::diversity(x = ipa_fun_groups$fg_hab_forest, index = "simpson")

## For shrubland or grassland species:
ipa_data$hab_shrub_richness <- apply(ipa_fun_groups$fg_hab_shrubopen > 0, 1, sum)
ipa_data$hab_shrub_abund <- apply(ipa_fun_groups$fg_hab_shrubopen, 1, sum)
ipa_data$hab_shrub_simpson <- vegan::diversity(x = ipa_fun_groups$fg_hab_shrubopen, index = "simpson")

## For rupestrian species:
ipa_data$hab_rupest_richness <- apply(ipa_fun_groups$fg_hab_rupest > 0, 1, sum)
ipa_data$hab_rupest_abund <- apply(ipa_fun_groups$fg_hab_rupest, 1, sum)
ipa_data$hab_rupest_simpson <- vegan::diversity(x = ipa_fun_groups$fg_hab_rupest, index = "simpson")



##### *** 1.2.2.6. Social behaviour group diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = wtraits$social_behaviour)

ipa_fun_groups[[16]] <- ttt[[1]] # Solitary species.
names(ipa_fun_groups)[16] <- "fg_solitary"
ipa_fun_groups[[17]] <- ttt[[2]] # Gregarious species.
names(ipa_fun_groups)[17] <- "fg_gregarious"
ipa_fun_groups[[18]] <- ttt[[3]] # Species living in small groups.
names(ipa_fun_groups)[18] <- "fg_smallgroups"

## For solitary species:
ipa_data$soc_solit_richness <- apply(ipa_fun_groups$fg_solitary > 0, 1, sum)
ipa_data$soc_solit_abund <- apply(ipa_fun_groups$fg_solitary, 1, sum)
ipa_data$soc_solit_simpson <- vegan::diversity(x = ipa_fun_groups$fg_solitary, index = "simpson")

## For gregarious species:
ipa_data$soc_greg_richness <- apply(ipa_fun_groups$fg_gregarious > 0, 1, sum)
ipa_data$soc_greg_abund <- apply(ipa_fun_groups$fg_gregarious, 1, sum)
ipa_data$soc_greg_simpson <- vegan::diversity(x = ipa_fun_groups$fg_gregarious, index = "simpson")

## For species living in small groups:
ipa_data$soc_smallgrp_richness <- apply(ipa_fun_groups$fg_smallgroups > 0, 1, sum)
ipa_data$soc_smallgrp_abund <- apply(ipa_fun_groups$fg_smallgroups, 1, sum)
ipa_data$soc_smallgrp_simpson <- vegan::diversity(x = ipa_fun_groups$fg_smallgroups, index = "simpson")



##### *** 1.2.2.7. Migratory diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = ipa_traits$migratory)

ipa_fun_groups[[19]] <- ttt[[1]] # Non-migratory species.
names(ipa_fun_groups)[19] <- "fg_resident"
ipa_fun_groups[[20]] <- ttt[[2]] # Migratory species.
names(ipa_fun_groups)[20] <- "fg_migratory"


## For non-migratory species:
ipa_data$migrat_no_richness <- apply(ipa_fun_groups$fg_resident > 0, 1, sum)
ipa_data$migrat_no_abund <- apply(ipa_fun_groups$fg_resident, 1, sum)
ipa_data$migrat_no_simpson <- vegan::diversity(x = ipa_fun_groups$fg_resident, index = "simpson")

## For migratory species:
ipa_data$migrat_yes_richness <- apply(ipa_fun_groups$fg_migratory > 0, 1, sum)
ipa_data$migrat_yes_abund <- apply(ipa_fun_groups$fg_migratory, 1, sum)
ipa_data$migrat_yes_simpson <- vegan::diversity(x = ipa_fun_groups$fg_migratory, index = "simpson")



##### *** 1.2.2.8. Conservation status group diversity ----
ttt <- functional_groups(community_table = ipa_data, grouping_factor = ipa_traits$iucn_status)

ipa_fun_groups[[21]] <- cbind(ttt[[3]], ttt[[4]]) # For species that are NT or VU.
names(ipa_fun_groups)[21] <- "fg_ntvu"


## For non-migratory species:
ipa_data$conserv_richness <- apply(ipa_fun_groups$fg_ntvu > 0, 1, sum)
ipa_data$conserv_abund <- apply(ipa_fun_groups$fg_ntvu, 1, sum)
ipa_data$conserv_simpson <- vegan::diversity(x = ipa_fun_groups$fg_ntvu, index = "simpson")

rm(ttt, wphylo_dist, rphylo_dist, functional_groups)






########### *-----------------------------------------------------* ############
############################### To do list #####################################
# ---------------------------------------------------------------------------- #
# * Explorer toutes les combinaisons possibles (avec les 2 typo de FU), d'abord graphiquement, puis stats
# * Faire un rapport
# * Faire analyse RLQ!!!!!
# ** (Try and plot species accumulation curves????)
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
########### *-----------------------------------------------------* ############


