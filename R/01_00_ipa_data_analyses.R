######## *------------------------------------------------------------* ########
####################### PRELIMINARY ANALYSES FOR ppl.ipa #######################
######## *------------------------------------------------------------* ########

# Note that I manually added this project and its files into my Github account (cf. '_devhistory_ppl.ipa.R'
# at the root of this project) that can be found at: https://github.com/mrelnoob/ppl.ipa

# ---------------------- #
##### 0. Data import #####
# ---------------------- #

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



##### ** 0.1.2. Data preparation and cleaning ----
# ________________________________________________

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



##### ** 0.1.3. Choice of the buffer radius size ----
# ___________________________________________________
# For now, we will only work with the data extracted from the 150m radius buffers, so we select them:
rmetrics %>% dplyr::filter(buffer_radius == 150) -> ipa_metrics
ipa_metrics %>% dplyr::select(-buffer_radius) -> wmetrics



##### ** 0.1.4. Missing data imputation ----
# __________________________________________

rtraits %>% dplyr::select(-sp_id, -genus, -species, -iucn_status) %>%
  as.data.frame() -> rtraits_mis # missForest only accepts data.frames or matrices (not tibbles).
# Variables must also be only numeric or factors (no character, date, etc.)!

## Actual data imputation using Random Forests:
set.seed(382)
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



##### ** 0.1.5. Excluding aquatic species ----
# ____________________________________________

taxa <- as.character(rtraits$sp_id)[which(rtraits$habitat %in% c("wetland", "riverine"))]

## Removing the columns or rows belonging to the 8 wetland or riverine species:
ipa_data <- as.data.frame(rdata[, !c(colnames(rdata) %in% taxa)])
ipa_traits <- as.data.frame(rtraits[!rtraits$sp_id %in% taxa, ])
# I decided to remove these species as they will bias analyses.





############################ ************************************************* ###############################
# -------------------------------------------------------------- #
##### 1. Computing specific and functional diversity indices #####
# -------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
##### * 1.1. Birds specific diversity ------------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 1.1.1. Species richness and abundances ----
# __________________________________________________

wdata <- ipa_data
ipa_data$sp_richness <- apply(wdata[,4:ncol(wdata)] > 0, 1, sum) # Total number of bird species per site.
ipa_data$sp_abund <- apply(wdata[,4:ncol(wdata)], 1, sum) # Total abundance of birds per site.



##### ** 1.1.2. Species diversity indices ----
# ____________________________________________

##### *** 2.1.2.1. Per site diversity indices ----
ipa_data$sp_shannon <- vegan::diversity(x = wdata[,4:ncol(wdata)], index = "shannon") # Shannon-Wiever index.
ipa_data$sp_simpson <- vegan::diversity(x = wdata[,4:ncol(wdata)], index = "invsimpson") # Inverse Simpson index.
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
rphylo_dist <- as.matrix(ape::cophenetic.phylo(x = tree)) # Sometimes, this function doesn't return the expected
# values (too many 0), and the code chunk should thus be run again. So be careful!



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
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
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
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
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
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
landscape.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
# gridExtra::grid.arrange(landscape.varplot, landscape.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (67.3%)
# of my three variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
wtraits$repro <- zzz # This variable opposes birds that lay few large eggs and fledge late (large values) vs
# birds that lay many small eggs with early fledging juveniles (~slow vs quick POLS)!

wtraits %>% dplyr::relocate(morpho, .after = body_mass) %>%
  dplyr::relocate(beak_size, .after = brain_mass) %>%
  dplyr::relocate(repro, .after = clutch_nb) %>%
  dplyr::select(-body_mass, -tarsus_length, -wing_length, -tail_length,
                -beak_length, -beak_width, -beak_depth,
                -clutch_size, -egg_mass, -fledging_age) -> wtraits



##### *** 1.2.1.3. Rao's entropy and redundancy indices ----
## As the 'rao.diversity()' function requires matching species name lists, I need to harmonize the order
# and names of the species included in the tables:
rphylo_dist -> wphylo_dist
found <- match(colnames(wphylo_dist), table = taxa, nomatch = 0) # This functions enables me to match each name
# of column of "wphylo_dist" with the index of its match in "taxa".
# Now, the following lines of code enable me to assign the matching code names for columns and rows in the
# order found in "found" (= conditional renaming).
colnames(wphylo_dist) <- as.character(ipa_traits$sp_id)[found]
rownames(wphylo_dist) <- as.character(ipa_traits$sp_id)[found]

# Now our three tables share the same code names (names ID - e.g. "pass_dome" for "Passer domesticus"), but not
# in the same order (because the tables were not built this way).
all(rownames(wtraits) == colnames(wdata)) # FALSE!
all(rownames(wtraits) == colnames(wphylo_dist)) # FALSE!
# So we need to re-order them:
wmeta_com <- SYNCSA::organize.syncsa(comm = wdata, traits = wtraits, phylodist = wphylo_dist)
all(rownames(wmeta_com$traits) == colnames(wmeta_com$community)) # TRUE!
all(rownames(wmeta_com$traits) == colnames(wmeta_com$phylodist)) # TRUE!

## Rao's indices computations:
# Creating groups of trait to assign valid weights:
traits_grp <- list(c("nesting_pref", "habitat", "hab_density"),
                   c("prim_lifestyle", "urban_tolerance", "social_behaviour", "brain_mass"),
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
rm(taxa_m, taxa, traits_grp, tree, found)



##### *** 1.2.1.4. FD indices and CWM traits ----
## Computing additional functional diversity (FD) indices and Community Weighted Mean traits (might be long
# to run):
wmetrics <- FD::dbFD(x = wmeta_com$traits, a = wmeta_com$community, w.abun = TRUE,
                     calc.FRic = TRUE, stand.FRic = TRUE, # To calculate functional richness (i.e. the convex
                     # hull volume of the PCA of the trait space).
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
  return(data_subsets) # Must be within the function but NOT in the for-loop (otherwise it will obviously only
  # return the first element of the list)!
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




summary(wtraits)
# FORAGING BEHAVIOUR?????????? other?

rm(ttt)





############################ ************************************************* ###############################
# ------------------------------------------ #
##### 2. Exploratory data analyses (EDA) #####
# ------------------------------------------ #

summary(wtraits)
summary(ipa_metrics)
summary(ipa_data)

# First, let's reduce the dataset:
wdata <- cbind(ipa_data[,-c(4:57)],
               ipa_metrics[, c(3:29,37,48,50,64,66)])
wdata %>% dplyr::select(-neighbourhood) -> wdata
summary(wdata)



# ---------------------------------------------------------------------------- #
##### * 2.1. Outliers detection ------------------------------------------------
# ---------------------------------------------------------------------------- #

ppl.tits::uni.boxplots(wdata)
ppl.tits::uni.dotplots(wdata)
# It appears that:
#  - There are some extreme values for the various abundances, or the Ff metric.
#  - Some distributions might be problematic, e.g.: the abundance variables, carni_simpson, bh_m, barea_m.



# ---------------------------------------------------------------------------- #
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



# ---------------------------------------------------------------------------- #
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



# ---------------------------------------------------------------------------- #
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



# ---------------------------------------------------------------------------- #
##### * 2.5. Final formatting --------------------------------------------------
# ---------------------------------------------------------------------------- #

## As suggested, I remove BD, BASTD, BF, BW and the F1 factors (or at least, I won't use them):
wdata2 %>% dplyr::mutate(log_bcount = log10(bcount+1),
                               log_fh = log10(fh_d1b1_euc),
                               log_ff = log10(ff_d1b1_euc+1)) -> wdata3

summary(wdata3)
ppl.tits::uni.dotplots(wdata3)





############################ ************************************************* ###############################
# ----------------------------------------------- #
##### 3. Modelling the bird species diversity #####
# ----------------------------------------------- #

# ---------------------------------------------------------------------------- #
##### * 3.1. Species richness models -------------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 3.1.1. Poisson or negative binomial GL(M)M ----
# ______________________________________________________

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





##### ** 3.1.2. Diagnostics and assumption checks ----
# ____________________________________________________

##### *** 3.1.2.1. Residuals extraction, autocorrelation and collinearity ----
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



##### *** 3.1.2.2. Distribution (family, ZI, dispersion) ----
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
# ##### *** 3.1.2.3. Linearity ----
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
# ##### *** 3.1.2.4. Model goodness-of-fit (GOF) and performances ----
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
# ##### *** 3.1.2.5. Posterior predictive simulations ----
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





##### ** 3.1.3. Conclusions ----
# ______________________________

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



########### TO DO LIST ----
# ----------------------- #
# * Finish computing FG!!!!!!
# * Try CAH???
# * Explorer toutes les combinaisons possibles (avec les 2 typo de FU), d'abord graphiquement, puis stats
# * Faire un rapport
# * Faire analyse RLQ!!!!!
# ** (Try and plot species accumulation curves????)
# ----------------------- #
