######## *------------------------------------------------------------* ########
#################### Trait-environment analyses for PPL.IPA ####################
######## *------------------------------------------------------------* ########

# -------------------------------------- #
##### 0. Data import and preparation #####
# -------------------------------------- #

# For now, you have to run the previous script to load the required data... I'll change it later!
dev.off()
wdata <- ipa_data[,4:49]
rownames(wdata) <- ipa_data$id_ipa

ipa_metrics[,c("species_richness", "evenness_class",
               "bcount", "bprop", "bh_m", "barea_m", "bgaps",
               "rdens", "rortho_m",
               "hprop", "sprop", "fprop",
               "cfh_d1b1_euc", "cfs_d1b1_euc", "cff_d1b1_euc")] -> wmetrics # I only keep a very limited set
# of environmental and urban metrics as we cannot test all combinations. Note that I removed the "bconti"
# variable because it contained true NA which are not allowed by some ordination analyses. I also removed
# the light and noise pollution variables as the urban metrics should satisfyingly approximate them. Finally,
# note also that I only keep 3 connectivity metrics (CF metrics) that better reflect large scale multi-
# generational spatial dynamics than the F-metrics.
rownames(wmetrics) <- ipa_metrics$id_ipa

wmeta_com$traits %>% dplyr::select(-development_mode, -trophic_niche, -foraging_behaviour,
                                   -beak_size, -kipps_distance, -clutch_nb,
                                   -habitat, -hab_density, -urban_tolerance, -social_behaviour) %>%
  dplyr::mutate(nesting_guild = dplyr::case_when(
    nesting_pref == "tree_branch" | nesting_pref == "shrub_tree_branch" |
      nesting_pref == "treetop" ~ "tree",
    nesting_pref == "ground" ~ "ground",
    nesting_pref == "building" ~ "building",
    nesting_pref == "cavity" ~ "cavity",
    nesting_pref == "shrub" | nesting_pref == "shrub_ground" |
      nesting_pref == "building_shrub" ~ "shrub"),
    foraging_niche = dplyr::case_when(
      foraging_strata == "all_strata" ~ "all_strata",
      foraging_strata == "air" ~ "air",
      foraging_strata == "ground" ~ "excl_ground",
      foraging_strata == "ground_and_lower_strata" | foraging_strata == "lower_strata" ~ "gr_low_strata",
      foraging_strata == "midhigh_strata" | foraging_strata == "tree_canopy" ~ "upper_strata")) %>%
  dplyr::select(-nesting_pref, -foraging_strata) %>%
  dplyr::relocate(nesting_guild, .before = trophic_level) %>%
  dplyr::relocate(foraging_niche, .after = trophic_level) %>%
  dplyr::mutate(dplyr::across(where(is.character), factor)) -> wtraits # Strong simplification, including
# factor levels, to reduce the number of binary comparisons during the Fourth Corner analysis. Note that
# I use the table stored in the working metacommunity object so for it to share the same species order as
# the community table.

xy <- cbind(ipa_metrics$coord_x, ipa_metrics$coord_y) # IPA sites coordinates.





############################ ************************************************* ###############################
# ----------------------- #
##### 1. RLQ analysis #####
# ----------------------- #
# *----------------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.1. Preliminary two-tables analyses -----------------------------------
# ---------------------------------------------------------------------------- #

## A preliminary step of RLQ analysis is to perform the separate analyses of each table. Because of the two
# tables associated with the two margins of the L table (L = link; the site x species table), the L table has
# to be symmetric and should thus be transformed using a Correspondence Analysis.



##### ** 1.1.1. L table ordination (Correspondence Analysis) ----
# _______________________________________________________________

## The site x species table (L table) can be viewed as a contingency table. As such, correlations between rows
# and columns can be computed as a Chi2 statistic. E.g.:
#   test1 <- chisq.test(wdata)
#   test1$statistic # X² = 13019.06!
# The CoA decompose this link across several axes:
otable.l_coa <- ade4::dudi.coa(df = wdata, scannf = FALSE, nf = 2) # 'otable' is for "ordination table", 'l'
# is for the L table, and 'coa' is for Correspondence Analysis! The same logic will be followed in the next
# sections.
# sum(otable.l_coa$eig)*sum(wdata) # Yields the same value!
ade4::scatter(x = otable.l_coa, poseig = "bottomright")
ade4::sco.distri(score = otable.l_coa$l1[,1], df = wdata, labels = colnames(wdata)) # To get the conditional
# variances of the columns (species) on the first axis (i.e. something like the variances of the species
# abundances in the sites represented by this axis in which they occur).
## For some reason, there consistently is an overlap between "Falco subbuteo" and "Cuculus canorus". A possible
# reason could be that they are double-ones (they co-occur in the same single site). Moreover, some species
# seem to have quite variable abundances among sites, especially the "aerial" species (e.g. Apus apus).
ade4::sco.distri(otable.l_coa$c1[,1], df = data.frame(t(wdata)), labels = rownames(wdata)) # To get the
# conditional variances of the rows (sites) on the first axis (i.e. something like the variances, on this
# axis, of the species of this site).
## On the 1st axis, IPA074 seem to have an oddly large variance of species abundances, but that seems normal
# as it is solely drawn by the high abundances of some "aerial" species.
par(mfrow = c(1, 2))
ade4::sco.label(otable.l_coa$co[, 1], lab = colnames(wdata), reverse = TRUE,
          boxes = FALSE, horizontal = FALSE, lim = c(-2.5, 2.5), pos.lab = 0.3)
ade4::sco.label(otable.l_coa$l1[, 1], horizontal = FALSE, lim = c(-2.5, 2.5),
          boxes = FALSE)
ade4::s.value(xy, otable.l_coa$l1[, 1], addaxes = FALSE, include.origin = FALSE)
ade4::s.value(xy, otable.l_coa$l1[, 2], addaxes = FALSE, include.origin = FALSE) # Ordinations suggest
# the existence of a rather clear spatial structure.
dev.off()



##### ** 1.1.2. R table ordination (Principal Component Analysis) ----
# ____________________________________________________________________

# As the site x environment (R table) contains both quantitative and (one) qualitative variables , we should
# theoretically ordinate it using an Hill and Smith's Analysis. Furthermore, to enable the coupling of this
# table with the species table, we also have to weigh lines using the LINE WEIGHTS from the CoA.
# Unfortunately, the Hill & Smith's Analysis precludes the inclusion of ordered factors and the alternative,
# a "mixed analysis", does not allow row weightings.
# Consequently, I should either drop my ordered factor or convert it into a numeric variable and then run
# a Principle Component Analysis (PCA). I chose the latter solution (I could also have changed the factor
# into an unordered one but I deemed preferable to keep the ordered nature of its levels despite having to
# assume an imprecise difference of 1-unit between them - i.e. the conversion below assumes that the
# difference between A and B and C and D levels is always equal to 1, which is likely not exactly true):
wmetrics %>% dplyr::mutate(plant_evenness = dplyr::case_when(
  evenness_class == "D" ~ 0,
  evenness_class == "C" ~ 1,
  evenness_class == "B" ~ 2,
  evenness_class == "A" ~ 3)) %>%
  dplyr::rename(plant_richness = species_richness) %>%
  dplyr::relocate(plant_evenness, .after = plant_richness) %>%
  dplyr::select(-evenness_class) -> wmetrics

otable.r_pca <- ade4::dudi.pca(df = wmetrics, row.w = otable.l_coa$lw, # Row weights.
                             scannf = FALSE, nf = 2)
ade4::scatter(x = otable.r_pca)
summary(otable.r_pca) # Indicates that the first 2 axes account for ~30% and ~21% of the total inertia.
otable.r_pca$c1 %>% dplyr::arrange(desc(CS2)) # To get the variable loadings on the axes (on descending order
# according to the second axis 'CS2' values).
# --> The first PC seems to represent a kind of building density gradient, as it opposes building metrics and
#     landcover metrics (or 'bgaps', i.e. the proportion of gaps between buildings) and, to a some extent, the
#     connectivity variables (although their loadings/scores are not that high on this axis), suggesting that
#     connectivity is generally higher in scarcely built sites.
# --> The second PC seems to represent a urban park/industrial area to residential housing gradient, as it
#     opposes sites with few large and well spaced buildings with a large amount of quite highly connected
#     forest patches, against sites characterised by many small buildings (e.g. houses), higher shrub habitat
#     proportions and connectivity, higher woody plant diversity (e.g. ornamentals), and quite high
#     grassland connectivity.
ade4::inertia.dudi(x = otable.r_pca, row.inertia = TRUE, col.inertia = TRUE) # To get the contributions
# of rows and columns in the ordination.
## Some sites seem quite (overly?) influential: e.g. IPA056 and IPA071. Otherwise, we could say that the
# first 2 axes adequately describe a large portion of the urban gradient.
par(mfrow = c(1, 2))
ade4::s.value(xy, otable.r_pca$l1[, 1], addaxes = FALSE, include.origin = FALSE)
ade4::s.value(xy, otable.r_pca$l1[, 2], addaxes = FALSE, include.origin = FALSE) # Ordinations once again
# show some spatial patterns confirming our interpretation. The spatial arrangement of the 2nd axis values
# furthermore emphasises the fact that urban gradients go beyond simple centre-periphery or dense-not dense
# gradients.
dev.off()



##### ** 1.1.3. Q table ordination (Hill & Smith's Analysis) ----
# _______________________________________________________________

# As the species x traits (Q table) contains both quantitative and (one) qualitative variables , we should
# ordinate it using an Hill and Smith's Analysis. Furthermore, to enable the coupling of this table with
# the species table, we also have to weigh lines using the COLUMN WEIGHTS from the CoA.
otable.q_hsa <- ade4::dudi.hillsmith(df = wtraits, row.w = otable.l_coa$cw,
                                     scannf = FALSE, nf = 2)
ade4::scatter(x = otable.q_hsa)
summary(otable.q_hsa) # Indicates that the first 2 axes account for ~31% and ~18% of the projected inertia.
otable.q_hsa$c1 %>% dplyr::arrange(desc(CS1)) # Or CS2.
# --> The first axis seems is rather hard to interpret as it opposes birds with many different attributes,
#     but it seems partly linked to the reproductive and foraging strategies with, on one side, small "quick
#     POLS" birds that forage and nest mainly in woody vegetation or on the ground and, on the other side,
#     big "slow POLS" birds that forage in the air or exclusively on the ground (e.g. swallows or pigeons).
# --> The second PC clearly opposes aerial birds (and their other traits) or some small forest passerines to
#     bigger birds such as corvids or pigeons.
dev.off()



##### ** 1.1.4. RL and QL tests (Co-Inertia Analysis) ----
# ________________________________________________________

# ## To investigate if the links between species and environment or between traits and distribution are
# # strong and significant, we can run Co-Inertia Analyses (COIA):
# otable.rl_coia <- ade4::coinertia(dudiX = otable.l_coa, dudiY = otable.r_pca, scannf = FALSE, nf = 2)
# otable.l_coa2 <- ade4::dudi.coa(df = t(wdata), scannf = FALSE, nf = 2) # Same CoA but with a transposed matrix.
# otable.ql_coia <- ade4::coinertia(dudiX = otable.l_coa2, dudiY = otable.q_hsa, scannf = FALSE, nf = 2)
#
# test1 <- ade4::randtest(otable.rl_coia, fixed = 1)
# test2 <- ade4::randtest(otable.ql_coia, fixed = 1)
# ## That doesn't work. The 'randtest' function returns an unfathomable error message. So I'll skip this part
# # for now.





# *------------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.2. Three-tables synthesis (RLQ) --------------------------------------
# ---------------------------------------------------------------------------- #

## RLQ analyses use the factorial analyses made in the preliminary steps to create its own ordinations,
# but these new ordinations do not necessarily match the previous ones.
o.rlq <- ade4::rlq(dudiR = otable.r_pca, dudiL = otable.l_coa, dudiQ = otable.q_hsa,
                   scannf = FALSE, nf = 2)
## To test the significance of the global link R-Q with a permutation test:
# ade4::randtest(xtest = o.rlq) # DOES NOT WORK WITH MY DATA. WHY???

summary(o.rlq) # Cumulative projected inertia for the 2 first RLQ axes is 93%!
plot(o.rlq) # The graph produced shows:
# * The ordination (row scores) of sites (upper left) and species (upper right).
# * The contributions of the environmental variables (lower left) and traits (lower right).
## We can see that the 2 RLQ axes are well linked to the 2 PCA axis of the R table (as shown by the left
# correlation circle and by the high inertia/co-inertia ratio for R in the summary). The link is quite
# weaker for the HSA axes of the Q table.

## Interpretation of the link between R and the RLQ axes (projection of the environmental variables):
ade4::s.arrow(dfxy = o.rlq$l1, boxes = FALSE) # 'l1' contains the normed scores of the R variables.
ade4::s.label(dfxy = o.rlq$mR, add.plot = TRUE)
dev.new()
par(mfrow = c(1, 2))
ade4::s.value(dfxy = xy, z = o.rlq$lR[,1], # 'lR' is the row coordinates of the R table on the chosen RLQ axis.
              addaxes = FALSE, include.origin = FALSE) # If we include the origin, the plot is too small.
ade4::s.value(dfxy = xy, z = o.rlq$lR[,2],
              addaxes = FALSE, include.origin = FALSE)
dev.off()
## As with R_pca's 1st axis, the 1st RLQ axis seems to oppose centre and periphery and thus partly represents
# a building density gradient (as well as a forest connectivity gradient).
## The 2nd axis is harder to interpret. It opposes sites with high forest connectivity, large gaps between
# buildings, and/or rather low buildings (sometimes small), with a low woody plant richness and evenness, low
# vegetation cover and connectivity (except the forest one), against the opposite.


# AFINIR§§§§ Interprétation Q sur RLQ!!! + P.15 tuto + Tuto 2013 + Braga???
# Je commence à me dire que certains sites devraient être retirés car ils tirent trop les analyses! IPA074


## To reproduce and enhance the two factorial projection plots:
ade4::s.arrow(dfxy = o.rlq$c1, xlim = c(-1,1), boxes = FALSE) # 'c1' contains the normed scores (coordinates)
# of the traits.
ade4::s.label(dfxy = o.rlq$li, add.plot = TRUE) # 'li' contains the coordinates of the environmental variables.
# Note that, for factors, arrows can go well beyond one and far exceed the arrows of numeric variables, but
# that doesn't mean that their importance exceed that of the latter, they're just plotted on different scales
# (or at least, I think).
## See also Jacquet & Prodon (2014) [in French].







