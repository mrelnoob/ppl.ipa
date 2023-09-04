######## *------------------------------------------------------------* ########
#################### Trait-environment analyses for PPL.IPA ####################
######## *------------------------------------------------------------* ########

# -------------------------------------- #
##### 0. Data import and preparation #####
# -------------------------------------- #

# For now, you have to run the previous script to load the required data... I'll change it later!
dev.off()
wdata <- ipa_data[,4:49]

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






############################ ************************************************* ###############################
# ----------------------- #
##### 1. RLQ analysis #####
# ----------------------- #
# *---------------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.1. Preliminary two-tables analyses -----------------------------------
# ---------------------------------------------------------------------------- #
#
# # A preliminary step of RLQ analysis is to perform the separate analyses of each table. Because of the two
# tables associated with the two margins of the L table (L = link; the site x species table), the L table has
# to be symmetric and should thus be transformed using a Correspondence Analysis.

##### ** 1.1.1. Correspondence Analysis (L table) ----
# ____________________________________________________

# The site x species table (L table) can be viewed as a contingency table. As such, correlations between rows
# and columns can be computed as a Chi2 statistic. E.g.:
#   test1 <- chisq.test(wdata)
#   test1$statistic # X² = 13019.06!
# The CoA decompose this link across several axes:
otable.l_coa <- ade4::dudi.coa(df = wdata, scannf = FALSE, nf = 2) # 'otable' is for "ordination table", 'l'
# is for the L table, and 'coa' is for Correspondence Analysis! The same logic will be followed in the next
# sections.
# sum(otable.l_coa$eig)*sum(wdata) # Yields the same value!
ade4::scatter(x = otable.l_coa, poseig = "bottomright")
ade4::sco.distri(score = otable.l_coa$l1[,1], df = wdata, labels = colnames(wdata))
# For some reason, there consistently is an overlap between "Falco subbuteo" and "Cuculus canorus". A possible
# reason could be that they are double-ones (they co-occur in the same single site).
par(mfrow = c(1, 2))
ade4::sco.label(otable.l_coa$co[, 1], lab = colnames(wdata), reverse = TRUE,
          boxes = FALSE, horizontal = FALSE, lim = c(-2.5, 2.5), pos.lab = 0.3)
ade4::sco.label(otable.l_coa$l1[, 1], horizontal = FALSE, lim = c(-2.5, 2.5),
          boxes = FALSE)
dev.off()





##### ** 1.1.2. H&S or PC Analysis (R table) ----
# _______________________________________________

# As the site x environment (R table) contains both quantitative and (one) qualitative variables , we should
# theoretically ordinate it using an Hill and Smith's Analysis. Furthermore, to enable the coupling of this
# table with the species table, we also have to weigh lines using the weight from the CoA. Unfortunately,
# the Hill & Smith's Analysis precludes the inclusion of ordered factors and the alternative, a "mixed
# analysis", does not allow row weightings.
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
otable.r_pca$eig/sum(otable.r_pca$eig) # Indicates that the first 2 axes account for ~30% and ~21% of the
# total inertia, respectively:
# --> The first PC seems to represent a kind of building density gradient, as it opposes building metrics and
#     landcover metrics (or 'bgaps', i.e. the proportion of gaps between buildings) and, to a some extent, the
#     connectivity variables (although their loadings/scores are not that high on this axis), suggesting that
#     connectivity is generally higher in scarcely built sites.
# --> The second PC seems to represent a urban park/industrial area to residential housing gradient, as it
#     opposes sites with few large and well spaced buildings with a large amount of quite highly connected
#     forest patches, against sites characterised by many small buildings (e.g. houses), higher shrub habitat
#     proportions and connectivity, higher woody plant diversity (e.g. ornamentals), and quite high
#     grassland connectivity.


##### P.9 du tuto 2009


#
# data(aviurba, package = "ade4")
# test1 <- chisq.test(aviurba$fau)
# aviurba$species.names.fr
