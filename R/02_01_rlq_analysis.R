######## *------------------------------------------------------------* ########
#################### Trait-environment analyses for PPL.IPA ####################
######## *------------------------------------------------------------* ########

# -------------------------------------- #
##### 0. Data import and preparation #####
# -------------------------------------- #

## For now, you have to run the previous script to load the required data... I'll change it later!
## Besides, for this script, I'll exceptionally load the {ade4} package because the latter was not written to
# allow the use of the pkg::fun() syntax I usually use. The use of this syntax caused error messages when I ran
# permutation tests on my factorial analyses (see code below). I had to ask the developers of the package.
dev.off()
library(ade4)

##### ATTEMPT: RLQ without the 6 sites that have < 2 buildings! ----
##### ATTEMPT: RLQ without the 6 sites that have < 2 buildings! ----
##### ATTEMPT: RLQ without the 6 sites that have < 2 buildings! ----
# None of these sites hosted unique species: IPA number: 012a, 056, 074, 079, 097 and 098.

wdata <- ipa_data[,4:49]
rownames(wdata) <- ipa_data$id_ipa
wdata <- wdata[!rownames(wdata) %in% c("IPA012a", "IPA056", "IPA074", "IPA079", "IPA097", "IPA098"),]

ipa_metrics[,c("species_richness", "evenness_class",
               "bcount", "bprop", "bh_m", "bconti", "barea_m", "bgaps", # BCONTI back in da place§§§§§§§§
               "rdens", "rortho_m",
               "hprop", "sprop", "fprop",
               "cfh_d1b1_euc", "cff_d1b1_euc")] -> wmetrics # I only keep a very limited set
# of environmental and urban metrics as we cannot test all combinations. Note that I removed the "bconti"
# variable because it contained true NA which are not allowed by some ordination analyses. I also removed
# the light and noise pollution variables as the urban metrics should satisfyingly approximate them. Finally,
# note also that I only keep 3 connectivity metrics - CF metrics - that better reflect large scale multi-
# generational dispersal dynamics than the F-metrics.
rownames(wmetrics) <- ipa_metrics$id_ipa
wmetrics <- wmetrics[!rownames(wmetrics) %in% c("IPA012a", "IPA056", "IPA074", "IPA079", "IPA097", "IPA098"),]

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
xy <- xy[-c(23,86,104,109,127,128),] # The indices of the deleted sites!





############################ ************************************************* ###############################
# ----------------------- #
##### 1. RLQ analysis #####
# ----------------------- #
# *----------------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.1. Preliminary one-table analyses ------------------------------------
# ---------------------------------------------------------------------------- #

## A preliminary step of RLQ analysis is to perform the separate analyses of each table. Because of the two
# tables associated with the two margins of the L table (L = link; the site x species table), the L table has
# to be symmetric and should thus be transformed using a Correspondence Analysis.



##### ** 1.1.1. L table ordination (Correspondence Analysis) ----
# _______________________________________________________________

## The site x species table (L table) can be viewed as a contingency table. As such, correlations between rows
# and columns can be computed as a Chi2 statistic. E.g.:
#   test1 <- chisq.test(wdata)
#   test1$statistic # X² = 9780.6!
# The CoA decompose this link across several axes:
otable.l_coa <- dudi.coa(df = wdata, scannf = FALSE, nf = 2) # 'otable' is for "ordination table", 'l'
# is for the L table, and 'coa' is for Correspondence Analysis! The same logic will be followed in the next
# sections.
# sum(otable.l_coa$eig)*sum(wdata) # Yields the same value!
scatter(x = otable.l_coa, poseig = "bottomright")
sco.distri(score = otable.l_coa$l1[,1], df = wdata, labels = colnames(wdata)) # To get the conditional
# variances of the columns (species) on the first axis (i.e. something like the variances of the species
# abundances in the sites represented by this axis in which they occur).
## For some reason, there consistently is an overlap between "Falco subbuteo" and "Cuculus canorus". A possible
# reason could be that they are double-ones (they co-occur in the same single site). Moreover, some species
# seem to have quite variable abundances among sites, but that seem globally alright.
sco.distri(otable.l_coa$c1[,1], df = data.frame(t(wdata)), labels = rownames(wdata)) # To get the
# conditional variances of the rows (sites) on the first axis (i.e. something like the variances, on this
# axis, of the species of this site).
## On the 1st axis, some sites seem to have large variances of species abundances, but that seems normal
# given their locations.
par(mfrow = c(1, 2))
sco.label(otable.l_coa$co[, 1], lab = colnames(wdata), reverse = TRUE,
          boxes = FALSE, horizontal = FALSE, lim = c(-2.5, 2.5), pos.lab = 0.3)
sco.label(otable.l_coa$l1[, 1], horizontal = FALSE, lim = c(-2.5, 2.5),
          boxes = FALSE)
s.value(xy, otable.l_coa$l1[, 1], addaxes = FALSE, include.origin = FALSE)
s.value(xy, otable.l_coa$l1[, 2], addaxes = FALSE, include.origin = FALSE) # Ordinations suggest the existence
# of a rather clear spatial structure, and we find again some of the influential sites (often located on the
# border of the study area: e.g. IPA077, IPA071, IPA080).
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

otable.r_pca <- dudi.pca(df = wmetrics, row.w = otable.l_coa$lw, # Row weights.
                             scannf = FALSE, nf = 2)
scatter(x = otable.r_pca)
summary(otable.r_pca) # Indicates that the first 2 axes account for ~32.7% and ~18.4% of the total inertia.
otable.r_pca$c1 %>% dplyr::arrange(desc(CS2)) # To get the variable loadings on the axes (on descending order
# according to the second axis 'CS2' values).
# --> The first PC seems to represent a kind of building density gradient, as it opposes sites with high
#     building proportion, height, and area to sites with high vegetation proportions, connectivity, as well
#     as high building gaps and contiguity (e.g. single residential housing areas).
# --> The second PC seems to represent a urban park/industrial area to residential housing gradient, as it
#     opposes sites with few large and well spaced buildings with a large amount forest patches, against
#     sites characterised by many small buildings (e.g. houses), higher shrub habitat proportions and
#     connectivity.
inertia.dudi(x = otable.r_pca, row.inertia = TRUE, col.inertia = TRUE) # To get the contributions
# of rows and columns in the ordination.
## Some sites seem quite (overly?) influential: e.g. IPA077. Otherwise, we could say that the
# first 2 axes adequately describe a large portion of the urban gradient.
par(mfrow = c(1, 2))
s.value(xy, otable.r_pca$l1[, 1], addaxes = FALSE, include.origin = FALSE)
s.value(xy, otable.r_pca$l1[, 2], addaxes = FALSE, include.origin = FALSE) # Ordinations once again
# show some spatial patterns confirming our interpretation. The spatial arrangement of the 2nd axis values
# furthermore emphasises the fact that urban gradients go beyond simple centre-periphery or dense-not dense
# gradients.
dev.off()



##### ** 1.1.3. Q table ordination (Hill & Smith's Analysis) ----
# _______________________________________________________________

# As the species x traits (Q table) contains both quantitative and (one) qualitative variables , we should
# ordinate it using an Hill and Smith's Analysis. Furthermore, to enable the coupling of this table with
# the species table, we also have to weigh lines using the COLUMN WEIGHTS from the CoA.
otable.q_hsa <- dudi.hillsmith(df = wtraits, row.w = otable.l_coa$cw,
                                     scannf = FALSE, nf = 2)
scatter(x = otable.q_hsa)
summary(otable.q_hsa) # Indicates that the first 2 axes account for ~31.5% and ~18% of the projected inertia.
otable.q_hsa$c1 %>% dplyr::arrange(desc(CS1)) # Or CS2.
# --> The first axis seems is rather hard to interpret as it opposes birds with many different attributes,
#     but it seems partly linked to the reproductive and foraging strategies with, on one side, small "quick
#     POLS" birds that forage and nest mainly in woody vegetation or on the ground and, on the other side,
#     big "slow POLS" birds that forage in the air or exclusively on the ground (e.g. swallows or pigeons).
# --> The second PC clearly opposes aerial birds (and their other traits) or some small forest passerines to
#     bigger birds such as corvids or pigeons.
dev.off()





# *----------------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.2. Preliminary two-table analyses ------------------------------------
# ---------------------------------------------------------------------------- #

##### ** 1.2.1. RL and QL tests (Co-Inertia Analysis) ----
# ________________________________________________________

## To investigate if the links between species and environment or between traits and distributions are
# strong and significant, we can run Co-Inertia Analyses (COIA):
otable.rl_coia <- coinertia(dudiX = otable.l_coa, dudiY = otable.r_pca, scannf = FALSE, nf = 2)
otable.l_coa2 <- dudi.coa(df = t(wdata), scannf = FALSE, nf = 2) # Same CoA but with a transposed matrix.
otable.ql_coia <- coinertia(dudiX = otable.l_coa2, dudiY = otable.q_hsa, scannf = FALSE, nf = 2)

test1 <- randtest(otable.rl_coia, fixed = 1) # Remember, it only works if the ordinated objects have been
# created by loading {ade4} and avoiding the pkg::fun() syntax!
test2 <- randtest(otable.ql_coia, fixed = 1)
# The first test is highly significant (p = 0.001) but the 2nd one does not work. It seems that the method is
# not yet available for COIA between a Correspondence Analysis and a Hill & Smith Analysis. I hope it will
# work for the RLQ analysis.





# *------------------------------------------------* #####
# ---------------------------------------------------------------------------- #
##### * 1.3. Three-tables synthesis (RLQ Analysis) -----------------------------
# ---------------------------------------------------------------------------- #

##### ** 1.3.1. Actual RLQ analysis ----
# ______________________________________

## RLQ analyses use the factorial analyses made in the preliminary steps to create its own ordinations,
# but these new ordinations do not necessarily match the previous ones.
o.rlq <- rlq(dudiR = otable.r_pca, dudiL = otable.l_coa, dudiQ = otable.q_hsa,
                   scannf = FALSE, nf = 2)
## To test the significance of the global link R-Q with a permutation test:
randtest(xtest = o.rlq) # Remember, it only works if the ordinated objects have been created by loading {ade4}
# and avoiding the pkg::fun() syntax!
## Model 2 is highly significant (p = 0.001) but not model 4 (p = 0.166)!

summary(o.rlq) # Cumulative projected inertia for the 2 first RLQ axes is 87.3%!
## The "Inertia & coinertia R" section shows the projected inertia of the 1st axis (and the 1-2 axes plan) of
## the environmental variables ordination (here a PCA on the R table) onto the RLQ axes. The "max" column
## shows the optimum inertia that could be achieved while the "ratio" is "inertia/max"!
## The "Inertia & coinertia Q" section is the same thing but for the trait ordination (here a HSA on the
## Q table).
# The ratios from these two sections of the summary highlight the degree of matching between the axes of the
# simple ordinations and those of the RLQ analyses (and, somehow, their contributions). They can be close but
# not necessarily. The same information is depicted on the correlation circles of the plot. Here, we can see
# that the 2 RLQ axes are well linked to the 2 PCA axis of the R table (so they pretty much ordinate sites
# the same way) but the relationship is weaker for the HSA axes of the Q table.
## The "Correlation L" section relates (I think) to the correlation between the COA axes (from the L table)
## and the RLQ axes, that is to the ordination axes that account for both the environment and the traits.
# We can see that the factorial coordinates on the RLQ axes are far from being optimal, probably because the
# environment does not explain all the trait (and species) distribution, which is quite logical.

dev.new()
plot(o.rlq) # The graph produced shows:
# * The ordination (row scores) of sites (upper left) and species (upper right).
# * The contributions of the environmental variables (lower left) and traits (lower right).

## Interpretation of the link between R and the RLQ axes (projection of the environmental variables):
dev.new()
s.arrow(dfxy = o.rlq$l1, boxes = FALSE) # 'l1' contains the normed scores of the R variables.
s.label(dfxy = o.rlq$mR, add.plot = TRUE) # 'mR' contains the normed scores of sites.
dev.new()
par(mfrow = c(1, 2))
s.value(dfxy = xy, z = o.rlq$lR[,1], # 'lR' is the row coordinates of the R table on the chosen RLQ axis.
              addaxes = FALSE, include.origin = FALSE) # If we include the origin, the plot is too small.
s.value(dfxy = xy, z = o.rlq$lR[,2],
              addaxes = FALSE, include.origin = FALSE)
# --> Quite similarly to the (R) PCA's 1st axis, the 1st RLQ axis seems to oppose centre and periphery
#     and thus partly represents a building density gradient. Archetypal sites with HIGH values on this
#     axis are city-centre sites while those with LOW values are urban parks, fringes, campuses or low
#     density residential areas.
# --> The 2nd axis is indeed quite different than the PCA's 2nd axis as it opposes sites with high gap
#     proportions and high forest connectivity (and, to a lesser extent, either high building contiguity
#     or street density), to sites lacking these attributes but having high plant diversity and vegetation
#     cover or high building density and proportions. Archetypal sites with HIGH values on this axis are
#     some mixed housing sites located near important forest corridors, while it is quite hard to define
#     archetypal sites with LOW values.
## The second axis, although accounting for a much lower inertia than the first, truly breaks the centre-
# periphery caricatural opposition.
dev.off()


## Interpretation of the link between Q and the RLQ axes (projection of the traits):
dev.new()
s.arrow(dfxy = o.rlq$c1, boxes = FALSE) # 'c1' contains the normed scores of the Q variables.
s.label(dfxy = o.rlq$mQ, add.plot = TRUE) # 'mQ' contains the normed scores of species.
## Note that, for factors, arrows can go well beyond one and far exceed the arrows of numeric variables,
## but that doesn't mean that their importance exceed that of the latter, they're just plotted on different
## scales (or at least, I think).
# --> Here, the first RLQ axis seems to oppose building nesters to ground nesters and strata generalist
#     feeders. Most traits are not well represented. So it is quite different than the (Q) HSA's first axis.
#     Birds typical of HIGH values on this axis are pigeons or corvid species. Those typical of LOW values
#     are Sylvia curruca, Aegithalos caudatus, Saxicola rubecula, Ficedula hypoleuca, of Phylloscopus
#     trochyllus.
# --> Quite similarly to the (Q) HSA's 2nd axis, the 2nd RLQ axis seems to oppose aerial predators (such as
#     falcons, swallows and swifts) to the other bird species.
## Even if they are plotted on different scales, the numeric traits do not seem to be very important to
## define the axis except, perhaps, "REPRO" and "MORPHO" for the 1st axis and the HWI for the 2nd one.
dev.off()


## Plot all important plots together:
dev.new()
par(mfrow = c(1,2))
s.arrow(dfxy = o.rlq$l1, boxes = FALSE)
s.label(dfxy = o.rlq$lR, add.plot = TRUE, boxes = FALSE)
s.arrow(dfxy = o.rlq$c1, boxes = FALSE)
s.label(dfxy = o.rlq$lQ, add.plot = TRUE, boxes = FALSE)



##### ** 1.2.2. Fourth corner analysis ----
# _________________________________________

fc_rlq <- fourthcorner(tabR = wmetrics, tabL = wdata, tabQ = wtraits,
                       modeltype = 6, # To combine model 2 and 4.
                       nrepet = 9999, # Took 6-7 min to run.
                       p.adjust.method.G = "fdr", p.adjust.method.D = "fdr") # We use the "False discovery
# rate" method (Benjamini & Hochberg, 1995) to control for multiple testing.
plot(fc_rlq, alpha = 0.05, stat = "D2", col = c("lightgrey", "yellowgreen", "red3", "purple4")) # Green means
# that a significant positive relationship has been found and red a negative one.
## When no correction is applied, we seen some significant relationships but that are mostly driven by aerial
# species or urban exploiters, so nothing very interesting. When FDR is controlled for, nothing is significant.
# We have to rethink and refocus our analyses.


#######################
#######################
#######################
## Tuto 2013 section 4 (combining both approaches), but:
## --> I should refocus: e.g. ratios of metrics, without building nesters...... ????
## See also Jacquet & Prodon (2014) [in French] + book Thioulouse 2018 + Braga 2018 ???!!!








########### *-----------------------------------------------------* ############
############################### To do list #####################################
# ---------------------------------------------------------------------------- #
# * Explorer toutes les combinaisons possibles (avec les 2 typo de FU), d'abord graphiquement, puis stats
# * Faire un rapport
# * Faire analyse RLQ!!!!!
# * -- Faire une 2ème RLQ avec des ratios de métriques, pour tester les combinaisons (et réduire le nombre de
#      prédicteurs)???????
# ** (Try and plot species accumulation curves????)
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
########### *-----------------------------------------------------* ############
