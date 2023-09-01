######## *------------------------------------------------------------* ########
#################### Trait-environment analyses for PPL.IPA ####################
######## *------------------------------------------------------------* ########

# ---------------------- #
##### 0. Data import #####
# ---------------------- #

# For now, you have to run the previous script to load the required data... I'll change it later!
par(.pardefault)
wdata <- ipa_data[,4:49]





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
ltable_coa <- ade4::dudi.coa(df = wdata, scannf = FALSE, nf = 2)
# sum(Ltable_coa$eig)*sum(wdata) # Yields the same value!
ade4::scatter(x = ltable_coa, poseig = "bottomright")
ade4::sco.distri(score = ltable_coa$l1[,1], df = wdata, labels = colnames(wdata))
##### PBBBBBB: sur abondance des martinnets et hirondelles
##### PBBBBBB: sur abondance des martinnets et hirondelles
##### PBBBBBB: sur abondance des martinnets et hirondelles
##### PBBBBBB: sur abondance des martinnets et hirondelles
##### superposition de falco et accupiter!!!!!!!!! why???????????????????
##### superposition de falco et accupiter!!!!!!!!! why???????????????????
##### superposition de falco et accupiter!!!!!!!!! why???????????????????
##### superposition de falco et accupiter!!!!!!!!! why???????????????????
#####
##### P.7 du tuto 2009



data(aviurba, package = "ade4")
test1 <- chisq.test(aviurba$fau)
aviurba$species.names.fr
