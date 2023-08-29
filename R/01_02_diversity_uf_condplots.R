
# ------------------------------------------------------- #
##### 1. Conditional boxplot for results presentation #####
# ------------------------------------------------------- #

colnames(ipa_data)
wdata <- cbind(ipa_metrics[,c(1, 3:7)],ipa_data[,c(50:ncol(ipa_data))])
summary(wdata)



# ---------------------------------------------------------------------------- #
##### * 1.1. Birds specific diversity ------------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 1.1.1. Using the first UF typology ----
# __________________________________________________

##### ~~~~~~~~~ For the birds species richness ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(10, 4, 3, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_richness~wdata$urban_type, outline=TRUE,
                  ylab="Specific richness (Nb. of bird species)",
                  xlab="", las=2,
                  xaxt = "n", # Do not plot the default labels
                  type="n", border="orange", col="tomato",
                  boxwex=0.7, boxcol= "tomato", boxlwd=0.01,
                  lty=1, staplewex=0,
                  whisklwd=3, medlwd=3, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1.5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

# ### To add jittered data points to the graph:
# mylevels <- levels(wdata$urban_type)
# levelproportions <- summary(wdata$urban_type)/nrow(wdata)
#
# for(i in 1:length(mylevels)){
#   thislevel <- mylevels[i]
#   thisvalues <- wdata[wdata$urban_type==thislevel, "sp_richness"]
#
#   # take the x-axis indices and add a jitter, proportional to the N in each level
#   myjitter <- jitter(rep(i, length(thisvalues)), amount=levelproportions[i]/2)
#   points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9), cex=0.8)
# }





# ##### For the birds species abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$sp_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Birds abundance",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="orange", col="darkred", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 8, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the birds species Shannon index ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$sp_shannon~wdata$urban_type_2, outline=TRUE,
#                         ylab="Shannon-Wiever diversity index",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="orange", col="darkred", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the birds species evenness index ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$sp_evenness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Pielou's evenness index",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="orange", col="darkred", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the carnivorous birds richness ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$carni_richness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Carnivorous birds richness",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="navyblue", col="deepskyblue2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.7, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the carnivorous birds abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$carni_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Carnivorous birds abundance",
#                         ylim=c(0,50),
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="navyblue", col="deepskyblue2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the herbivorous birds richness ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$herbi_richness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Herbivorous birds richness",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="navyblue", col="deepskyblue2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.4, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the herbivorous birds abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$herbi_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Herbivorous birds abundance",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="navyblue", col="deepskyblue2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 4, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the omnivorous birds richness ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$omni_richness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Omnivorous birds richness",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="navyblue", col="deepskyblue2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.4, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the omnivorous birds abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$omni_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Omnivorous birds abundance",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="navyblue", col="deepskyblue2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the cavity nesters richness ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$ncav_richness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Cavity nesters richness",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.7, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the cavity nesters abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$ncav_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Cavity nesters abundance",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the tree nesters richness ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$ntree_richness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Tree branch or canopy nesters richness",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.5, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





##### For the tree nesters abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$ntree_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Tree branch or canopy nesters abundance",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the shrub nesters richness ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$nshrub_richness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Shrub nesters richness",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.25, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the shrub nesters abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$nshrub_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Shrub nesters abundance",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 1, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the open or ground nesters richness ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$nopen_richness~wdata$urban_type_2, outline=TRUE,
#                         ylab="Open area nesters richness (e.g. ground, water)",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.5, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the open or ground nesters abundance ----
# par(font.lab = 4, font.axis=6,
#     mar = c(8, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# # end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
# bp <- graphics::boxplot(wdata$nopen_abund~wdata$urban_type_2, outline=TRUE,
#                         ylab="Open area nesters abundance (e.g. ground, water)",
#                         xlab="", las=2,
#                         xaxt = "n", # Do not plot the default labels
#                         type="n", border="seagreen4", col="palegreen2", lty=1, staplewex=0,
#                         whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 1, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For Rao's quadratic entropy index ----
# par(font.lab = 4, font.axis=6,
#     mar = c(10, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# bp <- graphics::boxplot(wdata$sp_richness~wdata$urban_type_2, outline=TRUE,
#                   ylab="Rao's quadratic entropy (Q)",
#                   xlab="", las=2,
#                   xaxt = "n", # Do not plot the default labels
#                   type="n", border="coral", col="violetred4", lty=1, staplewex=0,
#                   whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For Rao's functional redundancy index ----
# par(font.lab = 4, font.axis=6,
#     mar = c(10, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# bp <- graphics::boxplot(wdata$fun_redund~wdata$urban_type_2, outline=TRUE,
#                   ylab="Rao's functional redundancy index",
#                   xlab="", las=2,
#                   xaxt = "n", # Do not plot the default labels
#                   type="n", border="coral", col="violetred4", lty=1, staplewex=0,
#                   whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.01, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)





# ##### For the Gini-Simpson index ----
# par(font.lab = 4, font.axis=6,
#     mar = c(10, 4, 2, 2) + 0.5,
#     bty="l", fg="gray4")
#
# bp <- graphics::boxplot(wdata$gini_simpson~wdata$urban_type_2, outline=TRUE,
#                   ylab="Gini-Simpson diversity index",
#                   xlab="", las=2,
#                   xaxt = "n", # Do not plot the default labels
#                   type="n", border="coral", col="violetred4", lty=1, staplewex=0,
#                   whisklwd=2, boxwex=0.7, boxlwd=0.1, medlwd=2.6, pch=19, cex=0.7)
#
# tick <- seq_along(bp$names)
# axis(1, at = tick, labels = FALSE)
# text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
#      # the number that is subtracted.
#      srt = 45, xpd = TRUE, adj = 1, font = 9)
