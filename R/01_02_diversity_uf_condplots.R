
# ------------------------------------------------------- #
##### 1. Conditional boxplot for results presentation #####
# ------------------------------------------------------- #

colnames(ipa_data)
wdata <- cbind(ipa_metrics[,c(1, 3:7)],ipa_data[,c(50:ncol(ipa_data))])
wcwm <- ipa_cwm
summary(wdata)



# ---------------------------------------------------------------------------- #
##### * 1.1. Bird species diversity indices ------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 1.1.1. Using UF1 (original typology) ----
# ________________________________________________

##### ~~~~~~~~~ For the bird species richness ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_richness~wdata$urban_type, outline=TRUE,
                  ylab="Specific richness (Nb. of bird species)",
                  xlab="", las=2,
                  xaxt = "n", # Do not plot the default labels
                  type="n", border="orange", col="tomato",
                  boxwex=0.7, boxcol= "tomato", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                  # same colour as the rest of the box to make it invisible.
                  lty=1, staplewex=0,
                  whisklwd=2, medlwd=2, pch=20, cex=1.25)

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



##### ~~~~~~~~~ For the bird species abundance ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_abund~wdata$urban_type, outline=TRUE,
                        ylab="Birds abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="orange", col="tomato",
                        boxwex=0.7, boxcol= "tomato", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 15, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For the bird species Shannon index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_shannon~wdata$urban_type, outline=TRUE,
                        ylab="Shannon-Wiever index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="orange", col="tomato",
                        boxwex=0.7, boxcol= "tomato", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.15, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For the bird species Simpson index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Simpson index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="orange", col="tomato",
                        boxwex=0.7, boxcol= "tomato", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For the bird species Pielou's evenness index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_evenness~wdata$urban_type, outline=TRUE,
                        ylab="Pielou's evenness index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="orange", col="tomato",
                        boxwex=0.7, boxcol= "tomato", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.03, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





########## * ---------------------- * ##########
##### ** 1.1.2. Using UF3 (hclust typology) ----
# ______________________________________________

##### ~~~~~~~~~ For the bird species richness ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Specific richness (Nb. of bird species)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="palegreen4", col="lightseagreen",
                        boxwex=0.7, boxcol= "lightseagreen", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1.5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For the bird species abundance ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_abund~wdata$urban_type_3, outline=TRUE,
                        ylab="Birds abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="palegreen4", col="lightseagreen",
                        boxwex=0.7, boxcol= "lightseagreen", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 15, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For the bird species Shannon index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_shannon~wdata$urban_type_3, outline=TRUE,
                        ylab="Shannon-Wiever index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="palegreen4", col="lightseagreen",
                        boxwex=0.7, boxcol= "lightseagreen", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.15, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For the bird species Simpson index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Simpson index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="palegreen4", col="lightseagreen",
                        boxwex=0.7, boxcol= "lightseagreen", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For the bird species Pielou's evenness index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")

# end_point = 0.5 + nrow(mtcars) + nrow(mtcars) - 1 #this is the line which does the trick (together with barplot "space = 1" parameter)
bp <- graphics::boxplot(wdata$sp_evenness~wdata$urban_type_3, outline=TRUE,
                        ylab="Pielou's evenness index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="palegreen4", col="lightseagreen",
                        boxwex=0.7, boxcol= "lightseagreen", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)

tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.03, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





############################ ************************************************* ###############################

# ---------------------------------------------------------------------------- #
##### * 1.2. Bird functional diversity indices ---------------------------------
# ---------------------------------------------------------------------------- #
##### ** 1.2.1. Using UF1 (original typology) ----
# ________________________________________________

##### ~~~~~~~~~ For Rao's functional diversity index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$rao_q~wdata$urban_type, outline=TRUE,
                        ylab="Rao's quadratic entropy index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For Rao's functional redundancy index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_redund~wdata$urban_type, outline=TRUE,
                        ylab="Functional redundancy",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.015, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For Rao's phylogenetic diversity index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$rao_phy~wdata$urban_type, outline=TRUE,
                        ylab="Rao's phylogenetic diversity index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For Rao's phylogenetic redundancy index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$phy_redund~wdata$urban_type, outline=TRUE,
                        ylab="Phylogenetic redundancy",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional richness index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_richness~wdata$urban_type, outline=TRUE,
                        ylab="Functional richness (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional evenness index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_evenness~wdata$urban_type, outline=TRUE,
                        ylab="Functional evenness (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.03, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional diversity index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_diversity~wdata$urban_type, outline=TRUE,
                        ylab="Functional diversity (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional dispersion index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_dispersion~wdata$urban_type, outline=TRUE,
                        ylab="Functional dispersion (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="tan1", col="indianred1",
                        boxwex=0.7, boxcol= "indianred1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.02, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





########## * ---------------------- * ##########
##### ** 1.2.2. Using UF3 (hclust typology) ----
# ______________________________________________

##### ~~~~~~~~~ For Rao's functional diversity index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$rao_q~wdata$urban_type_3, outline=TRUE,
                        ylab="Rao's quadratic entropy index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For Rao's functional redundancy index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_redund~wdata$urban_type_3, outline=TRUE,
                        ylab="Functional redundancy",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.015, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For Rao's phylogenetic diversity index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$rao_phy~wdata$urban_type_3, outline=TRUE,
                        ylab="Rao's phylogenetic diversity index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For Rao's phylogenetic redundancy index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$phy_redund~wdata$urban_type_3, outline=TRUE,
                        ylab="Phylogenetic redundancy",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional richness index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Functional richness (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional evenness index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_evenness~wdata$urban_type_3, outline=TRUE,
                        ylab="Functional evenness (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.03, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional diversity index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_diversity~wdata$urban_type_3, outline=TRUE,
                        ylab="Functional diversity (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~ For DF functional dispersion index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fun_dispersion~wdata$urban_type_3, outline=TRUE,
                        ylab="Functional dispersion (FD)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="darkgreen", col="aquamarine4",
                        boxwex=0.7, boxcol= "aquamarine4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.02, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





############################ ************************************************* ###############################

# ---------------------------------------------------------------------------- #
##### * 2.1. Bird functional groups diversity ----------------------------------
# ---------------------------------------------------------------------------- #
##### ** 2.1.1. Using UF1 (original typology) ----
# ________________________________________________

##### *** 2.1.1.1. Trophic guild diversity ----
##### ~~~~~~~~~~~ Carnivorous species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$carni_richness/wdata$sp_richness~wdata$urban_type, outline=TRUE,
                        ylab="Carnivore species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$carni_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Carnivore species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$carni_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Carnivore species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Herbivorous species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$herbi_richness~wdata$urban_type, outline=TRUE,
                        ylab="Herbivore species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$herbi_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Herbivore species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$herbi_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Herbivore species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Omnivorous species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$omni_richness~wdata$urban_type, outline=TRUE,
                        ylab="Omnivore species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$omni_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Omnivore species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$omni_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Omnivore species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.1.2. Nesting guild diversity ----
##### ~~~~~~~~~~~ Cavity nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ncav_richness~wdata$urban_type, outline=TRUE,
                        ylab="Cavity nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ncav_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Cavity nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ncav_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Cavity nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Tree nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ntree_richness~wdata$urban_type, outline=TRUE,
                        ylab="Tree nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ntree_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Tree nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ntree_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Tree nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Shrub nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nshrub_richness~wdata$urban_type, outline=TRUE,
                        ylab="Shrub nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nshrub_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Shrub nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nshrub_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Shrub nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Building nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nbuild_richness~wdata$urban_type, outline=TRUE,
                        ylab="Building nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nbuild_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Building nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nbuild_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Building nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Ground nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nground_richness~wdata$urban_type, outline=TRUE,
                        ylab="Ground nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nground_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Ground nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nground_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Ground nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.1.3. Foraging guild diversity ----
##### ~~~~~~~~~~~ Exclusive ground foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fexground_richness~wdata$urban_type, outline=TRUE,
                        ylab="Exclusive ground foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fexground_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Exclusive ground foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fexground_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Exclusive ground foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Ground foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fground_richness~wdata$urban_type, outline=TRUE,
                        ylab="Ground foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fground_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Ground foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fground_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Ground foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Lower strata foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$flower_richness~wdata$urban_type, outline=TRUE,
                        ylab="Lower strata foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$flower_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Lower strata foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$flower_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Lower strata foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Upper strata foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fupper_richness~wdata$urban_type, outline=TRUE,
                        ylab="Upper strata foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fupper_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Upper strata foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fupper_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Upper strata foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.1.4. Habitat preferences diversity ----
##### ~~~~~~~~~~~ Forest or woodland species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_forest_richness~wdata$urban_type, outline=TRUE,
                        ylab="Forest or woodland species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_forest_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Forest or woodland species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_forest_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Forest or woodland species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Shrub or grassland species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_shrub_richness~wdata$urban_type, outline=TRUE,
                        ylab="Shrub or grassland species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_shrub_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Shrub or grassland species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_shrub_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Shrub or grassland species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Rupestrian species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_rupest_richness~wdata$urban_type, outline=TRUE,
                        ylab="Rupestrian species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_rupest_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Rupestrian species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_rupest_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Rupestrian species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.1.5. Social behaviour guild diversity ----
##### ~~~~~~~~~~~ Solitary species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_solit_richness~wdata$urban_type, outline=TRUE,
                        ylab="Solitary species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_solit_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Solitary species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_solit_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Solitary species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Gregarious species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_greg_richness~wdata$urban_type, outline=TRUE,
                        ylab="Gregarious species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_greg_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Gregarious species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_greg_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Gregarious species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Small grouped species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_smallgrp_richness~wdata$urban_type, outline=TRUE,
                        ylab="Small grouped species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_smallgrp_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Small grouped species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_smallgrp_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Small grouped species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.1.6. Migratory guild diversity ----
##### ~~~~~~~~~~~ Non-migratory species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_no_richness~wdata$urban_type, outline=TRUE,
                        ylab="Non-migratory species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1.2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_no_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Non-migratory species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_no_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Non-migratory species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Migratory species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_yes_richness~wdata$urban_type, outline=TRUE,
                        ylab="Migratory species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_yes_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Migratory species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_yes_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Migratory species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.1.7. Vulnerable birds diversity ----
##### ~~~~~~~~~~~ NTVU species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$conserv_richness~wdata$urban_type, outline=TRUE,
                        ylab="Vulnerable species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.4, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$conserv_abund~wdata$urban_type, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Vulnerable species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$conserv_simpson~wdata$urban_type, outline=TRUE,
                        ylab="Vulnerable species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="hotpink2", col="rosybrown1",
                        boxwex=0.7, boxcol= "rosybrown1", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





########## * ---------------------- * ##########
##### ** 2.1.2. Using UF3 (hclust typology) ----
# ______________________________________________

##### *** 2.1.2.1. Trophic guild diversity ----
##### ~~~~~~~~~~~ Carnivorous species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$carni_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Carnivore species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$carni_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Carnivore species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$carni_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Carnivore species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Herbivorous species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$herbi_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Herbivore species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$herbi_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Herbivore species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$herbi_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Herbivore species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Omnivorous species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$omni_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Omnivore species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$omni_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Omnivore species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$omni_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Omnivore species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.2.2. Nesting guild diversity ----
##### ~~~~~~~~~~~ Cavity nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ncav_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Cavity nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ncav_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Cavity nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ncav_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Cavity nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Tree nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ntree_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Tree nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ntree_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Tree nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$ntree_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Tree nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Shrub nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nshrub_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Shrub nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nshrub_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Shrub nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nshrub_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Shrub nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Building nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nbuild_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Building nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nbuild_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Building nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nbuild_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Building nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Ground nesters ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nground_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Ground nesters richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nground_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Ground nesters abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$nground_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Ground nesters diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.2.3. Foraging guild diversity ----
##### ~~~~~~~~~~~ Exclusive ground foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fexground_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Exclusive ground foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fexground_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Exclusive ground foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fexground_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Exclusive ground foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Ground foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fground_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Ground foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fground_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Ground foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fground_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Ground foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.03, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Lower strata foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$flower_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Lower strata foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$flower_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Lower strata foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$flower_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Lower strata foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.06, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Upper strata foragers ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fupper_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Upper strata foragers richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fupper_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Upper strata foragers abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$fupper_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Upper strata foragers diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.2.4. Habitat preferences diversity ----
##### ~~~~~~~~~~~ Forest or woodland species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_forest_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Forest or woodland species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1.2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_forest_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Forest or woodland species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_forest_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Forest or woodland species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Shrub or grassland species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_shrub_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Shrub or grassland species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_shrub_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Shrub or grassland species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_shrub_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Shrub or grassland species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Rupestrian species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_rupest_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Rupestrian species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_rupest_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Rupestrian species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$hab_rupest_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Rupestrian species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.2.5. Social behaviour guild diversity ----
##### ~~~~~~~~~~~ Solitary species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_solit_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Solitary species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_solit_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Solitary species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_solit_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Solitary species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Gregarious species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_greg_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Gregarious species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_greg_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Gregarious species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_greg_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Gregarious species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Small grouped species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_smallgrp_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Small grouped species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_smallgrp_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Small grouped species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$soc_smallgrp_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Small grouped species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.2.6. Migratory guild diversity ----
##### ~~~~~~~~~~~ Non-migratory species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_no_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Non-migratory species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 1.2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_no_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Non-migratory species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_no_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Non-migratory species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ Migratory species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_yes_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Migratory species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_yes_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Migratory species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 2, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$migrat_yes_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Migratory species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





##### *** 2.1.2.7. Vulnerable birds diversity ----
##### ~~~~~~~~~~~ NTVU species ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$conserv_richness~wdata$urban_type_3, outline=TRUE,
                        ylab="Vulnerable species richness",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.4, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$conserv_abund~wdata$urban_type_3, outline=FALSE, # OUTLIERS NOT PLOTTED!
                        ylab="Vulnerable species abundance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$conserv_simpson~wdata$urban_type_3, outline=TRUE,
                        ylab="Vulnerable species diversity (Simpson index)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="olivedrab3", col="khaki2",
                        boxwex=0.7, boxcol= "khaki2", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)





############################ ************************************************* ###############################

# ---------------------------------------------------------------------------- #
##### * 3.1. CWM traits --------------------------------------------------------
# ---------------------------------------------------------------------------- #

# From now on, I won't plot every combination of CWM trait with UF typologies, but only those that show
# interesting patterns. If some CMW variables are not present in the plotting-code chunks, it means that
# they were deemed un-exploitable.

##### ** 3.1.1. Using both UF typologies (UF1 and UF3) ----
# _________________________________________________________

##### ~~~~~~~~~~~ CWM nesting guild ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_building~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of building nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_building~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of building nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_cavity~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of cavity nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.07, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_cavity~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of cavity nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.07, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_treetop~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of treetop nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_treetop~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of treetop nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_shrub~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of shrub nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$nesting_pref_shrub~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of shrub nesters",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM trophic level categories ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$trophic_level_carnivore~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of carnivore birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$trophic_level_carnivore~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of carnivore birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$trophic_level_herbivore~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of herbivore birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$trophic_level_herbivore~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of herbivore birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$trophic_level_omnivore~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of omnivore birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$trophic_level_omnivore~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of omnivore birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM foraging behaviour ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_hawking~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of hawking birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_hawking~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of hawking birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_scratching~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of scratching birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.07, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_scratching~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of scratching birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.08, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_gleaning~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of gleaning birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.07, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_gleaning~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of gleaning birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.08, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_probing~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of probing birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.01, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_behaviour_probing~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of probing birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.01, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM foraging strata ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_air~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of air foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_air~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of air foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.09, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_ground~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of ground foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_ground~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of ground foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_ground_and_lower_strata~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of ground and lower strata foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.08, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_ground_and_lower_strata~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of ground and lower strata foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.08, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_lower_strata~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of lower strata foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.04, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_lower_strata~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of lower strata foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_midhigh_strata~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of mid-height strata foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.04, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$foraging_strata_midhigh_strata~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of mid-height strata foraging birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM morphology and reproduction ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$morpho~wdata$urban_type, outline=TRUE,
                        ylab="CWM morphology value",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.4, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$morpho~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM morphology value",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$brain_mass~wdata$urban_type, outline=TRUE,
                        ylab="CWM brain mass",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.4, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$brain_mass~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM brain mass",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$brain_ratio~wdata$urban_type, outline=TRUE,
                        ylab="CWM brain ratio",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.002, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$brain_ratio~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM brain ratio",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.0025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$beak_size~wdata$urban_type, outline=TRUE,
                        ylab="CWM beak size value",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$beak_size~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM beak size value",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hwi~wdata$urban_type, outline=TRUE,
                        ylab="CWM hand-wing index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hwi~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM hand-wing index",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 3, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$kipps_distance~wdata$urban_type, outline=TRUE,
                        ylab="CWM kipps distance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$kipps_distance~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM kipps distance",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 5, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$clutch_nb~wdata$urban_type, outline=TRUE,
                        ylab="CWM clutch numbers (per year)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.25, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$clutch_nb~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM clutch numbers (per year)",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.25, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$repro~wdata$urban_type, outline=TRUE,
                        ylab="CWM reproductive strategy",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.25, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$repro~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM reproductive strategy",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.25, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM habitat preferences ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_forest~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of forest birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_forest~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of forest birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_woodland~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of woodland birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_woodland~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of woodland birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_rupestrian~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of rupestrian birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_rupestrian~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of rupestrian birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_shrubland~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of shrubland birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_shrubland~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of shrubland birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_grassland~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of grassland birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$habitat_grassland~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of grassland birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.025, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM habitat density preferences ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hab_density_dense~wdata$urban_type, outline=TRUE,
                        ylab="CWM abund. of densely wooded habitat birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hab_density_dense~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abund. of densely wooded habitat birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hab_density_semi_open~wdata$urban_type, outline=TRUE,
                        ylab="CWM abund. of semi-open habitat birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hab_density_semi_open~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abund. of semi-open habitat birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hab_density_open~wdata$urban_type, outline=TRUE,
                        ylab="CWM abund. of open habitat birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$hab_density_open~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abund. of open habitat birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.1, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM primary lifestyle ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_insessorial~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of insessorial birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_insessorial~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of insessorial birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_aerial~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of aerial birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_aerial~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of aerial birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_terrestrial~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of terrestrial birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_terrestrial~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of terrestrial birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_generalist~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of generalist lifestyle birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$prim_lifestyle_generalist~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of generalist lifestyle birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)



##### ~~~~~~~~~~~ CWM social behaviour ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$social_behaviour_solitary~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of solitary birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$social_behaviour_solitary~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of solitary birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$social_behaviour_gregarious~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of gregarious birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$social_behaviour_gregarious~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of gregarious birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$social_behaviour_small_groups~wdata$urban_type, outline=TRUE,
                        ylab="CWM abundance of small grouped birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="sandybrown", col="violetred4",
                        boxwex=0.7, boxcol= "violetred4", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)

par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(8, 4, 0.5, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wcwm$social_behaviour_small_groups~wdata$urban_type_3, outline=TRUE,
                        ylab="CWM abundance of small grouped birds",
                        xlab="", las=2,
                        xaxt = "n", # Do not plot the default labels
                        type="n", border="yellowgreen", col="darkcyan",
                        boxwex=0.7, boxcol= "darkcyan", boxlwd=0.01, # I cannot set 'boxlwd=0' so I assign it the
                        # same colour as the rest of the box to make it invisible.
                        lty=1, staplewex=0,
                        whisklwd=2, medlwd=2, pch=20, cex=1.25)
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(x = tick, y = par("usr")[3] - 0.05, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)
