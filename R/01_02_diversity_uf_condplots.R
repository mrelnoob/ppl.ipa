
# ------------------------------------------------------- #
##### 1. Conditional boxplot for results presentation #####
# ------------------------------------------------------- #

colnames(ipa_data)
wdata <- cbind(ipa_metrics[,c(1, 3:7)],ipa_data[,c(50:ncol(ipa_data))])
summary(wdata)



# ---------------------------------------------------------------------------- #
##### * 1.1. Bird species diversity indices ------------------------------------
# ---------------------------------------------------------------------------- #
##### ** 1.1.1. Using UF1 (original typology) ----
# ________________________________________________

##### ~~~~~~~~~ For the bird species richness ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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





##### ** 1.1.2. Using UF3 (hclust typology) ----
# ______________________________________________

##### ~~~~~~~~~ For the bird species richness ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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





##### ** 1.2.2. Using UF3 (hclust typology) ----
# ______________________________________________

##### ~~~~~~~~~ For Rao's functional diversity index ----
par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
    bty="l", fg="gray4")
bp <- graphics::boxplot(wdata$carni_richness~wdata$urban_type, outline=TRUE,
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
text(x = tick, y = par("usr")[3] - 0.8, labels = bp$names, # For the distance to the axis, you have to tune
     # the number that is subtracted.
     srt = 45, xpd = TRUE, adj = 1, font = 3)


par(font.lab = 4, font.axis=6, font.lab= 2,
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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
    mar = c(10, 4, 3, 2) + 0.5,
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






