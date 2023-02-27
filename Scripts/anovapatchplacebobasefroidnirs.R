selectfroidpatchslope1 <- selectfroidpatchslope[-c(4), ]
selectbasepatchslope1 <- selectbasepatchslope[-c(13),]
selectfroidplaceboslope1 <- selectfroidplaceboslope[-c(4),]
selectbaseplaceboslope1 <- selectbaseplaceboslope[-c(13),]

sujet <- c(1:13)

selectbasepatchslope1 <- as.data.frame(selectbasepatchslope1[ , 1])
colnames(selectbasepatchslope1) <- "slopepatchbase"
basepatch <- c("basepatch")
selectbasepatchslope1 <- cbind(selectbasepatchslope1 ,basepatch , sujet)
colnames(selectbasepatchslope1) <- c("slope" , "instant_mesure" , "sujet")

selectfroidpatchslope1 <- as.data.frame(selectfroidpatchslope1[ , 1])
colnames(selectfroidpatchslope1) <- "slopepatchfroid"
froidpatch <- c("froidpatch")
selectfroidpatchslope1 <- cbind(selectfroidpatchslope1 , froidpatch, sujet)
colnames(selectfroidpatchslope1) <- c("slope" , "instant_mesure" , "sujet")

selectbaseplaceboslope1 <- as.data.frame(selectbaseplaceboslope1[ , 1])
colnames(selectbaseplaceboslope1) <- "slopepatchpost"
baseplacebo <- c("baseplacebo")
selectbaseplaceboslope1 <- cbind(selectbaseplaceboslope1 , baseplacebo , sujet)
colnames(selectbaseplaceboslope1) <- c("slope" , "instant_mesure" , "sujet")

selectfroidplaceboslope1 <- as.data.frame(selectfroidplaceboslope1[ , 1])
colnames(selectfroidplaceboslope1) <- "froidplacebo"
froidplacebo <- c("froidplacebo")
selectfroidplaceboslope1 <- cbind(selectfroidplaceboslope1 , froidplacebo , sujet)
colnames(selectfroidplaceboslope1) <- c("slope" , "instant_mesure" , "sujet")


shapiro.test(selectbasepatchslope1$slope)
shapiro.test(selectfroidpatchslope1$slope)
shapiro.test(selectfroidplaceboslope1$slope)
shapiro.test(selectbaseplaceboslope1$slope)


test4 <- rbind(selectbasepatchslope1 , selectfroidpatchslope1 , selectbaseplaceboslope1 , selectfroidplaceboslope1)

plotano1 <- ggboxplot(test4 , x = "instant_mesure", y = "slope", 
                   color = "instant_mesure", palette = c("#0416f5" , "#b00000" , "#19a3e8" , "#fd4c4c"),
                   order = c("baseplacebo" , "basepatch" , "froidplacebo" , "froidpatch"),
                   add = "jitter" ,
                   ylab = "pente", xlab = "instant_mesure") +
  stat_summary(geom = "crossbar", fun = mean , linetype = "dashed") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1
  ) +
  labs(title = "TSIPatch_placebo_froid_patch")

plotano1


anova_test(data = test4 , dv = slope , wid = sujet , within = instant_mesure)

