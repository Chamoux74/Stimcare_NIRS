selectbasepatchslope2 <- selectbasepatchslope1[-c(2, 3, 14), ]
selectbaseplaceboslope2 <- selectbaseplaceboslope1[-c(2, 3 , 14),]
selectpostpatchslope1 <- selectpostpatchslope[-c(1 , 11),]
selectpostplaceboslope1 <- selectpostplaceboslope[-c(1, 11),]
selectpost48patchslope1 <- selectpost48patchslope[-c(2,3,12),]
selectpost48placeboslope1 <- selectpost48placeboslope[-c(2,3,12),]

sujet1 <- c(1:11)

selectbaseplaceboslope2 <- as.data.frame(selectbaseplaceboslope2[ , 1])
colnames(selectbaseplaceboslope2) <- "slopeplacebobase"
baseplacebo <- c("baseplacebo")
selectbaseplaceboslope2 <- cbind(selectbaseplaceboslope2 , baseplacebo , sujet1)
colnames(selectbaseplaceboslope2) <- c("slope" , "instant_mesure" , "sujet")

selectbasepatchslope2 <- as.data.frame(selectbasepatchslope2[ , 1])
colnames(selectbasepatchslope2) <- "slopeplacebobase"
basepatch <- c("basepatch")
selectbasepatchslope2 <- cbind(selectbasepatchslope2 , basepatch , sujet1)
colnames(selectbasepatchslope2 ) <- c("slope" , "instant_mesure" , "sujet")

selectpostplaceboslope1 <- as.data.frame(selectpostplaceboslope1[ , 1])
colnames(selectpostplaceboslope1) <- "slopeplacebopost"
postplacebo <- c("postplacebo")
selectpostplaceboslope1 <- cbind(selectpostplaceboslope1 , postplacebo , sujet1)
colnames(selectpostplaceboslope1) <- c("slope" , "instant_mesure" , "sujet")

selectpostpatchslope1 <- as.data.frame(selectpostpatchslope1[ , 1])
colnames(selectpostpatchslope1) <- "slopepatchpost"
postpatch <- c("postpatch")
selectpostpatchslope1 <- cbind(selectpostpatchslope1 , postpatch , sujet1)
colnames(selectpostpatchslope1) <- c("slope" , "instant_mesure" , "sujet")

selectpost48patchslope1 <- as.data.frame(selectpost48patchslope1[ , 1])
colnames(selectpost48patchslope1) <- "slopepatchpost48"
post48patch <- c("post48patch")
selectpost48patchslope1 <- cbind(selectpost48patchslope1 , post48patch , sujet1)
colnames(selectpost48patchslope1) <- c("slope" , "instant_mesure" , "sujet")

selectpost48placeboslope1 <- as.data.frame(selectpost48placeboslope1[ , 1])
colnames(selectpost48placeboslope1 ) <- "slopeplacebopost48"
post48placebo <- c("post48placebo")
selectpost48placeboslope1  <- cbind(selectpost48placeboslope1  , post48placebo , sujet1)
colnames(selectpost48placeboslope1 ) <- c("slope" , "instant_mesure" , "sujet")

shapiro.test(selectpostpatchslope1$slope)
shapiro.test(selectpostplaceboslope1$slope)
shapiro.test(selectpost48patchslope1$slope)
shapiro.test(selectpost48placeboslope1$slope)

test5 <-
  rbind(
    selectbaseplaceboslope2 ,
    selectbasepatchslope2  ,
    selectpostpatchslope1 ,
    selectpostplaceboslope1,
    selectpost48patchslope1 ,
    selectpost48placeboslope1
  )

plotexo <- ggboxplot(
  test5 ,
  x = "instant_mesure",
  y = "slope",
  color = "instant_mesure",
  palette = c("#00AFBB" , "#003aff" , "#FC4E07" , "#ff0000" , "#00ff0c" , "#008906"),
  order = c(
    "baseplacebo" ,
    "basepatch" ,
    "postplacebo" ,
    "postpatch" ,
    "post48placebo" ,
    "post48patch"
  ),
  add = "jitter" ,
  ylab = "pente",
  xlab = "instant_mesure" ,
  title = "TSIexercice_placebo_patch"
) +
  stat_summary(geom = "crossbar",
               fun = mean ,
               linetype = "dashed")+
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1
  )


plotexo

anova_test(data = test5 , dv = slope , wid = sujet , within = instant_mesure)
