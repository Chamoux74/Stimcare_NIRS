library(tidyverse)
library(ggpubr)
library(rstatix)
library(data.table)
library(patchwork)

selectbasepatch <- datacoefpatch[rownames(datacoefpatch) %like% "BASE",]
selectbaseplacebo <- datacoefplacebo[rownames(datacoefplacebo) %like% "BASE",]

selectbasepatchslope <- as.data.frame(selectbasepatch[ , 2])
colnames(selectbasepatchslope) <- "slopepatch"
patch <- c("patch")
selectbasepatchslope <- cbind(selectbasepatchslope ,patch)
colnames(selectbasepatchslope) <- c("slope" , "groupe")
selectbasepatchslope <- selectbasepatchslope[-c(1, 3 , 4 , 7, 8) ,]

selectbaseplaceboslope <- as.data.frame(selectbaseplacebo[ , 2])
colnames(selectbaseplaceboslope) <- "slopeplacebo"
placebo <- c("placebo")
selectbaseplaceboslope <- cbind(selectbaseplaceboslope ,placebo)
colnames(selectbaseplaceboslope) <- c("slope" , "groupe")
selectbaseplaceboslope <- selectbaseplaceboslope[-c(1, 3 , 4 , 7, 8) ,]

hist(selectbasepatch$slope)
hist(selectbaseplacebo$slope)

shapiro.test(selectbasepatch$slope)
shapiro.test(selectbaseplacebo$slope)

x <- selectbasepatchslope$slope
y <- selectbaseplaceboslope$slope

t.test(x , y , paired = TRUE)

test1 <- rbind(selectbasepatchslope , selectbaseplaceboslope)

plotB <- ggboxplot(test1 , x = "groupe", y = "slope", notch = TRUE ,
          color = "groupe", palette = c("#00AFBB" , "#E7B800"),
          order = c("patch" , "placebo"),
          add = "jitter"  , 
          ylab = "pente", xlab = "Groupe") +
  stat_summary(geom = "crossbar", fun = mean , linetype = "dashed") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1
  ) +
  labs(title = "TSIBase")

plotB 
