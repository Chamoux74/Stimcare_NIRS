library(tidyverse)
library(ggpubr)
library(rstatix)
library(data.table)
library(patchwork)

selectfroidpatch <- datacoefpatch[rownames(datacoefpatch) %like% "FROID",]
selectfroidplacebo <- datacoefplacebo[rownames(datacoefplacebo) %like% "FROID",]

selectfroidpatchslope <- as.data.frame(selectfroidpatch[ , 2])
colnames(selectfroidpatchslope) <- "slopepatch"
patch <- c("patch")
selectfroidpatchslope <- cbind(selectfroidpatchslope ,patch)
colnames(selectfroidpatchslope) <- c("slope" , "groupe")
selectfroidpatchslope <- selectfroidpatchslope[-c(1, 3, 7, 8, 13, 14, 18) ,]

selectfroidplaceboslope <- as.data.frame(selectfroidplacebo[ , 2])
colnames(selectfroidplaceboslope) <- "slopeplacebo"
placebo <- c("placebo")
selectfroidplaceboslope <- cbind(selectfroidplaceboslope ,placebo)
colnames(selectfroidplaceboslope) <- c("slope" , "groupe")
selectfroidplaceboslope <- selectfroidplaceboslope[-c(1, 3, 7, 8 , 13) ,]

hist(selectfroidpatch$slope)
hist(selectfroidplacebo$slope)

shapiro.test(selectfroidplacebo$slope)
shapiro.test(selectfroidpatch$slope)

x <- selectfroidpatchslope$slope
y <- selectfroidplaceboslope$slope

t.test(x , y , paired = TRUE)

test <- rbind(selectfroidpatchslope , selectfroidplaceboslope)

plotf <- ggboxplot(test , x = "groupe", y = "slope", notch = TRUE , bxp.errorbar = FALSE ,  
                   color = "groupe", palette = c("#00AFBB" , "#E7B800"),
                   order = c("patch" , "placebo"),
                   add = "jitter"  , 
                   ylab = "pente", xlab = "Groupe") +
stat_summary(geom = "crossbar", fun = mean , linetype = "dashed") +
  stat_summary(geom = "errorbar" , fun.data = mean_sd , colour = "grey" , linetype = "dotted" , size = 1) +
  labs(title = "TSIFroid")

summary(selectfroidpatchslope)
sd(selectfroidpatchslope$slope)

plotf 
