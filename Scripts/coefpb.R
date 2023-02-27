library(tidyverse)
library(dataPreparation)
library(ggplot2)
library(ggpmisc)
library(plyr)
library(dplyr)

datasimpleplacebo <- lapply(dfnirsplacebo, '[' , c("Rx1Tx1_Tx2_Tx3_TSI"))

linenumbersplacebo <- column_to_rownames(linenumbersplacebo, var = "nom")
linenumbersplacebo <- linenumbersplacebo[sort(rownames(linenumbersplacebo)), ]
datasimpleplacebo <- datasimpleplacebo[sort(names(datasimpleplacebo))]

dftestplacebo <-
  mapply(function(x, row_select) {
    x[c(row_select:nrow(x)), ]
  },
  x = datasimpleplacebo,
  row_select = linenumbersplacebo) %>% 
  lapply(as.data.frame)

curveplacebo <- function(inputdata) {
  ggplot(inputdata, aes(x =  1:length(inputdata[[1]]), y = inputdata[[1]])) +
    geom_point(color = "green" , lwd = 1)
}

plotsplacebo <- lapply(dftestplacebo, curveplacebo)
plotsplacebo$GabrielMercuriPOSTPB.Rx1Tx1_Tx2_Tx3_TSI

finplacebo <- function(inputdata) {
  inputdata = inputdata[0:20,]
}

datafinplacebo <- lapply(dftestplacebo , finplacebo)

temps <-
  c(0.5,
    1,
    1.5,
    2,
    2.5,
    3,
    3.5,
    4,
    4.5,
    5,
    5.5,
    6,
    6.5,
    7,
    7.5,
    8,
    8.5,
    9,
    9.5,
    10)
temps <- as.data.frame(temps)

tempsplacebo <- function(inputdata) {
  inputdata = cbind(inputdata , temps)
}
datafin_tpsplacebo <- lapply(datafinplacebo , tempsplacebo)

curveplacebo2 <- function(inputdata) {
  ggplot(inputdata , aes(x = temps , y = inputdata)) +
    geom_point(
      size = 4 ,
      shape = 1 ,
      color = 'black' ,
      lwd = 3
    ) +
    geom_line(color = "green" , lwd = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                color = "red") +
    stat_poly_line() +
    stat_poly_eq(aes(label = after_stat(eq.label))) +
    stat_poly_eq(label.y = 0.9)
}

plotsplacebo2 <- lapply(datafin_tpsplacebo , curveplacebo2)
plotsplacebo2$GabrielMercuriPOSTPB.Rx1Tx1_Tx2_Tx3_TSI


coefsplacebo <- list()
listrplacebo <- list()
for (i in 1:length(datafin_tpsplacebo)) {
  fit <- lm(inputdata ~ temps, data = datafin_tpsplacebo[[i]])
  coefsplacebo[[i]] <- coef(fit)
  listrplacebo[[i]] <- summary(fit)$r.squared
}

names(coefsplacebo) <- names(datafin_tpsplacebo)
names(listrplacebo) <- names(datafin_tpsplacebo)
coefsplacebo <- as.data.frame(coefsplacebo)
listrplacebo <- as.data.frame(listrplacebo)
datacoefplacebo <- rbind(coefsplacebo , listrplacebo)
row.names(datacoefplacebo) <- c("intercept" , "slope" , "rsquared")

datacoefplacebo <- as.data.frame(t(datacoefplacebo))
