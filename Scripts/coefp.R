library(tidyverse)
library(dataPreparation)
library(ggplot2)
library(ggpmisc)
library(plyr)
library(dplyr)

datasimplepatch <- lapply(dfnirspatch, '[' , c("Rx1Tx1_Tx2_Tx3_TSI"))


linenumberspatch <- column_to_rownames(linenumberspatch, var = "nom")
linenumberspatch <- linenumberspatch[sort(rownames(linenumberspatch)), ]
datasimplepatch <- datasimplepatch[sort(names(datasimplepatch))]

dftestpatch <-
  mapply(function(x, row_select) {
    x[c(row_select:nrow(x)), ]
  },
  x = datasimplepatch,
  row_select = linenumberspatch) %>% 
  lapply(as.data.frame)

curvepatch <- function(inputdata) {
  ggplot(inputdata, aes(x =  1:length(inputdata[[1]]), y = inputdata[[1]])) +
    geom_point(color = "green" , lwd = 1)
}

plotspatch <- lapply(dftestpatch, curvepatch)
plotspatch$GabrielMercuriPOSTP.Rx1Tx1_Tx2_Tx3_TSI

finpatch <- function(inputdata) {
  inputdata = inputdata[0:20,]
}

datafinpatch <- lapply(dftestpatch , finpatch)

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

tempspatch <- function(inputdata) {
  inputdata = cbind(inputdata , temps)
}
datafin_tpspatch <- lapply(datafinpatch , tempspatch)

curvepatch2 <- function(inputdata) {
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

plotspatch3 <- lapply(datafin_tpspatch , curvepatch2)
plotspatch3$GabrielMercuriPOSTP.Rx1Tx1_Tx2_Tx3_TSI

coefspatch <- list()
listrpatch <- list()
for (i in 1:length(datafin_tpspatch)) {
  fit <- lm(inputdata ~ temps, data = datafin_tpspatch[[i]])
  coefspatch[[i]] <- coef(fit)
  listrpatch[[i]] <- summary(fit)$r.squared
}

names(coefspatch) <- names(datafin_tpspatch)
names(listrpatch) <- names(datafin_tpspatch)
coefspatch <- as.data.frame(coefspatch)
listrpatch <- as.data.frame(listrpatch)
datacoefpatch <- rbind(coefspatch , listrpatch)
row.names(datacoefpatch) <- c("intercept" , "slope" , "rsquared")

datacoefpatch <- as.data.frame(t(datacoefpatch))

