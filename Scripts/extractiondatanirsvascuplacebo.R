library(readxl)
library(readr)

nirsvascupb <-
  list.files(
    path = "C:/Users/maxch/Git/NIRS_vascu/Data/PLACEBO",
    pattern = "\\.xlsx$",
    all.files = TRUE,
    full.names = TRUE
  )
dfnirspb <- lapply (nirsvascupb, read_xlsx)
names(dfnirspb) <- tools::file_path_sans_ext(basename(nirsvascupb))
dfnirspb1 <- lapply(dfnirspb, function(x) {
  x <- x[-c(1:50),]
})
ChangeNames <- function(x) {
  names(x) <-
    c(
      "Sample_number",
      "Rx1Tx1_O2Hb",
      "Rx1Tx1_HHb",
      "Rx1Tx1_tHb",
      "Rx1Tx2_O2Hb",
      "Rx1Tx2_HHb",
      "Rx1Tx2_tHb",
      "Rx1Tx3_O2Hb",
      "Rx1Tx3_HHb",
      "Rx1Tx3_tHb",
      "Rx1Tx1_Tx2_Tx3_TSI",
      "Rx1Tx1_Tx2_Tx3_TSIFitFactor",
      "Event"
    )
  return(x)
}
dfnirsplacebo <- lapply(dfnirspb1, ChangeNames)

linenumbersplacebo <- read.csv("Data/linenumberpb.csv")


