library(readxl)
library(readr)


nirsvascu <-
  list.files(
    path = "C:/Users/maxch/Git/NIRS_vascu/Data//PATCH",
    pattern = "\\.xlsx$",
    all.files = TRUE,
    full.names = TRUE
  )
dfnirs <- lapply (nirsvascu, read_xlsx)
names(dfnirs) <- tools::file_path_sans_ext(basename(nirsvascu))
dfnirs1 <- lapply(dfnirs, function(x) {
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
dfnirspatch <- lapply(dfnirs1, ChangeNames)

linenumberspatch <- read.csv("Data/linenumberp.csv")
