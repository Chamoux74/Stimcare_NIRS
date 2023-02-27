selectfroidpatch1 <- selectfroidpatch[-c (13, 18), ]

selectbasepatchslope1 <- as.data.frame(selectbasepatch[ , 2])
colnames(selectbasepatchslope1) <- "slopebasepatch"
base <- c("patchbase")
selectbasepatchslope1 <- cbind(selectbasepatchslope1 ,base)
colnames(selectbasepatchslope1) <- c("slope" , "moment")

selectfroidpatchslope1 <- as.data.frame(selectfroidpatch1[ , 2])
colnames(selectfroidpatchslope1) <- "slopepatchfroid"
froid <- c("froid")
selectfroidpatchslope1 <- cbind(selectfroidpatchslope1 , froid)
colnames(selectfroidpatchslope1) <- c("slope" , "moment")

test <- rbind(selectbasepatchslope1 , selectfroidpatchslope1)

plot1 <- ggboxplot(test , x = "moment", y = "slope", 
                   color = "moment", palette = c("#00AFBB" , "#E7B800"),
                   order = c("patchbase" , "froid"),
                   ylab = "pente", xlab = "moment") 


plot1 

x <- selectbasepatchslope1$slope
y <- selectfroidpatchslope1$slope

t.test(x , y , paired = TRUE)
