selectbaseplaceboslope1 <- as.data.frame(selectbaseplacebo[ , 2])
colnames(selectbaseplaceboslope1) <- "slopebasepatch"
baseplacebo <- c("placebobase")
selectbaseplaceboslope1 <- cbind(selectbaseplaceboslope ,baseplacebo)
colnames(selectbaseplaceboslope1) <- c("slope" , "moment")
selectbaseplaceboslope1 <- selectbaseplaceboslope1[, -c(2)]
colnames(selectbaseplaceboslope1) <- c("slope" , "moment")

selectfroidplaceboslope1 <- as.data.frame(selectfroidplacebo[ , 2])
colnames(selectfroidplaceboslope1) <- "slopepatchfroid"
froidplacebo <- c("placebofroid")
selectfroidplaceboslope1 <- cbind(selectfroidplaceboslope1, froidplacebo)
colnames(selectfroidplaceboslope1) <- c("slope" , "moment")

test <- rbind(selectbaseplaceboslope1  , selectfroidplaceboslope1)

plot1 <- ggboxplot(test , x = "moment", y = "slope", 
                   color = "moment", palette = c("#00AFBB" , "#E7B800"),
                   order = c("placebobase" , "placebofroid"),
                   ylab = "pente", xlab = "moment") 


plot1 

x <- selectbaseplaceboslope1$slope
y <- selectfroidplaceboslope1$slope

t.test(x , y , paired = TRUE)
