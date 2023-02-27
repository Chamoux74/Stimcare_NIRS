selectpostpatch <- datacoefpatch[rownames(datacoefpatch) %like% "POST",]
selectpostplacebo <- datacoefplacebo[rownames(datacoefplacebo) %like% "POST",]

filtered_dfplacebo <- selectpostplacebo[-grep("POST48P", rownames(selectpostplacebo)), ]
filtered_dfplacebo <- filtered_dfplacebo[-c(5), ]
selectpostplacebo <- filtered_dfplacebo
filtered_dfpatch <- selectpostpatch[-grep("POST48P", rownames(selectpostpatch)), ]
filtered_dfpatch <- filtered_dfpatch[-c(5, 20), ]
selectpostpatch <- filtered_dfpatch

selectpostpatchslope <- as.data.frame(selectpostpatch[ , 2])
colnames(selectpostpatchslope) <- "slopepatch"
patch <- c("postpatch")
selectpostpatchslope <- cbind(selectpostpatchslope ,patch)
colnames(selectpostpatchslope) <- c("slope" , "groupe")
selectpostpatchslope <- selectpostpatchslope[-c(3 , 4 , 5 , 6,  11), ]

selectpostplaceboslope <- as.data.frame(selectpostplacebo[ , 2])
colnames(selectpostplaceboslope) <- "slopeplacebo"
postplacebo <- c("postplacebo")
selectpostplaceboslope <- cbind(selectpostplaceboslope , postplacebo)
colnames(selectpostplaceboslope) <- c("slope" , "groupe")
selectpostplaceboslope <- selectpostplaceboslope [-c(3 , 4 , 5 , 6,  11), ]

hist(selectpostpatchslope$slope)
hist(selectpostplaceboslope$slope)

shapiro.test(selectpostplaceboslope$slope)
shapiro.test(selectpostpatchslope$slope)

wilcox.test(selectbasepatch$slope , selectbaseplacebo$slope)

x <- selectpostpatchslope$slope
y <- selectpostplaceboslope$slope

t.test(x , y , paired = TRUE)

test3 <- rbind(selectpostpatchslope , selectpostplaceboslope)

plotpost <- ggboxplot(test3 , x = "groupe", y = "slope", notch = TRUE , 
                   color = "groupe", palette = c("#00AFBB" , "#E7B800"),
                   order = c("postpatch" , "postplacebo"),
                   add = "jitter" ,
                   ylab = "pente", xlab = "Groupe") +
  stat_summary(geom = "crossbar", fun = mean , linetype = "dashed") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1
  ) +
  labs(title = "TSIPost")

plotpost
