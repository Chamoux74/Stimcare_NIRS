selectpost48patch <- datacoefpatch[rownames(datacoefpatch) %like% "POST48",]
selectpost48placebo <- datacoefplacebo[rownames(datacoefplacebo) %like% "POST48",]

filtered_dfpatch <- selectpost48patch[-c(6, 8 , 21), ]
selectpost48patch <- filtered_dfpatch

selectpost48patchslope <- as.data.frame(selectpost48patch[ , 2])
colnames(selectpost48patchslope) <- "slopepatch"
patch <- c("patch")
selectpost48patchslope <- cbind(selectpost48patchslope ,patch)
colnames(selectpost48patchslope) <- c("slope" , "groupe")
selectpost48patchslope <- selectpost48patchslope[-c(1, 3, 4, 11) ,]

selectpost48placeboslope <- as.data.frame(selectpost48placebo[ , 2])
colnames(selectpost48placeboslope) <- "slopeplacebo"
placebo <- c("placebo")
selectpost48placeboslope <- cbind(selectpost48placeboslope ,placebo)
colnames(selectpost48placeboslope) <- c("slope" , "groupe")
selectpost48placeboslope <- selectpost48placeboslope[-c(1, 3, 4, 11) ,]

hist(selectpost48patchslope$slope)
hist(selectpost48placeboslope$slope)

shapiro.test(selectpost48placebo$slope)
shapiro.test(selectpost48patch$slope)

x <- selectpost48patchslope$slope
y <- selectpost48placeboslope$slope

t.test(x , y , paired = TRUE)

wilcox.test(selectpost48patchslope$slope , selectpost48placeboslope$slope)


test2 <- rbind(selectpost48patchslope , selectpost48placeboslope)

plotpost48 <- ggboxplot(test2 , x = "groupe", y = "slope", notch = TRUE ,
                   color = "groupe", palette = c("#00AFBB" , "#E7B800"),
                   order = c("patch" , "placebo"),
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
  labs(title = "TSIPost48")

plotpost48
