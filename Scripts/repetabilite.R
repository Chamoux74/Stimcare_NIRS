test6 <- cbind(selectbaseplaceboslope2$slope, selectpost48placeboslope1$slope)
test6 <- as.data.frame(test6)


mean <- as.data.frame(rowMeans(test6))
test6 <- cbind(mean , test6)
diff <- as.data.frame(test6$V1 - test6$V2)

test6 <- cbind(diff , test6)

meandiff <- mean(test6$`test6$V1 - test6$V2`)

lower <- meandiff - 1.96*sd(test6$`test6$V1 - test6$V2`)
upper <- meandiff + 1.96*sd(test6$`test6$V1 - test6$V2`)

colnames(test6) <- c("diff" , "mean" , "base" , "post48")

ggplot(test6, aes(x = mean , y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = meandiff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement")

patchrep <- rbind(selectbasepatchslope2$slope, selectpost48patchslope1$slope)
patchrep <- as.data.frame(t(patchrep))

meanpatch <- as.data.frame(rowMeans(patchrep))
patchrep <- cbind(meanpatch , patchrep)
diffpatch <- as.data.frame(patchrep$V1 - patchrep$V2)

patchrep <- cbind(diffpatch , patchrep)

meandiff <- mean(patchrep$`rowMeans(patchrep)`)

lower <- meandiff - 1.96*sd(patchrep$`patchrep$V1 - patchrep$V2`)
upper <- meandiff + 1.96*sd(patchrep$`patchrep$V1 - patchrep$V2`)

colnames(patchrep) <- c("diff" , "mean" , "base" , "post48")

ggplot(patchrep, aes(x = mean , y = diff)) +
  geom_point(size = 2) +
  geom_hline(yintercept = meandiff) +
  geom_hline(yintercept = lower, color = "red", linetype = "dashed") +
  geom_hline(yintercept = upper, color = "red", linetype = "dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement")

#mesure de repetabilité

test7 <- as.data.frame(cbind(selectbaseplaceboslope2$slope, selectpost48placeboslope1$slope))
colnames(test7) <- c("d1" , "d2")

anova <-aov(test7$d1 ~ test7$d2)
summary(anova)

t.test(test7$d1 , test7$d2)

mean(test4$d1 + test4$d2)

meand1 <- mean(test7$d1)
sd1 <- sd (test7$d1)
meand2 <- mean(test7$d2)
sd2 <- sd(test7$d2)

cvd1 <- sd1/meand1*100
cvd2 <- sd2/meand2*100

ggplot(test4, aes( x = instant_mesure , y = slope )) +
  geom_line(aes(x = instant_mesure , group = sujet , color = as.factor(sujet)) , size = 0.8 , position = "identity") +
  geom_point(
    aes(x = instant_mesure , group = sujet),
    shape = 21,
    colour = "black",
    size = 2,
    position = "identity"
  ) +
  geom_boxplot(
    aes(x = instant_mesure , y = slope) ,
    width = .25,
    fill = "white" , alpha = 0.3
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    width = 0.1
  ) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    size = 1,
    position = "identity"
  ) +
  scale_color_manual( 
    values = c(
      "#00AFBB" ,
                "#FC4E07" ,
                "purple" ,
                "#0416f5" ,
                "#b00000" ,
                "#19a3e8" ,
                "#fd4c4c" ,
                "#E7B800" ,
                "#5ef11a" , 
                "#c58ede" ,
                "#3e020b" ,
                "#febd02" ,
                "#16161e" , 
                "#24844b" ,
                "#f604fd" ,
                "#439bab" ,
                "#c5c896" ,
                "#6e711d"
    )) +
  labs(color = "sujet") 


plot(anova , which = 2)

Var_repet = summary(anova)[[1]]$Mean[2]

CM_fact = summary(anova)[[1]]$Mean[1]

n =2 # Nombre de r ́ep ́etations par Op ́erateur
Var_ol = (CM_fact-Var_repet)/n

ET_repet = sqrt(Var_repet)
ET_ol = sqrt(Var_ol)
ET_repro = sqrt(ET_repet^2+ET_ol^2)
