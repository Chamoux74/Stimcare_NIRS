#plot des variations individuels sur la NIRS Froid vs Base

ggplot(test4, aes(x = instant_mesure, y = slope )) +
  geom_line(aes(x = instant_mesure, group = sujet , color = as.factor(sujet)) , size = 0.8 , position = "identity") +
  geom_point(
    aes(x = instant_mesure, group = sujet),
    shape = 21,
    colour = "black",
    size = 2,
    position = "identity"
  ) +
  geom_boxplot(
    aes(x = instant_mesure, y = slope),
    width = .25,
    fill = "white" , alpha = 0.3
  ) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    size = 1,
    position = "identity"
  ) +
 +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    linetype = "dotted" ,
    size = 1
  ) +
  labs(title = "TSIPatch_placebo_froid_base_indiv")

#plot des variations individuels sur la NIRS BASE, POST et POST48

test5$instant_mesure <- factor(test5$instant_mesure , c("basepatch" ,
                                                        "baseplacebo" ,
                                                        "postpatch" ,
                                                        "postplacebo" ,
                                                        "post48patch" ,
                                                        "post48placebo"))

ggplot(test5, aes( x = instant_mesure , y = slope )) +
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
    fun = mean,
    geom = "crossbar",
    size = 3 , 
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
  labs(color = "sujet") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    linetype = "dotted" ,
    size = 1
  ) +
  labs(title = "TSIPatch_placebo_exercice_indiv")

#analyse de la plus petite différences significatives (PPDS)

model <- aov(slope~instant_mesure , data = test3)

out <- LSD.test(model,"instant_mesure", p.adj="bonferroni")

out

plot(out)