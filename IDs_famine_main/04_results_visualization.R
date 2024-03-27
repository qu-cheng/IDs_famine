# ================== Fig 1 ==========================

SC.shp@data = left_join(SC.shp@data, famine.intensity)

Figure.1A = tm_shape(SC.shp) +
  tm_fill(c("CSSI"), 
          style= "quantile",
          #breaks = c(0.279, 0.438, 0.488, 0.521, 0.58),
          title = "Cohort size shrinkage\nindex (CSSI)",
          legend.format = list(fun = function(x) round(x, 2)),
          palette = "Reds", 
          contrast = c(0.1, 0.65)) +
  tm_borders(col = "black") +
  tm_text("Pref.name", size = 0.7) +
  tm_legend(position = c("left","bottom")) +
  tm_layout(#title = expression(bold("A")), 
    inner.margins = c(0.00, 0.05, 0.00, 0.02), 
    title.size = 1.2, 
    title.fontface = "bold",
    frame = FALSE, 
    legend.title.size = 0.9, 
    legend.text.size = 0.75)



incidence.dat = read_rds("./data/Incidence.dat.rds")
SC.shp@data = left_join(SC.shp@data, incidence.dat)

Figure.1B = tm_shape(SC.shp) +
  tm_fill(c("incidence"), 
          style= "quantile",
          #breaks = c(0.279, 0.438, 0.488, 0.521, 0.58),
          title = "Annual mean incidence \nrate (1/100000)",
          legend.format = list(fun = function(x) round(x, 1)),
          palette = "Reds", 
          contrast = c(0.1, 0.65)) +
  tm_borders(col = "black") +
  tm_text("Pref.name", size = 0.7) +
  tm_legend(position = c("left","bottom")) +
  tm_layout(#title = expression(bold("B")), 
    inner.margins = c(0.00, 0.05, 0.00, 0.02), 
    title.size = 1.2, 
    title.fontface = "bold",
    frame = FALSE, 
    legend.title.size = 0.9, 
    legend.text.size = 0.75)

# tmap_save(tmap_arrange(Figure.A, Figure.B, nrow = 1), "./output/INLA.Fig/Figure 1AB.pdf", width = 8.5, height = 6)

pop.dat = pop.data %>% 
  group_by(DiagnoseYear,
           Age = cut(Age, 
                     breaks = c(seq(0,78,3),Inf),
                     right = FALSE)) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() 

inc.dat = disease.data %>% 
  group_by(DiagnoseYear, Age = cut(Age, breaks = c(seq(0,78,3),Inf), right = FALSE)) %>%
  summarise(case =sum(case)) %>%
  left_join(pop.dat) %>% 
  mutate(incidence = case/Population*100000) %>% 
  ungroup() %>%
  dplyr::select(1:2, 5) %>%
  as_tibble()


polygons.F1 = 
  data.frame(
    ID = rep(1:6, each = 5),
    x = c(c(2004.5,2007.5, 2007.5, 2004.5, 2004.5) + rep(seq(0,by = 3, length.out = 6), each = 5)),
    y = c(15.5, 15.5, 16.5, 16.5, 15.5) + rep(seq(from = 0, by = 1, length.out = 6), each = 5))

polygons.F2 = 
  data.frame(ID = rep(1:6, each = 5),
             x = c(c(2004.5,2007.5, 2007.5, 2004.5, 2004.5) + rep(seq(0,by = 3, length.out = 6), each = 5)),
             y = c(8.5, 8.5, 9.5, 9.5, 8.5) + rep(seq(from = 0, by = 1, length.out = 6), each = 5))


qn = quantile(inc.dat$incidence, 
              seq(0.01,0.99,length.out = 25), 
              na.rm = TRUE)
qn01 = c(qn, range(inc.dat$incidence)) %>% 
  rescale() %>% sort()

Figure.1C = inc.dat %>% 
  ggplot(aes(DiagnoseYear, Age)) + 
  geom_abline(intercept = seq(-641.6667,-681.6667,-1), slope = 1/3, linewidth = 0.5) + 
  geom_tile(aes(fill = incidence), 
            color = NA, alpha = 0.90) + 
  scale_fill_gradientn(colours = colorRampPalette(rev(c("#B71126", "#D73027", "#F46D43", "#FDAE61", '#ffffe5','#d9f0a3','#78c679','#006837')))(20), values = qn01,
                       breaks = c(0, 150, 300, 450, 600, 1000),
                       labels = c("0", "150", "300" ,"450", "600", "1000")) +
  annotate("text", x = 2021.5, y = 19.5, label = "F1", size = 3.5) + 
  annotate("text", x = 2021.5, y = 12.5, label = "F2", size = 3.5) + 
  geom_polygon(data = polygons.F1, aes(x = x, y= y, group = ID), fill = NA, col = "black", linewidth = 0.5) +
  geom_polygon(data = polygons.F2, aes(x = x, y= y, group = ID), fill = NA, col = "black", linewidth = 0.5) +
  labs(x = "Year of diagnosis", 
       y = "Age group", 
       fill = "Incidence rate\n(1/100,000)") + 
  coord_fixed(ratio = 1.1) + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(0.35, "cm"), 
        legend.key.height = unit(0.2,"cm"), 
        legend.margin = margin(t = -.3, unit = "cm"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7, 
                                   angle = 45),
        plot.title = element_text(hjust = -0.25,
                                  vjust = -5,
                                  size = 13,
                                  face = "bold"),
        axis.ticks = element_line(size = 0.3), 
        axis.text.y = element_text(size = 9.2),
        axis.text.x = element_text(size = 9.2),
        axis.title = element_text(size = 12)) + 
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2005, 2022,3), 
                     limits = c(2004.5,2022.5)) +  
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.8, "cm"))  + 
  ggtitle(expression(bold("C")))

ggsave(Figure.1C, "./output/INLA.Fig/Figure 1C.pdf", height = 5, width = 3.5)


# APC effect visualization
APC.output = read_rds("output/INLA.model/provincial_level_APC.effects.rds")

plot.dat = bind_rows(APC.output) %>% 
  bind_rows() %>% 
  separate(Disease_Sex, 
           into = c("Disease", "Sex"),
           sep = "-") %>% 
  mutate(Disease = str_replace_all(Disease, "_", " "),
         Disease = ifelse(Disease == "TB", "PTB", Disease)) %>%
  mutate(Disease = factor(Disease, levels = c("All", "PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E")),
         Sex = factor(Sex, levels = c("Total", "Female", "Male")))

labeller = function(variable, value){
  label = str_c("(", LETTERS[value], ") ", value)
  return(label)
}

# =================================================
#                Fig 2 Age effects
# =================================================
plot.dat %>% 
  unnest(Age.effect) %>% 
  mutate(across(c(4, 6:8), ~exp(.x))) %>% 
  ggplot(aes(ID, mean, col = Sex,
             size = Sex)) + 
  geom_line() + 
  geom_hline(yintercept = 1, lty = 2, size = 0.4) + 
  geom_ribbon(aes(x = ID, 
                  ymin = `0.025quant`, 
                  ymax = `0.975quant`,
                  fill = Sex,
                  alpha = Sex),
              col = NA) + 
  facet_wrap(~Disease, scales = "free",
             labeller = labeller) + 
  scale_color_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) + 
  scale_fill_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) +
  scale_size_manual(values = c(0.75, 0.5, 0.5)) +
  scale_alpha_manual(values = c(0.3, 0.3, 0.15)) +
  labs(col = "", fill = "",
       alpha = "", size = "",
       x = "Age at Diagnosis",
       y = expression(e^alpha[i])) +
  theme_cowplot() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, 
                                  face = "bold",
                                  hjust = 0),
        axis.text = element_text(size = 9.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 13.5, face = "bold"),
        legend.position = "top",
        legend.justification = "center",
        legend.key.height = unit(0.25, "cm"), 
        legend.margin = margin(t = -0.05, b = -0.4, unit = "cm")) 

# ggsave("./output/INLA.Fig/Age effct.pdf", height = 5, width = 8.5)

# =================================================
#                Fig 3 Period effects
# =================================================
plot.dat %>% 
  unnest(Period.effect) %>% 
  mutate(across(c(5, 7:9), ~exp(.x))) %>% 
  ggplot(aes(ID, mean, col = Sex, size = Sex)) + 
  geom_line() + 
  geom_hline(yintercept = 1, lty = 2) + 
  geom_ribbon(aes(x = ID, 
                  ymin = `0.025quant`, 
                  ymax = `0.975quant`,
                  fill = Sex,
                  alpha = Sex),
              col = NA) + 
  facet_wrap(~Disease, scales = "free",
             labeller = labeller) + 
  scale_color_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) + 
  scale_fill_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) +
  scale_size_manual(values = c(0.75, 0.5, 0.5)) +
  scale_alpha_manual(values = c(0.3, 0.3, 0.15)) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, 
                                  face = "bold",
                                  hjust = 0),
        axis.text = element_text(size = 9.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 13.5, face = "bold"),
        legend.position = "top",
        legend.justification = "center",
        legend.key.height = unit(0.25, "cm"), 
        legend.margin = margin(t = -0.05, b = -0.4, unit = "cm")) + 
  labs(col = "", fill = "",
       alpha = "", size = "",
       x = "Year of Diagnosis",
       y = expression(e^pi[j])) 

# ggsave("./output/INLA.Fig/Period effct.pdf", height = 5, width = 8.5)

# =================================================
#                Fig 4 Cohort effects
# =================================================
Cohort.results = plot.dat %>% 
  unnest(Cohort.effect) %>% 
  mutate(across(c(6, 8:10), ~exp(.x))) %>% 
  select(1:2, 5:10) %>% 
  group_nest(Disease, Sex)

output = vector("list", 36)
names(output) = str_c(Cohort.results$Disease, Cohort.results$Sex, sep = "-")

for (i in 1:nrow(Cohort.results)) {
  
  current.result = Cohort.results$data[[i]]
  
  output[[i]] = province.cohort.smooth(current.result, F1.start = 1957, F1.end = 1963, F2.start = 1978, F2.end = 1984) 
  
}

cohort.smooth = bind_rows(output, .id = "Disease_Sex") %>% 
  separate(Disease_Sex,
           into = c("Disease", "Sex"),
           sep = "-") %>% 
  filter(Cohort %in% 1940:2000) %>% 
  mutate(Disease = factor(Disease, levels = c("All","PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"))) %>% 
  mutate(Sex = factor(Sex, levels = c("Total", "Female", "Male")))


line_types = c("Estimated" = 1, "Counterfactual" = 2)

cohort.smooth %>% 
  ggplot(aes(x = Cohort,
             y = Cohort.effect.e,
             col = Sex)) +
  geom_vline(xintercept = c(1960, 1981), lty = 1) +
  geom_line(aes(size = Sex, alpha = Sex, lty = "Estimated")) + 
  facet_wrap(~Disease, ncol = 4, 
             scales = "free",
             labeller = labeller) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.3) +
  geom_line(aes(x = Cohort, 
                y = cohort.predict,
                col = Sex,
                lty = "Counterfactual")) +
  geom_ribbon(aes(x = Cohort, 
                  ymin = `0.025quant`, 
                  ymax = `0.975quant`,
                  fill = Sex),
              col = NA,
              alpha = 0.1) + 
  scale_x_continuous(breaks = seq(1940, 2000, 20)) +
  scale_linetype_manual(name = "", 
                        values = line_types) + 
  scale_color_manual(values = c("black", "#d75427","#00abf0")) + 
  scale_fill_manual(values = c("black", "#d75427","#00abf0")) +
  scale_size_manual(values = c(0.75, 0.5, 0.5)) +
  scale_alpha_manual(values = c(0.9, 0.75, 0.75)) + 

  labs(x = "Year of birth",
       y = expression(e^gamma[k])) +
  theme_classic() + 
  theme(strip.background = element_blank(),
        axis.line = element_line(colour = "black", 
                                 size = 0.35),
        plot.title = element_text(size = 15, face = "bold",
                                  vjust = -3.5),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        strip.text = element_text(face = "bold", 
                                  size = 10, 
                                  hjust = 0),
        axis.text = element_text(size = 9.5),
        legend.position = "top",
        legend.key.height = unit(0.2, "cm"), 
        legend.margin = margin(t = -0.1,b = -0.4, unit = "cm"), 
        plot.margin = margin(c(0.15, 0.4, 0.1,  0.1), unit = "cm"),
        legend.title = element_blank()) + 
  guides(linetype = guide_legend(reverse = TRUE)) + 
  annotate("text", x = 1962, y = Inf, 
           label = "F1", size = 3.5,
           hjust = 0, vjust = 1.5) + 
  annotate("text", x = 1983, y = Inf, 
           label = "F2", size = 3.5,
           hjust = 0, vjust = 1.5)  


# =================================================
#           Fig 5A provincial-level IRR
# =================================================
RR = read_csv("./output/RR/provincial.RR.by.Sex.csv") %>% 
  mutate(Sex = factor(Sex, levels = c("Total", "Female", "Male")),
         Disease = ifelse(Disease == "TB", "PTB", Disease)) %>% 
  arrange(Sex) %>% 
  mutate(Disease = str_replace(Disease, "_", " ")) %>%
  mutate(Disease = factor(Disease, 
                          levels = c("All", "PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"),
                          labels = c("**All**", "PTB", "Hep. B", "Inf.<br />diarrhea", "Syphilis", "HIV<br />AIDS", "Bacillary<br />dysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E"))) %>% 
  arrange(Disease, Sex) 

Fig5A = RR %>% 
  ggplot(aes(Disease,
             y = F1.mean)) +
  geom_rect(aes(xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = "gray90", alpha = 0.1, col = NA)+ 
  geom_point(aes(color = Sex, alpha = Sex), 
             size = 0.9,
             position = position_dodge(0.5)) + 
  geom_hline(yintercept = 1, lty = 3) +
  geom_errorbar(aes(ymin = F1.low,
                    ymax = F1.high,
                    color = Sex,
                    alpha = Sex,
                    size = Sex),
                width = 0.3,
                position = position_dodge(0.5)) +
  scale_color_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) + 
  scale_alpha_manual(values = c(0.75, 1, 0.75)) + 
  scale_size_manual(values = c(0.8, 0.45, 0.45)) + 
  scale_y_continuous(limits = c(0.8, 1.5)) +
  theme_classic() + 
  theme(legend.position = c(.8, .95), 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        legend.direction = "horizontal",
        legend.text = element_text(size = 11),
        plot.margin = margin(5.5, 5.5, -2, 5.5, "pt"),
        axis.text.x = ggtext::element_markdown(),
        axis.text = element_text(size = 10)) + 
  labs(x = "", 
       y = "F1 IRR") 

# ggsave("./output/INLA.Fig/F1 IRR.pdf", width = 7.2, height = 4)  
