# ================== Fig 1 ==========================

SC.shp@data = left_join(SC.shp@data, famine.intensity)

Figure.1A = tm_shape(SC.shp) +
  tm_fill(c("CSSI"), 
          style = "quantile",
          #breaks = c(0.279, 0.438, 0.488, 0.521, 0.58),
          title = "Cohort size shrinkage\nindex (CSSI)",
          legend.format = list(fun = function(x) round(x, 2)),
          palette = "Reds", 
          contrast = c(0.1, 0.65)) +
  tm_borders(col = "black") +
  tm_text("Pref.name", size = 0.7) +
  tm_legend(position = c("left","bottom")) +
  tm_layout(# title = expression(bold("A")), 
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
              seq(0.1, 0.9,length.out = 8), 
              na.rm = TRUE)

# qn = quantile(inc.dat$incidence, 
#               c(0.10, 0.28, 0.35, 0.47, 0.50, 0.54, 0.67, 0.79, 0.85), 
#               na.rm = TRUE)

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
  separate(Disease_Sex, 
           into = c("Disease", "Sex"),
           sep = "-") %>% 
  mutate(Disease = str_replace_all(Disease, "_", " "),
         Disease = ifelse(Disease == "TB", "PTB", Disease)) %>%
  mutate(Disease = factor(Disease, levels = c("All", "PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E")),
         Sex = factor(Sex, levels = c("Total", "Female", "Male"))) %>% 
  filter(Sex == "Total")

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
  # mutate(across(6:8, ~ifelse(!Sex == "Total", NA, .x))) %>%
  ggplot(aes(ID, mean)) + 
  geom_line(col = "#08306B") + 
  geom_hline(yintercept = 1, lty = 2, size = 0.4) + 
  geom_ribbon(aes(x = ID, 
                  ymin = `0.025quant`,  
                  ymax = `0.975quant`),
              alpha = 0.15,
              fill = "#08306B",
              col = NA) + 
  facet_wrap(~Disease, scales = "free",
             labeller = labeller) + 
  
  scale_size_manual(values = c(0.75, 0.45, 0.45)) +
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
  # mutate(across(6:8, ~ifelse(!Sex == "Total", NA, .x))) %>%
  ggplot(aes(ID, mean)) + 
  geom_line(col = "#08306B",) + 
  geom_hline(yintercept = 1, lty = 2) + 
  geom_ribbon(aes(x = ID, 
                  ymin = `0.025quant`, 
                  ymax = `0.975quant`),
              alpha = 0.15,
              fill = "#08306B",
              col = NA) + 
  facet_wrap(~Disease, scales = "free",
             labeller = labeller) + 
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
  # mutate(across(6:8, ~ifelse(!Sex == "Total", NA, .x))) %>%
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

# The excluded cohorts used for interpolating the expected cohort effects for F1 in the absence of famine.

dots.dat = cohort.smooth %>% 
  filter(Cohort %in% 1957:1963)


# The excluded cohorts used for interpolating the expected cohort effects for F2 in the absence of famine.

triangles.dat = cohort.smooth %>% 
  filter(Cohort %in% 1978:1984)

cohort.smooth %>% 
  # mutate(across(c(`0.025quant`:`0.975quant`, cohort.predict), ~ifelse(!Sex == "Total", NA, .x))) %>% 
  ggplot(aes(x = Cohort,
             y = Cohort.effect.e),
         col = "#3C5488FF") +
  geom_vline(xintercept = c(1960, 1981), lty = 1) +
  geom_line(aes(lty = "Estimated")) + 
  geom_point(aes(Cohort, Cohort.effect.e), dots.dat,
             size = 1.2) + 
  geom_point(aes(Cohort, Cohort.effect.e), 
             shape = 17,
             triangles.dat,
             size = 1.2) +
  facet_wrap(~Disease, ncol = 4, 
             scales = "free",
             labeller = labeller) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.3) +
  geom_line(aes(x = Cohort, 
                y = cohort.predict,
                lty = "Counterfactual")) +
  geom_ribbon(aes(x = Cohort, 
                  ymin = `0.025quant`, 
                  ymax = `0.975quant`,),
              fill = "#3C5488FF",
              col = NA,
              alpha = 0.1) + 
  scale_x_continuous(breaks = seq(1940, 2000, 20)) +
  scale_linetype_manual(name = "", 
                        values = line_types) + 
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

# ggsave("./output/INLA.Fig/Cohort effct.pdf", height = 5, width = 8.5)

# =================================================
#                          Fig 5 
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

# Fig5A provincial-level IRR
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

# Fig 5B: CSSI and F1 IRR by prefecture and disease

# ref
mean_level = colMeans(model.matrix(~ factor(Disease), data = Pref.RR.results))[-1] %>% mean()

Fig5B = predict(meta.reg.F1, matrix(c(newmods, rep(mean_level, length(newmods)*10)), ncol = 11)) %>% 
  as_tibble() %>% 
  mutate(x = seq(0.25,0.6,0.01)) %>% 
  ggplot() + 
  geom_point(data = Pref.RR.results,
             aes(x = CSSI, 
                 y = F1.mean, 
                 size = size1,
                 col = Disease)) + 
  scale_color_manual(values = 
                       c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#6A6599FF", "#91D1C2FF", "#DF8F44FF", "#00A1D5FF", "#ADB6B6FF", "#A20056FF"),
                     labels = c("PTB", "Hep. B", "Inf. diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E")) + 
  guides(size = "none",
         color = guide_legend(ncol = 2, byrow = FALSE)) +
  geom_line(aes(x = x, y = exp(pred)), 
            linewidth = 1.2, alpha = 0.99,
            col = "#08306B") + 
  geom_ribbon(aes(x = x,
                  ymin = exp(pi.lb), 
                  ymax = exp(pi.ub)), 
              fill = "#08306B",
              alpha = 0.15) + 
  geom_hline(yintercept = 1, lty = 2) +
  theme_classic() +
  theme(legend.position = "right",
        legend.justification = "left",
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 9.5)) + 
  labs(col = "") +
  xlab("CSSI") + 
  ylab("F1 IRR")


# Fig 5C: CSSI and F1 IRR by prefecture for each individual disease

appender = function(Disease) str_c("(",LETTERS[1:11], ") ", Disease)

Fig5C = meta.results %>% 
  ggplot() + 
  geom_hline(yintercept = 1, 
             linetype = 3,
             size = 0.25) + 
  geom_point(data = Pref.RR.results,
             aes(x = CSSI, 
                 y = F1.mean, 
                 size = size1*0.8, 
                 color = p.value)) + 
  geom_line(aes(x = x, y = exp(pred), col = p.value), 
            linewidth = 1.2, alpha = 0.85) +
  geom_ribbon(aes(x = x,
                  ymin = exp(ci.lb), 
                  ymax = exp(ci.ub),
                  fill = p.value), 
              alpha = 0.15) +
  facet_wrap(~Disease, 
             scales = "free", 
             nrow = 2) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9,
                                  hjust = 0),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(3, "cm"),
        legend.text = element_text(size = 7.5),
        legend.position = "bottom",
        legend.margin = margin(-0.3, 0, -0.3, 0, unit = "lines")) +
  xlab("CSSI") + 
  ylab("F1 IRR") + 
  guides(size = "none", fill = "none") + 
  labs(col = "p-value") +
  scale_color_gradientn(colours = brewer.pal(9, "Blues")[9:4],
                        values = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                        breaks = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1)) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues")[9:4],
                       values = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                       breaks = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1))

# save_plot("./output/INLA.Fig/F1.pdf",plot_grid(Fig5A, Fig5B, Fig5C, ncol = 1, rel_heights = c(3,5,5), labels = c("(A)", "(B)", "(C)"), label_size = 12), base_width = 8, base_height = 9)

