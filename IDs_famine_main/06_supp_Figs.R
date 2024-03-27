# load and process data
disease.data = read_csv("./data/11.by_sex_pref_disease.data.csv") %>% 
  drop_na() %>% 
  mutate(Disease = ifelse(Disease == "TB", "PTB", Disease))

all.data = disease.data %>% 
  group_by(DiagnoseYear, Pref, Age, Sex) %>% 
  summarise(case = sum(case)) %>% 
  mutate(Disease = "All")

disease.data = disease.data %>% 
  bind_rows(all.data)

pop.data = read_csv("./data/pop.by_pref_age_sex_2005-2022.csv") %>% 
  pivot_longer(-c(1:3),
               names_to = "Pref",
               values_to = "Population") %>%
  rename(DiagnoseYear = Year) %>% 
  mutate(across(2:4, ~as.numeric(.))) %>% 
  filter(DiagnoseYear >= 2005)


# =================================================
#                     Fig S1 
# =================================================

dat = disease.data %>% 
  filter(!Disease == "All") %>% 
  group_by(Disease) %>% 
  summarise(case = sum(case)) %>% 
  mutate(per = round(case/sum(case)*100, 2)) %>% 
  mutate(Disease = str_c(Disease, " (", per, "%)"))

ggplot(dat, aes(area = case, 
                fill = Disease, 
                label = Disease)) +
  scale_fill_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#6A6599FF", "#91D1C2FF", "#DF8F44FF", "#00A1D5FF", "#ADB6B6FF", "#A20056FF")) +
  treemapify::geom_treemap(layout = "squarified") +
  geom_treemap_text(place = "centre", size = 12) + 
  labs(fill = "") + 
  theme(legend.position = "none")

# ggsave("./output/supp_Fig/S1.treemap.pdf", height = 5, width = 7)


# =================================================
#                     Fig S2 
# =================================================

pop.dat = pop.data %>% 
  group_by(Pref, DiagnoseYear) %>% 
  summarise(Population = sum(Population)) %>% 
  group_by(Pref) %>% 
  summarise(Population = mean(Population))

incidence.dat = disease.data %>% 
  filter(!Disease == "All") %>% 
  group_by(DiagnoseYear, Disease, Pref) %>% 
  summarise(Case = sum(case)) %>% 
  group_by(Disease, Pref) %>% 
  summarise(Case = mean(Case)) %>% 
  left_join(pop.dat) %>% 
  mutate(incidence = Case/Population*100000) %>% 
  dplyr::select(1, 2, 5) %>% 
  mutate(Disease = factor(Disease, levels = c("PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"))) %>% 
  arrange(Disease)

output = vector("list", 11)
for (i in 1:11) {
  
  current.dat = incidence.dat %>% 
    filter(Disease == unique(incidence.dat$Disease)[i])
  
  current.shp = SC.shp
  
  current.shp@data = current.shp@data %>% 
    left_join(current.dat)
  
  output[[i]] = tm_shape(current.shp) +
    tm_fill("incidence", 
            style= "quantile",
            title = "Annual mean incidence\nrate(1/100,000)",
            legend.format = list(fun = function(x) round(x, 1)),
            palette = "Reds", 
            contrast = c(0.1, 0.75)) +
    tm_borders(col = "black") +
    tm_text("Pref.name", size = 0.3) +
    tm_legend(position = c("left","bottom")) +
    tm_layout(title = str_c("(", LETTERS[i], ") ", unique(incidence.dat$Disease)[i]), 
              inner.margins = c(0.05, 0.10, 0.00, 0.00), 
              title.size = 0.75, 
              title.position = c("center","TOP"),
              legend.position = c("left","bottom"),
              title.fontface = "bold",
              legend.title.size = 0.45,
              legend.text.size = 0.35,
              frame = FALSE)
  
}

# tmap_save(tmap_arrange(output, nrow = 3), "./output/supp_Fig/inc.map.pdf", width = 8, height = 6)


# =================================================
#                     Fig S3 
# =================================================

pop.dat_7 = pop.data %>% 
  group_by(DiagnoseYear,
           Age = cut(Age, 
                     breaks = c(0, seq(12,78,3),Inf),
                     right = FALSE)) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() 

inc.dat_7 = disease.data %>% 
  filter(!Disease == "All") %>% 
  filter(Disease %in% c("HIV/AIDS", "Syphilis",
                        "Gonorrhea",
                        "Hepatitis B", "Hepatitis E",
                        "Hepatitis C", "PTB")) %>% 
  group_by(DiagnoseYear, Disease, Age = cut(Age, breaks = c(0, seq(12,78,3),Inf), right = FALSE)) %>%
  summarise(case =sum(case)) %>%
  left_join(pop.dat_7) %>% 
  mutate(incidence = case/Population*100000) %>% 
  ungroup() %>%
  dplyr::select(1:3, 6) %>%
  pivot_wider(names_from = "DiagnoseYear",
              values_from = "incidence",
              values_fill = 0) %>% 
  pivot_longer(-c(1:2) ,
               names_to = "DiagnoseYear",
               values_to = "incidence") %>% 
  mutate(DiagnoseYear = as.numeric(DiagnoseYear)) %>%
  as_tibble()

pop.dat_5 = pop.data %>% 
  group_by(DiagnoseYear,
           Age = cut(Age, 
                     breaks = c(seq(0,72,3),Inf),
                     right = FALSE)) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() 

inc.dat_5 = disease.data %>% 
  filter(!Disease == "All") %>%
  filter(!Disease %in% c("HIV/AIDS", "Syphilis",
                         "Gonorrhea",
                         "Hepatitis B", 
                         "Hepatitis E",
                         "Hepatitis C", "PTB")) %>% 
  group_by(DiagnoseYear, Disease, Age = cut(Age, breaks = c(seq(0,72,3),Inf), right = FALSE)) %>%
  summarise(case =sum(case)) %>%
  left_join(pop.dat_5) %>% 
  mutate(incidence = case/Population*100000) %>% 
  as_tibble()

inc_dat = inc.dat_7 %>% 
  bind_rows(inc.dat_5) %>% 
  mutate(Disease = str_replace_all(Disease, "_", " ")) %>% 
  mutate(Disease = factor(Disease, levels = c("PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"))) %>% 
  arrange(Disease)

inc.heatmap = vector("list", 11)

for (i in 1:n_distinct(inc_dat$Disease)) {
  
  if(i %in% c(1, 2, 4, 5, 7:8, 11)){
    current.incidence = inc_dat %>% 
      filter(Disease == unique(Disease)[i]) %>% 
      mutate(Age = factor(Age, levels = unique(inc.dat_7$Age)))
    
    qn = quantile(current.incidence$incidence, 
                  seq(0.01,0.99,length.out = 18), 
                  na.rm = TRUE)
    qn01 = c(qn, range(current.incidence$incidence)) %>% 
      rescale() %>% sort()
    polygons.F1 = 
      data.frame(
        ID = rep(1:6, each = 5),
        x = c(c(2004.5,2007.5, 2007.5, 2004.5, 2004.5) + rep(seq(0,by = 3, length.out = 6), each = 5)),
        y = c(15.5, 15.5, 16.5, 16.5, 15.5) + rep(seq(from = 0, by = 1, length.out = 6), each = 5) - 3)
    
    polygons.F2 = 
      data.frame(ID = rep(1:6, each = 5),
                 x = c(2004.5,2007.5, 2007.5, 2004.5, 2004.5) + rep(seq(0, by = 3, length.out = 6), each = 5),
                 y = c(8.5, 8.5, 9.5, 9.5, 8.5) + rep(seq(from = 0, by = 1, length.out = 6), each = 5)- 4)
  }    
  
  else {
    current.incidence = inc_dat %>% 
      filter(Disease == unique(Disease)[i]) %>% 
      mutate(Age = factor(Age, levels = unique(inc.dat_5$Age)))
    
    qn = quantile(current.incidence$incidence, 
                  seq(0.01,0.99,length.out = 18), 
                  na.rm = TRUE)
    qn01 = c(qn, range(current.incidence$incidence)) %>% 
      rescale() %>% sort()
    
    polygons.F1 = 
      data.frame(
        ID = rep(1:6, each = 5),
        x = c(c(2004.5,2007.5, 2007.5, 2004.5, 2004.5) + rep(seq(0,by = 3, length.out = 6), each = 5)),
        y = c(15.5, 15.5, 16.5, 16.5, 15.5) + rep(seq(from = 0, by = 1, length.out = 6), each = 5))
    
    polygons.F2 = 
      data.frame(ID = rep(1:6, each = 5),
                 x = c(c(2004.5,2007.5, 2007.5, 2004.5, 2004.5) + rep(seq(0,by = 3, length.out = 6), each = 5)),
                 y = c(8.5, 8.5, 9.5, 9.5, 8.5) + rep(seq(from = 0, by = 1, length.out = 6), each = 5))
    
  }
  
  inc.heatmap[[i]] = current.incidence %>% 
    ggplot(aes(DiagnoseYear, Age)) + 
    geom_abline(intercept = seq(-641.6667,-681.6667,-1), slope = 1/3, linewidth = 0.5) + 
    geom_tile(aes(fill = incidence), 
              color = NA, alpha = 0.90) + 
    scale_fill_gradientn(colours = colorRampPalette(rev(c("#B71126", "#D73027", "#F46D43", "#FDAE61", '#ffffe5','#d9f0a3','#78c679','#006837')))(20),  values = qn01) +
    annotate("text", x = 2021.5, y = 19.5, label = "F1", 
             size = 3.5) + 
    annotate("text", x = 2021.5, y = 11.5, label = "F2", 
             size = 3.5) + 
    geom_polygon(data = polygons.F1, aes(x = x, y= y, group = ID), fill = NA, col = "black", linewidth = 0.3) +
    geom_polygon(data = polygons.F2, aes(x = x, y= y, group = ID), fill = NA, col = "black", linewidth = 0.3) +
    labs(x = "Year of diagnosis", 
         y = "Age group", 
         fill = "Incidence rate\n(1/100,000)") + 
    coord_fixed(ratio = 1.1) + 
    theme(legend.position="bottom", 
          legend.key.width = unit(0.35, "cm"), 
          legend.key.height = unit(0.2,"cm"), 
          legend.margin = margin(t = -.3, unit = "cm"), 
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6, 
                                     angle = 45),
          axis.ticks = element_line(size = 0.3), 
          axis.text.y = element_text(size = 6),
          axis.text.x = element_text(size = 6.5),
          plot.title = element_text(hjust = 0.5,
                                    size = 11,
                                    face = "bold")) + 
    scale_x_continuous(expand = c(0, 0),
                       breaks = seq(2005, 2022,3), 
                       limits = c(2004.5,2022.5)) +  
    scale_y_discrete(expand = c(0, 0)) +
    theme(panel.border = element_rect(fill = NA,
                                      colour = "black"))+
    ggtitle(str_c("(", LETTERS[i], ") ", unique(inc_dat$Disease)[i]))
}

Figure.S3 = plot_grid(plotlist = inc.heatmap, ncol = 6)
# save_plot("./output/supp_Fig/S3.heatmap.pdf", Figure.S3, base_height = 7, base_width = 13.5) 


# =================================================
#                     Fig S4 
# =================================================

pop.dat = pop.data %>% 
  group_by(Sex, DiagnoseYear) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup()

inc.dat = disease.data %>% 
  group_by(DiagnoseYear,Disease, Sex) %>% 
  summarise(Case = sum(case)) %>% 
  ungroup() %>% 
  left_join(pop.dat) %>% 
  mutate(incidence = Case/Population*100000) %>% 
  mutate(Disease = str_replace_all(Disease, "_", " "))

labeller = function(variable, value){
  label = str_c("(", LETTERS[value], ") ", value)
  label
}

inc.dat %>%
  mutate(Disease = factor(Disease, levels = c("All", "PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"))) %>% 
  arrange(Disease) %>% 
  ggplot(aes(DiagnoseYear, incidence, col = Sex)) +
  geom_line() + 
  facet_wrap(~ Disease, 
             scales = "free", 
             labeller = labeller) + 
  theme_classic() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10.2,
                                  face = "bold",
                                  hjust = 0),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 9.5), 
        axis.text.y = element_text(size = 9.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 8.5),
        legend.position = "top",
        legend.justification = "center",
        legend.margin = margin(b = -0.4, unit = "cm"),
        legend.key.height = unit(0.2, "cm")) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_npg() + 
  labs(x = "DiagnoseYear", 
       y = "Incidence rate (1/100000)")

# ggsave("./output/supp_Fig/S5.temporal.trend.pdf", height = 5, width = 8.5)


# =================================================
#                     Fig S5 
# =================================================

lev_disease = disease.data %>% 
  group_by(Disease) %>% 
  summarise(case = sum(case)) %>% 
  mutate(Disease = reorder(Disease, -case))


pop.dat = pop.data %>% 
  mutate(Cohort = DiagnoseYear-Age) %>% 
  group_by(Cohort, Sex) %>% 
  summarise(pop.avg = sum(Population)/14) %>% 
  mutate(pop.avg = round(pop.avg, 0))

cohort.inc = disease.data %>% 
  mutate(Cohort = round((DiagnoseYear-Age), 0)) %>%
  group_by(Disease, Cohort, Sex) %>% 
  summarise(Case = sum(case)/14) %>% 
  ungroup() %>% 
  left_join(pop.dat) %>% 
  drop_na() %>% 
  mutate(Disease = factor(Disease, levels = levels(lev_disease$Disease))) %>% 
  mutate(incidence = Case/pop.avg*100000)

cohort.inc$Disease %>% unique

cohort.inc %>% 
  filter(Cohort %in% 1940:2000) %>% 
  ggplot(aes(Cohort, incidence)) + 
  geom_rect(aes(xmin = 1958, 
                xmax = 1962, 
                ymin = -Inf, 
                ymax = Inf),
            fill = "#e5f5f9", 
            alpha = .2) +
  geom_rect(aes(xmin = 1979, xmax = 1983, ymin = -Inf, ymax = Inf),fill = "#e5f5f9", alpha = .2) + 
  geom_line(aes(col = Sex), size = 0.50) +
  geom_vline(xintercept = c(1960, 1981), 
             lty = 3, 
             size = 0.5) +
  scale_x_continuous(breaks = seq(1940, 1980, 20)) +
  facet_wrap(~Disease, 
             scales = "free", 
             ncol = 4,
             labeller = labeller) +
  xlab("Year of birth") + 
  ylab("Annual mean cohort-specific incidence (1/100000)") + 
  theme_classic() + 
  scale_color_npg() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", 
                                  size = 10.3,
                                  hjust = 0),
        axis.ticks = element_line(size = 0.3),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.position = "top",
        legend.margin = margin(b = -0.4, unit = "cm"),
        legend.key.height = unit(0.2, "cm"))  +
  guides(color = guide_legend(reverse = TRUE)) 

# ggsave("./output/supp_Fig/Figure S6.pdf", height = 5, width = 8.5)


# =================================================
#                     Fig S6 
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

FigS6A = RR %>% 
  ggplot(aes(Disease,
             y = F2.mean)) +
  geom_rect(aes(xmin = -Inf, xmax = 1.5, 
                ymin = -Inf, ymax = Inf), 
            fill = "gray90", 
            alpha = 0.1,
            col = NA) + 
  geom_point(aes(color = Sex, alpha = Sex), 
             size = 0.9,
             position = position_dodge(0.5)) + 
  geom_hline(yintercept = 1, lty = 3) +
  geom_errorbar(aes(ymin = F2.low,
                    ymax = F2.high,
                    color = Sex,
                    alpha = Sex,
                    size = Sex),
                width = 0.3,
                position = position_dodge(0.5)) +
  scale_color_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) + 
  scale_alpha_manual(values = c(0.75, 1, 0.75)) + 
  scale_size_manual(values = c(0.8, 0.45, 0.45)) + 
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
       y = "F2 IRR") + 
  scale_y_continuous(breaks = seq(0.8, 1.4, 0.2))


Pref.RR.results = read_csv("./output/RR/INLA_pref_RR_structured_age_iid_cohort.csv") %>% 
  mutate(Disease = ifelse(Disease == "TB", "PTB", Disease)) %>% 
  #filter(!Pref %in% c(5108, 5113, 5116, 5132)) %>%
  group_by(Disease) %>% 
  mutate(size1 = (1/F1.log.sd)/(1/max(F1.log.sd)),
         size2 = (1/F2.log.sd)/(1/max(F2.log.sd))) %>%
  ungroup() %>% 
  left_join(SC.shp@data) %>% 
  mutate(Disease = str_replace_all(Disease, "_", " "),
         Disease = str_replace_all(Disease, "\\.", "/")) %>%
  left_join(famine.intensity) %>% 
  mutate(Disease = factor(Disease, levels = c("PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"),
                          labels = c("PTB", "Hep. B", "Inf.\ndiarrhea", "Syphilis", "HIV/AIDS", "Bacillary\ndysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E"))) %>%
  arrange(Disease)


meta.reg.F2 = rma(yi = F2.log.mean, 
                  sei = F2.log.sd, 
                  mods = ~ CSSI + factor(Disease),
                  data = Pref.RR.results, 
                  method = "REML")

Q1 = predict(meta.reg.F2, newmods = rep(43.82262/100, 11))
Q3 = predict(meta.reg.F2, newmods = rep(52.07122/100, 11))

slope.sim = rnorm(10000, summary(meta.reg.F2)$beta[2], summary(meta.reg.F2)$se[2])

intercept.sim = rnorm(10000, summary(meta.reg.F2)$beta[1], summary(meta.reg.F2)$se[1])

Q1.RR.sim = exp(intercept.sim + slope.sim*43.82262/100)
Q3.RR.sim = exp(intercept.sim + slope.sim*52.07122/100)

IQR.F2 = (Q3.RR.sim - Q1.RR.sim)/Q1.RR.sim

quantile(IQR.F2, c(0.025, 0.5, 0.975))*100

newmods = seq(0.25,0.6,0.01)

mean_level = colMeans(model.matrix(~ factor(Disease), data = Pref.RR.results))[-1] %>% mean()


FigS6B = predict(meta.reg.F2, matrix(c(newmods, rep(mean_level, length(newmods)*10)), ncol = 11)) %>%
  as_tibble() %>% 
  mutate(x = seq(0.25,0.6,0.01)) %>% 
  ggplot() + 
  geom_point(data = Pref.RR.results,
             aes(x = CSSI, 
                 y = F2.mean, 
                 size = size2,
                 col = Disease)) + 
  scale_color_manual(values = 
                       c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#6A6599FF", "#91D1C2FF", "#DF8F44FF", "#00A1D5FF", "#ADB6B6FF", "#A20056FF"),
                     labels = c("PTB", "Hep. B", "Inf. diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E")) + 
  guides(size = "none",
         color = guide_legend(ncol = 2, byrow = FALSE)) +
  geom_line(aes(x = x, y = exp(pred)), 
            linewidth = 1.2, alpha = 0.99,
            col = "#9ECAE1") + 
  geom_ribbon(aes(x = x,
                  ymin = exp(pi.lb), 
                  ymax = exp(pi.ub)), 
              fill = "#9ECAE1",
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
  ylab("F2 IRR") 

# ggsave("./output/INLA.Fig/F1 random-meta-analysis.pdf", height = 4.5, width = 8)

# fixed-effect meta analysis for each disease
reshape.res = function(df){
  df = df %>% as_tibble() %>%
    mutate(x = seq(0.25,0.6,0.01))
}


meta.p.value = Pref.RR.results %>%  
  dplyr::select(F2.log.mean, F2.log.sd, CSSI, Disease, Pref) %>%  
  group_nest(Disease) %>%
  mutate(rma.model = map(data, ~rma(yi = F2.log.mean,
                                    sei = F2.log.sd,
                                    mods = ~ CSSI,
                                    #method = "FE",
                                    data = .))) %>% 
  mutate(tidy_summary = map(rma.model, tidy)) %>%
  unnest(tidy_summary) %>%
  filter(term == "CSSI") %>%
  select(Disease, estimate, p.value) %>%
  mutate(p.cat = cut(p.value, breaks = c(0, 0.01, 0.05, 0.1, 0.3, 0.5, Inf))) 

meta.results = Pref.RR.results %>%  
  dplyr::select(F2.log.mean, F2.log.sd, CSSI, Disease, Pref) %>%  
  group_nest(Disease) %>% 
  mutate(rma.model = map(data, ~rma(yi = F2.log.mean, 
                                    sei = F2.log.sd, 
                                    mods = ~ CSSI,
                                    #method = "FE",
                                    data = .))) %>% 
  mutate(tidy_summary = map(rma.model, tidy)) %>%
  unnest(tidy_summary) %>%
  filter(term == "CSSI") %>%
  mutate(pred = map(rma.model, ~predict(.x, newmods=seq(0.25, 0.6, 0.01)))) %>% 
  mutate(pred = map(pred, ~reshape.res(.x))) %>% 
  unnest(pred) %>%
  left_join(meta.p.value)

Pref.RR.results2 = Pref.RR.results %>% 
  left_join(meta.p.value) 

appender = function(Disease) str_c("(",LETTERS[1:11], ") ", Disease)

FigS6C = meta.results %>% 
  ggplot() + 
  geom_hline(yintercept = 1, 
             linetype = 3,
             size = 0.25) + 
  geom_point(data = Pref.RR.results2,
             aes(x = CSSI, 
                 y = F2.mean, 
                 size = size2*0.8, 
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
  ylab("F2 IRR") + 
  guides(size = "none", fill = "none") + 
  labs(col = "p-value") +
  scale_color_gradientn(colours = brewer.pal(9, "Blues")[9:4],
                        values = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                        breaks = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1)) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues")[9:4],
                       values = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                       breaks = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1))

# save_plot("./output/INLA.Fig/F2.pdf", plot_grid(FigS6A, FigS6B, FigS6C, ncol = 1, rel_heights = c(3,5,5), labels = c("(A)", "(B)", "(C)"), label_size = 12), base_width = 8, base_height = 9)


