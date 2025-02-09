# =====================  Fig S1 ============================= #
pref.pop = pop.data %>% 
  filter(DiagnoseYear == 2010) %>% 
  group_by(Age, Pref) %>% 
  summarise(Population = sum(Population)) %>% 
  left_join(SC.shp@data)  

F2.dat = pref.thres %>% 
  mutate(low.cohort = mid.cohort-2,
         high.cohort = mid.cohort+2) %>% 
  mutate(across(low.cohort:high.cohort, ~abs(.-2010))) %>% 
  rename("xmin" = "high.cohort",
         "xmax" = "low.cohort") %>% 
  mutate(F2 = (xmin + xmax)/2,
         text = "F2") %>% 
  left_join(SC.shp@data) %>% 
  select(Pref, xmin, xmax, F2, text, Pref.name) %>% 
  mutate(ymin = -Inf, 
         ymax = Inf) 

F1.dat = pref.thres %>% 
  mutate(low.cohort = 1958,
         high.cohort = 1962) %>% 
  mutate(across(low.cohort:high.cohort, ~abs(.-2010))) %>%
  rename("xmin" = "high.cohort",
         "xmax" = "low.cohort") %>% 
  mutate(F1 = (xmin + xmax)/2,
         text = "F1") %>% 
  left_join(SC.shp@data) %>% 
  select(Pref, Pref.name, xmax:text) %>% 
  mutate(ymin = -Inf, 
         ymax = Inf) 


pref.pop %>% 
  mutate(Pref.name = fct_reorder(Pref.name, Pref),
         Population = Population/1000) %>% 
  arrange(Pref.name) %>% 
  ggplot() +
  geom_rect(data = F2.dat, 
            aes(xmin = xmin, 
                xmax = xmax, 
                ymin = ymin, 
                ymax = ymax,
                fill = text)) +
  geom_rect(data = F1.dat, 
            aes(xmin = xmin, 
                xmax = xmax, 
                ymin = ymin, 
                ymax = ymax,
                fill = text)) + 
  scale_fill_manual(values = c("F1" = "darkgrey", "F2" = "lightgrey")) + 
  geom_text(data = F2.dat,
            aes(x = F2, y = Inf, label = text), 
            vjust = 1, size = 2.5) + 
  geom_text(data = F1.dat,
            aes(x = F1, y = Inf, label = text), 
            vjust = 1, size = 2.5) + 
  geom_line(aes(Age, Population)) +
  facet_wrap(~reorder(Pref.name, Pref), ncol = 4, scales = "free_y") + 
  labs(y = "Population size (thousands)",
       x = "Age") + 
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0.01),
        axis.text.y = element_text(size = 10),
        legend.position = c(0.45, 0.05),
        legend.title = element_blank()) + 
  guides(fill = guide_legend(direction = "horizontal"))

# ggsave("./output/SFig/FigS1.pdf", width = 10, height = 7)


# =====================  Fig S2 ============================= #

# iid/besag intercept 2
# iid/besag Age 2
# iid/besag Period 2
# iid Cohort 1
# 2*2*2*1 = 8 formulas

formula1 = Case ~ -1 + f(PrefID, model = "besag", constr = FALSE, graph = "SC.graph") +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "iid")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "iid")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))

formula2 = Case ~ -1 + f(PrefID, model = "besag", constr = FALSE, graph = "SC.graph") +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))


formula3 = Case ~ -1 + f(PrefID, model = "iid", constr = FALSE, graph = "SC.graph") +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "iid")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "iid")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))

formula4 = Case ~ -1 + f(PrefID, model = "iid", constr = FALSE, graph = "SC.graph") +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))


formula5 = Case ~ -1 + f(PrefID, model = "besag", constr = FALSE, graph = "SC.graph") +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "iid")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))


formula6 = Case ~ -1 + f(PrefID, model = "besag", constr = FALSE, graph = "SC.graph") +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "iid")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))


formula7 = Case ~ -1 + f(PrefID, model = "iid", constr = FALSE) +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "iid")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))


formula8 = Case ~ -1 + f(PrefID, model = "iid", constr = FALSE) +
  f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "iid")) +
  f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "besag", graph = "SC.graph")) +
  f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))

formulas = list(formula1, formula2, formula3, formula4, formula5, formula6, formula7, formula8)


DIC = matrix(NA, ncol = 8, nrow = 11)

colnames(DIC) = c("formula1", "formula2", "formula3", "formula4", "formula5", "formula6", "formula7", "formula8")

rownames(DIC) = unique(pref.data$Disease)

for(i in 1:length(disease.list))
{
  
  current.data = pref.data %>%
    filter(Disease == disease.list[i]) %>%
    mutate(Pref.A1 = PrefID,   # replicates of PrefID for using in INLA
           Pref.P1 = PrefID,
           Pref.C1 = PrefID)
  
  for (j in 1:length(formulas)) {
    
    print(str_c("Processing", i, j, sep = "-"))
    
    formula = formulas[[j]]
    
    fit00 = inla(formula, 
                 data = current.data,
                 family = "nbinomial", 
                 offset = log(Population),
                 control.inla = list(strategy = 'adaptive'), 
                 control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, waic = TRUE, return.marginals = TRUE),
                 control.fixed = list(correlation.matrix = TRUE),
                 control.predictor = list(link = 1, compute = TRUE), verbose = FALSE)
    
    DIC[i, j] = fit00$dic$dic
    
  }
  DIC
}

DIC.dat = DIC %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Disease") %>% 
  set_names("Disease", 
            "Besag_Int\niid_A\niid_P\niid_C", 
            "Besag_Int\nBesag_A\nBesag_P\niid_C", 
            "iid_Int\niid_A\niid_P\niid_C",
            "iid_Int\nBesag_A\nBesag_P\niid_C",
            "Besag_Int\niid_A\nBesag_P\niid_C", 
            "Besag_Int\nBesag_A\niid_P\niid_C", 
            "iid_Int\nBesag_A\niid_P\niid_C",
            "iid_Int\niid_A\nBesag_P\niid_C") %>% 
  pivot_longer(-1, 
               names_to = "formula", 
               values_to = "DIC")

qn = quantile(heatmap.dat$DIC, 
              seq(0.01,0.99,length.out = 18), 
              na.rm = TRUE)

qn01 = c(qn, range(heatmap.dat$DIC)) %>% 
  rescale() %>% sort()

heatmap.dat %>% 
  group_by(Disease) %>% 
  mutate(DICStan = c(rescale(DIC))) %>% 
  ggplot(aes(formula, Disease, fill = DICStan)) + 
  geom_tile() + 
  geom_text(aes(label = round(DIC, 0)),
            size = 3) + 
  scale_fill_gradientn(colours = colorRampPalette(rev(c("#B71126", "#D73027", "#F46D43", "#FDAE61", '#ffffe5')))(20),  
                       values = qn01,
                       name = "Standardized DIC") + 
  theme_bw() + 
  scale_x_discrete(expand = c(0, 0), 
                   position = "top") +
  scale_y_discrete(expand = c(0, 0)) + 
  labs(x = "", y = "") + 
  theme(axis.ticks = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 9))

# ggsave("./output/SFig/FigS2.pdf", height = 5, width = 7)


# =====================  Fig S3 ============================= #

dat = disease.data %>% 
  filter(!Disease == "All") %>% 
  group_by(Disease) %>% 
  summarise(case = sum(case)) %>% 
  mutate(per = round(case/sum(case)*100, 2)) %>% 
  mutate(Disease = str_c(Disease, " (", per, "%)"))

ggplot(dat, aes(area = case, 
                fill = Disease, 
                label = Disease)) +
  treemapify::geom_treemap(start = "topleft") +
  geom_treemap_text(place = "centre", size = 12,
                    start = "topleft") +
  scale_fill_manual(values = c("#ADB6B6FF", "#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#6A6599FF", "#91D1C2FF", "#DF8F44FF", "#00A1D5FF", "#A20056FF")) +
  labs(fill = "") + 
  theme(legend.position = "none")

# ggsave("./output/SFig/FigS2.pdf", height = 5, width = 7)


# =====================  Fig S4 ============================= #
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

# tmap_save(tmap_arrange(output, nrow = 3), "./output/SFig/FigS4.pdf", width = 8, height = 6)


# =====================  Fig S5 ============================= #
pop.dat_7 = pop.data %>% 
  group_by(DiagnoseYear,
           Age = cut(Age, 
                     breaks = c(0, seq(12,78,3),Inf),
                     right = FALSE)) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() 

inc.dat_7 = disease.data %>% 
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

FigS5 = plot_grid(plotlist = inc.heatmap, ncol = 6)

# save_plot("./output/SFig/FigS5.pdf", Figure.S3, base_height = 7, base_width = 13.5) 

# =====================  Fig S6 ============================= #

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

# ggsave("./output/SFig/FigS6.pdf", height = 4, width = 6)

# =====================  Fig S7 ============================= #

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

# ggsave("./output/SFig/FigS7.pdf", height = 5, width = 8.5)



# =====================  Fig S8 ============================= #
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

# APC effects stratified by sex
plot.dat %>% 
  unnest(Age.effect) %>% 
  mutate(across(c(4, 6:8), ~exp(.x))) %>% 
  # mutate(across(6:8, ~ifelse(!Sex == "Total", NA, .x))) %>%
  ggplot(aes(ID, mean,
             col = Sex,
             size = Sex)) + 
  geom_line() + 
  geom_hline(yintercept = 1, lty = 2, size = 0.4) + 
  geom_ribbon(aes(x = ID, 
                  ymin = `0.025quant`,  
                  ymax = `0.975quant`,
                  fill = Sex),
              alpha = 0.15,
              col = NA) + 
  facet_wrap(~Disease, scales = "free",
             labeller = labeller) + 
  scale_color_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) +
  # scale_fill_manual(values = c("black", NA, NA)) +
  scale_fill_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) +
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
# ggsave("./output/SFig/FigS8.pdf", height = 5, width = 8.5)

# =====================  Fig S9 ============================= #

# Period effects stratified by sex
plot.dat %>% 
  unnest(Period.effect) %>% 
  mutate(across(c(5, 7:9), ~exp(.x))) %>% 
  # mutate(across(6:8, ~ifelse(!Sex == "Total", NA, .x))) %>%
  ggplot(aes(ID, mean, col = Sex, size = Sex)) + 
  geom_line() + 
  geom_hline(yintercept = 1, lty = 2) + 
  geom_ribbon(aes(x = ID, 
                  ymin = `0.025quant`, 
                  ymax = `0.975quant`,
                  fill = Sex),
              alpha = 0.15,
              col = NA) + 
  facet_wrap(~Disease, scales = "free",
             labeller = labeller) + 
  scale_color_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) + 
  scale_fill_manual(values = c("black", "#E64B35FF", "#4DBBD5FF")) +
  # scale_fill_manual(values = c("black", NA, NA)) +
  scale_size_manual(values = c(0.75, 0.45, 0.45)) +
  # scale_alpha_manual(values = c(0.3, 0.3, 0.15)) +
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

# ggsave("./output/SFig/FigS9.pdf", height = 5, width = 8.5)


# =====================  Fig S10 ============================ #
# Cohort effects stratified by sex
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

cohort.smooth %>% 
  # mutate(across(c(`0.025quant`:`0.975quant`, cohort.predict), ~ifelse(!Sex == "Total", NA, .x))) %>% 
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
  scale_size_manual(values = c(0.75, 0.45, 0.45)) +
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

# ggsave("./output/SFig/FigS10.pdf", height = 5, width = 8.5)

# =====================  Fig S11 ============================ #

F1.RR.dat = vector("list", n_distinct(disease.list))
F2.RR.dat = vector("list", n_distinct(disease.list))
names(F1.RR.dat) = disease.list
names(F2.RR.dat) = disease.list

for (i in 1:n_distinct(disease.list)) {
  
  print(str_c("Processing", i, sep = "-"))
  
  current.data = province.data %>%
    filter(Disease == disease.list[i]) %>%
    mutate(Cx.event = case_when(Cx.new == 1957 ~ "Cx.1957",
                                Cx.new == 1960 ~ "Cx.1960",
                                Cx.new == 1963 ~ "Cx.1963",
                                Cx.new == 1978 ~ "Cx.1978",
                                Cx.new == 1981 ~ "Cx.1981",
                                Cx.new == 1984 ~ "Cx.1984",
                                TRUE ~ "other")) %>% 
    mutate(Cx.event = factor(Cx.event, levels = c("Cx.1957", "Cx.1960", "Cx.1963", "Cx.1978", "Cx.1981", "Cx.1984", "other")))
  
  # 计算队列的样条基函数时，用Cx.new更合适 
  # 导致估计出来的队列效应为锯齿状曲线 (Cx 3年一个周期)
  ns.C = ns(current.data$Cx, df = 3)
  colnames(ns.C) = str_c("nsC", colnames(ns.C), sep = "")
  
  Cx.dummies = model.matrix(~ Cx.event - 1, data = current.data) %>%
    as.data.frame()  
  
  colnames(Cx.dummies) = str_remove(colnames(Cx.dummies), "Cx.event")
  
  current.data = cbind(current.data, ns.C, Cx.dummies) 
  
  colnames(current.data)
  
  formula0 = Case ~ f(Ax, model = "rw1") +
    f(Px, model = "rw1") +
    ns.C + 
    Cx.1957 + 
    Cx.1960 +
    Cx.1963 +
    Cx.1978 + 
    Cx.1981 + 
    Cx.1984
  
  fit00 = inla(formula0, 
               data = current.data, 
               family = "nbinomial", 
               offset = log(Population),
               control.inla = list(strategy = 'adaptive'), 
               control.compute = list(dic = TRUE, 
                                      config = TRUE, 
                                      cpo = TRUE, 
                                      return.marginals = FALSE),
               control.fixed = list(correlation.matrix = TRUE, 
                                    prec.intercept = 1, 
                                    prec = 1),
               control.predictor = list(link = 1, 
                                        compute = TRUE), 
               verbose = FALSE)
  
  n.sample = inla.posterior.sample(n = 1000000, fit00)
  
  F1.RR.dat[[i]] = inla.posterior.sample.eval("Cx.1960", n.sample) %>%
    t() %>% 
    as.data.frame() %>% 
    set_names("F1RR") %>% 
    rownames_to_column(var = "n.sample") %>% 
    mutate(F1RR = exp(F1RR))
  
  print(str_c("Processing", i, "F1", sep = "-"))
  
  F2.RR.dat[[i]] = inla.posterior.sample.eval("Cx.1981", n.sample) %>%
    t() %>% 
    as.data.frame() %>% 
    set_names("F2RR") %>% 
    rownames_to_column(var = "n.sample") %>% 
    mutate(F2RR = exp(F2RR)) 
  
  print(str_c("Processing", i, "F2", sep = "-"))
  
}

# F1.RR.dat %>% write_rds("./output/RR/spline_dummy_province_F1RR.rds")
# 
# F2.RR.dat %>% write_rds("./output/RR/spline_dummy_province_F2RR.rds")


F1RR = F1.RR.dat %>% 
  bind_rows(.id = "Disease") %>% 
  as_tibble() %>% 
  group_by(Disease) %>% 
  summarise(F1.mean = mean(F1RR), 
            F1.low = quantile(F1RR, 0.025),
            F1.high = quantile(F1RR, 0.975))

F2RR = F2.RR.dat %>% 
  bind_rows(.id = "Disease") %>% 
  as_tibble() %>% 
  group_by(Disease) %>% 
  summarise(F2.mean = mean(F2RR), 
            F2.low = quantile(F2RR, 0.025),
            F2.high = quantile(F2RR, 0.975))

F1RR %>% 
  left_join(F2RR) %>% 
  write_csv("./output/RR/province_RR_spline_dummy.csv")

P1 = read_csv("./output/RR/province_RR_spline_dummy.csv") %>%
  mutate(var = "Nature Spline")

P2 = read_csv("./output/RR/provincial.RR.by.Sex.csv") %>% 
  filter(Sex == "Total") %>% 
  mutate(var = "RW1")

bind_rows(P1, P2) %>%
  mutate(Disease = factor(Disease, 
                          levels = c("All", "TB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"),
                          labels = c("**All**", "PTB", "Hep. B", "Inf.<br />diarrhea", "Syphilis", "HIV<br />AIDS", "Bacillary<br />dysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E"))) %>% 
  ggplot(aes(Disease,
             y = F1.mean, 
             colour = var)) +
  geom_point(size = 0.9,
             position = position_dodge(0.5)) + 
  geom_hline(yintercept = 1, lty = 3) +
  geom_errorbar(aes(ymin = F1.low,
                    ymax = F1.high),
                width = 0.15,
                position = position_dodge(0.5)) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text = element_text(size = 9), 
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size = 11,
                                  face = "bold")) + 
  labs(x = "", 
       y = "F1 IRR")

# ggsave("./output/SFig/Fig.S11.pdf", height = 4.5, width = 7)

# =====================  Fig S12 ============================ #

IRR = function(current.inla.model, n.samples = 100, F1 = c(1957, 1960, 1963), F2 = c(1978, 1981, 1984), cohort.rm = 1939, current.data, ncores = 7)
{
  post.samples = inla.posterior.sample(n = n.samples, current.inla.model)
  
  age.sample = inla.posterior.sample.eval("Ax", post.samples) %>%
    as_tibble() %>% 
    mutate(Age = current.inla.model$summary.random$Ax$ID) %>% 
    pivot_longer(1:length(post.samples),
                 names_to = "n.sample",
                 values_to = "Age.effect") %>% 
    group_nest(n.sample) %>% 
    rename("Age.effect" = "data")
  
  period.sample = inla.posterior.sample.eval("Px", post.samples) %>%
    as_tibble() %>% 
    mutate(DiagnoseYear = current.inla.model$summary.random$Px$ID) %>% 
    pivot_longer(1:length(post.samples),
                 names_to = "n.sample",
                 values_to = "Period.effect") %>% 
    group_nest(n.sample) %>%
    rename("Period.effect" = "data")
  
  cohort.sample = inla.posterior.sample.eval("Cx.new", post.samples) %>%
    as_tibble() %>% 
    mutate(Cohort = current.inla.model$summary.random$Cx.new$ID) %>%
    pivot_longer(1:length(post.samples),
                 names_to = "n.sample",
                 values_to = "Cohort.effect") %>% 
    group_by(n.sample) %>% 
    arrange(Cohort) %>% 
    slice(-c(1, 2), -c(n(),n()-1)) %>%
    ungroup() %>% 
    group_nest(n.sample) %>% 
    rename("Cohort.effect" = "data")
  
  intercept.sample = inla.posterior.sample.eval("Intercept", post.samples) %>%
    as_tibble() %>% 
    pivot_longer(1:length(post.samples),
                 names_to = "n.sample",
                 values_to = "interept") %>% 
    group_nest(n.sample) %>% 
    rename("intercept" = "data")
  
  current.APC = age.sample %>% 
    left_join(period.sample) %>% 
    left_join(cohort.sample) %>% 
    left_join(intercept.sample)
  
  cl = makeCluster(ncores, type = "SOCK")
  registerDoSNOW(cl)
  
  RR.results = foreach(i = 1:nrow(current.APC), .combine = "rbind", .packages = c("fANCOVA","tidyverse", "mgcv")) %dopar%{
    
    current.age = current.APC$Age.effect[[i]]
    current.period = current.APC$Period.effect[[i]]
    current.cohort = current.APC$Cohort.effect[[i]]
    current.intercept = current.APC$intercept[[i]]
    
    n.sample = current.APC$n.sample[[i]]
    
    
    current.cohort.rm = current.cohort %>% 
      filter(!Cohort %in% c(F1, F2, cohort.rm - 3, cohort.rm, cohort.rm + 3)) 
    
    current.gam = gam(Cohort.effect ~ s(Cohort),
                      data = current.cohort.rm)
    
    current.cohort.gam.predict = predict(current.gam, newdata = current.cohort, se.fit = TRUE)
    
    current.cohort$cohort.predict = current.cohort.gam.predict$fit
    
    current.cohort$cohort.predict.sd = current.cohort.gam.predict$se.fit
    
    Age = c(2006,2006,2006, 2009,2009,2009, 2012,2012,2012, 2015,2015,2015,2018,2018,2018, 2021, 2021, 2021) - cohort.rm
    
    famine.cohort.F1 = data.frame(Age = Age, DiagnoseYear = 2005:2022, Cohort = cohort.rm)
    
    famine.F1 = famine.cohort.F1 %>% 
      as_tibble() %>% 
      left_join(current.age) %>% 
      left_join(current.period) %>% 
      left_join(current.cohort) %>% 
      mutate(intercept = current.intercept) %>% 
      left_join(current.data[, c("Ax", "Px", "Population", "Case")], by = c("Age" = "Ax", "DiagnoseYear" = "Px"))
    
    
    GAM.F1.sample = matrix(NA, nrow(famine.F1), n.samples)
    
    
    current.RR = tibble(n.sample = n.sample,
                        F1.RR = rep(NA, n.samples))
    
    GAM.F1.sample[1,] = rnorm(n.samples, mean = famine.F1$cohort.predict, sd = famine.F1$cohort.predict.sd)
    
    
    GAM.F1.sample = GAM.F1.sample %>% 
      as.data.frame() %>% 
      fill(everything()) %>% 
      as.matrix()
    
    current.RR$F1.RR = apply(GAM.F1.sample, 2, function(x) sum(famine.F1$Case)/sum(exp(famine.F1$Age.effect + famine.F1$Period.effect + x + famine.F1$intercept)*famine.F1$Population))  
    
    
    current.RR
    
  }
  
  stopCluster(cl)
  
  RR.results
  
}

IRR.dat = IRR.data %>% 
  filter(str_detect(Disease_Sex, "Total"))

output = vector("list", 12)

names(output) = IRR.dat$Disease_Sex

for (i in 1:nrow(IRR.dat)) {
  
  print(str_c("Processing", i, sep = "-"))
  
  current.inla.model = IRR.dat$inla.model[[i]]
  current.data = IRR.dat$data[[i]]
  
  Cohorts = seq(1945, 1993, 3)
  
  IRR.output = vector("list", length(Cohorts))
  
  names(IRR.output) = str_c("Cohort", Cohorts, sep = "")
  
  for (j in 1:length(Cohorts)) {
    
    cohort.rm = Cohorts[[j]]
    
    IRR.output[[j]] = IRR(current.inla.model, n.samples = 500, F1 = c(1957, 1960, 1963), F2 = c(1978, 1981, 1984), cohort.rm = cohort.rm, current.data, ncores = 7)
    
  }
  
  output[[i]] = IRR.output
  
}

current.IRR = list()

for (i in 1:12) {
  
  current.IRR[[i]] = bind_rows(output[[i]], .id = "Cohort") %>% 
    mutate(Disease = names(output)[i])
  
  IRR = bind_rows(current.IRR)
}

labeller = function(variable, value){
  label = str_c("(", LETTERS[value], ") ", value)
  return(label)
}

IRR %>% 
  mutate(Cohort = str_extract(Cohort, "\\d+"),
         Cohort = as.numeric(Cohort),
         Disease = str_remove(Disease, "-Total"),
         Disease = factor(Disease, levels = c("All", "TB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"))) %>% 
  # filter(Cohort %in% 1950:1990) %>% 
  ggplot(aes(Cohort, F1.mean)) + 
  geom_rect(aes(xmin = 1957, 
                xmax = 1963, 
                ymin = -Inf, 
                ymax = Inf),
            fill = "#e5f5f9", 
            alpha = .2) +
  geom_rect(aes(xmin = 1978, xmax = 1984, ymin = -Inf, ymax = Inf),fill = "#e5f5f9", alpha = .2) + 
  annotate("text", x = 1960, y = Inf, label = "F1") + 
  geom_point(size = 0.75) + 
  geom_hline(yintercept = 1, lty = 2, size = 0.2) +
  facet_wrap(~Disease, ncol = 3, scales = "free",
             labeller = labeller) + 
  geom_errorbar(aes(ymin = F1.low, ymax = F1.high),
                width = 0.9) + 
  theme_classic() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = -0.01,
                                  size = 10.5),
        axis.text = element_text(size = 9)) + 
  labs(y = "F1 IRR")

ggsave("./output/SFig/FigS12.pdf", height = 6, width = 9)


# sensitivity analysis

knots = 2:5
k.F1.output = vector("list", 4)
k.F2.output = vector("list", 4)

for (k in knots) {
  
  RR.output.1960 = vector("list", 11)
  RR.output.1981 = vector("list", 11)
  
  disease.list = unique(pref.data$Disease)
  names(RR.output.1960) = disease.list
  names(RR.output.1981) = disease.list
  
  for (i in 1:n_distinct(disease.list)) {
    
    print(str_c("Processing", i, sep = "-"))
    
    current.data = province.data %>%
      filter(Disease == disease.list[i]) %>%
      mutate(Cx.event = case_when(Cx.new == 1957 ~ "Cx.1957",
                                  Cx.new == 1960 ~ "Cx.1960",
                                  Cx.new == 1963 ~ "Cx.1963",
                                  Cx.new == 1978 ~ "Cx.1978",
                                  Cx.new == 1981 ~ "Cx.1981",
                                  Cx.new == 1984 ~ "Cx.1984",
                                  TRUE ~ "other")) %>% 
      mutate(Cx.event = factor(Cx.event, levels = c("Cx.1957", "Cx.1960", "Cx.1963", "Cx.1978", "Cx.1981", "Cx.1984", "other")))
    
    # 计算队列的样条基函数时，用Cx.new更合适 
    # 导致估计出来的队列效应为锯齿状曲线 (Cx 3年一个周期)
    ns.C = ns(current.data$Cx, knots = k)
    colnames(ns.C) = str_c("nsC", colnames(ns.C), sep = "")
    
    Cx.dummies = model.matrix(~ Cx.event - 1, data = current.data) %>%
      as.data.frame()  
    
    colnames(Cx.dummies) = str_remove(colnames(Cx.dummies), "Cx.event")
    
    current.data = cbind(current.data, ns.C, Cx.dummies) 
    
    colnames(current.data)
    
    formula0 = Case ~ f(Ax, model = "rw1") +
      f(Px, model = "rw1") +
      ns.C + 
      Cx.1957 + 
      Cx.1960 +
      Cx.1963 +
      Cx.1978 + 
      Cx.1981 + 
      Cx.1984
    
    fit00 = inla(formula0, 
                 data = current.data, 
                 family = "nbinomial", 
                 offset = log(Population),
                 control.inla = list(strategy = 'adaptive'), 
                 control.compute = list(dic = TRUE, 
                                        config = TRUE, 
                                        cpo = TRUE, 
                                        return.marginals = FALSE),
                 control.fixed = list(correlation.matrix = TRUE, 
                                      prec.intercept = 1, 
                                      prec = 1),
                 control.predictor = list(link = 1, 
                                          compute = TRUE), 
                 verbose = FALSE)
    
    n.sample = inla.posterior.sample(n = 100000, fit00)
    
    RR.output.1960[[i]] = inla.posterior.sample.eval("Cx.1960", n.sample) %>%
      t() %>% 
      as.data.frame() %>% 
      set_names("F1RR") %>% 
      rownames_to_column(var = "n.sample") %>% 
      mutate(F1RR = exp(F1RR))
    
    RR.output.1981[[i]] = inla.posterior.sample.eval("Cx.1981", n.sample) %>%
      t() %>% 
      as.data.frame() %>% 
      set_names("F2RR") %>% 
      rownames_to_column(var = "n.sample") %>% 
      mutate(F2RR = exp(F2RR)) 
    
    print(str_c("Processing", i, "F2", sep = "-"))
    
  }
  
  k.F1.output[[k-1]] = RR.output.1960 %>% 
    bind_rows(.id = "Disease") %>% 
    as_tibble() %>% 
    group_by(Disease) %>% 
    summarise(F1.mean = mean(F1RR), 
              F1.low = quantile(F1RR, 0.025),
              F1.high = quantile(F1RR, 0.975))
  
  k.F2.output[[k-1]] = RR.output.1981 %>% 
    bind_rows(.id = "Disease") %>% 
    as_tibble() %>% 
    group_by(Disease) %>% 
    summarise(F2.mean = mean(F2RR), 
              F2.low = quantile(F2RR, 0.025),
              F2.high = quantile(F2RR, 0.975))
  
}


names(k.F1.output) = knots
names(k.F2.output) = knots

bind_rows(k.F1.output, .id = "knots") %>% write_csv("F1IRR_knots2345.csv")

bind_rows(k.F2.output, .id = "knots") %>% write_csv("F2IRR_knots2345.csv")

bind_rows(k.F2.output, .id = "knots")
bind_rows(k.F1.output, .id = "knots") %>% 
  mutate(Disease = factor(Disease, 
                          levels = c("All", "TB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"),
                          labels = c("**All**", "PTB", "Hep. B", "Inf.<br />diarrhea", "Syphilis", "HIV<br />AIDS", "Bacillary<br />dysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E"))) %>% 
  ggplot(aes(Disease,
             y = F1.mean, 
             colour = knots)) +
  geom_point(size = 0.6,
             position = position_dodge(0.5)) + 
  geom_hline(yintercept = 1, lty = 3) +
  geom_errorbar(aes(ymin = F1.low,
                    ymax = F1.high),
                width = 0.25,
                position = position_dodge(0.5)) +
  scale_color_npg() + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0),
        legend.position = "top",
        # legend.title = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text = element_text(size = 9), 
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size = 11,
                                  face = "bold")) + 
  labs(x = "", 
       y = "F1 IRR")

# ggsave("./output/SFig/FigST3.1.pdf", height = 4.5, width = 7)

df = 3:6
df.F1.output = vector("list", 4)
df.F2.output = vector("list", 4)

names(df.F1.output) = c("NA", df)
names(df.F2.output) = c("NA", df)

for (k in df) {
  
  RR.output.1960 = vector("list", 11)
  RR.output.1981 = vector("list", 11)
  
  disease.list = unique(pref.data$Disease)
  names(RR.output.1960) = disease.list
  names(RR.output.1981) = disease.list
  
  for (i in 1:n_distinct(disease.list)) {
    
    print(str_c("Processing", i, sep = "-"))
    
    current.data = province.data %>%
      filter(Disease == disease.list[i]) %>%
      mutate(Cx.event = case_when(Cx.new == 1957 ~ "Cx.1957",
                                  Cx.new == 1960 ~ "Cx.1960",
                                  Cx.new == 1963 ~ "Cx.1963",
                                  Cx.new == 1978 ~ "Cx.1978",
                                  Cx.new == 1981 ~ "Cx.1981",
                                  Cx.new == 1984 ~ "Cx.1984",
                                  TRUE ~ "other")) %>% 
      mutate(Cx.event = factor(Cx.event, levels = c("Cx.1957", "Cx.1960", "Cx.1963", "Cx.1978", "Cx.1981", "Cx.1984", "other")))
    
    ns.C = ns(current.data$Cx, df = k)
    colnames(ns.C) = str_c("nsC", colnames(ns.C), sep = "")
    
    Cx.dummies = model.matrix(~ Cx.event - 1, data = current.data) %>%
      as.data.frame()  
    
    colnames(Cx.dummies) = str_remove(colnames(Cx.dummies), "Cx.event")
    
    current.data = cbind(current.data, ns.C, Cx.dummies) 
    
    colnames(current.data)
    
    formula0 = Case ~ f(Ax, model = "rw1") +
      f(Px, model = "rw1") +
      ns.C + 
      Cx.1957 + 
      Cx.1960 +
      Cx.1963 +
      Cx.1978 + 
      Cx.1981 + 
      Cx.1984
    
    fit00 = inla(formula0, 
                 data = current.data, 
                 family = "nbinomial", 
                 offset = log(Population),
                 control.inla = list(strategy = 'adaptive'), 
                 control.compute = list(dic = TRUE, 
                                        config = TRUE, 
                                        cpo = TRUE, 
                                        return.marginals = FALSE),
                 control.fixed = list(correlation.matrix = TRUE, 
                                      prec.intercept = 1, 
                                      prec = 1),
                 control.predictor = list(link = 1, 
                                          compute = TRUE), 
                 verbose = FALSE)
    
    n.sample = inla.posterior.sample(n = 200000, fit00)
    
    RR.output.1960[[i]] = inla.posterior.sample.eval("Cx.1960", n.sample) %>%
      t() %>% 
      as.data.frame() %>% 
      set_names("F1RR") %>% 
      rownames_to_column(var = "n.sample") %>% 
      mutate(F1RR = exp(F1RR))
    
    RR.output.1981[[i]] = inla.posterior.sample.eval("Cx.1981", n.sample) %>%
      t() %>% 
      as.data.frame() %>% 
      set_names("F2RR") %>% 
      rownames_to_column(var = "n.sample") %>% 
      mutate(F2RR = exp(F2RR)) 
    
    print(str_c("Processing", i, "F2", sep = "-"))
    
  }
  
  df.F1.output[[k-1]] = RR.output.1960 %>% 
    bind_rows(.id = "Disease") %>% 
    as_tibble() %>% 
    group_by(Disease) %>% 
    summarise(F1.mean = mean(F1RR), 
              F1.low = quantile(F1RR, 0.025),
              F1.high = quantile(F1RR, 0.975))
  
  df.F2.output[[k-1]] = RR.output.1981 %>% 
    bind_rows(.id = "Disease") %>% 
    as_tibble() %>% 
    group_by(Disease) %>% 
    summarise(F2.mean = mean(F2RR), 
              F2.low = quantile(F2RR, 0.025),
              F2.high = quantile(F2RR, 0.975))
  
}

bind_rows(df.F1.output, .id = "knots") %>% write_csv("F1IRR_df3456.csv")

bind_rows(df.F2.output, .id = "knots") %>% write_csv("F2IRR_df3456.csv")


bind_rows(df.F1.output, .id = "df") %>% 
  mutate(Disease = factor(Disease, 
                          levels = c("All", "TB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"),
                          labels = c("**All**", "PTB", "Hep. B", "Inf.<br />diarrhea", "Syphilis", "HIV<br />AIDS", "Bacillary<br />dysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E"))) %>% 
  ggplot(aes(Disease,
             y = F1.mean, 
             colour = df)) +
  geom_point(size = 0.75,
             position = position_dodge(0.5)) + 
  geom_hline(yintercept = 1, lty = 3) +
  geom_errorbar(aes(ymin = F1.low,
                    ymax = F1.high),
                width = 0.2,
                position = position_dodge(0.5)) +
  scale_color_npg() + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0),
        legend.position = "top",
        # legend.title = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text = element_text(size = 9), 
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size = 11,
                                  face = "bold")) + 
  labs(x = "", 
       y = "F1 IRR")

ggsave("./output/SFig/Fig.ST3.2.pdf", height = 4.5, width = 7)

# =====================  Fig S13 ============================ #

RR = read_csv("./output/RR/provincial.RR.by.Sex.csv") %>% 
  mutate(Sex = factor(Sex, levels = c("Total", "Female", "Male")),
         Disease = ifelse(Disease == "TB", "PTB", Disease)) %>% 
  arrange(Sex) %>% 
  mutate(Disease = str_replace(Disease, "_", " ")) %>%
  mutate(Disease = factor(Disease, 
                          levels = c("All", "PTB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"),
                          labels = c("**All**", "PTB", "Hep. B", "Inf.<br />diarrhea", "Syphilis", "HIV<br />AIDS", "Bacillary<br />dysentery", "Hep. C", "Gonorrhea", "Hep. A", "AHC", "Hep. E"))) %>% 
  arrange(Disease, Sex) 


FigS13A = RR %>% 
  ggplot(aes(Disease,
             y = F2.mean)) +
  geom_rect(aes(xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = "gray90", alpha = 0.1, col = NA)+ 
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
       y = "F2 IRR")

# ref
mean_level = colMeans(model.matrix(~ factor(Disease), data = Pref.RR.results))[-1] %>% mean()

meta.reg.F2 = rma(yi = F2.log.mean, 
                  sei = F2.log.sd, 
                  mods = ~ CSSI + factor(Disease),
                  data = Pref.RR.results, 
                  method = "REML")

rma.model = Pref.RR.results %>%  
  dplyr::select(F2.log.mean, F2.log.sd, CSSI, Disease, Pref) %>%
  group_nest(Disease) %>% 
  mutate(rma.model = map(data, ~rma(yi = F2.log.mean, 
                                    sei = F2.log.sd, 
                                    mods = ~ CSSI,
                                    data = .))) %>% 
  mutate(tidy_summary = map(rma.model, tidy)) %>%
  unnest(tidy_summary) %>%
  filter(term == "CSSI") 

P.value = rma.model %>%
  select(Disease, estimate, p.value) %>%
  mutate(p.cat = cut(p.value, breaks = c(0, 0.01, 0.05, 0.1, 0.3, 0.5, Inf)))


meta.results = rma.model %>%
  mutate(pred = map(rma.model, ~predict(.x, newmods= seq(0.25, 0.6, 0.01)))) %>% 
  mutate(pred = map(pred, ~reshape.res(.x))) %>% 
  unnest(pred) %>%
  left_join(P.value)


FigS13B = predict(meta.reg.F2, matrix(c(newmods, rep(mean_level, length(newmods)*10)), ncol = 11)) %>% 
  as_tibble() %>% 
  mutate(x = seq(0.25,0.6,0.01)) %>% 
  ggplot() + 
  geom_point(data = Pref.RR.results,
             aes(x = CSSI, 
                 y = F2.mean, 
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


appender = function(Disease) str_c("(",LETTERS[1:11], ") ", Disease)

FigS13C = meta.results %>% 
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

# save_plot("./output/SFig/FigS13.pdf",plot_grid(FigS13A, Figs13B, FigS13C, ncol = 1, rel_heights = c(3,5,5), labels = c("(A)", "(B)", "(C)"), label_size = 12), base_width = 8, base_height = 9)32



