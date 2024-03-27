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


meta.reg.F1 = rma(yi = F1.log.mean, 
                  sei = F1.log.sd, 
                  mods = ~ CSSI + factor(Disease),
                  data = Pref.RR.results, 
                  method = "REML")

Q1 = predict(meta.reg.F1, newmods = rep(43.82262/100, 11))
Q3 = predict(meta.reg.F1, newmods = rep(52.07122/100, 11))

slope.sim = rnorm(10000, summary(meta.reg.F1)$beta[2], summary(meta.reg.F1)$se[2])

intercept.sim = rnorm(10000, summary(meta.reg.F1)$beta[1], summary(meta.reg.F1)$se[1])

Q1.RR.sim = exp(intercept.sim + slope.sim*43.82262/100)
Q3.RR.sim = exp(intercept.sim + slope.sim*52.07122/100)

IQR.F1 = (Q3.RR.sim - Q1.RR.sim)/Q1.RR.sim

quantile(IQR.F1, c(0.025, 0.5, 0.975))*100

newmods = seq(0.25, 0.6, 0.01)

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
   

# fixed-effect meta analysis for each disease
reshape.res = function(df){
  df = df %>% as_tibble() %>%
    mutate(x = seq(0.25,0.6,0.01))
}


meta.p.value = Pref.RR.results %>%  
  dplyr::select(F1.log.mean, F1.log.sd, CSSI, Disease, Pref) %>%  
  group_nest(Disease) %>% 
  mutate(rma.model = map(data, ~rma(yi = F1.log.mean, 
                                    sei = F1.log.sd, 
                                    mods = ~ CSSI,
                                    data = .))) %>% 
  mutate(tidy_summary = map(rma.model, tidy)) %>%
  unnest(tidy_summary) %>%
  filter(term == "CSSI") %>%
  select(Disease, estimate, p.value) %>%
  mutate(p.cat = cut(p.value, breaks = c(0, 0.01, 0.05, 0.1, 0.3, 0.5, Inf))) 

meta.results = Pref.RR.results %>%  
  dplyr::select(F1.log.mean, F1.log.sd, CSSI, Disease, Pref) %>%  
  group_nest(Disease) %>% 
  mutate(rma.model = map(data, ~rma(yi = F1.log.mean, 
                                    sei = F1.log.sd, 
                                    mods = ~ CSSI,
                                    data = .))) %>% 
  mutate(tidy_summary = map(rma.model, tidy)) %>%
  unnest(tidy_summary) %>%
  filter(term == "CSSI") %>%
  mutate(pred = map(rma.model, ~predict(.x, newmods= seq(0.25, 0.6, 0.01)))) %>% 
  mutate(pred = map(pred, ~reshape.res(.x))) %>% 
  unnest(pred) %>%
  left_join(meta.p.value)

Pref.RR.results2 = Pref.RR.results %>% 
  left_join(meta.p.value) 

appender = function(Disease) str_c("(",LETTERS[1:11], ") ", Disease)

Fig5C = meta.results %>% 
  ggplot() + 
  geom_hline(yintercept = 1, 
             linetype = 3,
             size = 0.25) + 
  geom_point(data = Pref.RR.results2,
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


rma.mod = meta.results %>% 
  distinct(Disease, .keep_all = TRUE) %>% .$rma.model

rma.mod %>% map("pval") %>% 
  as.data.frame() %>%
  t() %>% 
  as_tibble() %>% 
  rename(CSSI.pval = V2)

coef = rma.mod %>% map("beta") %>% 
  as.data.frame() %>%
  t() %>% 
  as_tibble() %>% 
  mutate(Disease = c("TB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E"))

CIs = vector("list", 11)

for (i in 1:11) {
  meta.reg.F1 = rma.mod[[i]]
  
  Q1 = predict(meta.reg.F1, newmods = rep(43.82262/100, 12))
  Q3 = predict(meta.reg.F1, newmods = rep(52.07122/100, 12))
  
  slope.sim = rnorm(100000, summary(meta.reg.F1)$beta[2], summary(meta.reg.F1)$se[2])
  
  intercept.sim = rnorm(100000, summary(meta.reg.F1)$beta[1], summary(meta.reg.F1)$se[1])
  
  Q1.RR.sim = exp(intercept.sim + slope.sim*43.82262/100)
  Q3.RR.sim = exp(intercept.sim + slope.sim*52.07122/100)
  
  IQR.F1 = (Q3.RR.sim - Q1.RR.sim)/Q1.RR.sim
  
  CIs[[i]] = quantile(IQR.F1, c(0.025, 0.5, 0.975))*100
}

Table.S4 = bind_rows(CIs) %>% 
  mutate(Disease = c("TB", "Hepatitis B", "Infectious diarrhea", "Syphilis", "HIV/AIDS", "Bacillary dysentery", "Hepatitis C", "Gonorrhea", "Hepatitis A", "AHC", "Hepatitis E")) %>%
  left_join(coef)

