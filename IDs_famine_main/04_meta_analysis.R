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


# A pooled meta-regression model to estimate the common effect of CSSI across all diseases
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


# A separate meta-regression model for each individual disease
reshape.res = function(df){
  df = df %>% as_tibble() %>%
    mutate(x = seq(0.25,0.6,0.01))
}

rma.model = Pref.RR.results %>%  
  dplyr::select(F1.log.mean, F1.log.sd, CSSI, Disease, Pref) %>%
  group_nest(Disease) %>% 
  mutate(rma.model = map(data, ~rma(yi = F1.log.mean, 
                                    sei = F1.log.sd, 
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

Pref.RR.results = Pref.RR.results %>%
  left_join(P.value)


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

