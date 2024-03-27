# run model
inla.model = vector("list", 11)
disease.list = unique(pref.data$Disease)
names(inla.model) = disease.list

for(i in 1:length(disease.list))
{
  
  print(str_c("Processing", i, sep = "-"))
  
  current.data = pref.data %>%
    filter(Disease == disease.list[i]) %>%
    mutate(Pref.A1 = PrefID,   # replicates of PrefID for using in INLA
           Pref.P1 = PrefID,
           Pref.C1 = PrefID)

  formula0 = Case ~ -1 + f(PrefID, model = "iid", constr = FALSE) +
    f(Ax, model = "rw1", group = Pref.A1, control.group = list(model = "besag", graph = "SC.graph")) +
    f(Px, model = "rw1", group = Pref.P1, control.group = list(model = "iid")) +
    f(Cx.new, model = "rw1", group = Pref.C1, control.group = list(model = "iid"))
  
  fit00 = inla(formula0, 
               data = current.data,
               family = "nbinomial", 
               offset = log(Population),
               control.inla = list(strategy = 'adaptive'), 
               control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, waic = TRUE, return.marginals = TRUE),
               control.fixed = list(correlation.matrix = TRUE),
               control.predictor = list(link = 1, compute = TRUE), verbose = FALSE)
  
  inla.model[[i]] = fit00 
}

inla.model %>% write_rds("./output/INLA.model/inla.model.structured.age.rds")


inla.model = read_rds("./output/INLA.model/inla.model.structured.age.rds")
Pref.table = data.frame(Pref = SC.shp$Pref,
                        PrefID = 1:21)
pref.F2 = pref.thres %>% 
  left_join(Pref.table) %>% 
  arrange(PrefID)


# ========= estimate prefecture-level IRRs ==========
pref.RR.list = vector("list", 11)
names(pref.RR.list) = names(inla.model)

for (k in 1:length(inla.model)) {
  
  print(str_c("Processing", k, sep = " "))
  
  current.inla.model = inla.model[[k]]
  current.data = pref.data %>%
    filter(Disease == names(pref.RR.list)[[k]])
  
  pref.RR.list[[k]] = pref.RR(current.inla.model, n.samples = 1000, F1.start = 1957, F1.end = 1963, F2.start = pref.thres$mid.cohort - 3, F2.end = pref.F2$mid.cohort + 3, F2 = pref.F2$mid.cohort, current.data, ncores = n.cores)
  
  current.RR = pref.RR.list[[k]] %>% 
    group_nest(PrefID)
  
  path = str_c("./output/RR/", gsub("/",".", names(inla.model)[k]), ".structured.age.RR.rds", sep = "")
  
  write_rds(current.RR, path)
}


Pref.RR.results = NULL

for(i in 1:11) {
  path = str_c("./output/RR/", gsub("/",".", names(inla.model)[i]), ".structured.age.RR.rds", sep = "")
  
  current.RR = read_rds(path)
  
  RR = current.RR %>% 
    unnest(data) %>% 
    group_by(PrefID) %>% 
    summarise(F1.mean = mean(F1.RR), 
              F1.low = quantile(F1.RR, 0.025),
              F1.high = quantile(F1.RR, 0.975),
              F2.mean = mean(F2.RR), 
              F2.low = quantile(F2.RR, 0.025),
              F2.high = quantile(F2.RR, 0.975),
              F1.log.mean = mean(log(F1.RR)), 
              F1.log.sd = sd(log(F1.RR)), 
              F2.log.mean = mean(log(F2.RR)), 
              F2.log.sd = sd(log(F2.RR))) %>% 
    mutate(Disease = names(inla.model)[i])
  
  Pref.RR.results = rbind(Pref.RR.results, RR)
}


Pref.RR.results %>% 
  left_join(Pref.table) %>% 
  write_csv("./output/RR/INLA_pref_RR_structured_age_iid_cohort.csv")
