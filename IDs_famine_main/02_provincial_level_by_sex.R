# Run model
province.data.bydisease = read_rds("./data/provincial_model_data.rds")

province.data = read_rds("./data/provincial_model_data_alldisease.rds") %>%
  mutate(Disease = "All") %>%
  rbind(province.data.bydisease) 

list.name = str_c(province.data$Disease, province.data$Sex, sep = "-")

inla.model = vector("list", nrow(province.data))

output = vector("list", nrow(province.data))

names(inla.model) = list.name

for(i in 1:nrow(province.data))
{
  print(str_c("Processing", i, sep = "-"))
  
  current.data = province.data$data[[i]]  %>% 
    mutate(ID = row_number())
  
  Disease_Sex = list.name[[i]]
           
  formula0 = Case ~ f(Ax, model = "rw1") +
    f(Px, model = "rw1") +
    f(Cx.new, model = "rw1") 
  
  fit00 = inla(formula0, 
               data = current.data,
               family = "nbinomial", 
               offset = log(Population),
               control.inla = list(strategy = 'adaptive'), 
               control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, return.marginals = TRUE),
               control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
               control.predictor = list(link = 1, compute = TRUE), verbose = FALSE)
  
  Cohort.fit = fit00$summary.random$Cx.new %>% 
    mutate(Disease_Sex = Disease_Sex)

  Age.fit = fit00$summary.random$Ax %>% 
    mutate(Disease_Sex = Disease_Sex)
 
  Period.fit = fit00$summary.random$Px %>% 
    mutate(Disease_Sex = Disease_Sex)
  
  output[[i]] = tibble(Disease_Sex,
         Age.effect = list(Age.fit),
         Period.effect = list(Period.fit),
         Cohort.effect = list(Cohort.fit))
  
  inla.model[[i]] = fit00 
}

# save BAPC model results
INLA.model = tibble(Disease_Sex = names(inla.model), inla.model)
INLA.model %>% write_rds("output/INLA.model/provincial_level_INLA.rds")
bind_rows(output) %>% write_rds("output/INLA.model/provincial_level_APC.effects.rds")  # for plotting



# ========= estimate provincial-level IRRs ==========
INLA.model = read_rds("output/INLA.model/provincial_level_INLA.rds")

province.data = read_rds("./data/provincial_model_data_alldisease.rds") %>%
  mutate(Disease = "All") %>%
  rbind(province.data.bydisease) %>% 
  mutate(Disease_Sex = str_c(Disease, Sex, sep = "-")) %>% select(4, 2)

IRR.data = province.data %>% 
  left_join(INLA.model)

RR.output = vector("list", nrow(INLA.model))
names(RR.output) = IRR.data$Disease_Sex


# since Monte Carlo simulation and posterior sampling method are used here, the results may not be exactly the same from run to run; this function paralellize the calculation with the foreach package. The number of cores used can be changed by setting the ncore parameter.
for (i in 1:nrow(INLA.model)) {
  
  print(str_c("Processing", i, sep = "-"))
  
  current.inla.model = IRR.data$inla.model[[i]]
  current.data = IRR.data$data[[i]]
  
  RR.output[[i]] = provincial.IRR(current.inla.model, n.samples = 1000, F1.start = 1957, F1.end = 1963, F2.start = 1978, F2.end = 1984, F2 = 1981, current.data, ncores = n.cores)
}

RR.results = RR.output %>% 
  bind_rows(.id = "Disease_Sex") %>% 
  group_nest(Disease_Sex) %>% 
  separate(Disease_Sex,
           into = c("Disease", "Sex"),
           sep = "-")

RR.results %>% write_rds("./output/RR/proincial_IRR_by_sex.rds")

RR = RR.results %>% 
  unnest(data) %>% 
  group_by(Disease, Sex) %>% 
  summarise(F1.mean = mean(F1.RR),
            F1.low = quantile(F1.RR, 0.025),
            F1.high = quantile(F1.RR, 0.975),
            F2.mean = mean(F2.RR), 
            F2.low = quantile(F2.RR, 0.025),
            F2.high = quantile(F2.RR, 0.975))

RR 

RR %>% write_csv("./output/RR/provincial.RR.by.Sex.csv")
