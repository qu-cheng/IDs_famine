 # R script to load packages and prepare data for APC analysis
library(readr)
library(spdep)
library(raster)
library(tidyverse)
library(ggplot2)
library(tmap)
library(treemapify)
library(cowplot)
library(ggsci)
library(RColorBrewer)
library(scales)
library(mgcv)
library(foreach)
library(doSNOW)
library(metafor)
library(INLA)
library(broom)


n.cores = 8   # number of cores used for computation

## STEP 1. load data 
#==== for fitting models ====
province.data = read_rds("./data/provincial_model_data.rds")
pref.data = read_rds("./data/prefecture_model_data.rds")

#==== for making plots ====
# prefecture-level disease data
disease.data = read_csv("./data/11.by_sex_pref_disease.data.csv")

# prefecture-level population data
pop.data = read_csv("./data/pop.by_pref_age_sex_2005-2022.csv") %>% 
  pivot_longer(-c(1:3),
               names_to = "Pref",
               values_to = "Population") %>%
  rename(DiagnoseYear = Year) %>% 
  mutate(across(2:4, ~as.numeric(.))) %>% 
  filter(DiagnoseYear >= 2005)

famine.intensity = read_csv("./data/Famine intensity by pref.csv")  # prefecture-level famine intensity

pref.thres = read_csv("./data/F2 time by prefecture population data.csv") 
# birth year ranges of F2 for each individual prefecture

SC.shp = shapefile("./data/Sichuan_City.shp")
colnames(SC.shp@data)[2] = "Pref"
SC.shp@data$Pref.name = c("Chengdu", "Zigong","Panzhihua","Luzhou","Deyang","Mianyang","Guangyuan","Suining","Neijiang","Leshan","Nanchong","Meishan","Yibin","Guang'an","Dazhou","Yaan","Bazhong","Ziyang","Aba","Ganzi","Liangshan")

SC.shp.nb = poly2nb(SC.shp)
nb2INLA("SC.graph", SC.shp.nb)

CSSI = read.csv("./data/Famine intensity by pref.csv")

# smooth the cohort effect(GAM)
province.cohort.smooth = function(results, F1.start = 1957, F1.end = 1963, F2.start = 1978, F2 = 1981, F2.end = 1984) {
  
  current.cohort = results
  
  colnames(current.cohort)[1:3] = c("Cohort","Cohort.effect.e","Cohort.std")
  
  current.cohort.rm = current.cohort %>% 
    filter(!Cohort %in% c(seq(F1.start, F1.end, 3), seq(F2.start, F2.end, 3))) %>%  
    slice(-c(1, 2), -c(n(),n()-1))
  
  current.gam = gam(Cohort.effect.e ~ s(Cohort), data = current.cohort.rm)
  
  current.cohort$cohort.predict = predict(current.gam, newdata = current.cohort)
  
  current.cohort 
}

# Estimate province-level IRR
provincial.IRR = function(current.inla.model, n.samples = 1000, F1.start = 1957, F1.end = 1963, F2.start = 1978, F2.end = 1984, F2 = 1981, current.data, ncores = 7)
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
      filter(!Cohort %in% c(seq(F1.start, F1.end, 3), seq(F2.start, F2.end, 3))) 
    
    current.gam = gam(Cohort.effect ~ s(Cohort),
                      data = current.cohort.rm)
    
    current.cohort.gam.predict = predict(current.gam, newdata = current.cohort, se.fit = TRUE)
    
    current.cohort$cohort.predict = current.cohort.gam.predict$fit

    
    
    
    
    current.cohort$cohort.predict.sd = current.cohort.gam.predict$se.fit
    
    famine.cohort.F1 = data.frame(Age = rep(seq(46,61,3), each = 3), DiagnoseYear = 2005:2022, Cohort = (F1.start + F1.end)/2)
    
    famine.cohort.F1$Age = c(2006,2006,2006, 2009,2009,2009, 2012,2012,2012, 2015,2015,2015,2018,2018,2018, 2021, 2021, 2021) - (F1.start + F1.end)/2
    
    famine.cohort.F2 = data.frame(DiagnoseYear = 2005:2022, Cohort = F2, period.collapse = c(2006,2006,2006, 2009,2009,2009, 2012,2012,2012, 2015,2015,2015,2018, 2018, 2018, 2021, 2021, 2021))
    
    famine.cohort.F2$Age = famine.cohort.F2$period.collapse - famine.cohort.F2$Cohort
    
    famine.F1 = famine.cohort.F1 %>% 
      as_tibble() %>% 
      left_join(current.age) %>% 
      left_join(current.period) %>% 
      left_join(current.cohort) %>% 
      mutate(intercept = current.intercept) %>% 
      left_join(current.data[, c("Ax", "Px", "Population", "Case")], by = c("Age" = "Ax", "DiagnoseYear" = "Px"))
    
    famine.F2 = famine.cohort.F2 %>% 
      as_tibble() %>% 
      left_join(current.age) %>% 
      left_join(current.period) %>% 
      left_join(current.cohort) %>%
      mutate(intercept = current.intercept) %>%
      left_join(current.data[, c("Ax", "Px", "Population", "Case")], by = c("Age" = "Ax", "DiagnoseYear" = "Px"))
    
    
    GAM.F1.sample = matrix(NA, nrow(famine.F1), n.samples)
    GAM.F2.sample = matrix(NA, nrow(famine.F2),n.samples)
    
    current.RR = tibble(n.sample = n.sample,
                        F1.RR = rep(NA, n.samples),
                        F2.RR = rep(NA, n.samples))
    
    GAM.F1.sample[1,] = rnorm(n.samples, mean = famine.F1$cohort.predict, sd = famine.F1$cohort.predict.sd)
    
    GAM.F2.sample[1,] = rnorm(n.samples, mean = famine.F2$cohort.predict, sd = famine.F2$cohort.predict.sd)
    
    GAM.F1.sample = GAM.F1.sample %>% 
      as.data.frame() %>% 
      fill(everything()) %>% 
      as.matrix()
    
    GAM.F2.sample = GAM.F2.sample %>% 
      as.data.frame() %>% 
      fill(everything()) %>% 
      as.matrix()
    
    current.RR$F1.RR = apply(GAM.F1.sample, 2, function(x) sum(famine.F1$Case)/sum(exp(famine.F1$Age.effect + famine.F1$Period.effect + x + famine.F1$intercept)*famine.F1$Population))  
    
    current.RR$F2.RR = apply(GAM.F2.sample, 2, function(x) sum(famine.F2$Case)/sum(exp(famine.F2$Age.effect + famine.F2$Period.effect + x + famine.F2$intercept)*famine.F2$Population))  
    
    current.RR
    
  }
  
  stopCluster(cl)
  
  RR.results
  
}


# Estimate prefecture-level IRR
pref.RR = function(current.inla.model, n.samples = 1000, F1.start = 1957, F1.end = 1963, F2.start = rep(1978, 21), F2.end = rep(1984, 21), F2 = rep(1981, 21), current.data, ncores = 7)
{
  post.sample = inla.posterior.sample(n = n.samples, current.inla.model)
  
  age.sample = inla.posterior.sample.eval("Ax", post.sample) %>%
    as_tibble() %>% 
    mutate(Age = current.inla.model$summary.random$Ax$ID,
           PrefID = rep(1:21, each = length(unique(current.inla.model$summary.random$Ax$ID)))) %>% 
    pivot_longer(1:length(post.sample),
                 names_to = "n.sample",
                 values_to = "Age.effect") %>% 
    group_nest(PrefID) %>% 
    rename("Age.effect" = "data")
  
  period.sample = inla.posterior.sample.eval("Px", post.sample) %>%
    as_tibble() %>% 
    mutate(DiagnoseYear = current.inla.model$summary.random$Px$ID,
           PrefID = rep(1:21, each = length(unique(current.inla.model$summary.random$Px$ID)))) %>% 
    pivot_longer(1:length(post.sample),
                 names_to = "n.sample",
                 values_to = "Period.effect") %>% 
    group_nest(PrefID) %>%
    rename("Period.effect" = "data")
  
  cohort.sample = inla.posterior.sample.eval("Cx.new", post.sample) %>%
    as_tibble() %>% 
    mutate(Cohort = current.inla.model$summary.random$Cx.new$ID,
           PrefID = rep(1:21, each = length(unique(current.inla.model$summary.random$Cx.new$ID)))) %>%
    pivot_longer(1:length(post.sample),
                 names_to = "n.sample",
                 values_to = "Cohort.effect") %>% 
    group_by(PrefID, n.sample) %>% 
    arrange(Cohort) %>% 
    slice(-c(1, 2), -c(n(),n()-1)) %>%
    ungroup() %>% 
    group_nest(PrefID) %>% 
    rename("Cohort.effect" = "data")
  
  intercept.sample = inla.posterior.sample.eval("PrefID", post.sample) %>% 
    as_tibble() %>% 
    mutate(PrefID = 1:21) %>% 
    pivot_longer(1:length(post.sample),
                 names_to = "n.sample",
                 values_to = "intercept") %>% 
    group_nest(PrefID) %>% 
    rename("intercept" = "data")
  
  current.APC = age.sample %>% 
    left_join(period.sample) %>% 
    left_join(cohort.sample) %>% 
    left_join(intercept.sample)
  
  cl = makeCluster(ncores, type = "SOCK")
  registerDoSNOW(cl)
  
  RR.results = foreach(i = 1:nrow(current.APC), .combine = "rbind", .packages = c("fANCOVA","tidyverse", "mgcv")) %dopar%{
    
    pref.age = current.APC$Age.effect[[i]]
    pref.period = current.APC$Period.effect[[i]]
    pref.cohort = current.APC$Cohort.effect[[i]]
    pref.intercept = current.APC$intercept[[i]]
    
    pref.id = current.APC$PrefID[[i]]
    
    current.pref.data = current.data %>%
      filter(PrefID == pref.id)
    
    output = list()
    for (j in 1:n.samples) {
      
      
      current.age = pref.age %>% 
        filter(n.sample == unique(pref.age$n.sample)[[j]])
      
      current.period = pref.period %>% 
        filter(n.sample == unique(pref.age$n.sample)[[j]])
      
      current.cohort = pref.cohort %>% 
        filter(n.sample == unique(pref.age$n.sample)[[j]])
      
      current.intercept = pref.intercept %>% 
        filter(n.sample == unique(pref.age$n.sample)[[j]])
      
      current.cohort.rm = current.cohort %>% 
        filter(!Cohort %in% c(seq(F1.start, F1.end, 3), seq(F2.start[pref.id], F2.end[pref.id], 3)))
      
      current.gam = gam(Cohort.effect ~ s(Cohort),
                        data = current.cohort.rm)
      
      current.cohort.gam.predict = predict(current.gam, newdata = current.cohort, se.fit = TRUE)
      
      current.cohort$cohort.predict = current.cohort.gam.predict$fit
      
      current.cohort$cohort.predict.sd = current.cohort.gam.predict$se.fit
      
      famine.cohort.F1 = data.frame(Age = rep(seq(46,61,3), each = 3), DiagnoseYear = 2005:2022, Cohort = (F1.start + F1.end)/2)
      
      famine.cohort.F1$Age = c(2006,2006,2006, 2009,2009,2009, 2012,2012,2012, 2015,2015,2015,2018,2018,2018, 2021, 2021, 2021) - (F1.start + F1.end)/2
      
      famine.cohort.F2 = data.frame(DiagnoseYear = 2005:2022, Cohort = F2[pref.id], period.collapse = c(2006,2006,2006, 2009,2009,2009, 2012,2012,2012, 2015,2015,2015,2018, 2018, 2018, 2021, 2021, 2021))
      
      famine.cohort.F2$Age = famine.cohort.F2$period.collapse - famine.cohort.F2$Cohort
      
      famine.F1 = famine.cohort.F1 %>% 
        as_tibble() %>% 
        left_join(current.age) %>% 
        left_join(current.period) %>% 
        left_join(current.cohort) %>% 
        left_join(current.intercept) %>% 
        left_join(current.pref.data[, c("Px", "Ax", "Population", "Case")], by = c("DiagnoseYear" = "Px", "Age" = "Ax")) 
      
      famine.F2 = famine.cohort.F2 %>% 
        as_tibble() %>% 
        left_join(current.age) %>% 
        left_join(current.period) %>% 
        left_join(current.cohort) %>%
        left_join(current.intercept) %>% 
        left_join(current.pref.data[, c("Px", "Ax", "Population", "Case")], by = c("DiagnoseYear" = "Px", "Age" = "Ax")) 
      
      GAM.F1.sample = matrix(NA, nrow(famine.F1), n.samples)
      GAM.F2.sample = matrix(NA, nrow(famine.F2),n.samples)
      
      current.RR = tibble(PrefID = pref.id,
                          n.sample = unique(pref.age$n.sample)[[j]],
                          F1.RR = rep(NA, n.samples),
                          F2.RR = rep(NA, n.samples))
      GAM.F1.sample[1,] = rnorm(n.samples, mean = famine.F1$cohort.predict, sd = famine.F1$cohort.predict.sd)
      
      GAM.F2.sample[1,] = rnorm(n.samples, mean = famine.F2$cohort.predict, sd = famine.F2$cohort.predict.sd)
      
      GAM.F1.sample = GAM.F1.sample %>% 
        as.data.frame() %>% 
        fill(everything()) %>% 
        as.matrix()
      
      GAM.F2.sample = GAM.F2.sample %>% 
        as.data.frame() %>% 
        fill(everything()) %>% 
        as.matrix()
      
      current.RR$F1.RR = apply(GAM.F1.sample, 2, function(x) sum(famine.F1$Case)/sum(exp(famine.F1$Age.effect + famine.F1$Period.effect + x + famine.F1$intercept)*famine.F1$Population))
      
      current.RR$F2.RR = apply(GAM.F2.sample, 2, function(x) sum(famine.F2$Case)/sum(exp(famine.F2$Age.effect + famine.F2$Period.effect + x + famine.F2$intercept)*famine.F2$Population))
      
      output[[j]] =  current.RR
      
    }
    
    bind_rows(output)
    
  }
  
  stopCluster(cl) 
  
  RR.results
}
