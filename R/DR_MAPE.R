# ---
# title: "Calculate DR metrics and MAPE for stage-biased (bounds) and empirically informed structures"
# author: "Ella White and Julie Louvrier"
# date: "2024-12-17"
# ---

#### Packages and data ####

# remotes::install_github("JulieLouvrier/demres")

library(here)
library(tidyverse)
library(demres)

load(here("data/ngo_matrices.RData"))
load(here("data/ser_matrices.RData"))
load(here("data/ngorongoro_popstruc.rds"))
load(here("data/serengeti_popstruc.rds"))

#### Ngorongoro DR and MAPE ####

ngo_DR <- resilience(listA = ngo_matrix_list, 
                            metrics = "all", 
                            vector = crater_pop_structures,
                            bounds = TRUE, 
                            time = "both", 
                            popname = "Ngorongoro")

ngo_MAPE <- as.data.frame(demres_dist(table = ngo_DR, f = "long")) |> 
  select(MAPE)


#### Serengeti DR and MAPE ####

ser_DR <- resilience(listA = ser_matrix_list, 
                         metrics = "all", 
                         vector = serengeti_pop_structures,
                         bounds = TRUE, 
                         time = "both", 
                         popname = "Serengeti")

ser_MAPE <- as.data.frame(demres_dist(table = ser_DR, f = "long")) |> 
  select(MAPE)

#### Randomised initial population structures DR and MAPE ####

## function

set.seed(56)

ranvecs_demres <- function(order = 10, n = 1000, metrics, matrix_list, pop){
  
  vecs <- purrr::pmap(.l = list(metric = metrics),
                      .f = function(metric){
                        
                        vectors <- MCMCpack::rdirichlet(n = n, alpha = rep(1, order))
                        
                        test <- lapply(1:nrow(vectors), FUN = function(x){
                          resilience(listA = matrix_list, 
                                             metrics = metric,
                                             vector = vectors[x,],
                                             time = "both",
                                             bounds = TRUE,
                                             popname = paste0(pop, x))
                        })
                        testi <- bind_rows(test)
                        
                        xx <- pmap(.l = list(index = 1:length(test)),
                                   .f = function(index){
                                     
                                     mape <- demres_dist(table = test[[index]], 
                                                         f = "long")  
                                     mape <- as.data.frame(mape) |> 
                                       select(MAPE) |> 
                                       filter(!is.na(MAPE)) |> 
                                       mutate(metric = metric)
                                     
                                     
                                   })
                        
                        xxx <- bind_rows(xx)
                        
                        test_list <- list(xxx, testi)
                        names(test_list) <- c("MAPE", "values")
                        test_list
                      })
  
  names(vecs) <- metrics
  vecs
}

#### run functions for both populations ####

## currently an error with resilience() unable to run maxatt/maxamp, so only running inertia and reactivity below

crater_random_MAPE <- ranvecs_demres(order = 10, 
                                     n = 1000, 
                                     metrics = c("inertia", "reac"), 
                                     matrix_list = ngo_matrix_list, 
                                     pop = "Ngorongoro")


serengeti_random_MAPE <- ranvecs_demres(order = 10, 
                                        n = 1000, 
                                        metrics = c("inertia", "reac"), 
                                        matrix_list = matrix_list_serengeti, 
                                        pop = "Serengeti")