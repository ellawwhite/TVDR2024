# ---
# title: "Build matrix population models"
# author: "Ella White"
# date: "2024-12-17"
# ---

#### Packages and data ####

library(tidyverse)
library(here)

ngo_S <- read.csv(here("data/ngorongoro_S.csv"))
ser_S <- read.csv(here("data/serengeti_S.csv"))

ngo_transmat <- read.csv(here("data/ngorongoro_transmatrix.csv"),
                         row.names = 1)  
ser_transmat <- read.csv(here("data/serengeti_transmatrix.csv"),
                         row.names = 1)  

ngo_sexrat <- read.csv(here("data/sex_ratio_ngo.csv"))
ngo_litsize <- read.csv(here("data/litter_size_ngo.csv"))

ser_sexrat <- read.csv(here("data/sex_ratio_ser.csv"))
ser_litsize <- read.csv(here("data/litter_size_ser.csv"))


#### Organise and tidy data ####

ngo_fecundity <- ngo_litsize |> 
  select(-sample.size) |> 
  left_join(ngo_sexrat) |> 
  select(date, rank, litter.size, ratio) |> 
  mutate(ratio = 1 - ratio) |> 
  mutate(fecundity = litter.size * ratio,
         stratum = case_when(rank == "high" ~ "br.h",
                             rank == "low" ~ "br.l",
                             TRUE ~ NA))

ngo_S <- ngo_S |> 
  select(estimate, time, stratum) |> 
  mutate(stratum = case_when(stratum == "A" ~ "cub.l",
                             stratum == "B" ~ "sub.l",
                             stratum == "C" ~ "ya.l",
                             stratum == "D" ~ "br.l",
                             stratum == "E" ~ "nb.l",
                             stratum == "F" ~ "cub.h",
                             stratum == "G" ~ "sub.h",
                             stratum == "H" ~ "ya.h",
                             stratum == "I" ~ "br.h",
                             stratum == "J" ~ "nb.h",
                             TRUE ~ NA))

ngo_transmat <- ngo_transmat |> 
  `rownames<-`(c("cub.l", "sub.l", "ya.l", "br.l", "nb.l", "cub.h", "sub.h", "ya.h", "br.h", "nb.h")) |> 
  `colnames<-`(c("cub.l", "sub.l", "ya.l", "br.l", "nb.l", "cub.h", "sub.h", "ya.h", "br.h", "nb.h"))  |>
  mutate(x = c(1, 3, 5, 9, 7, 2, 4, 6, 10, 8)) |> 
  arrange(x) |> 
  select(cub.l, cub.h, sub.l, sub.h, ya.l, ya.h, nb.l, nb.h, br.l, br.h)


ser_fecundity <- ser_litsize |> 
  select(-sample.size) |> 
  left_join(ser_sexrat) |> 
  select(date, rank, litter_size, ratio) |> 
  mutate(ratio = 1 - ratio) |> 
  mutate(fecundity = litter_size * ratio,
         stratum = case_when(rank == "high" ~ "br.h",
                             rank == "low" ~ "br.l",
                             TRUE ~ NA))

ser_S <- ser_S |> 
  select(estimate, time, stratum) |> 
  mutate(stratum = case_when(stratum == "A" ~ "cub.l",
                             stratum == "B" ~ "sub.l",
                             stratum == "C" ~ "ya.l",
                             stratum == "D" ~ "br.l",
                             stratum == "E" ~ "nb.l",
                             stratum == "F" ~ "cub.h",
                             stratum == "G" ~ "sub.h",
                             stratum == "H" ~ "ya.h",
                             stratum == "I" ~ "br.h",
                             stratum == "J" ~ "nb.h",
                             TRUE ~ NA))

ser_transmat <- ser_transmat |> 
  `rownames<-`(c("cub.l", "sub.l", "ya.l", "br.l", "nb.l", "cub.h", "sub.h", "ya.h", "br.h", "nb.h")) |> 
  `colnames<-`(c("cub.l", "sub.l", "ya.l", "br.l", "nb.l", "cub.h", "sub.h", "ya.h", "br.h", "nb.h"))  |>
  mutate(x = c(1, 3, 5, 9, 7, 2, 4, 6, 10, 8)) |> 
  arrange(x) |> 
  select(cub.l, cub.h, sub.l, sub.h, ya.l, ya.h, nb.l, nb.h, br.l, br.h)


#### Matrix function ####


build_matrices <- function(from, to, S_est, trans_mat, fecundity){
  
  mat_list <- purrr::pmap(.l = list(year = from:to),
                          
                          .f = function(year){
                            
                            trans_mat <- t(trans_mat)
                            surv.mat <- trans_mat 
                            surv.mat[] <- NA
                            fecundity <- fecundity |> 
                              filter(date == year)
                            
                            S_est <- S_est |> 
                              filter(time == year) |> 
                              select(-time) |> 
                              pivot_wider(values_from = estimate, names_from = stratum)
                            
                            S.mat <- bind_rows(S_est, S_est, S_est, S_est, S_est, S_est, S_est, S_est, S_est, S_est) |> 
                              select(cub.l, cub.h, sub.l, sub.h, ya.l, ya.h, nb.l, nb.h, br.l, br.h)
                            
                            S.trans_mat <- S.mat * trans_mat
                            S.trans_mat[1, 9] <- fecundity$fecundity[fecundity$stratum == "br.l"]
                            S.trans_mat[2, 10] <- fecundity$fecundity[fecundity$stratum == "br.h"]
                            
                            full.mat <- as.matrix(S.trans_mat)
                            print(full.mat)
                            
                          })
}

#### Perform function and save results ####

ngo_matrix_list <- build_matrices(from = 1997, to = 2022, S_est = ngo_S, trans_mat = ngo_transmat, fecundity = ngo_fecundity) 

save(ngo_matrix_list, file = here("output/ngo_matrices.RData"))

ser_matrix_list <- build_matrices(from = 1990, to = 2018, S_est = ser_S, trans_mat = ser_transmat, fecundity = ser_fecundity) 

save(ser_matrix_list, file = here("output/ser_matrices.RData"))

