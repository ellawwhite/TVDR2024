# ---
# title: "Mark-recapture analyses Ngorongoro"
# author: "Ella White"
# date: "2024-12-17"
# ---
  
#### Packages and data ####

library(RMark)
library(tidyverse)
library(here)

eht_ngo <- read.csv(here("data/EHT_Ngo.csv"))

#### Processing capture history data ####

## processing data
ngo_processed <- process.data(eht_ngo, 
                                   model = "Multistrata", 
                                   begin.time = 1997)

## design data list
ngo_ddl <- make.design.data(ngo_processed, 
                                 parameters = list(S = list(pim.type = "time"),
                                                   p = list(pim.type = "constant"), 
                                                   Psi = list(pim.type = "constant")))

#### Fixing impossible transitions to 0 ####

ngo_ddl$Psi$fix <- NA

## low rank

## CUB -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "A" &  ngo_ddl$Psi$tostratum == "C"] <- 0

## CUB -> BREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "A" &  ngo_ddl$Psi$tostratum == "D"] <- 0

## CUB -> NONBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "A" &  ngo_ddl$Psi$tostratum == "E"] <- 0

## SUBADULT -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "B" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "C" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> SUBADULT
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "C" &  ngo_ddl$Psi$tostratum == "B"] <- 0

## BREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "D" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## BREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "D" &  ngo_ddl$Psi$tostratum == "B"] <- 0

## BREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "D" &  ngo_ddl$Psi$tostratum == "C"] <- 0

## NONBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "E" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## NONBREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "E" &  ngo_ddl$Psi$tostratum == "B"] <- 0

## NONBREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "E" &  ngo_ddl$Psi$tostratum == "C"] <- 0

##high rank

## CUB -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "F" &  ngo_ddl$Psi$tostratum == "H"] <- 0

## CUB -> BREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "F" &  ngo_ddl$Psi$tostratum == "I"] <- 0

## CUB -> NONBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "F" &  ngo_ddl$Psi$tostratum == "J"] <- 0

## SUBADULT -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "G" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "H" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> SUBADULT
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "H" &  ngo_ddl$Psi$tostratum == "G"] <- 0

## BREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "I" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## BREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "I" &  ngo_ddl$Psi$tostratum == "G"] <- 0

## BREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "I" &  ngo_ddl$Psi$tostratum == "H"] <- 0

## NONBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "J" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## NONBREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "J" &  ngo_ddl$Psi$tostratum == "G"] <- 0

## NONBREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "J" &  ngo_ddl$Psi$tostratum == "H"] <- 0

## high to low

## CUB -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "F" &  ngo_ddl$Psi$tostratum == "C"] <- 0

## CUB -> BREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "F" &  ngo_ddl$Psi$tostratum == "D"] <- 0

## CUB -> NONBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "F" &  ngo_ddl$Psi$tostratum == "E"] <- 0

## SUBADULT -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "G" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "H" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> SUBADULT
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "H" &  ngo_ddl$Psi$tostratum == "B"] <- 0


## BREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "I" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## BREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "I" &  ngo_ddl$Psi$tostratum == "B"] <- 0

## BREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "I" &  ngo_ddl$Psi$tostratum == "C"] <- 0

## NONBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "J" &  ngo_ddl$Psi$tostratum == "A"] <- 0

## NONBREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "J" &  ngo_ddl$Psi$tostratum == "B"] <- 0

## NONBREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "J" &  ngo_ddl$Psi$tostratum == "C"] <- 0

## low to high

## CUB -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "A" &  ngo_ddl$Psi$tostratum == "H"] <- 0

## CUB -> BREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "A" &  ngo_ddl$Psi$tostratum == "I"] <- 0

## CUB -> NONBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "A" &  ngo_ddl$Psi$tostratum == "J"] <- 0

## SUBADULT -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "B" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "C" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> SUBADULT
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "C" &  ngo_ddl$Psi$tostratum == "G"] <- 0

## BREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "D" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## BREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "D" &  ngo_ddl$Psi$tostratum == "G"] <- 0

## BREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "D" &  ngo_ddl$Psi$tostratum == "H"] <- 0

## NONBREED -> CUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "E" &  ngo_ddl$Psi$tostratum == "F"] <- 0

## NONBREED -> SUB
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "E" &  ngo_ddl$Psi$tostratum == "G"] <- 0

## NONBREED -> PREBREED
ngo_ddl$Psi$fix[ngo_ddl$Psi$stratum == "E" &  ngo_ddl$Psi$tostratum == "H"] <- 0


#### Run model ####

S.time.sin <- list(formula = ~-1+time:stratum, link = "sin") 

p.strata.sin <- list(formula = ~-1+stratum, link = "sin") 

psi.mlogit <- list(formula = ~-1 + stratum:tostratum, link = "mlogit") 

model_ngo <- make.mark.model(data = ngo_processed, 
                                  ddl = ngo_ddl, 
                                  parameters = list(S = S.time.sin, 
                                                    p = p.strata.sin,
                                                    Psi = psi.mlogit),
                                  model.name = "ngorongoro",
                                  useddl = TRUE)
model_ngo_output <- run.mark.model(model = model_ngo, 
                                     filename = "markdump/ngorongoro",
                                     prefix = "ngorongoro",
                                     realvcv = TRUE)

#### Extract and save reals####

psilist <- get.real(model_ngo_output, "Psi", vcv = TRUE, expand = FALSE)
psivalues <- psilist$estimates 
transition.matrix <- TransitionMatrix(psivalues, vcv.real = psilist$vcv.real)

transition.matrix.est <- transition.matrix$TransitionMat

slist <- get.real(model_ngo_output, "S", vcv = TRUE, expand = FALSE)
svalues <- slist$estimates

plist <- get.real(model_ngo_output, "p", vcv = TRUE, expand = FALSE)
pvalues <- plist$estimates 


write.csv(svalues, here("output/ngorongoro_S.csv"), row.names = FALSE)
write.csv(pvalues, here("output/ngorongoro_p.csv"), row.names = FALSE)
write.csv(transition.matrix.est, here("output/ngorongoro_transmatrix.csv"))

save(model.full.crt, ngo_ddl, ngo_processed, file = here("output/ngorongoro_msmr"))

export.MARK(x = ngo_processed, project.name = 'ngorongoro_msmr', 
            model = model_ngo_output, replace = TRUE)