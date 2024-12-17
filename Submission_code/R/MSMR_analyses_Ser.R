# ---
# title: "Mark-recapture analyses Serengeti"
# author: "Ella White"
# date: "2024-12-17"
# ---

#### Packages and data ####

library(RMark)
library(tidyverse)
library(here)

eht_ser <- read.csv(here("data/EHT_Ser.csv"))

#### Processing capture history data ####

## processing data
ser_processed <- process.data(eht_ser, 
                              model = "Multistrata", 
                              begin.time = 1997)

## design data list
ser_ddl <- make.design.data(ser_processed, 
                            parameters = list(S = list(pim.type = "time"),
                                              p = list(pim.type = "constant"), 
                                              Psi = list(pim.type = "constant")))

#### Fixing impossible transitions to 0 ####

ser_ddl$Psi$fix <- NA

## low rank

## CUB -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "A" &  ser_ddl$Psi$tostratum == "C"] <- 0

## CUB -> BREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "A" &  ser_ddl$Psi$tostratum == "D"] <- 0

## CUB -> NONBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "A" &  ser_ddl$Psi$tostratum == "E"] <- 0

## SUBADULT -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "B" &  ser_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "C" &  ser_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> SUBADULT
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "C" &  ser_ddl$Psi$tostratum == "B"] <- 0

## BREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "D" &  ser_ddl$Psi$tostratum == "A"] <- 0

## BREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "D" &  ser_ddl$Psi$tostratum == "B"] <- 0

## BREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "D" &  ser_ddl$Psi$tostratum == "C"] <- 0

## NONBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "E" &  ser_ddl$Psi$tostratum == "A"] <- 0

## NONBREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "E" &  ser_ddl$Psi$tostratum == "B"] <- 0

## NONBREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "E" &  ser_ddl$Psi$tostratum == "C"] <- 0

##high rank

## CUB -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "F" &  ser_ddl$Psi$tostratum == "H"] <- 0

## CUB -> BREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "F" &  ser_ddl$Psi$tostratum == "I"] <- 0

## CUB -> NONBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "F" &  ser_ddl$Psi$tostratum == "J"] <- 0

## SUBADULT -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "G" &  ser_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "H" &  ser_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> SUBADULT
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "H" &  ser_ddl$Psi$tostratum == "G"] <- 0

## BREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "I" &  ser_ddl$Psi$tostratum == "F"] <- 0

## BREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "I" &  ser_ddl$Psi$tostratum == "G"] <- 0

## BREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "I" &  ser_ddl$Psi$tostratum == "H"] <- 0

## NONBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "J" &  ser_ddl$Psi$tostratum == "F"] <- 0

## NONBREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "J" &  ser_ddl$Psi$tostratum == "G"] <- 0

## NONBREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "J" &  ser_ddl$Psi$tostratum == "H"] <- 0

## high to low

## CUB -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "F" &  ser_ddl$Psi$tostratum == "C"] <- 0

## CUB -> BREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "F" &  ser_ddl$Psi$tostratum == "D"] <- 0

## CUB -> NONBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "F" &  ser_ddl$Psi$tostratum == "E"] <- 0

## SUBADULT -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "G" &  ser_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "H" &  ser_ddl$Psi$tostratum == "A"] <- 0

## PREBREED -> SUBADULT
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "H" &  ser_ddl$Psi$tostratum == "B"] <- 0


## BREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "I" &  ser_ddl$Psi$tostratum == "A"] <- 0

## BREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "I" &  ser_ddl$Psi$tostratum == "B"] <- 0

## BREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "I" &  ser_ddl$Psi$tostratum == "C"] <- 0

## NONBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "J" &  ser_ddl$Psi$tostratum == "A"] <- 0

## NONBREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "J" &  ser_ddl$Psi$tostratum == "B"] <- 0

## NONBREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "J" &  ser_ddl$Psi$tostratum == "C"] <- 0

## low to high

## CUB -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "A" &  ser_ddl$Psi$tostratum == "H"] <- 0

## CUB -> BREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "A" &  ser_ddl$Psi$tostratum == "I"] <- 0

## CUB -> NONBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "A" &  ser_ddl$Psi$tostratum == "J"] <- 0

## SUBADULT -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "B" &  ser_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "C" &  ser_ddl$Psi$tostratum == "F"] <- 0

## PREBREED -> SUBADULT
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "C" &  ser_ddl$Psi$tostratum == "G"] <- 0

## BREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "D" &  ser_ddl$Psi$tostratum == "F"] <- 0

## BREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "D" &  ser_ddl$Psi$tostratum == "G"] <- 0

## BREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "D" &  ser_ddl$Psi$tostratum == "H"] <- 0

## NONBREED -> CUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "E" &  ser_ddl$Psi$tostratum == "F"] <- 0

## NONBREED -> SUB
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "E" &  ser_ddl$Psi$tostratum == "G"] <- 0

## NONBREED -> PREBREED
ser_ddl$Psi$fix[ser_ddl$Psi$stratum == "E" &  ser_ddl$Psi$tostratum == "H"] <- 0


#### Run model ####

S.time.sin <- list(formula = ~-1+time:stratum, link = "sin") 

p.strata.sin <- list(formula = ~-1+stratum, link = "sin") 

psi.mlogit <- list(formula = ~-1 + stratum:tostratum, link = "mlogit") 

model_ser <- make.mark.model(data = ser_processed, 
                             ddl = ser_ddl, 
                             parameters = list(S = S.time.sin, 
                                               p = p.strata.sin,
                                               Psi = psi.mlogit),
                             model.name = "serengeti",
                             useddl = TRUE)
model_ser_output <- run.mark.model(model = model_ser, 
                                   filename = "markdump/serengeti",
                                   prefix = "serengeti",
                                   realvcv = TRUE)

#### Extract and save reals####

psilist <- get.real(model_ser_output, "Psi", vcv = TRUE, expand = FALSE)
psivalues <- psilist$estimates 
transition.matrix <- TransitionMatrix(psivalues, vcv.real = psilist$vcv.real)

transition.matrix.est <- transition.matrix$TransitionMat

slist <- get.real(model_ser_output, "S", vcv = TRUE, expand = FALSE)
svalues <- slist$estimates

plist <- get.real(model_ser_output, "p", vcv = TRUE, expand = FALSE)
pvalues <- plist$estimates 


write.csv(svalues, here("output/serengeti_S.csv"), row.names = FALSE)
write.csv(pvalues, here("output/serengeti_p.csv"), row.names = FALSE)
write.csv(transition.matrix.est, here("output/serengeti_transmatrix.csv"))

save(model.full.crt, ser_ddl, ser_processed, file = here("output/serengeti_msmr"))

export.MARK(x = ser_processed, project.name = 'serengeti_msmr', 
            model = model_ser_output, replace = TRUE)
