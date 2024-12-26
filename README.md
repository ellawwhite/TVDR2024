# TVDR2024
Data and code for White et al. (in submission) - Resilience of a long-lived mammal: time and demographic structure matter

Files in R folder

•	MSMR_analyses_Ngo: runs multistate mark-recapture models using package Rmark and provided encounter history data for Ngorongoro population (EHT_Ngo.csv)

•	MSMR_analyses_Ser: runs multistate mark-recapture models using package Rmark and provided encounter history data for Serengeti population (EHT_Ser.csv)

•	Build_MPMs.R: builds matrix population models from multistate mark-recapture results and estimated fecundity (using data xxx)

•	DR_MAPE.R: calculates time-varying and time-constant resilience metrics and resulting mean absolute percentage error


Files in data folder

•	EHT_Ngo.csv/EHT_Ser.csv: encounter history information for Ngorongoro/Serengeti populations

  o	Contains one column: ch (capture history). Letters refer to different combinations of social and demographic states, while 0 is “not seen”

•	litter_size_ngo.csv/litter_size_ser.csv: annual average litter size by rank class extracted from raw demographic data for both populations

  o	contains six columns: date = year, rank = high/low, sd = standard deviation, se = standard error, litter.size = average number of cubs produced per litter for high/low ranking females, sample.size = number of litters used to calculate litter size in each rank class and year
  
•	ngo_matrices.Rdata/ser_matrices.Rdata: R data containing the list of matrices produced in the Build_MPMs.R script for each population. 

•	ngorongoro_popstruc.rds/serengeti_popstruc.rds: R data containing list of initial vectors (population structures) extracted from raw data.

•	ngorongoro_S.csv/serengeti_S.csv: S (survival) estimates from MSMR models run in MSMR_analyses_Ser/Ngo.R. 

  o	important columns are estimate = estimate of survival for a given year and state, se = standard error around estimate of survival, time = year, stratum = letters representing different combinations of social and demographic states
  
•	ngorongoro_transmatrix.csv/serengeti_transmatrix.csv: 10 by 10 matrix for each population containing the transition rates between states. Output of MSMR models run in MSMR_analyses_Ser/Ngo.R.

•	sex_ratio_ngo.csv/sex_ratio_ser.csv: annual sex ratio of cubs extracted from raw demographic data for both populations.

  o	Contains five columns: date = year, female = number of female cubs, male = number of male cubs, ratio = sex ratio calculated from nmale/nmale+nfemale, sample size = total cubs of both sexes in this year
