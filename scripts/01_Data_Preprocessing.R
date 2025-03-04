#Load Packages
library(tidyverse)
library(readxl)
library(dplyr)

#Load Data
data <- read_excel("raw_data/KAP_RAW.xlsx")

#Removing Missing Values 

sum(is.na(sections))
sections<- na.omit(sections)

sections <- data |> select(8:50)

#changing Column Names
colnames(sections) <- paste0("Q", 8:50)


#PRESCRIPTION PATTERN (PRACTICE)

pp <- sections |> select(Q8:Q10) |> mutate(across(Q8:Q10, ~case_when(
  . == "None" ~ 8.0,
  . == "1-4 patients" ~ 8.2,
  . == "5 patients" ~ 8.5,
  . == "6-9 patients" ~ 8.7,
  . == "9" ~ 8.9,
  . == "All 10 patients" ~ 8.10,
  . == "Patients in outpatient and hospitalized patients" ~ 9.1,
  . == "Patients at outpatient" ~ 9.2,
  . == "Hospitalized patients" ~ 9.3,
  . == "9" ~ 9,
  . == "No" ~ 10.0,
  . == "Yes" ~ 10.1,
  TRUE ~ NA_real_
))) |>
  
  mutate(pp_mean= mean(c_across(Q8:Q10), na.rm = TRUE))

#Frequency and Percentage

library(dplyr)
library(tidyr)

pp_freq_long <- sections |> 
  select(Q8:Q10) |> 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |> 
  count(Question, Response, name = "Frequency")

print(pp_freq_long)

pp_freq_percent <- pp_freq_long |> 
  group_by(Question) |> 
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) |> 
  ungroup()

print(pp_freq_percent)





#AWARENESS AND ATTITUDE ON THE CURRENT SCOPE OF ANTIBIOTIC RESISTANCE

aa<- sections |> select(Q11:Q17) |> mutate(across(Q11:Q17, ~case_when(
  . == "None" ~ 17.0,
  . == "Very little" ~ 17.1,
  . == "Average" ~ 17.2,
  . == "Good" ~ 17.3,
  . == "Excellent" ~ 17.4,
  . == "Neutral" ~ 11.0,
  . == "I strongly disagree" ~ -11.2,
  . == "I disagree" ~ -11.1,
  . == "I agree" ~ 11.1,
  . == "I strongly agree" ~ 11.2,
  TRUE ~ NA_real_
))) |>
  
  mutate(aa_mean= mean(c_across(Q11:Q17), na.rm = TRUE))

#Frequency and Percentage


library(dplyr)
library(tidyr)

aa_freq_long <- sections |> 
  select(Q11:Q17) |> 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |> 
  count(Question, Response, name = "Frequency")

print(aa_freq_long)

aa_freq_percent <- kua_freq_long |> 
  group_by(Question) |> 
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) |> 
  ungroup()

print(aa_freq_percent)


#CHOICE OF ANTIBIOTIC 

ca <- sections |> select(Q18:Q22) |> mutate(across(Q18:Q22, ~case_when(
  . == "A bit confident" ~ 18.1,
  . == "Confident" ~ 18.2,
  . == "Very confident" ~ 18.3,
  . == "Neutral/I have no idea" ~ 18.0,
  . == "Never" ~ 20.0,
  . == "Sometimes" ~ 20.1,
  . == "Mostly" ~ 20.3,
  . == "Half of the times" ~ 20.2,
  TRUE ~ NA_real_
))) |>
  
  mutate(ca_mean= mean(c_across(Q18:Q22), na.rm = TRUE))

#Frequency and Percentage


library(dplyr)
library(tidyr)

ca_freq_long <- sections |> 
  select(Q18:Q22) |> 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |> 
  count(Question, Response, name = "Frequency")

print(ca_freq_long)

ca_freq_percent <- ca_freq_long |> 
  group_by(Question) |> 
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) |> 
  ungroup()

print(ca_freq_percent)


unique(ca$Q18)
unique(ca$Q19)
unique(ca$Q20)
unique(ca$Q21)
unique(ca$Q22)

#SOURCE OF INFORMATION ON ANTIBIOTICS PRESCRIBING AND RESISTANCE 

sia <- sections |> select(Q23:Q35) |> mutate(across(Q23:Q35, ~case_when(
  . == "No" ~ 23.0,
  . == "Yes" ~ 23.1,
  . == "Very useful" ~ 35.3,
  . == "Useful" ~ 35.2,
  . == "Not at all useful" ~ 35.1,
  . == "I dont know" ~ 35.0,
  TRUE ~ NA_real_
))) |>
  
  mutate(sia_mean= mean(c_across(Q23:Q35), na.rm = TRUE))

#Frequency and Percentage


library(dplyr)
library(tidyr)

sia_freq_long <- sections |> 
  select(Q23:Q35) |> 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |> 
  count(Question, Response, name = "Frequency")

print(sia_freq_long)

sia_freq_percent <- sia_freq_long |> 
  group_by(Question) |> 
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) |> 
  ungroup()

print(sia_freq_percent)


#DECISION ABOUT ANTIBIOTIC PRESCRIBING

dap <- sections |> select(Q36:Q42)  |> 
  mutate(across(Q36:Q42, ~case_when(
    . == "I strongly agree"   ~ 36.2,
    . == "I agree"           ~ 36.1,
    . == "Neutral"           ~ 36.0,
    . == "Useful"            ~ 35.2,
    . == "disagree"          ~ -36.1,
    . == "I strongly disagree" ~ -36.2,
    TRUE ~ NA_real_
  ))) |>
  
  mutate(dap_mean= mean(c_across(Q36:Q42), na.rm = TRUE))

#Frequency and Percentage


library(dplyr)
library(tidyr)

dap_freq_long <- sections |> 
  select(Q36:Q42) |> 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |> 
  count(Question, Response, name = "Frequency")

print(dap_freq_long)

dap_freq_percent <- dap_freq_long |> 
  group_by(Question) |> 
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) |> 
  ungroup()

print(dap_freq_percent)



#KNOWLEDGE ON USE OF ANTIBIOTICS

kua <- sections|> select(Q43:Q50) |> mutate(across(Q43:Q50, ~case_when(
  . == "Amoxicillin orally" ~ 43.1,
  . == "Trimethoprim/sulphamethoxazole orally" ~ 43.2,
  . == "Amoxicillin/clavulanic acid orally" ~ 43.3,
  . == "Oral rehydration salts with no antibiotic" ~ 43.4,
  . == "Trimethoprim/sulphamethoxazole orally" ~ 44.1,
  . == "Amoxicillin orally" ~ 44.2,
  . == "Amoxicillin/clavulanic acid orally" ~ 44.3,
  . == "No antibiotic" ~ 44.0,
  . == "Patient A" ~ 45.1,
  . == "Patient B" ~ 45.2,
  . == "Patient A & B" ~ 45.3,
  . == "Neither patient A nor patient B" ~ 45.0,
  . == "Amoxicillin" ~ 46.1,
  . == "Ciprofloxacin" ~ 46.2,
  . == "Gentamicin" ~ 46.3,
  . == "Ciprofloxacin" ~ 47.1,
  . == "Metronidazole" ~ 47.2,
  . == "Trimethoprim/sulphamethoxazole" ~ 47.3,
  . == "Amoxicillin clavulanic acid" ~ 48.1,
  . == "Cefotaxime" ~ 48.2,
  . == "Ceftriaxone" ~ 48.3,
  . == "None of these antibiotics" ~ 48.0,
  . == "Clindamycin" ~ 49.1,
  . == "Vancomycin" ~ 49.3,
  . == "Orally, three times daily" ~ 50.1,
  . == "Parenterally, once daily" ~ 50.2,
  . == "Parenterally, three times daily" ~ 50.3,
  TRUE ~ NA_real_
))) |>
  
  mutate(kua_mean= mean(c_across(Q43:Q50), na.rm = TRUE))

#Frequency and Percentage


library(dplyr)
library(tidyr)

kua_freq_long <- sections |> 
  select(Q43:Q50) |> 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |> 
  count(Question, Response, name = "Frequency")

print(kua_freq_long)

kua_freq_percent <- kua_freq_long |> 
  group_by(Question) |> 
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) |> 
  ungroup()

print(kua_freq_percent)
