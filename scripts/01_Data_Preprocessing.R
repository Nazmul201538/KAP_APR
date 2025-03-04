#Load Packages
library(tidyverse)
library(readxl)
library(dplyr)

#Load Data
data <- read_excel("raw_data/KAP_RAW.xlsx")

sum(is.na(sections))
sections<- na.omit(sections)

sections <- data |> select(8:50)
colnames(sections) <- paste0("Q", 8:50)


#PRESCRIPTION PATTERN (PRACTICE)

pp <- sections |> select(Q8:Q10)
unique(pp$Q8:Q10)




#AWARENESS AND ATTITUDE ON THE CURRENT SCOPE OF ANTIBIOTIC RESISTANCE

aa<- sections |> select(Q11:Q17)

#CHOICE OF ANTIBIOTIC 

ca <- sections |> select(Q18:Q22)

#SOURCE OF INFORMATION ON ANTIBIOTICS PRESCRIBING AND RESISTANCE 

sia <- sections |> select(Q23:Q35)

#DECISION ABOUT ANTIBIOTIC PRESCRIBING

dap <- sections |> select(Q36:Q42)

#KNOWLEDGE ON USE OF ANTIBIOTICS

kua <- sections|> select(Q43:Q50)

#Calculate Frequency and Percentage
