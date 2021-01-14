# Features Engineering | Variáveis Categóricas | DSA

# Profession converter (job) for the use of technology at "médio", "alto" and "baixo". 

# Dataset: http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip

# Libraries
library(dplyr)
library(ggplot2)


# Loading data
dataset_bank <- read.table("bank-full.csv", header = TRUE, sep = ";")

# Data Conference
View(dataset_bank)
table(dataset_bank$job)

dataset_bank %>%
  group_by(job)%>%
  summarise(n = n())%>%
  ggplot(aes(x = job, y = n))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Adding Column
dataset_bank <- dataset_bank %>%
  mutate(technology_use = 
           case_when(job == 'admin.' ~ "medio",
                     job == 'blue-collar' ~ "baixo",
                     job == 'entrepreneur' ~ "alto",
                     job == 'housemaid' ~ "baixo",
                     job == 'management' ~ "medio",
                     job == 'retired' ~ "baixo",
                     job == 'self-employed' ~ "baixo",
                     job == 'services' ~ "medio",
                     job == 'student' ~ "alto",
                     job == 'technician' ~ "alto",
                     job == 'unemployed' ~ "baixo",
                     job == 'unknown' ~ "baixo"))

# Data Review
View(dataset_bank)
table(dataset_bank$technology_use)
round(prop.table(table(dataset_bank$technology_use)),2)

dataset_bank %>%
  group_by(technology_use)%>%
  summarise(n = n())%>%
  ggplot(aes(x = technology_use, y = n))+
  geom_bar(stat = "identity")
