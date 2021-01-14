# Features Engineering | Variáveis Categóricas | DSA

# Converter profissão (job) pelo uso da tecnologia em nível médio, alto e baixo 

# Dataset: http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip

# Bibliotecas
library(dplyr)
library(ggplot2)


# Carregando os dados
dataset_bank <- read.table("bank/bank-full.csv", header = TRUE, sep = ";")
View(dataset_bank)
table(dataset_bank$job)


# grafico
dataset_bank %>%
  group_by(job)%>%
  summarise(n = n())%>%
  ggplot(aes(x = job, y = n))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Acrescentando Coluna

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

#conferencia
View(dataset_bank)
table(dataset_bank$technology_use)
round(prop.table(table(dataset_bank$technology_use)),2)

dataset_bank %>%
  group_by(technology_use)%>%
  summarise(n = n())%>%
  ggplot(aes(x = technology_use, y = n))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
