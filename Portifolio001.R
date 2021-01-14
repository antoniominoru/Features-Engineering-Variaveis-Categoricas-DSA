# Features Engineering | Variáveis Categóricas

# Para esse estudo de caso, digamos que realmente queremos entender a profissão (job) de acordo 
# com o uso da tecnologia em uma determinada função. Nesse caso, começaríamos a classificar 
# cada uma das profissões em nível médio, alto e baixo em termos de uso de tecnologia.

# Dataset: http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip

# Carregando os dados
dataset_bank <- read.table("bank/bank-full.csv", header = TRUE, sep = ";")
View(dataset_bank)
table(dataset_bank$job)

# Carregando as bibliotecas
library(dplyr)
library(ggplot2)

dataset_bank %>%
  group_by(job)%>%
  summarise(n = n())%>%
  ggplot(aes(x = job, y = n))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# acrescentando Coluna alvo

dataset_bank <- dataset_bank %>%
  mutate(technology_use = 
           case_when(job == 'admin' ~ "medio",
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

View(dataset_bank)

# Agora vamos revisar rapidamente esse novo campo.
table(dataset_bank$technology_use)

# Vamos colocar isso em percentual
round(prop.table(table(dataset_bank$technology_use)),2)


