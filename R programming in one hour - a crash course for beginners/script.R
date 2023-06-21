# Acessando help-------------------
?mean
?sqrt
?ChickWeight

#Objetos e funções-----------------
#Operações aritméticas
2+5
5*7
#declaração de variáveis
a<-5
b<-7
a+b
sum(a,b)
# concatenação
ages <- c(19,31,20,25,23,21)
ages
mean(ages)
names<- c('André','Bruna', 'Caio', 'Daniela','Erick', 'Fabio')
names

#Criando dataframe-----------------
costumers <- data.frame(names, ages)
View(costumers) #exibe tabela do dataframe
str(costumers) #exibe informações do dataframe no console

#acessando colunas
costumers$names
costumers$ages

median(costumers$ages)

costumers[2,1] #diferente das outras linguagens, aqui se conta a partir do 1
costumers[1,]# pega primeira linha
costumers[,1]# pega primeira coluna
costumers[,]# retorna tudo

#Datasets para praticar-------------------------------
data()
View(Titanic)

# Instalando e usando bibliotecas -------------------------------
#install.packages("tidyverse") #precisei instalar pacotes para conseguir compilar as libs
library(tidyverse) 

View(starwars)

# Manipulando dados ----------------------------------------------
starwars %>% 
  filter(species == 'Human') %>% #filtra os humanos
  mutate(imc = mass/(height/100)^2) %>% # cria uma nova coluna com o imc dos personagens
  View()

starwars %>% 
  filter(height > 130 & mass< 200) %>% 
  mutate(height_in_meters = height/100) %>% 
  select(height_in_meters, mass) %>% #Seleciona duas colunas
  plot() #plota os dados em um gráfico de dispersão


# Estruturas de dados e tipos de variáveis--------------------------------------------
view(msleep)

glimpse(msleep)#retorna os tipos e os valores das colunas
head(msleep)# retorna as primeiras 6 linhas
class(msleep$name) #retorna o tipo de dado da coluna name
length(msleep)#Retorna a quantidade de colunas
length(msleep$name)#Retorna a quantidade linhas na coluna name
names(msleep) # retorna o nome das colunas
unique(msleep$vore)# vore é uma variável categórica. a função unique retorna os valores únicos

complete.cases(msleep) # Retorna true para quem não tiver dados ausentes
ausentes <- !complete.cases(msleep) #O ! vai inverter os booleanos
msleep[ausentes, ]# irá exibir todas as linhas que possuírem valores ausentes

?complete.cases

# Selecionando linhas e colunas ----------------------------------
starwars %>% 
  select(name, height, mass)# pega as respectivas colunas

starwars %>% 
  select(1,3)# seleciona a 1ª e a 3ª coluna

starwars %>% 
  select(1:3)# seleciona da 1ª até a 3ª coluna

starwars %>% 
  select(ends_with('color')) #auto explicativo

starwars %>% 
  select(name, species, everything()) %>% # name e species aparece primeiro e depois, as outras
  View()

# Mudando nome das colunas ---------------------------
starwars %>% 
  rename('character' = 'name') %>% 
  head()

# Mudanto tipo da variável -----------------------
class(starwars$hair_color)
starwars$hair_color<-as.factor(starwars$hair_color)
class(starwars$hair_color)

starwars %>% 
  mutate(hair_color = as.character(hair_color)) %>% 
  glimpse()

starwars$hair_color<-as.character(starwars$hair_color)
class(starwars$hair_color)

#Mudando nível de factors-----------------------------
df<- starwars
df$sex <- as.factor(df$sex) 
levels(df$sex)

df<- df %>% 
  mutate(sex = factor(
    sex,
    levels = c(
      'male', 'female', 'hermaphroditic', 'none'
    )
  ))

levels(df$sex)

# Renomeando valores categóricos ------------------------
starwars %>% 
  select(sex) %>% 
  mutate(sex = recode(sex,
                      'male' = 'masculino',
                      'female' = 'feminino'))

# Lidando com dados ausentes ----------------------------------
mean(starwars$height) #por ter valores NA, ele retorna NA
mean(starwars$height, na.rm = TRUE)

#Lidando com dados duplicados -----------------------------
ages <- c(19,31,20,25,23,21,19)
names<- c('André','Bruna', 'Caio', 'Daniela','Erick', 'Fabio', 'André')
costumers <- data.frame(names, ages)
costumers #retorna 7 valores

costumers %>%  # retorna 6, pq andré está duplicado
  distinct()
distinct(costumers)# Pode fazer assim também
costumers = distinct(costumers)
                     
costumers

#Condicional if_else ------------------------------------------
starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name, height_m) %>% 
  mutate(tellness =
           if_else(height_m > 1, #condicional
                  'short', #se verdadeiro
                  'tall')) #se falso

# Reshape data com Pivot wider-----------------------------------------
library(gapminder)
library(tidyverse)
View(gapminder)

gapminder_data <- gapminder %>% 
  select(
    country, year, lifeExp
  )
View(gapminder_data)
# Cria colunas para a expectativa de vida de cada ano
gapminder_data<-gapminder_data %>% 
  pivot_wider(
    names_from = year,
    values_from = lifeExp) 

View(gapminder_data)

# Reshape data com Pivot longer-----------------------------------------
gapminder_data<-gapminder_data %>% #Reverte
  pivot_longer(
    2:13,
    names_to = 'year',
    values_to = 'lifeExp') 

View(gapminder_data)

#Pegando indicadores --------------------------------------------------
View(msleep)
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)
mean(msleep$awake)
median(msleep$awake)
var(msleep$awake)
summary(msleep$awake)

# De mais de uma variável
msleep %>% 
  select(awake, sleep_total) %>% 
  summary()

#Criando tabelas a partir de colunas categóricas -----------------------
table(msleep$vore)

#Visualização de dados ---------------------------------------------
plot(pressure)

#graficos de barras
ggplot(
  data=starwars,
  mapping = aes(x=gender)
) + geom_bar()

#histograma
starwars %>% 
  drop_na(height) %>% 
  ggplot(
    mapping = aes(x = height)
  )+
  geom_histogram()

starwars %>% 
  drop_na(height) %>% 
  ggplot(
    aes(x = height)# ele atribui ao mapping por padrão
  )+
  geom_histogram()

#diagramas de caixa
starwars %>% 
  drop_na(height) %>% 
  ggplot(aes(x = height))+
  geom_boxplot(fill = 'red')+
  theme_bw()+
  labs(title = 'Diagrama de caixa - Altura',
       x='Altura dos personagens')

# density plot
starwars %>% 
  drop_na(height) %>% 
  filter(sex %in% c('male', 'female')) %>% 
  ggplot(aes(
    height, #pode omitir o x = height
    color = sex,
    fill = sex))+
  geom_density(alpha = 0.3)+
  theme_bw()+
  labs(title = 'Gráfico de densidade',
       x='Altura dos personagens')

#gráficos de dispersão
starwars %>% 
  filter(mass < 200) %>% 
  ggplot(aes(height, mass, color = sex))+
  geom_point(size = 2, alpha = 0.6)+
  theme_minimal()+
  labs(title = 'Gráfico de dispersão')

#Suavização
starwars %>% 
  filter(mass < 200) %>% 
  ggplot(aes(height, mass, color = sex))+
  geom_point(size = 2, alpha = 0.6)+
  geom_smooth()+
  facet_wrap(~sex)+
  theme_minimal()+
  labs(title = 'Gráfico de dispersão')

# Testagem de hipóteses -----------------------
View(gapminder)

gapminder %>% 
  filter(continent %in% c('Americas', 'Europe')) %>% 
  t.test(lifeExp ~ continent, data=.,
         altrnative = 'two.sided',
         paired = FALSE
         )
#Anova ----------------------------------------
gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c('Africa', 'Americas', 'Europe', 'Asia')) %>% 
  aov(lifeExp ~continent, data = .) %>% 
  summary()

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c('Africa', 'Americas', 'Europe', 'Asia')) %>% 
  aov(lifeExp ~continent, data = .) %>% 
  TukeyHSD()

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c('Africa', 'Americas', 'Europe', 'Asia')) %>% 
  aov(lifeExp ~continent, data = .) %>% 
  TukeyHSD() %>% 
  plot()

#Qui-quadrado --------------------------------------------------
head(iris)
flowers <- iris %>% 
  mutate(Size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c('Pequena', 'Média', 'Grande'))) %>% 
  select(Species, Size)

View(flowers)

flowers %>% 
  select(Size) %>% 
  table() %>% 
  chisq.test()

flowers %>% 
  table() %>% 
  chisq.test()

#Modelo linear --------------------------------------
cars %>% 
  lm(dist ~speed, data=.) %>% 
  summary()

cars %>% 
  lm(dist ~speed, data=.) %>% 
  plot()
