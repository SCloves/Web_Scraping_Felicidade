library(xml2)
library(rvest)
library(magrittr)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(NbClust)
library(cluster)
library(useful)
#library(rgl)
library(plot3D)
library(devtools)

# install_github("kassambara/factoextra")

# Pegando a primeira tabela do site
url1 <- "https://en.wikipedia.org/wiki/World_Happiness_Report"
felicidade <- url1 %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table()

# vamos ver como é a estrutura desse dataset que foi importado
str(felicidade)

# vamos transformar isso em um dataframe
df_felicidade <- data.frame(felicidade)

# vamos excluir colunas com ranks e scores e ficar com as outras colunas
df_felicidade <- df_felicidade[c(3, 6:11)] # pega apenas a coluna 3 e as 6-11

# renomeando alguns países da coluna Country
# para ficar no mesmo padrão que o banco de dados de mapa
df_felicidade$Country <- as.character(mapvalues(df_felicidade$Country, 
from = c("United States", "Congo (Kinshasa)", "Congo (Brazzaville)", 
         "Trinidad and Tobago"), 
to =  c("USA","Democratic Republic of the Congo", 
        "Democratic Republic of the Congo", "Trinidad")))

# Pegando a segunda tabela do site 
url2 <- 'https://en.wikipedia.org/wiki/List_of_countries_by_Social_Progress_Index'
social <- url2 %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>% 
  html_table(fill=T)

# vamos ver como é a estrutura desse dataset que foi importado
str(social)

df_social = data.frame(social)

# Novamente, vamos excluir os ranks e deixas as outras colunas
df_social <- df_social[c(1,5,7,9)]

# renomeando alguns países da coluna Country
# para ficar no mesmo padrão que o banco de dados de mapa
df_social$Country <- as.character(mapvalues(df_social$Country, 
                    from = c("United States", "Côte d'Ivoire",
                             "Democratic Republic of Congo", "Congo", 
                             "Trinidad and Tobago"),
                       to=c("USA", "Ivory Cost","Democratic Republic of the Congo", 
                            "Democratic Republic of the Congo", "Trinidad")))

# vamos transformar os dados das colunas 2 à 4 em numéricos
df_social[, 2:4] <- sapply(df_social[, 2:4], as.numeric)

# vamos juntar as duas tabelas
# vamos fazer um left-join para
# as colunas Country das duas tabelas
# isso irá pegar todos os dados onde os 
# nomes dos países de df_social são iguais ao
# nomes dos países de df_felicidade
soc.happy <- left_join(df_felicidade, 
                               df_social, by = c('Country' = 'Country'))

#Verificar se há valores ausentes no conjunto de dados combinados
mean(is.na(soc.happy[, 2:10]))

# A junção à esquerda dos dois arquivos de dados 
# introduziu valores ausentes (aproximadamente 3,5% do conjunto total de dados) 
# no conjunto de dados combinados. R oferece uma variedade de algoritmos 
# de imputação para preencher valores ausentes. 
# Aqui usaremos a estratégia de imputação mediana
# aplicada pela substituição de valores em falta 
# com a média para cada coluna.
for(i in 1:ncol(soc.happy[, 2:10])) {
  soc.happy[, 2:10][is.na(soc.happy[, 2:10][,i]), i] <- median(soc.happy[, 2:10][,i],
                                                               na.rm = TRUE)
}

# um resumão dos dados 
summary(soc.happy)

# Embora o passo de transformação de dados anterior 
# pudesse ter sido adequado para os próximos passos desta análise, 
# a função mostrada abaixo reavaliaria todas as variáveis para uma 
# média de 0 e desvio padrão de 1.

sd_scale <- function(x) {
                        (x - mean(x))/sd(x)
                        }
soc.happy[,2:10] <- as.data.frame(apply(soc.happy[,2:10], 2, sd_scale))

# resumindo sd_scale
summary(soc.happy[,2:10])

# Análise de correlação símples

# Agora os dados estão prontos para podermos correlaciona-los
# Vamos usar o método de pearson para isso
corr <- cor(soc.happy[, 2:10], method="pearson")

# Vamos plotar isso
ggplot(melt(corr, varnames=c("x", "y"), value.name="correlation"), 
       aes(x=x, y=y)) +
  geom_tile(aes(fill=correlation)) +
  scale_fill_gradient2(low="green", mid="yellow", high="red",
                       guide=guide_colorbar(ticks=FALSE, barheight = 5),
                       limits=c(-1,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Heatmap of Correlation Matrix", 
       x=NULL, y=NULL)

# A Análise de Componentes Principais(ACP) é uma estatística 
# multivariada amplamente utilizada para examinar as relações 
# entre várias variáveis quantitativas. ACP identifica padrões 
# nas variáveis para reduzir as dimensões do conjunto de dados 
# em regressão múltipla e agrupamento, entre outros.
soc.pca <-  princomp(soc.happy[, 2:10], graph=FALSE)

# vamos plotar isso  
screeplot(soc.pca, main = 'Análise de Componentes Principais',type="lines")

# A sintaxe para algoritmos de agrupamento requer a especificação do 
# número de clusters desejados (k =) como uma entrada. A questão prática 
# é qual o valor que k deve tomar? Na ausência de um conhecimento do assunto, 
# R oferece várias abordagens empíricas para selecionar um valor de k. 
# Uma dessas ferramentas R para o melhor número sugerido de clusters 
# é o pacote NbClust.
nbc <- NbClust(soc.happy[, 2:10], distance="manhattan", 
               min.nc=2, max.nc=30, method="ward.D", index='all')

# O algoritmo NbClust sugeriu uma solução de 3 clusters para o conjunto 
# de dados combinados. Então, vamos aplicar K = 3 nos próximos passos.
set.seed(4653)
pamK3 <- pam(soc.happy[, 2:10], diss=FALSE, 3, keep.data=TRUE)

# Agora podemos exibir países individuais e sobrepor suas atribuições 
# de cluster no plano definido pelos dois primeiros componentes principais.

# uma coluna que diz a qual closter pertence o país
soc.happy['cluster'] <- as.factor(pamK3$clustering)




