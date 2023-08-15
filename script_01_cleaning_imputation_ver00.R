########################################################################
## Description: Análise de Dados: Código desenvolvido para aula sobre
## limpeza, exploração e imputação de dados perdidos...
##
## Maintainer: UNIVALI / EMCT - NID Análise de Dados
## Author: Rodrigo Sant'Ana
## Created: qua mar  9 08:05:22 2022 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> 01. Instalando e Carregando pacotes / bibliotecas R...

######@> Instalação de novos pacotes / bibliotecas [Instalação de
######@> Pacotes precisa se executada apenas uma vez]...
install.packages(c("dplyr", "tidyr", "lubridate", "readxl", "ggplot2",
                   "patchwork", "sf", "ggcorrplot", "ggmap", "leaflet",
                   "Hmisc", "imputeTS", "stringr", "DataExplorer"),
                 dependencies = TRUE)

######@> Carregando os pacotes para uso [Carregamento dos pacotes
######@> precisa ser feito sempre que quiser usar o mesmo]...
library(Hmisc)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DataExplorer)
library(readxl)
library(ggplot2)
library(patchwork)
library(sf)
library(ggcorrplot)
library(ggmap)
library(leaflet)
library(imputeTS)

########################################################################
######@> 2. Importando a base de dados...

######@> Base de dados...
dados <- read_excel("Rio_Itajai_Estuario_ver00.xlsx",
                    sheet = 1)

########################################################################
######@> 3. Visualizando os dados...

######@> Visualizando as primeiras linhas da base...
head(dados) %>% data.frame()

######@> Visualizando toda a base...cuidado, bases grandes podem exigir muito poder computacional
View(dados)

######@> Visualizando a estrutura dos dados e como foram importados...maperar o tipo de variavel
glimpse(dados)

######@> Visualizando espacialmente a distribuição dos pontos
######@> amostrais...

#####@> visualização estática...

####@> passando os limites para imagem...faz um resumo estatistico-pega as lat e long
summary(dados)
br <- c(left = -48.7, bottom = -26.95, right = -48.6, top = -26.85)

####@> requisitando a imagem...
mm <- get_map(br, source = "stamen")

####@> visualizando o mapa...restringe a 1000 solicitacoes por dia
p00 <- ggmap(mm) +
    geom_point(data = dados, aes(x = Long, y = Lat, fill = Local),
               pch = 21, size = 2) +
    labs(x = "Longitude", y = "Latitude")
p00

#####@> visualização interativa...abre no browser
background <- addTiles(leaflet())
RioItajai <-
    addMarkers(
        background,
        lat = dados$Lat,
        lng = dados$Long,
        label = dados$Estacao,
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
    )
RioItajai

######@> Visualizando um descritivo estatístico completo dos dados...estatistica para cada dado
describe(dados)

########################################################################
######@> 3. Faxinando os dados...


######@> Corrigindo os problemas na variável "Local"...

#####@> Verificando os casos únicos...
table(dados$Local)

#####@> Corrigindo...
dados$Local <- ifelse(test = dados$Local %in%
                          c("Estuário", "estuario", "estuário"),
                      yes = "Estuário",
                      no = "Adjacência")

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Local)

######@> Corrigindo os problemas na variável "Mês"...

#####@> Verificando os casos únicos...
table(dados$Mês)

#####@> Corrigindo...
dados$Mês <- str_to_title(dados$Mês)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Mês)

######@> Corrigindo os problemas na variável "Draga"...

#####@> Verificando os casos únicos...
table(dados$Draga)

#####@> Corrigindo...
dados$Draga <- str_to_title(dados$Draga)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Draga)

#####@> Corrigindo 02...
dados$Draga <- ifelse(test = dados$Draga == "Nao",
                      yes = "Não",
                      no = dados$Draga)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Draga)

########################################################################
######@> 3. Imputando dados perdidos...

######@> Verificando os dados perdidos...
plot_missing(dados)

######@> Imputando pela média da série de dados...

#####@> Concentração de Carbonato...

####@> visualizando...
dados$Carb

####@> calculando a média...
mean(dados$Carb, na.rm = TRUE)

####@> imputando...
dados$Carb.imp <- na_mean(dados$Carb)

####@> visualizando...
dados$Carb.imp

#####@> Concentração de Matéria Orgânica...

#####@> Concentração de Sedimento Grosso...

#####@> Concentração de Sedimento Fino...

#####@> Concentração de Sedimento Cádmio...

########################################################################
######@> 4. Explorando a base de dados...

######@> Avaliando as variáveis físico-químicas individualmente...

#####@> Temperatura...

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Temp), "Desvio padrão" = sd(dados$Temp),
  quantile(dados$Temp))

####@> histograma...
ggplot(data = dados, aes(x = Temp)) +
    geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
                   fill = "white", colour = "black") +
    scale_x_continuous(breaks = seq(17, 26, 0.5), expand = c(0, 0),
                       limits = c(16.5, 26.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
    labs(x = "Temperatura (ºC)", y = "Frequência absoluta (n)")

#####@> pH...

#####@> Salinidade...

#####@> Sedimento fino...

#####@> Sedimento grosso...

#####@> Material orgânica...

#####@> Concentração de carbonatos...

#####@> Concentração de cádmio (imputado)...

#####@> Concentração de zinco (imputado)...

#####@> Concentração de niquel (imputado)...

#####@> Concentração de cromo (imputado)...

######@> Avaliando as variáveis físico-químicas em função de outras
######@> variáveis...

#####@> Temperatura...

####@> média, desvio padrão e quantis da distribuição por Local...
dados %>%
    group_by(Local) %>%
    summarise(Media = mean(Temp, na.rm = TRUE),
              Desvio =  sd(Temp, na.rm = TRUE),
              q2.5 = quantile(Temp, 0.025),
              q50 = quantile(Temp, 0.5),
              q97.5 = quantile(Temp, 0.975))

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Temp)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Temperatura (ºC)")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>%
    group_by(Local, Draga) %>%
    summarise(Media = mean(Temp, na.rm = TRUE),
              Desvio =  sd(Temp, na.rm = TRUE),
              q2.5 = quantile(Temp, 0.025),
              q50 = quantile(Temp, 0.5),
              q97.5 = quantile(Temp, 0.975))

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Temp)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga)
    labs(x = "Local", y = "Temperatura (ºC)")

#####@> pH...

#####@> Salinidade...

#####@> Sedimento fino...

#####@> Sedimento grosso...

#####@> Material orgânica...

#####@> Concentração de carbonatos...

#####@> Concentração de cádmio (imputado)...

#####@> Concentração de zinco (imputado)...

#####@> Concentração de niquel (imputado)...

#####@> Concentração de cromo (imputado)...


########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
