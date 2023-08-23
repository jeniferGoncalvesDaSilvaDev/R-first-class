########################################################################
## Description: Aplicação de análise exploratória à base de dados
## escolhida...
##
## Maintainer: UNIVALI / EP / NEI-Escola Politécnica
## Author: Rodrigo Sant'Ana
## Created: ter ago 22 17:30:54 2023 (-0300)
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

######@>----------------------------------------------------------------
######@> Setup R...
######@>----------------------------------------------------------------

######@> Instalando e carregando os pacotes que serão utilizados aqui...

#####@> Primeiro pacote base - pacman - ele irá verificar se os pacotes
#####@> listados já estão instalados, caso não, irá instalar os mesmos...
if(!require(pacman)) {
    print(paste0("Pacote ainda não instalado. Irei instalar agora!"))
    install.packages("pacman", dependencies = TRUE)
    library(pacman)
} else {
    print(paste0("Pacote já estava instalado e já carregado para o trabalho"))
    library(pacman)
}

#####@> Instalando e/ou carregando a lista de pacotes que serão
#####@> utilizados nesta aula...
p_load(ggplot2, dplyr, ggcorrplot, patchwork, readxl, stargazer, tidyr,
       basedosdados, DataExplorer, viridis)

######@> Padronizando o R para apresentação de números com até 10 casas
######@> decimais sem modificação para notação científica...
options(scipen = 10)

######@>----------------------------------------------------------------
######@> Carregando os dados...
######@>----------------------------------------------------------------

######@> Passando as credenciais para realizar o download da base
######@> escolhida...

#####@> Defina o seu projeto no Google Cloud [SIGA OS PASSOS no link
#####@> basedosdados.org]...
set_billing_id("")

#####@> Requisitando os dados disponibilizados pela basedosdados.org...
query <- bdplyr("br_mme_consumo_energia_eletrica.uf")
df <- bd_collect(query)

######@>----------------------------------------------------------------
######@> Explorando os dados...
######@>----------------------------------------------------------------

######@> Descrição da base...
## Nome                      Descrição
##======================================================================
## ano                       Ano
## mes                       Mês
## sigla_uf                  Sigla da Unidade da Federação
## tipo_consumo              Tipo de Consumo
## numero_consumidores       Número de Consumidores de Energia Elétrica
## consumo                   Consumo de Energia (MWh)

######@> Observando as primeiras 6 linhas da base recem importada...
head(df)

######@> Verificando os tipos de dados...
glimpse(df)

#####@> Outra forma de visualizar a estrutura dos dados...
plot_str(df)

######@> Verificando a existencia de dados perdidos...
plot_missing(df)

#####@> Hum... vamos compreender melhor o que está acontecendo aqui...

####@> Por ano...
table(is.na(df$numero_consumidores), df$ano)

####@> Por uf...
table(is.na(df$numero_consumidores), df$sigla_uf)

####@> Por tipo de consumo...
table(is.na(df$numero_consumidores), df$tipo_consumo)

####@> [INTERESSANTE] Podemos observar que o problema do missing data
####@> está para algumas categorias de tipo de consumo...

####@> Vamos criar uma tabela resumo para compreender melhor está perda
####@> de informação...
tabResumo <- df %>%
    group_by(tipo_consumo, sigla_uf) %>%
    summarise(Perdidos = sum(is.na(numero_consumidores)),
              N = n()) %>%
    mutate(Proporcao = Perdidos/N) %>%
    as.data.frame

p00 <- ggplot(data = tabResumo,
              aes(x = tipo_consumo, y = sigla_uf)) +
    geom_tile(fill = "white", colour = "black") +
    geom_text(aes(label = round(Proporcao, 2),
                  colour = ifelse(Proporcao > 0.05, ">5%", "<5%"))) +
    labs(x = "Tipo de Consumo", y = "Unidade da Federação",
         colour = "") +
    scale_colour_manual(values = c("black", "red")) +
    theme(legend.position = "none")
p00

####@> [LEGAL] algo acontece em Pernambuco e nos casos Cativo e Total...

######@> Bom, antes de partir para imputação, vamos continuar a
######@> exploração...

#####@> Qual o padrão de Consumo de Energia (MWh) por UF?
p00 <- ggplot(data = df, aes(x = sigla_uf, y = consumo)) +
    geom_boxplot() +
    labs(x = "Unidade da Federação", y = "Consumo de Energia (MWh)")
p00

#####@> Qual o padrão de Consumo de Energia (MWh) por UF e Ano?
tabAno <- df %>%
    group_by(ano, sigla_uf) %>%
    summarise(MWh = sum(consumo))

####@> Visualizando as séries temporais...
p00 <- ggplot(data = tabAno,
              aes(x = ano, y = MWh, colour = sigla_uf)) +
    geom_line() +
    labs(x = "Ano", y = "Consumo de Energia (MWh)")
p00

####@> Não ficou muito fácil de observar os padrões...
p00a <- ggplot(data = tabAno,
               aes(x = ano, y = MWh, colour = sigla_uf)) +
    geom_line() +
    facet_wrap(~sigla_uf, scales = "free_y") +
    labs(x = "Ano", y = "Consumo de Energia (MWh)") +
    theme(legend.position = "none")
p00a

#####@> Qual o padrão de Consumo de Energia (MWh) mensal por UF e Ano?
p00 <- ggplot(data = df,
               aes(x = factor(ano), y = consumo, colour = sigla_uf)) +
    geom_boxplot() +
    facet_wrap(~sigla_uf, scales = "free_y") +
    labs(x = "Ano", y = "Consumo de Energia (MWh)") +
    scale_x_discrete(breaks = factor(seq(2004, 2022, 4))) +
    theme(legend.position = "none")
p00

######@> Vamos dar uma olhada nos padrões de consumo em relação ao
######@> número de consumidores? Para isto, vamos começar olhando o
######@> padrão residencial...

######@> Filtrando os dados de Residencias apenas...
temp <- filter(df, tipo_consumo == "Residencial")

######@> Como será a relação Consumo x Consumidores...
p00 <- ggplot(data = temp,
              aes(x = numero_consumidores, y = consumo)) +
    geom_point(pch = 21, colour = "gray", fill = "gray", alpha = 0.5,
               size = 4) +
    facet_wrap(~sigla_uf, scales = "free") +
    labs(x = "Número de Consumidores", y = "Consumo de Energia (MWh)") +
    theme(legend.position = "none")
p00

#####@> Vamos incrementar? Conhecer a tendência - Linear?...
p00a <- ggplot(data = temp,
              aes(x = numero_consumidores, y = consumo)) +
    geom_point(pch = 21, colour = "gray", fill = "gray", alpha = 0.5,
               size = 4) +
    geom_smooth(method = "lm", colour = "red", fill = "red") +
    facet_wrap(~sigla_uf, scales = "free") +
    labs(x = "Número de Consumidores", y = "Consumo de Energia (MWh)") +
    theme(legend.position = "none")
p00a

#####@> Vamos incrementar? Conhecer a tendência - Loess?...
p00b <- ggplot(data = temp,
               aes(x = numero_consumidores, y = consumo)) +
    geom_point(pch = 21, colour = "gray", fill = "gray", alpha = 0.5,
               size = 4) +
    geom_smooth(method = "loess", colour = "red", fill = "red") +
    facet_wrap(~sigla_uf, scales = "free") +
    labs(x = "Número de Consumidores", y = "Consumo de Energia (MWh)") +
    theme(legend.position = "none")
p00b

######@> Poderíamos continuar infinitamente, porém, vamos por a mão na
######@> massa. Tentem aplicar estes conhecimentos aos dados de vocês.

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
