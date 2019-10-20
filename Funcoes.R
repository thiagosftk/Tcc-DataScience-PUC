install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(scales)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

funcao_juntar_sheets <- function(lista) {
  dataInicial <- data.frame()
  for(f in lista) {
    dataInicial <- rbind(f, dataInicial)
  }
  dataInicial
}		

local_dos_arquivos <- "C:/Users/GuiL/Desktop/workspace/Estudos/TCC - R"

acao_localizacao <- funcao_juntar_sheets(read_excel_allsheets(paste(local_dos_arquivos,"/DataSet acao localizacao (Tratado).xls", sep="")))
apartamentos <- funcao_juntar_sheets(read_excel_allsheets(paste(local_dos_arquivos,"/DataSet apartamentos (Tratado).xls", sep="")))
ruas <- funcao_juntar_sheets(read_excel_allsheets(paste(local_dos_arquivos,"/DataSet ruas (Tratado).xls", sep="")))
acao <- funcao_juntar_sheets(read_excel_allsheets(paste(local_dos_arquivos,"/DataSet ações (Tratado).xls", sep="")))

acao <- acao %>%
  rename(
    ID_ACAO = ID,
    DT_CRIACAO_ACAO = DT_CRIACAO,
    TIPO_ACAO = TIPO,
    STATUS_ACAO = STATUS,
    DT_FINALIZACAO_ACAO = DT_FINALIZACAO
  )

acao_localizacao <- acao_localizacao %>%
  rename(
    ID_ACAO_LOCALIZACAO = ID,
    TIPO_OPERACAO_ACAO_LOCALIZACAO = TIPO_OPERACAO
  )

ruas <- ruas %>%
  rename(
    ID_RUA = ID,
    NUMERO_RUA = NUMERO
  )

acao_localizacoes_join <- full_join(acao_localizacao, acao_localizacao, by=c("ID_ACAO"), copy = FALSE, keep = FALSE, name = NULL) %>%
  filter(TIPO_OPERACAO_ACAO_LOCALIZACAO.x == 0 & ID_ACAO_LOCALIZACAO.x != ID_ACAO_LOCALIZACAO.y)

acao_localizacao <- select(acao_localizacoes_join,
                           -c(TIPO_OPERACAO_ACAO_LOCALIZACAO.x, TIPO_OPERACAO_ACAO_LOCALIZACAO.y,
                              ID_RUA.y, ID_ACAO_LOCALIZACAO.x, ID_ACAO_LOCALIZACAO.y)) %>%
  rename(
    ID_RUA_ORIGEM = ID_RUA.x,
    ID_APARTAMENTO_ORIGEM = ID_APARTAMENTO.x,
    ID_APARTAMENTO_DESTINO = ID_APARTAMENTO.y
  )

acao_localizacoes_join <- 0

acao_local_join <- acao %>% inner_join(acao_localizacao, by = "ID_ACAO")

acao_local_join <- acao_local_join %>% 
  mutate(
    MES_CRIACAO_ACAO = month(DT_CRIACAO_ACAO),
    DIA_CRIACAO_ACAO = day(DT_CRIACAO_ACAO),
    HORA_CRIACAO_ACAO = hour(DT_CRIACAO_ACAO),
    MINUTO_CRIACAO_ACAO = minute(DT_CRIACAO_ACAO),
    IS_ORIGEM_APARTAMENTO = !is.na(ID_APARTAMENTO_ORIGEM),
    IS_SEGUNDA_QUINZENA = day(DT_CRIACAO_ACAO)>=15,
  )
acao_local_join <- select(acao_local_join, -c(DT_FINALIZACAO_ACAO, STATUS_ACAO,
                                              TIPO_ACAO, ID_APARTAMENTO_ORIGEM, 
                                              ID_RUA_ORIGEM)
                          )

acao_teste <- acao_local_join %>%
  group_by(ID_OBJETO, ID_USUARIO, MES_CRIACAO_ACAO, DIA_CRIACAO_ACAO) %>%
  mutate(
    LIMITE_10_ACOES_DIA=n()>=10
  ) %>%
  ungroup() %>%
  group_by(ID_OBJETO, ID_USUARIO, MES_CRIACAO_ACAO, DIA_CRIACAO_ACAO,
           HORA_CRIACAO_ACAO) %>%
  mutate(
    LIMITE_5_ACOES_HORA=n()>=5,
  ) %>%
  ungroup() %>%
  group_by(ID_OBJETO, ID_USUARIO, MES_CRIACAO_ACAO, DIA_CRIACAO_ACAO,
           ID_APARTAMENTO_DESTINO) %>%
  mutate(
    LIMITE_20_ACOES_DIA_APARTAMENTO=n()>=20
  ) %>%
  ungroup()



teste <- acao_teste %>%
  select (IS_ORIGEM_APARTAMENTO, LIMITE_10_ACOES_DIA, LIMITE_5_ACOES_HORA,
         LIMITE_20_ACOES_DIA_APARTAMENTO, IS_SEGUNDA_QUINZENA
  ) %>%
  mutate(
    IS_ORIGEM_APARTAMENTO = as.factor(IS_ORIGEM_APARTAMENTO),
    LIMITE_10_ACOES_DIA = as.factor(LIMITE_10_ACOES_DIA),
    LIMITE_5_ACOES_HORA = as.factor(LIMITE_5_ACOES_HORA),
    LIMITE_20_ACOES_DIA_APARTAMENTO = as.factor(LIMITE_20_ACOES_DIA_APARTAMENTO),
    IS_SEGUNDA_QUINZENA = as.factor(IS_SEGUNDA_QUINZENA)
  )


teste_grouped <- teste %>% group_by(FRAUDES = LIMITE_20_ACOES_DIA_APARTAMENTO) %>%
  summarise(qtd = n())

bp <- ggplot(teste_grouped, aes(x="", y=qtd, fill=FRAUDES))+geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)

pie

set.seed(127630)
.data <- c("training", "test") %>%
  sample(nrow(teste), replace = T) %>%
  split(teste, .)


rtree_fit <- rpart(LIMITE_20_ACOES_DIA_APARTAMENTO  ~ ., 
                   .data$training
)

rpart.plot(rtree_fit)   

summary(rtree_fit)

