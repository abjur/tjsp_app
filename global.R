require(dplyr)
require(stringr)

load("data/prod_tjsp_spr.RData")
load("data/coma_m.RData")

rm_accent <- function (x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))

fx <- function(x) {ifelse(x==-1, NA, x)}
dados <- prod_tjsp_spr %>%
  mutate(data=as.character(data)) %>%
  mutate_each(funs(fx)) %>%
  mutate(comarca=ifelse(municipio=='SAO SEBASTIAO DA GRAMA', 'SAO JOSE DO RIO PARDO', comarca)) %>%
  do(.[,colSums(is.na(.))<nrow(.)]) %>%
  mutate(entrancia=ifelse(str_detect(comarca, 'ARARAQUARA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'ATIBAIA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'AVARE'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'BARUERI'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'BOTUCATU'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'BRAGANCA PAULISTA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'CAMPINAS'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'CATANDUVA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'COTIA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'FERNANDOPOLIS'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'FRANCO DA ROCHA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'ITANHAEM'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'ITAPEVA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'JUNDIAI'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'LORENA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'MIRASSOL'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'MOGI DAS CRUZES'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'PIRACICABA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'RIBEIRAO PIRES'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'RIO CLARO'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SANTA ISABEL'), 'INICIA', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SAO CARLOS'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SAO SEBASTIAO'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SOROCABA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'TUPA'), 'INTER', entrancia)) %>%
  mutate(tipo_vara=ifelse(str_detect(nm_vara, 'FAZ') & planilha=='civel', 'fazen',
                          ifelse(str_detect(nm_vara, 'FAM') & planilha=='civel', 'famil',
                                 #ifelse(str_detect(nm_vara, 'UNI|CUMU') & planilha=='civel', 'cumul',
                                 planilha))) %>%
  filter(!str_detect(nm_vara, 'FALENCIAS')) %>%
  filter(!tipo_vara %in% c('jecri', 'infju', 'jeciv'))

tree <- function(hierarquia) {
  lista <- tapply(hierarquia$circunscricao, hierarquia$regiao, function(x) {
    l <- tapply(hierarquia$comarca[hierarquia$circunscricao %in% x], x, function(y) {
      l <- as.list(y)
      names(l) <- y
      l <- lapply(l, structure, sticon="")
      l
    }, simplify=F)
    l <- lapply(l, structure, sticon="")
    l
  }, simplify=F)
  lista <- lapply(lista, structure, sticon="")
  lista
}

pega_coma <- function(tr) {
  unl <- unlist(tr)
  cinco  <- unl[str_count(names(unl), '\\.') == 5]
  seis   <- unl[str_count(names(unl), '\\.') == 6]
  selected <- seis[str_detect(names(seis), 'selected')]
  nm_selected <- gsub('\\.state\\.selected', '',
                      names(selected[selected == 'TRUE']))
  nm <- gsub('\\.text', '', names(cinco))
  if(length(nm) > 0) {
    names(cinco) <- nm
    result <- as.character(cinco[names(cinco) %in% nm_selected])
    return(result)
  }
  return(character(0))
}


lab_entrancias <- setNames(unique(dados$entrancia), c('Final', 'Inicial', 'Intermediária'))
lab_tipo_varas <- setNames(unique(dados$tipo_vara), c('Cível',
                                                     'Família e Sucessões',
                                                     'Fazenda Pública',
                                                     #'Infância e Juventude',
                                                     'Criminal',
                                                     #'Juizado Especial Cível',
                                                     #'Cumulativa',
                                                     #'Juizado Especial Criminal',
                                                     'Execução Fiscal'))

