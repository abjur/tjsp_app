# unindo shapefiles e simplificando
require(ggmap)
require(rgdal)
require(rgeos)
require(maptools)
require(togeojson)
require(abjutils)
load('data/municipios.RData')
rm_accent <- function (x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))

eleitores <- read.csv2('data/eleitores_sp.csv', as.is=T, dec=',')[,c(3,5)] %>%
  select(municipio=Município, pop_ele=Quantidade) %>%
  mutate(pop_ele=as.numeric(gsub('\\.', '', pop_ele))) %>%
  filter(!is.na(pop_ele), !is.na(municipio), municipio !='') %>%
  group_by(municipio) %>%
  summarise(pop_ele=sum(pop_ele)) %>%
  ungroup %>%
  mutate(municipio=rm_accent(municipio)) %>%
  mutate(municipio=ifelse(municipio=='BIRITIBA MIRIM', 'BIRITIBA-MIRIM', municipio),
         municipio=ifelse(municipio=='EMBU DAS ARTES', 'EMBU', municipio),
         municipio=ifelse(municipio=='FLORINEA', 'FLORINIA', municipio),
         municipio=ifelse(municipio=='MOGI MIRIM', 'MOJI MIRIM', municipio))

data(pnud)
pnud <- pnud %>%
  filter(ANO==2010, UF==35) %>%
  select(municipio=Município, pop=pesotot, pop_urb=pesourb, idhm=IDHM, renda_pc=RDPC) %>%
  mutate(municipio=rm_accent(municipio), renda=renda_pc*pop)

data(cadmun)
cadmun <- cadmun %>%
  mutate(uf=as.character(uf)) %>%
  filter(uf=='SP') %>%
  select(municipio=MUNNOMEX, lat, lon) %>%
  filter(lon<0) %>%
  mutate(municipio=rm_accent(municipio),
         municipio=ifelse(municipio=='EMBU DAS ARTES', 'EMBU', municipio))

municipios <- municipios %>%
  mutate(municipio=ifelse(str_detect(municipio, 'BIRITIBA MIRIM'), 'BIRITIBA-MIRIM', municipio),
         municipio=ifelse(str_detect(municipio, 'BRODOSQUI'), 'BRODOWSKI', municipio),
         municipio=ifelse(str_detect(municipio, 'EMBU DAS ARTES'), 'EMBU', municipio),
         municipio=ifelse(str_detect(municipio, 'ESTRELA D OESTE'), 'ESTRELA DOESTE', municipio),
         municipio=ifelse(str_detect(municipio, 'FLORINEA'), 'FLORINIA', municipio),
         municipio=ifelse(str_detect(municipio, 'IPAUCU'), 'IPAUSSU', municipio),
         municipio=ifelse(str_detect(municipio, 'MOGI MIRIM'), 'MOJI MIRIM', municipio),
         municipio=ifelse(str_detect(municipio, 'PALMEIRA D OESTE'), 'PALMEIRA DOESTE', municipio),
         municipio=ifelse(str_detect(municipio, 'PARIQUERA ACU'), 'PARIQUERA-ACU', municipio),
         municipio=ifelse(str_detect(municipio, 'SANTA BARBARA D OESTE'), 'SANTA BARBARA DOESTE', municipio),
         municipio=ifelse(str_detect(municipio, 'SANTO ANTONIO DA POSSE'), 'SANTO ANTONIO DE POSSE', municipio),
         municipio=ifelse(str_detect(municipio, 'SEVERINEA'), 'SEVERINIA', municipio)) %>%
  mutate(circunscricao=str_trim(gsub('[^0-9A-Z ]', '', toupper(rm_accent(circunscricao)))),
         regiao=str_trim(gsub('[^0-9A-Z ]', '', toupper(rm_accent(regiao))))) %>%
  mutate(comarca=ifelse(municipio=='SAO SEBASTIAO DA GRAMA', 'SAO JOSE DO RIO PARDO', comarca)) %>%
  select(municipio, comarca, circunscricao, regiao, entrancia) %>%
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
  inner_join(eleitores, 'municipio') %>%
  inner_join(pnud, 'municipio') %>%
  inner_join(cadmun, 'municipio') %>%
  group_by(comarca) %>%
  mutate(pop_ele_coma=sum(pop_ele),
         pop_coma=sum(pop),
         pop_urb_coma=sum(pop_urb),
         renda_coma=sum(renda),
         idhm_coma=mean(idhm)) %>%
  ungroup


muni_map <- readOGR('data/shp', '35MUE250GC_SIR', stringsAsFactors=F, dropNULLGeometries=T, verbose=F)
muni_map$NM_MUNICIP <- rm_accent(iconv(muni_map$NM_MUNICIP, from='latin1', to='UTF-8'))

simp_map <- function(map, tol=0.01) {
  areas <- lapply(map@polygons, function(x) sapply(x@Polygons, function(y) y@area))
  bigpolys <- lapply(areas, function(x) which(x > tol))
  for(i in 1:length(bigpolys)){
    if(length(bigpolys[[i]]) >= 1 && bigpolys[[i]][1] >= 1){
      map@polygons[[i]]@Polygons <- map@polygons[[i]]@Polygons[bigpolys[[i]]]
      map@polygons[[i]]@plotOrder <- 1:length(map@polygons[[i]]@Polygons)
    }
  }
  return(map)
}

roundPolygons <- function(shptemp, digitss=3) {
  for(i in 1:length(shptemp)) {
    shptemp@polygons[[i]]@Polygons[[1]]@coords <- round(shptemp@polygons[[i]]@Polygons[[1]]@coords,digits=digitss)
  }
  shptemp
}

muni_map@data <- muni_map@data %>% inner_join(municipios, c('NM_MUNICIP'='municipio')) %>% arrange(ID)
muni_map <- muni_map 

coma_map <- unionSpatialPolygons(muni_map, factor(muni_map@data$comarca), threshold=1, avoidUnaryUnion=T)
coma_data <- muni_map@data %>% distinct(comarca)
row.names(coma_data) <- coma_data$comarca
coma_map <- SpatialPolygonsDataFrame(coma_map, data=as.data.frame(coma_data)) %>% roundPolygons(6)
# circ_map <- unionSpatialPolygons(coma_map, factor(coma_map@data$circunscricao), threshold=1, avoidUnaryUnion=T)
# regiao_map <- unionSpatialPolygons(muni_map, muni_map@data$regiao), threshold=1, avoidUnaryUnion=T)

coma_map <- coma_map %>%
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
  simp_map(.01)

# circ_map <- circ_map %>%
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   simp_map(.01)
# 
# regiao_map <- regiao_map %>%
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   gSimplify(tol=.02, topologyPreserve=TRUE) %>% 
#   simp_map(.01)

coma_m <- fortify(coma_map) %>% inner_join(coma_data, c('id'='comarca'))
# circ_m <- fortify(circ_map)
# regiao_m <- fortify(regiao_map)
save(coma_m, file='data/coma_m.RData')

