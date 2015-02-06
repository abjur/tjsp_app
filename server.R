library(shiny)
library(shinyTree)
library(ggplot2)
library(scales)
library(tidyr)
library(RColorBrewer)
library(xtable)
load('data/municipios_app.RData')

coloca_na <- function(comarca) {
  d <- rbind_list(comarca, data_frame(id=comarca$id[1]))
  return(d)
}

shinyServer(function(input, output, session) {

  map <- createLeafletMap(session, 'map')
  #mapa <- fromJSON('data/mapa.geojson')

  observe({
    map$clearShapes()

    coma <- comarcas()
    if(length(coma) > 0) {
      coma_m <- coma_m %>% filter(id %in% coma)
    }
    aux <- coma_m %>%
      filter(entrancia==input$entrancia) %>%
      group_by(id) %>%
      do(coloca_na(.)) %>%
      ungroup %>%
      arrange(id, order)

    cores <- aux %>%
      filter(!is.na(long)) %>%
      distinct(id) %>%
      mutate(cor=brewer.pal(10, "RdYlGn")[as.numeric(factor(regiao))])

    map$addPolygon(aux$lat, aux$long, cores$id,
                   lapply(cores$cor, function(x) { list(fillColor = x) }),
                   list(fill=TRUE, fillOpacity=.9, stroke=TRUE, opacity=.8, color="black", weight=1))

      map$clearMarkers()
      if(input$distritais) {
        map$clearMarkers()

        coma <- comarcas()
        d <- dados %>%
          filter(distrital, entrancia==input$entrancia,
                 (length(coma)==0 | comarca %in% coma)) %>%
          select(municipio, lat, lon) %>%
          distinct

        map$addMarker(d$lat, d$lon, d$municipio)
      }
  })

  dados_cg <- reactive({
    map$clearPopups()
    d <- dados %>%
      filter(tipo_vara==input$tipo_vara, entrancia==input$entrancia) %>%
      filter(data >= input$corte_temporal[1], data <= input$corte_temporal[2])

    coma <- comarcas()
    if(length(coma) > 0) {
      d <- d %>% filter(comarca %in% coma)
    }
    return(d)
  })

  comarcas <- reactive({
    coma <- sapply(input$tree, function(x) {
      sapply(x$children, function(y) {
        sapply(y$children, function(z) {
          if(z$state$selected) z$text
        })
      })
    })
    coma <- as.character(unlist(coma))
    return(coma)
  })

  tidy <- reactive({
    d <- dados_cg()
    if(nrow(d)<5) return(NULL)

    d <- d %>% unite(muni_vara, c(municipio, nm_vara))
    if(length(unique(d$muni_vara)) < 5) return(NULL)

    if(input$tipo_vara %in% c('civel', 'fazen', 'cumul')) {
      tidy <- d %>%
        select(foro, id_vara, muni_vara, data, comarca,
               p_estoque_conhe=p_civel_3_1_1, p_estoque_exjud=p_civel_3_1_2, p_estoque_extra=p_civel_3_1_3,
               p_distrib_conhe=p_civel_4_1_1, p_distrib_exjud=p_civel_4_1_2, p_distrib_extra=p_civel_4_1_3,
               p_audiencias=p_civel_5,
               p_pauta_aud_tit=p_civel_6_1, p_pauta_aud_aux=p_civel_6_2,
               p_sent_comp=p_civel_7_1, p_sent_resu=p_civel_7_2,
               p_dec_interloc=p_civel_8,
               p_func_escr=p_civel_9_1, p_func_ofic=p_civel_9_2, p_func_auxi=p_civel_9_3,
               p_embargos_prov=p_civel_10_1, p_embargos_nprov=p_civel_10_2, p_embargos_ncon=p_civel_10_3) %>%
        arrange(muni_vara, data) %>%
        mutate(p_pauta_aud_tit=as.numeric(gsub('[^0-9]', '', p_pauta_aud_tit)),
               p_pauta_aud_aux=as.numeric(gsub('[^0-9]', '', p_pauta_aud_aux))) %>%
        mutate(p_sent=p_sent_resu+p_sent_comp,
               p_func=p_func_ofic+p_func_auxi+p_func_escr) %>%
        mutate(r_sent_conhe=p_sent/(p_estoque_conhe+p_distrib_conhe),
               r_sent_exjud=p_sent/(p_estoque_exjud+p_distrib_exjud),
               r_sent_extra=p_sent/(p_estoque_extra+p_distrib_extra),
               r_sent_func=p_sent/p_func) %>%
        select(id_vara, data, muni_vara, comarca, p_sent, p_func,
               p_distrib_conhe, p_distrib_exjud, p_distrib_extra,
               p_estoque_conhe, p_estoque_exjud, p_estoque_extra,
               starts_with('r_')) %>%
        filter(r_sent_conhe + r_sent_exjud + r_sent_extra + r_sent_func < Inf)
      return(tidy)
    } else if(input$tipo_vara == 'famil') {
      tidy <- d %>%
        select(foro, id_vara, muni_vara, data, comarca,
               p_estoque_conhe=p_civel_3_1_1, p_estoque_exjud=p_civel_3_1_2, p_estoque_extra=p_civel_3_1_3,
               p_distrib_conhe=p_civel_4_1_1, p_distrib_exjud=p_civel_4_1_2, p_distrib_extra=p_civel_4_1_3,
               p_audiencias=p_civel_5,
               p_pauta_aud_tit=p_civel_6_1, p_pauta_aud_aux=p_civel_6_2,
               p_sent_comp=p_civel_7_1, p_sent_resu=p_civel_7_2,
               p_dec_interloc=p_civel_8,
               p_func_escr=p_civel_9_1, p_func_ofic=p_civel_9_2, p_func_auxi=p_civel_9_3,
               p_embargos_prov=p_civel_10_1, p_embargos_nprov=p_civel_10_2, p_embargos_ncon=p_civel_10_3) %>%
        arrange(muni_vara, data) %>%
        mutate(p_pauta_aud_tit=as.numeric(gsub('[^0-9]', '', p_pauta_aud_tit)),
               p_pauta_aud_aux=as.numeric(gsub('[^0-9]', '', p_pauta_aud_aux))) %>%
        mutate(p_sent=p_sent_resu+p_sent_comp,
               p_func=p_func_ofic+p_func_auxi+p_func_escr) %>%
        mutate(r_sent_conhe=p_sent/(p_estoque_conhe+p_distrib_conhe),
               r_sent_exjud=p_sent/(p_estoque_exjud+p_distrib_exjud),
               #r_sent_extra=p_sent/(p_estoque_extra+p_distrib_extra),
               r_sent_func=p_sent/p_func) %>%
        select(id_vara, data, muni_vara, comarca, p_sent, p_func,
               p_distrib_conhe, p_distrib_exjud, p_distrib_extra,
               p_estoque_conhe, p_estoque_exjud, p_estoque_extra,
               starts_with('r_')) %>%
        filter(r_sent_conhe + r_sent_exjud + r_sent_func < Inf)
    } else if(input$tipo_vara == 'crimi') {
      tidy <- d %>%
        select(foro, id_vara, muni_vara, data, comarca,
               p_pc_sem_sent=p_crimi_3_1, p_pc_com_sent=p_crimi_3_2, p_precatorias=p_crimi_4,
               p_juri_com=p_crimi_5_1, p_juri_sem=p_crimi_5_2, p_exec=p_crimi_6,
               p_crime_outros=p_crimi_7,
               p_crime_total_andamento=p_crimi_8,
               p_inque_dist=p_crimi_9_1,p_prec_dist=p_crimi_9_2, p_outr_dist=p_crimi_9_3,
               p_re_dist=p_crimi_9_4,p_juri_dist=p_crimi_9_5,p_exec_dist=p_crimi_9_6,
               p_denuncias=p_crimi_10,
               p_audiencias=p_crimi_11,
               p_pauta_aud_tit=p_crimi_12_1, p_pauta_aud_aux=p_crimi_12_2,
               p_pauta_juri = p_crimi_14,
               p_sent_comp=p_crimi_15_1, p_sent_resu=p_crimi_15_2,
               p_dec_interloc=p_crimi_16,
               p_func_escr=p_crimi_18_1, p_func_ofic=p_crimi_18_2, p_func_auxi=p_crimi_18_3,
               p_embargos_prov=p_crimi_17_1, p_embargos_nprov=p_crimi_17_2, p_embargos_ncon=p_crimi_17_3) %>%
        mutate(p_entrados=p_inque_dist+p_prec_dist+p_outr_dist+p_re_dist+p_juri_dist+p_exec_dist) %>%
        arrange(muni_vara, data) %>%
        mutate(p_pauta_aud_tit=as.numeric(gsub('[^0-9]', '', p_pauta_aud_tit)),
               p_pauta_aud_aux=as.numeric(gsub('[^0-9]', '', p_pauta_aud_aux)),
               p_pauta_juri=as.numeric(gsub('[^0-9]', '', p_pauta_juri))) %>%
        mutate(p_sent=p_sent_resu+p_sent_comp,
               p_func=p_func_ofic+p_func_auxi+p_func_escr) %>%
        mutate(r_sent_pc_com=p_sent/(p_pc_com_sent+p_pc_sem_sent+p_denuncias),
               r_sent_func=p_sent/p_func) %>%
        select(id_vara, data, muni_vara, comarca, p_sent, p_func,
               p_pc_com_sent, p_pc_sem_sent, p_denuncias, starts_with('r_')) %>%
        filter(r_sent_pc_com + r_sent_func < Inf)
      return(tidy)
    } else if(input$tipo_vara %in% c('execf')) {
      tidy <- d %>%
        select(foro, id_vara, muni_vara, data, comarca,
               p_estoque_fed=p_execf_3_1, p_estoque_est=p_execf_3_2, p_estoque_mun=p_execf_3_3,
               p_distrib_fed=p_execf_5_2, p_distrib_est=p_execf_5_3, p_distrib_mun=p_execf_5_4,
               p_execf_10_1, p_execf_10_2, p_execf_13_1, p_execf_13_2, p_execf_13_3) %>%
        mutate(p_sent=p_execf_10_1+p_execf_10_2, p_func=p_execf_13_1+p_execf_13_2+p_execf_13_3) %>%
        arrange(muni_vara, data) %>%
        mutate(r_sent_fed=p_sent/(p_estoque_fed+p_distrib_fed),
               r_sent_est=p_sent/(p_estoque_est+p_distrib_est),
               r_sent_mun=p_sent/(p_estoque_mun+p_distrib_mun),
               r_sent_func=p_sent/p_func) %>%
        select(id_vara, data, muni_vara, comarca, p_sent, p_func,
               p_distrib_fed, p_distrib_est, p_distrib_mun,
               p_estoque_fed, p_estoque_est, p_estoque_mun,
               starts_with('r_')) %>%
        filter(r_sent_fed + r_sent_est + r_sent_mun + r_sent_func < Inf)
      return(tidy)
    }
  })


  observe({
    map$clearPopups()
    ev <- input$map_shape_click
    print(ev)
    if(!is.null(ev)) {
      if(str_length(ev$id) > 1) {

      d <- coma_m %>%
        filter(id==ev$id) %>%
        group_by(id) %>%
        summarise(lat=mean(lat), long=mean(long)) %>%
        ungroup

      infos <- municipios %>%
        filter(comarca==d$id) %>%
        distinct(comarca) %>%
        transmute(comarca=comarca, População=pop_coma, `População urbana`=pop_urb_coma,
                  `Eleitores`=pop_ele_coma,
                  `Renda total`=renda_coma, `IDH médio`=idhm_coma) %>%
        gather(nm, val, -comarca) %>%
        mutate(val=prettyNum(val, big.mark='.', decimal.mark=',')) %>%
        select(-comarca) %>% data.frame

      names(infos) <- c('Comarca', d$id)

      content <- as.character(tagList(
        HTML(print(xtable(infos, align="lp{3cm}c", digits=c(0,0,0)),
                   type='html',
                   include.rownames=FALSE,
                   html.table.attributes = "class = 'table table-striped'"))
      ))

      map$showPopup(d$lat, d$long, content, d$id)
    }}
  })


  observe({
    map$clearPopups()
    ev <- input$map_marker_click
    if(!is.null(ev)) {

      d <- municipios %>%
        filter(municipio==ev$id) %>%
        distinct(municipio) %>%
        select(municipio, `População`=pop, `População urbana`=pop_urb, `Eleitores`=pop_ele,
               `Renda total`=renda, `IDH municipal`=idhm, lat, lon)

      infos <- d %>%
        select(-lat, -lon) %>%
        gather(nm, val, -municipio) %>%
        mutate(val=prettyNum(val, big.mark='.', decimal.mark=',')) %>%
        select(-municipio) %>% data.frame

      names(infos) <- c('Município', ev$id)

      content <- as.character(tagList(
        HTML(print(xtable(infos, align="lp{3cm}c", digits=c(0,0,0)),
                   type='html',
                   include.rownames=FALSE,
                   html.table.attributes = "class = 'table table-striped'"))
      ))
      map$showPopup((d$lat), d$lon, content, ev$id)
    }
  })


  agrupamento <- reactive({
    d <- tidy()

    if(input$tipo_vara %in% c('civel', 'crimi', 'famil', 'fazen', 'cumul', 'execf')) {
      agrup <- tidy() %>%
        select(muni_vara, data, starts_with('r_')) %>%
        gather(prod, num, -muni_vara, -data) %>%
        unite(prod_date, c(prod, data)) %>%
        mutate(prod_date=gsub('-', '_', prod_date)) %>%
        distinct(muni_vara, prod_date) %>%
        spread(prod_date, num) %>%
        na.omit

      row.names(agrup) <- agrup$muni_vara

      set.seed(20101916)

      validate(need(try({agrup %>% select(-muni_vara) %>% scale %>% kmeans(input$kmeans)}),
                    'Reduza o número de grupos'))
      km <- agrup %>% select(-muni_vara) %>% scale %>% kmeans(input$kmeans)

      km_df <- data_frame(muni_vara=names(km$cluster), grupo=as.character(km$cluster))
      res <- left_join(d, km_df, 'muni_vara')
      return(res)
    }
  })

  output$bd_test <- renderDataTable({
    d <- agrupamento()
    if(!is.null(d)) {

      if(input$tipo_vara=='crimi') {
        d <- d %>%
          group_by(grupo, id_vara) %>%
          summarise(`Número de varas`=1,
                    `Processos-crime com sentença`=last(p_pc_com_sent),
                    `Processos-crime sem sentença`=last(p_pc_sem_sent),
                    `Denúncias`=last(p_denuncias),
                    `Quantidade de funcionários`=last(p_func),
                    `Sentenças proferidas`=sum(p_sent)) %>%
          ungroup %>%
          select(-id_vara) %>%
          group_by(grupo) %>%
          summarise_each(funs(sum)) %>%
          ungroup %>%
          mutate(grupo=paste('Grupo', grupo)) %>%
          gather(Informação, qtd, -grupo) %>%
          spread(grupo, qtd) %>%
          mutate_each(funs(prettyNum(., big.mark='.', decimal.mark=',')),
                      starts_with('Grupo'))

      } else if(input$tipo_vara %in% c('civel', 'famil', 'fazen', 'cumul')) {
        d <- d %>%
          group_by(grupo, id_vara) %>%
          summarise(`Número de varas`=1,
                    `Estoque de conhecimento`=last(p_estoque_conhe),
                    `Estoque de execução judicial`=last(p_estoque_exjud),
                    `Estoque de execução de título extrajudicial`=last(p_estoque_extra),
                    `Quantidade de funcionários`=last(p_func),
                    `Sentenças proferidas`=sum(p_sent)) %>%
          ungroup %>%
          select(-id_vara) %>%
          group_by(grupo) %>%
          summarise_each(funs(sum)) %>%
          ungroup %>%
          mutate(grupo=paste('Grupo', grupo)) %>%
          gather(Informação, qtd, -grupo) %>%
          spread(grupo, qtd) %>%
          mutate_each(funs(prettyNum(., big.mark='.', decimal.mark=',')),
                      starts_with('Grupo'))
      } else if(input$tipo_vara %in% c('execf')) {
        d <- d %>%
          group_by(grupo, id_vara) %>%
          summarise(`Número de varas`=1,
                    `Estoque de Execuções Federais`=last(p_estoque_fed),
                    `Estoque de Execuções Estaduais`=last(p_estoque_est),
                    `Estoque de Execuções Municipais`=last(p_estoque_mun),
                    `Quantidade de funcionários`=last(p_func),
                    `Sentenças proferidas`=sum(p_sent)) %>%
          ungroup %>%
          select(-id_vara) %>%
          group_by(grupo) %>%
          summarise_each(funs(sum)) %>%
          ungroup %>%
          mutate(grupo=paste('Grupo', grupo)) %>%
          gather(Informação, qtd, -grupo) %>%
          spread(grupo, qtd) %>%
          mutate_each(funs(prettyNum(., big.mark='.', decimal.mark=',')),
                      starts_with('Grupo'))
      }

    }
    d
  }, options=list(pageLength = 10))

  output$tree <- renderTree({
    hierarquia <- dados %>% select(regiao, circunscricao, comarca) %>% distinct
    tree(hierarquia)
  })

  output$grafico_grupos <- renderPlot({
    d <- agrupamento()
    if(input$tipo_vara %in% c('civel', 'fazen', 'cumul')) {
      aux <- d %>%
        group_by(data, grupo) %>%
        summarise(r_sent_conhe=sum(p_sent)/(sum(p_estoque_conhe)+sum(p_distrib_conhe)),
                  r_sent_exjud=sum(p_sent)/(sum(p_estoque_exjud)+sum(p_distrib_exjud)),
                  r_sent_extra=sum(p_sent)/(sum(p_estoque_extra)+sum(p_distrib_extra)),
                  r_sent_func=sum(p_sent)/sum(p_func)) %>%
        ungroup %>%
        gather(prod, valor, -data, -grupo)
      aux$prod <- factor(aux$prod)
      levels(aux$prod) <- c('Sentenças por processo de conhecimento',
                            'Sentenças por processo de execução judicial',
                            'Sentenças por processo de execução de título extrajudicial',
                            'Sentenças por funcionário')
      aux$prod <- as.character(aux$prod)
      aux %>%
        ggplot(aes(x=as.Date(data), y=valor, colour=grupo)) +
        facet_wrap(~prod, scales='free_y') +
        geom_path(alpha=.9, size=1.5) +
        scale_x_date(breaks=date_breaks('3 months'), labels=date_format('%m-%Y')) +
        theme_bw() +
        labs(x='', y='Razão')+
        theme(axis.text.x=element_text(angle=45, hjust=1))
    } else if(input$tipo_vara %in% c('famil')) {
      aux <- d %>%
        group_by(data, grupo) %>%
        summarise(r_sent_conhe=sum(p_sent)/(sum(p_estoque_conhe)+sum(p_distrib_conhe)),
                  r_sent_exjud=sum(p_sent)/(sum(p_estoque_exjud)+sum(p_distrib_exjud)),
                  #r_sent_extra=sum(p_sent)/(sum(p_estoque_extra)+sum(p_distrib_extra)),
                  r_sent_func=sum(p_sent)/sum(p_func)) %>%
        ungroup %>%
        gather(prod, valor, -data, -grupo)
      aux$prod <- factor(aux$prod)
      levels(aux$prod) <- c('Sentenças por processo de conhecimento',
                            'Sentenças por processo de execução judicial',
                            'Sentenças por funcionário')
      aux$prod <- as.character(aux$prod)
      aux %>%
        ggplot(aes(x=as.Date(data), y=valor, colour=grupo)) +
        facet_wrap(~prod, scales='free_y', ncol = 2) +
        geom_path(alpha=.9, size=1.5) +
        scale_x_date(breaks=date_breaks('3 months'), labels=date_format('%m-%Y')) +
        theme_bw() +
        labs(x='', y='Razão')+
        theme(axis.text.x=element_text(angle=45, hjust=1))
    } else if(input$tipo_vara == 'crimi') {
      aux <- d %>%
        group_by(data, grupo) %>%
        summarise(r_sent_pc_com=sum(p_sent)/(sum(p_pc_com_sent)+sum(p_pc_sem_sent)+sum(p_denuncias)),
                  r_sent_func=sum(p_sent)/sum(p_func)) %>%
        ungroup %>%
        gather(prod, valor, -data, -grupo)
        aux$prod <- factor(aux$prod)
      levels(aux$prod) <- sort(c('Sentenças por funcionário',
                                 'Sentenças por processo'))
      aux$prod <- as.character(aux$prod)
      aux %>%
        ggplot(aes(x=as.Date(data), y=valor, colour=grupo)) +
        facet_wrap(~prod, scales='free_y') +
        geom_path(alpha=.9, size=1.5) +
        scale_x_date(breaks=date_breaks('3 months'), labels=date_format('%m-%Y')) +
        theme_bw() +
        labs(x='', y='Razão')+
        theme(axis.text.x=element_text(angle=45, hjust=1))
    } else if(input$tipo_vara == 'execf') {
      aux <- d %>%
        group_by(data, grupo) %>%
        summarise(r_sent_fed=sum(p_sent)/(sum(p_estoque_fed)+sum(p_distrib_fed)),
                  r_sent_est=sum(p_sent)/(sum(p_estoque_est)+sum(p_distrib_est)),
                  r_sent_mun=sum(p_sent)/(sum(p_estoque_mun)+sum(p_distrib_mun)),
                  r_sent_func=sum(p_sent)/sum(p_func)) %>%
        ungroup %>%
        gather(prod, valor, -data, -grupo)
      aux$prod <- factor(aux$prod)
      levels(aux$prod) <- c('Sentenças por Execução Estadual',
                            'Sentenças por Execução Federal',
                            'Sentenças por funcionário',
                            'Sentenças por Execução Municipal')
      aux$prod <- as.character(aux$prod)
      aux %>%
        ggplot(aes(x=as.Date(data), y=valor, colour=grupo)) +
        facet_wrap(~prod, scales='free_y') +
        geom_path(alpha=.9, size=1.5) +
        scale_x_date(breaks=date_breaks('3 months'), labels=date_format('%m-%Y')) +
        theme_bw() +
        labs(x='', y='Razão')+
        theme(axis.text.x=element_text(angle=45, hjust=1))
    }
  })

  nc <- 0
  nc1 <- 0
  output$grafico_comarca <- renderPlot({

    ev0 <- input$map_click
    ev <- input$map_shape_click
    ev1 <- input$map_marker_click

    if(!is.null(ev)) {
      if(ev[['.nonce']]==nc) {
        ev <- NULL
      } else {
        nc <<- ev[['.nonce']]
      }
    }

    if(!is.null(ev1) & input$distritais) {
      if(ev1[['.nonce']]==nc1) {
        ev1 <- NULL
      } else {
        nc1 <<- ev1[['.nonce']]
      }
    }

    validate(need(!is.null(ev) | !is.null(ev1), 'Por favor, clique em uma comarca ou município com foro distrital.'))

    if(!is.null(ev)) {
      aux <- tidy() %>%
        filter(comarca==ev$id)
    } else if(!is.null(ev1)) {
      aux <- tidy() %>%
        separate(muni_vara, c('municipio', 'nm_vara'), sep='_') %>%
        filter(municipio==ev1$id)
    }

    validate(need(nrow(aux) > 0, 'Por favor, clique em uma comarca ou município com foro distrital.'))

    if(!is.null(ev) | !is.null(ev1)) {
      p <- aux %>%
        select(data, id_vara, starts_with('p_'), starts_with('r_')) %>%
        gather(prod, valor, -data, -id_vara) %>%
        ggplot(aes(x=as.Date(data), y=valor, colour=id_vara)) +
        facet_wrap(~prod, scales='free_y') +
        geom_path(alpha=.9, alpha=.5) +
        scale_x_date(breaks=date_breaks('3 months'), labels=date_format('%m-%Y')) +
        guides(colour=F) +
        theme_bw() +
        labs(x='', y='Valor')+
        theme(axis.text.x=element_text(angle=45, hjust=1))
      return(p)
    }
  })
})
