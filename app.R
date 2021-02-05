# judicial waze ----------------------------------------------------------------
# copyright ABJ

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(plotly)
library(shinyTree)

# setup ------------------------------------------------------------------------
d_sf <- read_rds("data/d_sf_min.rds")
prod_tidy <- read_rds("data/prod_tidy.rds")

dd <- distinct(prod_tidy, regiao, circunscricao, comarca)
nested_list <- purrr::map(unique(dd$regiao), ~{
  reg <- .x
  purrr::map(unique(dd[dd$regiao == reg, ]$circunscricao), ~{
    purrr::map(unique(dd[dd$regiao == reg & dd$circunscricao == .x, ]$comarca),
               identity) %>%
      set_names(.)
  }) %>% set_names(unique(dd[dd$regiao == reg, ]$circunscricao))
}) %>% set_names(unique(dd$regiao))

unnest_list <- function(nested_list) {
  if (length(nested_list) == 0) return(tibble())
  nested_list %>%
    purrr::keep(~length(attr(.x, "ancestry")) == 2) %>%
    purrr::map_dfr(~{
      res <- .x
      pais <- set_names(attr(res, "ancestry"), "regiao", "circunscricao")
      tibble(comarca = as.character(res),
             regiao = pais[1],
             circunscricao = pais[2])
    })
}

# ui ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Waze do judiciário v0.2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("agreg", "Nível",
                  c("comarca", "regiao", "circunscricao")),
      selectInput("entrancia", "Entrância",
                  c("Entrância Final",
                    "Entrância Intermediária",
                    "Entrância Inicial")),
      selectInput("tipo_vara", "Tipo de vara", c("civel", "criminal")),
      numericInput("clusters", "Número de grupos", 2, 1, 10),
      dateRangeInput("datas", "Datas",
                     "2011-01-01", "2017-08-01",
                     "2011-01-01", "2017-08-01",
                     language = "pt-BR"),
      shinyTree("tree", checkbox = TRUE, search = TRUE)
    ),
    mainPanel(
      fluidRow(leafletOutput("mapa", height = 500)),
      fluidRow(
        column(width = 6, plotlyOutput("prod")),
        column(width = 6, dataTableOutput("tabela"))))))

# Server -----------------------------------------------------------------------
leaflet_sf <- function(.data_sf, prod_tidy, agreg) {
  pal <- colorQuantile("RdBu", NULL)
  d <- .data_sf$sf[[agreg]]
  if (nrow(d) == 0) {
    l <- leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      fitBounds(-52, -22, -46, -23)
    return(l)
  }
  aux <- prod_tidy %>%
    group_by_(agreg) %>%
    summarise(razao = mean(razao)) %>%
    mutate_(variavel = agreg) %>%
    mutate(label = paste(stringr::str_to_title(agreg),
                         ": <strong>", variavel,
                         "</strong><br/>Produtividade: ",
                         round(razao, 2),
                         sep = ""),
           label = purrr::map(label, HTML))
  d <- d %>%
    inner_join(aux, agreg) %>%
    sf::st_simplify(dTolerance = 0.01)
  if (nrow(d) == 1) d$razao <- "#808080" else d$razao <- pal(d$razao)
  d %>%
    as("Spatial") %>%
    leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    addPolygons(
      color = "black",
      weight = 1.5,
      opacity = .5,
      fillColor = ~razao,
      fillOpacity = .8,
      label = ~label,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
}

server <- function(input, output) {

  # ----------------------------------------------------------------------------
  dados_tree <- reactive({
    input$tree %>%
      get_selected() %>%
      unnest_list()
  })
  dados_mapa <- reactive({
    tree <- dados_tree()
    res <- d_sf
    if (!is.null(input$agreg) && input$agreg == "comarca") {
      res$sf$comarca <- res$sf$comarca[res$sf$comarca$entrancia == input$entrancia,]
    }
    if (length(tree) > 0) {
      res$sf[[input$agreg]] <- res$sf[[input$agreg]] %>%
        dplyr::semi_join(tree, input$agreg)
    }
    res
  })
  dados_prod <- reactive({

    print(input$datas)

    tree <- dados_tree()
    res <- filter_(prod_tidy, input$tipo_vara) %>%
      mutate(data = sprintf("%s-%s-01", year, month)) %>%
      filter(data >= input$datas[1], data <= input$datas[2])
    if (!is.null(input$agreg) && input$agreg == "comarca") {
      res <- filter(res, entrancia == input$entrancia)
    }
    if (length(tree) > 0) {
      res <- res %>%
        dplyr::semi_join(tree, input$agreg)
    }
    res
  })
  # ----------------------------------------------------------------------------
  cluster <- reactive({
    dados_prod() %>%
      select(razao, year, month) %>%
      kmeans(input$clusters)
  })
  output$mapa <- renderLeaflet({
    leaflet_sf(dados_mapa(), dados_prod(), input$agreg)
  })
  output$prod <- renderPlotly({
    p <- dados_prod() %>%
      mutate(grupo = as.character(cluster()$cluster)) %>%
      group_by(year, month, grupo) %>%
      summarise(metrica = median(razao)) %>%
      ungroup() %>%
      mutate(data = as.Date(sprintf("%s-%s-01", year, month))) %>%
      ggplot(aes(x = data, y = metrica, colour = grupo)) +
      geom_line() +
      theme_minimal(16)
    plotly::ggplotly(p)
  })
  output$tabela <- renderDataTable({
    dados_prod() %>%
      mutate(grupo = as.character(cluster()$cluster)) %>%
      group_by(grupo) %>%
      mutate(n = n()) %>%
      group_by(grupo, comarca, nm_court) %>%
      summarise(prod = mean(prod),
                trab = mean(trab),
                media = mean(razao)) %>%
      arrange(grupo, desc(media)) %>%
      ungroup()
  })
  output$tree <- renderTree({
    isolate(nested_list)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

