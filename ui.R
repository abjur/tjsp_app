library(shiny)
library(leaflet)
library(shinyTree)

shinyUI(fluidPage(

  # Controles
  absolutePanel(top=0, left=0, fixed=TRUE, actionButton('show1', label='', icon=icon('gear')), style='z-index:100'),
  tags$script(sprintf("$('#show1').click(function(){$('#div-controls').toggle();})")),

  div(id="div-controls", class="", style="margin: 0px 0px 0px 10px;",
      absolutePanel(wellPanel(
        selectInput('entrancia', 'Entrância', lab_entrancias, selected='FINAL'),
        selectInput('tipo_vara', 'Tipo de vara', lab_tipo_varas, selected='civel'),
        numericInput('kmeans', 'Quantos grupos', value=2, min=1, max=5, step=1),
        checkboxInput('distritais', 'Mostrar municípios com foro distrital?', value=FALSE)),
        top=20, left=100, width=200, style='z-index:10;', draggable=TRUE)
  ),

  # Filtros
  absolutePanel(top=0, right=0, fixed=TRUE, actionButton('show2', label='', icon=icon('tasks')), style='z-index:100'),
  tags$script(sprintf("$('#show2').click(function(){$('#div-filters').toggle();})")),

  div(id="div-filters", class="niveis", style="margin: 0px 0px 0px 10px;",
      absolutePanel(wellPanel(
        dateRangeInput(inputId='corte_temporal', label='De',
                       min='2011-09-01', max='2014-07-01',
                       start='2011-09-01', end='2014-07-01',
                       format='dd/mm/yyyy', separator='até', language='pt-BR'),
        h5("Selecionar comarcas"),
        shinyTree("tree", checkbox=TRUE, search=TRUE)),
        top=20, width=300, left='70%', style='z-index:10;', draggable=TRUE)
  ),

  fluidRow(h3('GeoVis TJSP', style='text-align:center')),

  # Mapa
  fluidRow(column(12, leafletOutput('map', '100%', '800px'))),

  # Gráficos e tabelas

  h4('Análise de agrupamento', style='text-align:center'),
  fluidRow(column(7, plotOutput('grafico_grupos', '100%', '600px')),
           column(5, dataTableOutput('bd_test')))#,

  #h4('Varas da comarca ou foro distrital', style='text-align:center'),
  #fluidRow(column(12, plotOutput('grafico_comarca', '100%', '600px')))#,

  #absolutePanel(verbatimTextOutput('saida'), top='50%', left='50%', draggable=TRUE, class="modal", width=300)


))
