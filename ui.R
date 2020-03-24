jsc <- '
$(document).ready(function () {
$(".sidebar-menu").children("li").on("click", function() {
$("#mult").toggle();
});
});
'
#Incrementar el tamaño de la carga de los archivos de 5MB A 500MB
options(shiny.maxRequestSize=500*1024^2)
options(stringsAsFactors = F)

title <- tags$a(href='https://infogram.com/covid-2019-ins-colombia-1hnq41zg9ord63z',#http://www.emtelco.com.co
                tags$img(src="GobEmtelco.png", height = '44', width = '125'),
                "geoCovid_19", target="_blank")

title1 <- tags$a(href='https://maps.google.com/',
                 tags$img(src="Google.png", height = '20', width = '50'), target="_blank")
Header <- dashboardHeader(title = title, titleWidth = 280)
# dashboardHeaderPlus(title = title,
#                     enable_rightsidebar = TRUE,
#                     rightSidebarIcon = "cogs" #folder-plus
# ),
####Sidebar#####

sidebar <- dashboardSidebar(width = 292,
                            #fileInput("Base", label = h2('Ingrese los datos'), accept =".xlsx"),
                            # br(),
                            # fluidRow(
                            #   column(8, selectInput('board', label = h2('Seleccione la Vista'),
                            #                         choices = c(none = 'none'), selected = "none")
                            #   ),
                            #
                            #   column(4, imageOutput("pic", height = "auto"))
                            # ),
                            ####control panel for choosing data####
                            # uiOutput("filter"),
                            # br(),
                            # uiOutput("dates"),
                            # br(),
                            # uiOutput("origen"),
                            # br(),

                            fluidRow(actionButton("draw", "Actualizar", icon("street-view"),#tachometer-alt
                                                  style="color: #fff; background-color: #00358E; border-color: #001A7B")),
                            fluidRow(downloadButton("saveData", "Descargar Información")),
                            #br(),
                            sidebarMenu(
                              menuItem("Mapa", tabName = "nav1", icon = shiny::icon("globe-americas")),
                              menuItem("Tabla", tabName = "nav2", icon = shiny::icon("buromobelexperte")),
                              menuItem(
                                "Filtrar información",
                                tabName = "dashboard",
                                icon = icon("th"),

                                tags$head(tags$style(HTML('.skin-blue .main-sidebar {
                                                          background-color: #737277;
                                                          }
                                                          ')))),
                              div(id = "mult",#style="display: none;",
                                  uiOutput("filter"),
                                  conditionalPanel(
                                    condition = "(input.filter)",
                                    uiOutput("dates"),
                                    uiOutput("origen")
                                  )
                                  ,

                                  uiOutput("point"),
                                  selectInput('color', label = 'Estado',
                                              choices = c(none = 'none'), selected = "none"),

                                  br(),
                                  #splitLayout(cellWidths = c("50%", "50%"),
                                  selectInput('plot', label = 'Seleccione el gráfico',
                                              choices = list(
                                                'Análisis de texto' = c(
                                                  'Nube de palabras' = "tCloud",
                                                  'Relación de palabras' = "bCloud" )

                                              ), selected = "tCloud"
                                  ),
                                  sliderInput("param", label = "Frecuencia de palabras", min = 1,
                                              max = 100, value = 5)

                              )
                                )

                                )

body <- dashboardBody(
  # add reference to CSS file
  # ensure that CSS file is in the www folder of the working directory
  tags$head(tags$script(jsc)),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")),
  #tags$head(tags$style(HTML(".small-box {height: 50px}"))),
  tags$head(tags$style(HTML("
                            #final_text {
                            text-align: center;
                            }
                            div.box-header {
                            text-align: center;
                            font-family: 'Source Sans Pro', 'Arial Rounded MT Bold', sans-serif ;
                            font-weight: bold;
                            font-size: 9px;
                            }
                            "))),


  ####Vista indicadores generales####

  tabItems(
    tabItem(tabName = "nav1",h2(textOutput("note")),
            # shiny::navbarPage( id="nav",

            #tabPanel("Mapa interactivo",
            div(class="simpleDiv",

                tags$head(
                  # Include our custom CSS
                  includeCSS("www/styles.css"),
                  includeScript("www/gomap.js")
                ),

                # If not using custom CSS, set height of leafletOutput to a number instead of percent
                leafletOutput("map", width="100%", height="800px"),
                tags$div(id="cite",
                         'Clientes georeferenciados por ',title1,position='bottomright'
                ),
                # Shiny versions prior to 0.11 should use class = "modal" instead.
                box( width = 1,
                     collapsible = TRUE,
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                   width = 755, height = "auto",
                                   fluidRow(
                                     # A static infoBox
                                     #infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                                     # Dynamic infoBoxes
                                     infoBoxOutput(width = 12,"progressBox2")#box(width = 12 ,title=h3("Exploratorio"),solidHeader = T)
                                     #,
                                     #valueBoxOutput("progressBox"),
                                     #infoBoxOutput("approvalBox")
                                   ),
                                   br(),
                                   # uiOutput("filter"),
                                   # conditionalPanel(
                                   #   condition = "(input.filter)",
                                   #   fluidRow(column(width = 7,uiOutput("dates")),
                                   #            column(width = 5,uiOutput("origen"))))
                                   # ,
                                   #
                                   # fluidRow(column(width = 7,uiOutput("point")),
                                   #          column(width = 5,selectInput('color', label = 'Estado de Acción',
                                   #                                       choices = c(none = 'none'), selected = "none"))),
                                   br(),
                                   plotly::plotlyOutput("histCentile", width = "100%", height = "250px"),
                                   # fluidRow(column(width = 5,selectInput('plot', label = 'Seleccione el gráfico',
                                   #                                       choices = list(
                                   #                                         'Análisis de texto' = c(
                                   #                                           'Nube de palabras' = "tCloud",
                                   #                                           'Relación de palabras' = "bCloud" )
                                   #
                                   #                                       ), selected = "bCloud"
                                   # )), column(width = 7,numericInput("param", label = "Frecuencia de palabras", value = 3))),#sliderInput("param", label = "Seleccione el Número de relaciones", min = 1,max = 100, value = 10)
                                   br(),
                                   # plotOutput("fig", width = "100%", height = "350px",click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
                                   #                                             hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                   #                                             brush = brushOpts(id = "plot_brush")),
                                   #          conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$h3("Procesando Bigram...", align="center"))

                                   plotOutput("fig",width = "100%", height = "310px")
                     )#,
                )


                # tags$div(id="cite",
                #          'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                # )
            ),

            conditionalPanel("false", icon("crosshair"))
            #)
    ),

    tabItem(tabName = "nav2",
            DT::dataTableOutput("ziptable")#,

            # fluidRow(
            #   p(class = 'text-center', downloadButton('x4', 'Descargar datos filtrados ')))
    )#,

    #conditionalPanel("false", icon("crosshair"))
  )
  )

#' App UI
shinyUI(dashboardPage(Header,
                        sidebar,
                        body
))
