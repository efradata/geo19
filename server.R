
#' App Server
#'
#' @param input Input object
#' @param output Output list
#' @param session Shiny session
shinyServer(function(input, output, session) {
  zipdata = NULL
  cleantable=NULL
  #first = TRUE
  Base=NULL
  library(readxl)
  library(readr)
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(extrafont)
  library(reshape2)
  library(writexl) #write_xlsx(BDretirados3, "D:/BDretirados3.xlsx")
  library(data.table)
  library(ggrepel)
  library(tibble)
  library(Hmisc)
  library(htmltools)
  library(leaflet)
  library(leaflet.extras)
  library(RColorBrewer)
  library(scales)
  library(lattice)
  library(dplyr)
  library(plotly)
  library(shinydashboard)
  library(shiny)
  library(flexdashboard)
  library(graphics)
  library(tm)
  library(qdapRegex)
  library(janeaustenr)
  library(igraph)
  library(ggraph)
  library(tau)
  library(wordcloud)
  library(shinydashboardPlus)
  library(ggmap)
  library(rgdal)
  library(httr)
  library(sp)
  library(raster)
  #setwd("//10.1.1.7/01 Oficina Planeación y Control/01 Analytics/Efrain/App COVID-19/")##cambiar ruta
  #setwd("C:/Users/Efrain/Desktop/App COVID-19/")##cambiar ruta
  source("funciones_modelo_mapa.R", encoding = "UTF-8")

  #Barrios = readOGR(dsn="./Barrios", layer="Barrios")
  Barrios = readOGR(dsn="Barrios", layer="Barrios")

  jsc <- '
$(document).ready(function () {
$(".sidebar-menu").children("li").on("click", function() {
$("#mult").toggle();
});
});
'

  Base <- reactive({
    #consulta query cada tantos minutos
    # df<-isolate(read_excel(input$Base$datapath)[,c("Efectivo","Agrupación","CONTRATO","NOMBRE_CLIENTE","IDENT_CLIENTE","DIRECCION","CICLO","CIUDAD_DPTO",#input$Base$datapath
    #                                                "SERVICIO_1","SERVICIO_2","SERVICIO_3","ENTREGA_CAMBIOS","MOTIVO","RESULTADO_SS","ESTADO_ACCION_TO",
    #                                                "ESTADO_ACCION_TV","ESTADO_ACCION_BA","ID_LLAMADA","FE_REGISTRO","USER_REG","ID_REGISTRO","BENEF_TO",
    #                                                "RECONFIG_TO","DESCUENTO_TO","BENEF_TV","RECONFIG_TV","DESCUENTO_TV","BENEF_BA","RECONFIG_BA",
    #                                                "DESCUENTO_BA","ORIGEN","NOMBRE_CDVS","ESTADO_ACCION_CP","TACTICO_CP","OBSERVACIONES")]#, header = TRUE, stringsAsFactors=F, na.strings=c("NA","NULL","null","#N/A",""), sep=";", encoding = "ISO-8859-1"
    #
    # )
    #df<-read_excel("./IdentificacindeCasos (20).xlsx")
    df<-read_excel("IdentificacindeCasos (20).xlsx")
    colnames(df)[colnames(df)=="Ha estado fuera del país en los últimos 14 días o en contacto con alguien que estuvo fuera del país?"] <- "Contacto"
    colnames(df)[colnames(df)=="Tos?"] <- "Sintoma_Tos"
    colnames(df)[colnames(df)=="Fiebre más de 38°?"] <- "Fiebre_mayor_38"
    colnames(df)[colnames(df)=="Dificultad respiratoria?"] <- "Dificul_respira"
    colnames(df)[colnames(df)=="Explicación de la situación"] <- "Descripcion"
    colnames(df)[colnames(df)=="Canal de Contacto"] <- "canal"
    colnames(df)[colnames(df)=="Cuál país?"] <- "pais_contacto"

    #for (i in 13:15) {df[,i]<-factor(df[,i], levels = c("NO", "SI"))}
    df$Sintoma_Tos<-factor(df$Sintoma_Tos, levels = c("NO", "SI"), ordered =T)
    df$Fiebre_mayor_38<-factor(df$Fiebre_mayor_38, levels = c("NO", "SI"), ordered =T)
    df$Dificul_respira<-factor(df$Dificul_respira, levels = c("NO", "SI"), ordered =T)
    df$Contacto<-factor(df$Contacto, levels = c("NO", "SI"), ordered =T)
    df$Género<-factor(df$Género, levels = c("M","F"))
    df$`Date Created`<- as.POSIXct(df$`Date Created`,"%Y-%m-%d %H:%M:%S",tz="America/Bogota")
    df$Fecha<-as.Date(df$`Date Created`)
    df$mes <- months(as.Date(df$`Date Created`)) %>%
      str_to_title(.)
    #New_MedalliaH$mes1<-month.abb[New_MedalliaH$mes1]
    df$mes1<-substr(df$mes,1,3)
    df$mes1 <- factor(df$mes1, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
    df$Dia <- strftime(df$`Date Created`, format = "%d")
    df$Mes_dia <- paste(df$mes1,df$Dia)

    df$Sintoma_Tos_d<-as.numeric(df$Sintoma_Tos)-1
    df$Fiebre_mayor_38_d<-as.numeric(df$Fiebre_mayor_38)-1
    df$Dificul_respira_d<-as.numeric(df$Dificul_respira)-1
    df$Contacto_d<-as.numeric(df$Contacto)-1
    df$paisc <- convert_text2(df$pais_contacto) %in% c("italia", "china", "francia",
                                                       "eeuu", "estados unidos", "espana")

    df <- df %>%
      dplyr::mutate(prob = rowSums(.[,23:26], na.rm = T),
                    prop = ifelse(prob == 4 , "Riesgo alto covid19",
                                  ifelse(prob == 3 & Contacto == "SI" & paisc == T, "Riesgo alto covid19",
                                         ifelse(prob == 3 & Contacto == "SI" & paisc == F, "Riesgo medio covid19",
                                                ifelse(prob == 3 & Contacto == "NO", "Riesgo medio covid19",
                                                       ifelse(prob == 2 & Contacto == "SI", "Riesgo medio covid19", #"HOLA")))))
                                                              ifelse(prob == 2 & Contacto == "NO", "Riesgo bajo covid19",
                                                                     ifelse(prob <= 1, "Riesgo bajo covid19", NA))))))))

    df<-df[,c(1:(ncol(df)-7),ncol(df))]
    # df<-df%>% tidyr::separate(`CIUDAD_DPTO`, c("MUNICIPIO", "DEPARTAMENTO"), "-", extra = "merge")%>%
    #   dplyr::mutate(Portafolio=apply(.[,10:12], 1, function(x) toString(na.omit(x))))%>%
    #   dplyr::mutate(MUNICIPIO = replace_na(Hmisc::capitalize(convert_text(MUNICIPIO)), ""),
    #                 DEPARTAMENTO = replace_na(Hmisc::capitalize(convert_text(DEPARTAMENTO)), ""),
    #                 C.DIRECCION = Convert_dir3(DIRECCION),
    #                 address = str_c(C.DIRECCION, MUNICIPIO, DEPARTAMENTO,"Colombia", sep = ", "),
    #                 ID_asdres = str_c(IDENT_CLIENTE,address, sep = "-"));df<- df[!duplicated(df$ID_asdres),];df$FE_REGISTRO<-as.Date(df$FE_REGISTRO, origin = "1899-12-30")
    #

    if (is.null(df) || nrow(df)>=60000)
      return(df=NULL)#df=NULL
    #df<-Data
    else
      return(df)
  })



  ####
  observeEvent(input$draw, {
    if(is.null(Base()) || nrow(Base()) >= 60000){
      output$ziptable <- DT::renderDataTable({NULL})
      output$map <- renderLeaflet({NULL})
      absolutePanel(NULL)
      output$progressBox2 <- renderInfoBox({NULL})
      output$histCentile<-plotly::renderPlotly({NULL})
      output$fig <- renderPlot({NULL})

      return( output$note <- renderText({
        paste("La Información montada supera los limites de geolocalización superior a 3000 registros, intenta con menos registros")
      }))
    }

    Base<-as.data.frame(Base())



    isolate({
      withProgress({
        setProgress(message = "Procesando mapa tenga paciencia...")

        output$note = renderText({NULL})

        zipdata <<- Geolocalizar(Base)#Base()
        zipdata <<- zipdata%>%
          subset(.,!is.na(lat))
        zipdata<-zipdata%>%
          dplyr::mutate(ID=paste(lat,lon,sep = "-"))%>%
          dplyr::group_by(ID)%>%#
          dplyr::mutate(n=n())%>%
          dplyr::ungroup() %>%
          dplyr::select(-ID)
        zipdata$zipcode <- seq(1,nrow(zipdata))
        zipdata$obs1<-convert_text2(zipdata$Descripcion)
        zipdata$obs1<-tm::removeWords(zipdata$obs1, words = c(stopwords$Palabras))
        zipdata$obs1<-qdapRegex::rm_white(zipdata$obs1)
        zipdata$obs1<-paste0(" ", zipdata$obs1)
        zipdata$obs1<-negation_word(zipdata$obs1)
        #zipdata$Portafolio<-iconv(zipdata$Portafolio, "UTF-8", sub="")
        #zipdata$MOTIVO<-iconv(zipdata$MOTIVO, "UTF-8", sub="")
        cleantable <<- zipdata %>%
          dplyr::select(
            Zipcode = zipcode,
            Codigo=`Entry #`,
            FE_REGISTRO=Fecha,
            Nombre=`Nombre completo`,
            `Cédula`,
            Género,
            Teléfono,
            Direccion = address,
            Descripcion,
            Contacto,
            pais_contacto,
            Sintoma_Tos,
            Fiebre_mayor_38,
            Dificul_respira,
            Otro_sintoma=`Otro síntoma? Cuál?`,
            Riego_Covid19=prop,
            canal,
            #ORIGEN,
            #Efectivo,
            #Agrupación,
            #Contrato=CONTRATO,
            #Cliente_ID=IDENT_CLIENTE,
            #Nombre_Cliente=NOMBRE_CLIENTE,
            #Dirección=C.DIRECCION,
            #Motivo=MOTIVO,
            #Portafolio,
            #Observaciones=OBSERVACIONES,
            #Barrio=barrio,
            City = `Lugar de residencia`,
            State = DEPARTAMENTO,
            #Departamento=DEPARTAMENTO,
            Lat = lat,
            Long = lon)
        cleantable <<- cleantable%>%
          subset(.,!is.na(Direccion))
        #zipdata$zipcode2 <- paste(zipdata$zipcode, , sep = "")
        #row.names(zipdata) <- zipdata$zipcode2
        #names(zipdata)
        #cleantable <- NULL
        #if(first)
        vec = c("none",unique(zipdata$prop))
        names(vec) = vec
        names(vec)[1] = "Seleccione el segmento"
        updateSelectInput(session, 'color', choices = vec, selected = 'none')

        # output$point = renderUI({
        #   motivos.names <- as.vector( unique(zipdata$MOTIVO) )
        #   motivos.names=c("none",motivos.names)
        #   names(motivos.names) = motivos.names
        #   names(motivos.names)[1] = "Todos los Motivos"
        #   selectInput("point","Motivos", choices=motivos.names, selected = 'none')
        # })

        output$dates = renderUI({
          start = min(zipdata$Fecha)
          end = max(zipdata$Fecha)

          dateRangeInput("dates", label = "Seleccione el rango de fecha", start = start, end = end,
                         min = min(zipdata$Fecha), max = max(zipdata$Fecha),language = "es", separator = " al ")

        })

        # output$origen = renderUI({
        #   Origen.names <- as.vector( unique(zipdata$ORIGEN) )%>% na.omit(.)
        #   selectInput("origen","Seleccione el origen", choices=c("All",Origen.names), selected = "All")#choices=c("All", filtro7), selected = "All"
        #
        # })

        # readCond = function(){
        #   zipdata
        #   #cleantable
        #   #read author, date, point, comment
        #   if(length(input$dates) != 0){
        #     zipdata <<- subset(zipdata, (FE_REGISTRO >= as.Date(input$dates[1]) & FE_REGISTRO <= as.Date(input$dates[2])) &
        #                           (ORIGEN == input$origen )
        #     )
        #     #Basegraf_Detract<<- subset(Basegraf, Contact.Center..LTR <= 6)
        #   }
        #   # else
        #   #   Basegraf<<- MedalliaH
        #   #
        #   # updateSummary()
        # }
        #

        output$filter = renderUI({
          #if(input$board != "none")
          checkboxInput('filter', "Filter", value = F)
        })

        output$saveData <- downloadHandler(

          filename = function() {
            paste("Datos_geolocalizados", "/_", Sys.Date(), '.xlsx', sep='')
          },
          content = function(file) {
            write_xlsx(zipdata, file)
          }
        )

        # observeEvent(c( input$dates, input$origen), {
        #
        #   data_0 <- reactive({
        #     if(length(input$dates) != 0)
        #       subset(zipdata, (FE_REGISTRO >= as.Date(input$dates[1]) & FE_REGISTRO <= as.Date(input$dates[2])) &
        #                (ORIGEN == input$origen ))
        #
        #   })
        #
        #   data_1<- reactive({
        #     if(length(input$dates) != 0)
        #       subset(cleantable, (FE_REGISTRO >= as.Date(input$dates[1]) & FE_REGISTRO <= as.Date(input$dates[2])) &
        #                (ORIGEN == input$origen ))
        #   })
        #
        # })



        # load data
        #observeEvent(c(input$dates, input$origen,input$color,input$point), {

        output$map <- renderLeaflet({NULL})
        #shiny::isolate({
        #withProgress({
        #setProgress(message = "Procesando mapa...")
        # data_0<<-as.data.frame(data_0())
        # data_1<<-as.data.frame(data_1())
        ## Interactive Map  reactivo ###########################################
        data_0 <- reactive({
          if(length(input$dates) != 0 )#&& input$origen!="All"

            subset(zipdata,(Fecha >= as.Date(input$dates[1]) & Fecha < as.Date(input$dates[2])+1) )#&#input$dates[1]input$dates[2](ORIGEN == "RETENCION NIVEL2" )
          #(ORIGEN == input$origen ))
          else
            zipdata

        })

        data_1<- reactive({
          if(length(input$dates) != 0 && input$origen!="All")
            subset(cleantable, (FE_REGISTRO >= as.Date(input$dates[1]) & FE_REGISTRO <as.Date(input$dates[2])+1))# &
          #(ORIGEN == input$origen ))

          else
            cleantable
        })



        data <- reactive({
          if(input$color!="none" )#&& input$point!="none"
            subset(data_0(), prop %in% input$color )#& MOTIVO %in% input$point

          # else if(input$color!="none" && input$point =="none")
          #   subset(data_0(), prop %in% input$color)
          else
            data_0()
        })

        data1<- reactive({
          if(input$color!="none" )#&& input$point!="none"
            subset(data_1(), Riego_Covid19 %in% input$color )#& Motivo %in% input$point

          # else if(input$color!="none" && input$point =="none")
          #   subset(data_1(), Efectivo %in% input$color)
          else
            data_1()
        })

        ####se crea data espacial barrios###
        espacial <-data()%>% dplyr::select(lon, lat)%>%
          na.omit(.)
        colnames(espacial) <- c("Longitude", "Latitude")
        coordinates(espacial) <- ~ Longitude + Latitude
        # class(espacial)
        # class(Barrios)
        #proj4string(p)
        #proj4string(x)
        crs(espacial) <- crs(Barrios)
        res <- over(espacial, Barrios)
        #class(res)#data.frame
        # Number of points within each "NOMBRE" attribute
        cnts <- data.frame(table(res$Barrio))
        names(cnts)[1] <- 'Barrio'

        # Join counts back to the polygons based on NOMBRE
        xFreq <- merge(Barrios, cnts, by='Barrio')
        #class(xFreq)
        # Fill 0 where NA, these regions had no points overlapping
        xFreq$Freq[is.na(xFreq$Freq)] <- 0
        ###espacial1
        # espacial1 <-data()%>% dplyr::select(lon, lat)%>%
        #   na.omit(.)
        # colnames(espacial1) <- c("Longitude", "Latitude")
        # coordinates(espacial1) <- ~ Longitude + Latitude
        # # class(espacial)
        # # class(Barrios)
        # #proj4string(p)
        # #proj4string(x)
        # #crs(espacial1) <- crs(Cali)
        # res1 <- over(espacial1, Cali)
        # #class(res)#data.frame
        # # Number of points within each "NOMBRE" attribute
        # cnts <- data.frame(table(res1$Barrio))
        # names(cnts)[1] <- 'Barrio'
        #
        # # Join counts back to the polygons based on NOMBRE
        # caliFreq <- merge(Cali, cnts, by='Barrio')
        # #class(xFreq)
        # # Fill 0 where NA, these regions had no points overlapping
        # caliFreq$Freq[is.na(caliFreq$Freq)] <- 0
        # data <- reactive({
        #   if(input$color!="none" && input$point!="none")
        #     subset(zipdata, Efectivo %in% input$color & MOTIVO %in% input$point)
        #
        #   else if(input$color!="none" && input$point =="none")
        #     subset(zipdata, Efectivo %in% input$color)
        #   else
        #     zipdata
        # })
        #
        # data1<- reactive({
        #   if(input$color!="none" && input$point!="none")
        #     subset(cleantable, Efectivo %in% input$color & Motivo %in% input$point)
        #
        #   else if(input$color!="none" && input$point =="none")
        #     subset(cleantable, Efectivo %in% input$color)
        #   else
        #     cleantable
        # })
        #

        # A reactive expression that returns the set of zips that are
        # in bounds right now
        zipsInBounds <- reactive({
          if (is.null(input$map_bounds))
            return(data()[FALSE,])
          bounds <- input$map_bounds
          latRng <- range(bounds$north, bounds$south)
          lngRng <- range(bounds$east, bounds$west)

          subset(data(),
                 lat >= latRng[1] & lat <= latRng[2] &
                   lon >= lngRng[1] & lon <= lngRng[2])
        })

        output$progressBox2 <- renderInfoBox({
          if (nrow(zipsInBounds()) == 0)
            stop("Cargando información...")#return(NULL)
          titulo <- "Estado: x1"
          #titulo <- sub("x", iconv(input$point, "UTF-8", sub=""),titulo)
          titulo <- sub("x1", iconv(input$color, "UTF-8", sub=""),titulo)
          infoBox(nrow(zipsInBounds()),titulo , icon = shiny::icon("user-tie"),
                  color = "aqua", fill = T,width = 12
          )
          # infoBox(
          #   "Número de clientes", nrow(data()), icon = icon("list"),
          #   color = "purple", fill = TRUE
          # )
        })
        ####cambio de nube a
        observe({#
          if (nrow(zipsInBounds()) == 0)
            return()#NULL
          output$fig <<- getFig(zipsInBounds(), plotType =input$plot, param = input$param)
        })
        # Create the map
        output$map <- renderLeaflet({
          leaflet() %>%
            addTiles(
              urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
              attribution = 'Desarrollado por <a href="http://www.emtelco.com.co/">Emtelco</a>'
            ) %>%
            setView(lng = -72, lat = 4, zoom=6)
        })


        ####Gráfico1 descriptivo motivos####
        output$histCentile<-plotly::renderPlotly({
          # If no zipcodes are in view, don't plot
          if (nrow(zipsInBounds()) == 0)
            return(NULL)#NULLstop("La selección no contiene información")
          tactico<-zipsInBounds()%>%  #zipsInBounds()
            dplyr::group_by(prop) %>%
            dplyr::summarise(frec = n()) %>%
            dplyr::arrange(., desc(frec));tactico$rank <- as.numeric(row.names(tactico));tactico <- tactico[complete.cases(tactico),]
          tactico$frec <- round(prop.table(tactico$frec)*100,2)
          tactico <-tactico %>%
            subset(., rank <= 10) %>%
            dplyr::select(prop,frec);tactico <- data.frame(tactico)

          if(nrow(tactico) == 0)
            stop("Cargando información...")#"No se generaron Motivos de retiro"
          p <- plot_ly(tactico, x = ~frec , y = ~reorder(prop, frec), name = 'Riesgo',
                       type = 'bar', orientation = 'h',
                       marker = list(color = 'rgb(235, 192, 0)',
                                     line = list(color = 'rgb(235, 192, 0.5)', width = 1))) %>%
            layout(yaxis = list(title = "",size = 4,showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1)),
                   xaxis = list(title = "",size = 4,zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
            add_annotations(xref = 'x', yref = 'y1',
                            x = tactico$frec,  y = tactico$prop,
                            text = paste(tactico$frec, '%'),
                            font = list(family = 'Arial', size = 10, color = 'rgb(0, 0, 0)'),
                            showarrow = FALSE)
          p

          # subplot(p,nrows = 1,shareX = TRUE)%>%
          #   layout(legend = list(x = 0.029, y = 1.15,
          #                        font = list(size = 8) ,font = list(family = 'Arial', size = 8, color = 'rgb(115,114,119)'),
          #                        showarrow = FALSE),
          #          margin = list(l = 200, r = 20, t = 50, b = 20))
        })
        ####Función Graficos####





        #
        # output$scatterCollegeIncome <- renderPlot({
        #   # If no zipcodes are in view, don't plot
        #   if (nrow(zipsInBounds()) == 0)
        #     return(NULL)
        #
        #   print(xyplot(lon ~ lat, data = zipsInBounds(), xlim = range(allzips$lat), ylim = range(allzips$lon)))
        # })



        # This observer is responsible for maintaining the circles and legend,
        # according to the variables the user has chosen to map to color and size.
        observe({
          # colorBy <- input$point
          # #sizeBy <- input$size
          #
          # if (colorBy == "superzip") {
          #   # Color and palette are treated specially in the "superzip" case, because
          #   # the values are categorical instead of continuous.
          #   colorData <- ifelse(data()$centile >= (100 - input$threshold), "yes", "no")
          #   pal <- colorFactor("viridis", colorData)
          # } else {
          #   colorData <- data()[[colorBy]]
          #   pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
          # }
          e<-min(data()$n)
          f<-max(data()$n)
          g<-f/5
          mybins=seq(e,f+g, by=g)

          mypalette = colorBin( palette=c("#0067B1","#50BDEB","#D4D800","#D4D800","#EBC000","#F06A06"),
                                domain=data()$n, na.color="transparent", bins=mybins)

          leafletProxy("map", data = data()) %>%
            clearShapes() %>%
            clearMarkers()  %>%
            clearMarkerClusters() %>%
            clearImages() %>%
            addTiles(group = "Iliminado",
                     options = tileOptions(opacity = 1)) %>%
            addProviderTiles(providers$Stamen.Toner, group = "Opaco",
                             options = providerTileOptions(opacity = 1)) %>%
            #clearShapes() %>%
            leaflet.extras:::addHeatmap( ~ lon, ~ lat,
                                         # blur = 35,
                                         # cellSize = 1,
                                         # radius = 4.5,
                                         max=.6,
                                         blur = 60,
                                         group = "Calor") %>%

            addCircleMarkers(~lon, ~lat, fillColor = ~mypalette(n), fillOpacity = 0.4, color="white", radius=~(n*10/(f))*3, stroke=F,
                             #label = mytext,
                             labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "12px", direction = "auto"),
                             popup = paste("<B>Nombre:</B> ", data()$`Nombre completo`, "<br/>",
                                           "<B>CC:</B> ", data()$`Cédula`, "<br/>",
                                           "<B>Género:</B> ", data()$`Género`,"<br/>",
                                           "<B>Teléfono:</B> ", data()$`Teléfono`, "<br/>",
                                           "<B>Contagio:</B>","</B> ",  data()$prop, "<br/>",
                                           #"<B>Estado:</B>","</B> ",  data()$Efectivo, "<br/>",
                                           "<B>Dirección:</B>","</B> ",  data()$address, "<br/>",
                                           #"<B>Barrio:</B> ", data()$barrio, "<br/>",
                                           "<B>Departamento:</B> ", data()$DEPARTAMENTO, "<br/>",
                                           #"<B>Municipio:</B> ", data()$MUNICIPIO, "<br/>"
                                           sep="") ,#%>%
                             #lapply(htmltools::HTML)
                             group = "Población") %>%
            addLegend("bottomleft", pal=mypalette, values=n, title="Frecuecia de\n casos por dirección",
                      layerId="colorLegend") %>%
            #addLegend( pal=mypalette, values= ~ n, opacity=0.9, title = "Frecuecia de\n clientes por dirección", position = "bottomright" )%>%
            #, label = content
            # Mapa de calor


            addCircleMarkers(~lon, ~lat,
                             group = "Centros",
                             radius = 10,
                             clusterOptions = markerClusterOptions(),
                             popup = paste("<B>Nombre:</B> ", data()$`Nombre completo`, "<br/>",
                                           "<B>CC:</B> ", data()$`Cédula`, "<br/>",
                                           "<B>Género:</B> ", data()$`Género`,"<br/>",
                                           "<B>Teléfono:</B> ", data()$`Teléfono`, "<br/>",
                                           "<B>Contagio:</B>","</B> ",  data()$prop, "<br/>",
                                           #"<B>Estado:</B>","</B> ",  data()$Efectivo, "<br/>",
                                           "<B>Dirección:</B>","</B> ",  data()$address, "<br/>",
                                           #"<B>Barrio:</B> ", data()$barrio, "<br/>",
                                           "<B>Departamento:</B> ", data()$DEPARTAMENTO, "<br/>",
                                           #"<B>Municipio:</B> ", data()$MUNICIPIO, "<br/>"
                                           sep="") ,#%>%
                             stroke = F
            ) %>%
            # addLabelOnlyMarkers(data = data(),~longitude, ~latitude,
            #                     clusterOptions = markerClusterOptions()
            # ) %>%
            addLayersControl(baseGroups = c("Iliminado", "Opaco"),
                             overlayGroups = c("Población","Centros", "Calor","barrios"),#"Medellín", "Envigado","Bogotá","Cali"
                             options = layersControlOptions(collapsed = F, autoZIndex = F, position = "topleft")
            ) %>%
            hideGroup(c("Calor","barrios")) %>%#"Medellín", "Envigado","Bogotá","Cali"
            # addLegend("bottomleft", pal=mypalette, values=n, title=colorBy,
            #            layerId="colorLegend")%>%
            leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE) %>%
            leaflet.extras::addDrawToolbar(editOptions=leaflet.extras::editToolbarOptions(selectedPathOptions=leaflet.extras::selectedPathOptions())) %>%
            leaflet.extras::addResetMapButton() %>%
            # leaflet.extras::addSearchFeatures(
            #   targetGroups  = "barrios",
            #   options = searchFeaturesOptions(zoom=16, openPopup=TRUE, firstTipSubmit = TRUE,
            #                                   autoCollapse = TRUE, hideMarkerOnCollapse = TRUE)) %>%
            addPolygons(data = xFreq,
                        fillColor = "green",
                        # highlightOptions = highlightOptions(
                        #   fillOpacity=1, weight=2, opacity=1, color='#ff0000',
                        #   bringToFront=TRUE, sendToBack = TRUE),
                        highlight = highlightOptions(weight = 5,
                                                     color = "red",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label=~iconv(Barrio, "UTF8"),
                        popup = paste("<B>Barrio:</B> ", iconv(xFreq$Barrio, "UTF8"), "<br/>",
                                      "<B>Cantidad:</B> ", xFreq$Freq, "<br/>"),
                        group = "barrios") #%>%
          # addPolygons(data = caliFreq,
          #             fillColor = "green",
          #             highlight = highlightOptions(weight = 5,
          #                                          color = "red",
          #                                          fillOpacity = 0.7,
          #                                          bringToFront = TRUE),
          #             label=~iconv(Barrio, "UTF8"),
          #             popup = paste("<B>Barrio:</B> ", iconv(caliFreq$Barrio, "UTF8"), "<br/>",
          #                           "<B>Cantidad:</B> ", caliFreq$Freq, "<br/>"),
          #             group = "Cali")
          # addLayersControl(overlayGroups = c("barrios", "Población"),
          #                  options = layersControlOptions(collapsed = FALSE))
          # # addSearchFeatures(
          # targetGroups = 'Población',
          # options = searchFeaturesOptions(
          #   propertyName='MUNICIPIO', zoom=2, openPopup = TRUE, firstTipSubmit = TRUE,
          #   autoCollapse = FALSE, hideMarkerOnCollapse = TRUE ))%>%
          # addPolygons(data=xFreq,
          #             fillColor = "green",
          #             highlight = highlightOptions(weight = 5,
          #                                          color = "red",
          #                                          fillOpacity = 0.7,
          #                                          bringToFront = TRUE),
          #             label=~iconv(NOMBRE, "UTF-8"),
          #             group = "Medellín") %>%
          # addPolygons(data=envigado,
          #             fillColor = "green",
          #             highlight = highlightOptions(weight = 5,
          #                                          color = "red",
          #                                          fillOpacity = 0.7,
          #                                          bringToFront = TRUE),
          #             label=~nombarrio,
          #             group = "Envigado") %>%
          #
          # addPolygons(data=Bogota,
          #             fillColor = "green",
          #             highlight = highlightOptions(weight = 5,
          #                                          color = "red",
          #                                          fillOpacity = 0.7,
          #                                          bringToFront = TRUE),
          #             label=~NOMBRE_COM,
          #             group = "Bogotá")%>%
          #
          # addPolygons(data=Cali,
          #             fillColor = "green",
          #             highlight = highlightOptions(weight = 5,
          #                                          color = "red",
          #                                          fillOpacity = 0.7,
          #                                          bringToFront = TRUE),
          #             label=~barrio,
          #             group = "Cali")
          #%>%
          # addLayersControl(overlayGroups = c("Medellín", "Envigado"),
          #                  options = layersControlOptions(collapsed = FALSE))
        })

        # Show a popup at the given location
        showZipcodePopup <- function(zipcode, lat, lng) {
          selectedZip <- zipdata[zipdata$zipcode == zipcode,]
          content <- as.character(tagList(
            tags$h4("Presiona el círculo", as.integer(selectedZip$centile)),
            tags$strong(HTML(sprintf("%s, %s %s",
                                     selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
            )))#, #tags$br(),
            #sprintf("Median household income: %s", dollar(selectedZip$college * 1000)), tags$br(),
            # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$Nombre)), tags$br(),
            #sprintf("Nombre: %s", selectedZip$Nombre),tags$br()
            # sprintf("cc: %s", selectedZip$cc),tags$br(),
            # sprintf("Portafolio: %s", selectedZip$Portafolio),
            # sprintf("Motivo: %s", selectedZip$Motivo),tags$br(),
            # sprintf("Táctico: %s", selectedZip$Tactico)
          ))
          leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
        }

        # When map is clicked, show a popup with city info
        observe({
          leafletProxy("map") %>% clearPopups()
          event <- input$map_shape_click
          if (is.null(event))
            return()

          isolate({
            showZipcodePopup(event$id, event$lat, event$lng)
          })
        })


        ## Data Explorer ###########################################

        # observe({
        #   cities <- if (is.null(input$states)) character(0) else {
        #     filter(data1(), State %in% input$states) %>%
        #       `$`('City') %>%
        #       unique() %>%
        #       sort()
        #   }
        #   stillSelected <- isolate(input$cities[input$cities %in% cities])
        #   updateSelectInput(session, "cities", choices = cities,
        #                     selected = stillSelected)
        # })
        #
        # observe({
        #   zipcodes <- if (is.null(input$states)) character(0) else {
        #     data1() %>%
        #       filter(State %in% input$states,
        #              is.null(input$cities) | City %in% input$cities) %>%
        #       `$`('Zipcode') %>%
        #       unique() %>%
        #       sort()
        #   }
        #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
        #   updateSelectInput(session, "zipcodes", choices = zipcodes,
        #                     selected = stillSelected)
        # })

        observe({
          if (is.null(input$goto))
            return()
          isolate({
            map <- leafletProxy("map")
            map %>% clearPopups()
            dist <- 0.005
            zip <- input$goto$zip
            lat <- input$goto$lat
            lng <- input$goto$lng
            showZipcodePopup(zip, lat, lng)
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
          })
        })

        output$ziptable <- DT::renderDataTable({
          #df<- data1()
          df <- data1() %>%
            #   # filter(
            #   #   Score >= input$minScore,
            #   #   Score <= input$maxScore,
            #   #   is.null(input$states) | State %in% input$states,
            #   #   is.null(input$cities) | City %in% input$cities,
            #   #   is.null(input$zipcodes) | Zipcode %in% input$zipcodes
            #   # ) %>%
            mutate(Zoom = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
          action <- DT::dataTableAjax(session, df);df$Lat<-as.character(df$Lat);df$Long<-as.character(df$Long)

          DT::datatable(df, filter="top", selection="single",
                        extensions = c('ColReorder', 'Buttons'),
                        options = list(
                          searchHighlight = TRUE,
                          dom = 'tB',
                          colReorder = list(realtime = FALSE),
                          autoWidth = TRUE,
                          scrollY = 500, scrollX = 2500, scroller = TRUE,
                          pageLength = nrow(df),
                          columnDefs = list(list(width = '20px', targets = c(9)),list(visible=FALSE, targets=c(1,20,21))),
                          buttons = list('colvis'),
                          #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          ajax = list(url = action)), escape = FALSE)

          # # download the filtered data
          #
          # output$x4 = downloadHandler(filename = function() {
          #   paste("Datos_filtrados", '.xlsx', sep='')
          # }, content = function(file) {
          #   s1 = input$x2_rows_all
          #   writexl::write_xlsx(df[s1, , drop = FALSE], file)
          # })


        })
        #})
        #})
        # })
        #
      })
    })
  })
})
