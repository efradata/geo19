
###################
# Functions
###################

# stopwords <- read.csv2("./Stopwords.csv",stringsAsFactors=F,header=TRUE, sep=";",
#                        na.strings=c("NA","NULL","null","#N/A","", " "))
stopwords <- read.csv2("Stopwords.csv",stringsAsFactors=F,header=TRUE, sep=";",
                       na.strings=c("NA","NULL","null","#N/A","", " "))

count_bigrams <- function(data) {
  data %>%
    tidytext::unnest_tokens(bigram, Base, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!word1 %in% stopwords$`Palabras`,
                  !word2 %in% stopwords$`Palabras`) %>%
    dplyr::count(word1, word2, sort = TRUE)
}


visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    igraph::graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 5) +
    theme_void()
}

convert_text<- function(x){
  x <- tolower(x)
  x <- chartr("áéíóú","aeiou", x)
  x <- chartr("ñ", "n", x)
  x <- chartr("ü", "u", x)
  x <- gsub("user", "", x)
  # x <- gsub("bagre", "el bagre", x)
  x <- gsub("\\.", "", x)
  # x <- gsub("[[:punct:]]", "", x)
  # x <- gsub("[[:digit:]]+", "", x)
  # x <- gsub("(.*):","",x)
  x <- qdapRegex::rm_white(x)
  x
}

convert_text2 <- function(x){
  x <- tolower(x)
  x <- chartr("áéíóú", "aeiou", x)
  x <- chartr("ñ", "n", x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:digit:]]+", "", x)
  #x <- gsub("(.*):","",x)
  x <- qdapRegex::rm_white(x)
  x <- gsub("“|–|…","",x)
  x <- gsub("\r?\n|\r", " ", x)
  x
}
visualize_radar <- function(radar) {
  c <- grDevices::col2rgb(c("#EBC000", "#00358E","#19A986", "#50BDEB"))
  radar <-reshape2::melt(radar,id.vars=c("Topico" ,"Supervisor"))
  radar <-reshape2::dcast(radar, variable ~ Topico + Supervisor, value.var = "value")
  #%>% chartJSRadar(showToolTipLabel = TRUE,
  # main = "NRC Years Radar")
  radarchart::chartJSRadar(radar,  showToolTipLabel=TRUE,colMatrix=c,polyAlpha=0.0,scaleStartValue=min(radar$`experiencia llamada`)-0.2,maxScale = max(radar$`experiencia llamada`)+0.2)
}

cambiar_nombres <- function(x){
  x <- gsub("MUY NEGATIVO","Muy_Negativo", x)
  x <- gsub("NEGATIVO","Negativo", x)
  x <- gsub("NEUTRO","Neutro", x)
  x <- gsub("MUY POSITIVO","Muy_Positivo", x)
  x <- gsub("POSITIVO","Positivo", x)
  x
}

getData = function(file){
  
  if(file == "Servicio"){
    data = readRDS(paste("./Data Visualizar/", file, ".rds", sep = ""))%>%
      dplyr::select(DIRECCION=address,ESTADO=CONCEPTO_ID,CLIENTE_ID,NOMBRE_CLIENTE=CLIENTE,DEPARTAMENTO,MUNICIPIO=NOMBRE_BARRIO,lon,lat)%>%
      dplyr::mutate(ID = paste(lon,lat,sep = "")) %>% 
      dplyr::group_by(ID) %>%
      dplyr::mutate(n=n())
    
  }
  else if(file == "Ventas"){
    data = readRDS(paste("./Data Visualizar/", file, ".rds", sep = ""))%>%
      dplyr::select(DIRECCION=address,ESTADO,PRODUCTO,CLIENTE_ID,NOMBRE_CLIENTE,DEPARTAMENTO,MUNICIPIO,lon,lat)%>%
      unique(.) %>% 
      dplyr::group_by(CLIENTE_ID,DIRECCION,ESTADO) %>% 
      dplyr::mutate(Portafolio =  paste(PRODUCTO, collapse = ", "))%>%
      dplyr::select(-PRODUCTO)%>% 
      unique(.)%>%
      dplyr::mutate(ID = paste(lon,lat,sep = "")) %>% 
      dplyr::group_by(ID) %>%
      dplyr::mutate(n=n())
  }
  else if(file == "intalaciones"){
    data = readRDS(paste("./Data Visualizar/", file, ".rds", sep = ""))%>%
      dplyr::select(DIRECCION=address,ESTADO=`Detalle Codigo Incompleto`,Estado,PRODUCTO=UNEServices_Product_Name,CLIENTE_ID=`Id Tarea`,NOMBRE_CLIENTE=Task_EngineerName,DEPARTAMENTO,MUNICIPIO,lon,lat) %>%
      unique(.) %>% 
      dplyr::group_by(CLIENTE_ID,DIRECCION) %>% 
      #dplyr::mutate(Portafolio =  paste(PRODUCTO, collapse = ", "))%>%
      #dplyr::select(-PRODUCTO)%>% 
      #unique(.)%>%
      dplyr::mutate(ID = paste(lon,lat,sep = "")) %>% 
      dplyr::group_by(ID) %>%
      dplyr::mutate(n=n())
  }
  
  return (data)
}

negation_word <- function(x){
  x <- gsub(" no ", " no_", x)
  x
}

getFig = function(data, plotType,param){
  
  ########end processing
  fig = NULL
  
  if(plotType == "bCloud"){
    fig = renderPlot({
      
      Base <- data.frame(data$obs1)
      names(Base)<-c("Base")
      #Base$Base <- gsub("(.*):","",Base$Base) #Se elimina lo que esta antes de :
      #Base$Base <- convert_text(Base$Base) #Se eliminan números, signos de puntuación, tildes, etc...
      Base$Base <- as.character(Base$Base)
      Base   <- as.character(unlist(Base))
      Base <- data.frame(Base)
      
      
      Base$Base<- qdapRegex::rm_white(Base$Base) #Se eliminan multiples espacios en blanco
      #detach("package:qdapRegex", unload=TRUE)
      Base$Base <- ifelse(Base$Base == "", NA, Base$Base)
      Base <- Base[complete.cases(Base), ] #Se eliminan filas en blanco
      
      Base <- data.frame(Base)
      Base$Base <- as.character(Base$Base)
      Base$Base   <- as.character(unlist(Base$Base))
      
      #PASO 5: Se crea la nube de palabras
      docs <- tm::Corpus(VectorSource(Base))
      
      #Contruimos la matrix para la nube y tabla de frecuencias
      
      Plano <- TermDocumentMatrix(docs)
      m <- as.matrix(Plano)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      #head(d, 100)
      #str(d)
      
      Base <- data.frame(Base)
      Base$Base <- as.character(Base$Base)
      
      title_frame <- Base
      title_bigrams <- title_frame %>%
        count_bigrams()
      
      if(nrow(title_bigrams) == 0 || max(title_bigrams$n) < param)
        stop("Es una opción demasiado grande, intente con una frecuencia menor de pares.")
      
      title_bigrams %>%
        dplyr::filter(n >= param ,
                      !str_detect(word1, "\\d"),
                      !str_detect(word2, "\\d")
        )%>%
        visualize_bigrams()
      
      
    })
  }
  
  
  
  else if(plotType == "tCloud"){
    fig = renderPlot({
      wc_corpus_clean <- tm::Corpus(VectorSource(data$obs1))
      wc_corpus_clean <- tm::tm_map(wc_corpus_clean, removeWords, stopwords$`Palabras`)
      
      bigramas <- tau::textcnt(wc_corpus_clean, n = 2, method = "string")
      bigramas <- bigramas[order(bigramas, decreasing = TRUE)]
      
      
      frases<-as.data.frame(bigramas)
      frases$woirds<-row.names(frases)
      colnames(frases)<-c("freq", "word")
      frases<-frases%>%
        filter(freq >= param)
      
      if(nrow(frases) == 0 || max(frases$freq) < param)
        stop("Es una opción demasiado grande, intente con una frecuencia menor")
      
      par(mar = rep(0, 4))
      wordcloud(frases$word,frases$freq,scale=c(4,.5), max.words = 300, 
                colors = c("#EBC000", "#00358E", "#D4D800", "#19A986", "#50BDEB","#0067B1", "#A1C1D5", "#737277"),random.order = FALSE, rot.per = .30)
      
      # 
      # 
      # wordcloud(wc_corpus_clean$word,wc_corpus_clean$freq,
      #           scale=c(4,.5),
      #           min.freq = input$freq,
      #           colors = c("#EBC000", "#00358E", "#D4D800", "#19A986", "#50BDEB","#0067B1", "#A1C1D5", "#737277"),    random.order = FALSE, rot.per = .30)
      # 
    })
  }
  
  return (fig)
  
  
}
Convert_dirna <- function(text){
  ttx <- gsub("^\\s|\\s$", "", text)
  ttx <- tolower(ttx)
  ttx <- gsub("rural_[[:digit:]]+_", "", ttx)
  ttx <- gsub(".diag |.avda |.cl |.cr |.circ |.tran ", " ",ttx)
  ttx <- gsub("\\(.+\\)", "", ttx)
  ttx <- gsub("([0-9])(\\s)([aA-zZ])", "\\1\\3", ttx) #quita el espacio entre el numero y la letra siguiente
  ttx <- gsub(" -", " ", ttx)
  ttx <- gsub("sur", " sur", ttx)
  ttx <- gsub("este", " este", ttx)
  ttx <- gsub("^\\s|\\s$", "", ttx)
  ttx <- gsub("\\s+", " ", ttx)
  ttx <- gsub("avda", "Av.", ttx)
  ttx <- gsub("avda", "Av.", ttx)
  ttx <- gsub("diagonal", "Dg.", ttx)
  ttx <- gsub(" diagonal", "Dg.", ttx)
  ttx <- gsub("diag", "Dg.", ttx)
  ttx <- gsub("tran", "Tv.", ttx)
  ttx <- gsub("cll", "Cl.", ttx)
  ttx <- gsub("barrio:", "", ttx)
  ttx <- gsub("barrio", "", ttx)
  ttx <- gsub("iatagui", "", ttx)
  ttx <- gsub("poblado", "",ttx)
  ttx <- gsub("niquia", "", ttx)
  ttx <- gsub("calle", "Cl.", ttx)
  ttx <- gsub("medelin", "medellin", ttx)
  ttx <- gsub("direccion:", "", ttx)
  ttx <- gsub("numero", "", ttx)
  ttx <- gsub("rio negro", "rionegro", ttx)
  ttx <- gsub("cl", "Cl.", ttx)
  ttx <- gsub("cra", "Cra.", ttx)
  ttx <- gsub("cr", "Cra.", ttx)
  ttx <- gsub("kr", "Cra.", ttx)
  ttx <- gsub("circular", "Cq.", ttx)
  ttx <- gsub("circ", "Cq.", ttx)
  ttx <- gsub("\\pi.*|\\int.*|\\ ed.*|\\cas.*", "", ttx)#\\in.*|
  #ttx <- gsub("to", " to", ttx)
  ttx <- gsub("ap", " ap", ttx)
  ttx <- gsub("\\ ap.*", "", ttx)#\\ to.*|
  #ttx <- gsub("bl", " bl", ttx)
  ttx <- gsub("\\ blo.*", "", ttx)
  ttx <-gsub("\\#", "",ttx)
  ttx <-gsub("\\-", " ",ttx)
  ttx <-gsub("\\,", " ",ttx)
  ttx <- qdapRegex::rm_white(ttx)
  ttx
}
####corrector Dirección
Convert_dir3 <- function(text){
  ttx <- gsub("^\\s|\\s$", "", text)
  ttx <- tolower(ttx)
  ttx <- gsub("rural_[[:digit:]]+_", "", ttx)
  ttx <- gsub(".diag |.avda |.cl |.cr |.circ |.tran ", " ", ttx)
  ttx <- gsub("\\(.+\\)", "", ttx)
  ttx <- gsub("([0-9])(\\s)([aA-zZ])", "\\1\\3", ttx) #quita el espacio entre el numero y la letra siguiente
  ttx <- gsub(" -", " ", ttx)
  ttx <- gsub("sur", " sur", ttx)
  ttx <- gsub("este", " este", ttx)
  ttx <- gsub("^\\s|\\s$", "", ttx)
  ttx <- gsub("\\s+", " ", ttx)
  ttx <- gsub("avda", "Av.", ttx)
  ttx <- gsub("avda", "Av.", ttx)
  ttx <- gsub("diagonal", "Dg.", ttx)
  ttx <- gsub(" diagonal", "Dg.", ttx)
  ttx <- gsub("diag", "Dg.", ttx)
  ttx <- gsub("tran", "Tv.", ttx)
  ttx <- gsub("cll", "Cl.", ttx)
  ttx <- gsub("barrio:", "", ttx)
  ttx <- gsub("niquia", "", ttx)
  ttx <- gsub("medellin", "", ttx)
  ttx <- gsub("barrio", "", ttx)
  ttx <- gsub("numero", "", ttx)
  ttx <- gsub("iatagui", "", ttx)
  ttx <- gsub("poblado", "",ttx)
  ttx <- gsub("calle", "Cl.", ttx)
  ttx <- gsub("medelin", "medellin", ttx)
  ttx <- gsub("direccion:", "", ttx)
  ttx <- gsub("rio negro", "rionegro", ttx)
  ttx <- gsub("cl", "Cl.", ttx)
  ttx <- gsub("cra", "Cra.", ttx)
  ttx <- gsub("cr", "Cra.", ttx)
  ttx <- gsub("kr", "Cra.", ttx)
  ttx <- gsub("circular", "Cq.", ttx)
  ttx <- gsub("circ", "Cq.", ttx)
  ttx <- gsub("\\pi.*|\\int.*|\\ ed.*|\\cas.*", "", ttx)#\\in.*|\\barr.*
  ttx <- gsub("to", " to", ttx)
  ttx <- gsub("ap", " ap", ttx)
  ttx <- gsub("\\ ap.*", "", ttx)#\\ to.*|
  ttx <- gsub("bl", " bl", ttx)
  ttx <- gsub("\\ bl.*", "", ttx)
  
  #B2B
  ttx <- gsub("blq", " blq", ttx)
  ttx <- gsub("of", " of", ttx)
  ttx <- gsub("\\ blq.*|\\ of.*", "", ttx)
  ttx <- gsub("lc", " lc", ttx)
  ttx <- gsub("local", " local", ttx)
  ttx <- gsub("loc", " loc", ttx)
  ttx <- gsub("\\ lc.*|\\ local.*|\\ loc.*", "", ttx)
  ttx <- gsub("edif", " edif", ttx)
  ttx <- gsub("sec", " sec", ttx)
  ttx <- gsub("\\ sec.*|\\ edif.*", "", ttx)
  ttx <- gsub("cs", " cs", ttx)
  ttx <- gsub("bdg", " bdg", ttx)
  ttx <- gsub("bg", " bg", ttx)
  ttx <- gsub("\\ cs.*|\\ bdg.|\\ bg.*", "", ttx)
  ttx <- gsub("trr", " trr", ttx)
  ttx <- gsub("\\ trr.*", "", ttx)
  #"edificio pacambe Cl. 5g #32-181barrio po, Antioquia, Colombia"
  
  #CC
  ttx <- gsub("cco", " cc", ttx) 
  ttx <- gsub("bodega", " bodega", ttx) 
  ttx <- gsub("\\ bodega.*", "", ttx)
  ttx <- gsub("zn", " zn", ttx) 
  ttx <- gsub("\\ zn.*", "", ttx)
  ttx <- gsub("Cq.unvalar", "circunvalar", ttx) 
  ttx <- gsub("([0-9]+)\\,([0-9])", "\\1\\2", x = ttx) #decimales numeros despues de comas
  ttx <- gsub("\\d{5,}", NA, ttx)
  ttx <- gsub("#","", ttx)
  ttx <- ifelse(!grepl("[0-9]", ttx), NA, ttx) #para quitar cuando solo hay texto en una celda
  ttx <- ifelse(!grepl("\\D", ttx), NA, ttx) #cuando solo hay n\u{fffd}meros en una celda
  ttx <- gsub("a\\?os", " años", ttx)
  ttx <- gsub("\\mz.*", "", ttx)
  ttx <- as.character(factor(replace(ttx, ttx== "", NA)))
  ttx <- stringr::str_trim(ttx)
  ttx <-gsub("\\-", " ",ttx)
  ttx <-gsub("\\,", " ",ttx)
  ttx <- qdapRegex::rm_white(ttx)
  ttx <- gsub("  ", " ", ttx)
  ttx
}
# #prueba15991141
# Data<-read_excel("./Copia de Montaje 24 y 25 F.xlsx")[,c("Efectivo","Agrupación","CONTRATO","NOMBRE_CLIENTE","IDENT_CLIENTE","DIRECCION","CICLO","CIUDAD_DPTO",#
#                                        "SERVICIO_1","SERVICIO_2","SERVICIO_3","ENTREGA_CAMBIOS","MOTIVO","RESULTADO_SS","ESTADO_ACCION_TO",
#                                        "ESTADO_ACCION_TV","ESTADO_ACCION_BA","ID_LLAMADA","FE_REGISTRO","USER_REG","ID_REGISTRO","BENEF_TO",
#                                        "RECONFIG_TO","DESCUENTO_TO","BENEF_TV","RECONFIG_TV","DESCUENTO_TV","BENEF_BA","RECONFIG_BA",
#                                        "DESCUENTO_BA","ORIGEN","NOMBRE_CDVS","ESTADO_ACCION_CP","TACTICO_CP","OBSERVACIONES")]

# # # # #write_xlsx(antioquia,"./fatan_poor_geo.xlsx")
# Data<-Data%>%tidyr::separate(`CIUDAD_DPTO`, c("MUNICIPIO", "DEPARTAMENTO"), "-", extra = "merge")%>%
#   mutate(Portafolio=apply(.[,10:12], 1, function(x) toString(na.omit(x))))%>%
#   mutate(MUNICIPIO = replace_na(Hmisc::capitalize(convert_text(MUNICIPIO)), ""),
#          DEPARTAMENTO = replace_na(Hmisc::capitalize(convert_text(DEPARTAMENTO)), ""),
#          C.DIRECCION = Convert_dir3(DIRECCION),
#          address = str_c(C.DIRECCION, MUNICIPIO, DEPARTAMENTO,"Colombia", sep = ", "),
#          ID_asdres = str_c(IDENT_CLIENTE,address, sep = "-"));Data<- Data[!duplicated(Data$ID_asdres),]

# Data<- Data[1:10,]
# paracorregir <- filter(Data, is.na(C.DIRECCION))
# 
# parageo <- filter(Data, !is.na(C.DIRECCION))
# 
# parageo <- distinct(Data, C.DIRECCION, .keep_all = T)
# 
# geocoded <- readRDS("./Geocoded/backup_old.rds")
# parageo <- anti_join(parageo, geocoded, by = "address")
# filtro<-read_excel("./fatan_poor_geo.xlsx")
# coincidencias <- filtro$address%>% unique(.)
# BD_TIP_map <-subset(Data, !(address %in% coincidencias))
# BD_TIP_map <-parageo
# BD_TIP_map1 <- BD_TIP_map[1:1000,]
# BD_TIP_map2 <- BD_TIP_map[1001:2000,]
# BD_TIP_map3 <- BD_TIP_map[2001:3000,]
# BD_TIP_map4 <- BD_TIP_map[3001:4000,]
# BD_TIP_map5 <- BD_TIP_map[4001:5000,]
# BD_TIP_map6 <- BD_TIP_map[5001:6004,]
# write_xlsx(BD_TIP_map6,"./pruebas/BD_TIP_map6.xlsx")
#Data<-df
#####Función geolocalización####
#Data<- read_excel("D:/Daniel Felipe Pérez Grajales/Desarrollo_Apps_2.0/App COVID-19/IdentificacindeCasos (20).xlsx")
#prueba<-Geolocalizar(Data)
Geolocalizar<-function(Data){

  ####Limpieza de base
  
  coordenadas_municipios <- read_excel("./CoordenadasM/coordenadas municipios.xlsx", 
                                       col_types = c("blank", "text", "blank", 
                                                     "text", "numeric", "numeric"))
  
  
  #aplicar funciones a las variables Municipio y Departamentos a archivo Coordenadas
  #names(coordenadas_municipios)
  coordenadas_municipios$Municipio<-convert_text(coordenadas_municipios$Municipio)
  coordenadas_municipios$Departamento<-convert_text(coordenadas_municipios$Departamento)
  
  vec_antioquia<-coordenadas_municipios %>% subset(Departamento=="antioquia") %>% 
    dplyr::select(Municipio)
  
  
  Data$`Lugar de residencia`<-convert_text(Data$`Lugar de residencia`)
  Data$`Lugar de residencia` <- gsub("bagre", "el bagre", Data$`Lugar de residencia`)
  Data$`Date Created`<- as.POSIXct(Data$`Date Created`,"%Y-%m-%d %H:%M:%S",tz="America/Bogota")
  fil<-c("antioquia",vec_antioquia$Municipio)
  
  ####BD con dirección
  x <- Data[!grepl(paste(fil,collapse="|"), Data$`Lugar de residencia`),] 
  ####BD minicipios
  
  x2 <- Data[grepl(paste(fil,collapse="|"), Data$`Lugar de residencia`),]
  ####BD estrictamente minicipios
  x2_1<-x2 %>% 
    subset(`Lugar de residencia`%in%fil)
  ####BD minicip-direcc
  x2_2<-x2 %>% 
    subset(!(`Lugar de residencia`%in%fil))
  ####BD corregir munipios rbin con x
  x2_3<-x2_2[!grepl(paste("antioquia",collapse="|"), x2_2$`Lugar de residencia`),]
  ####BD corregir munipios2
  x2_4<-x2_2[grepl(paste("antioquia",collapse="|"), x2_2$`Lugar de residencia`),]
  x2_4$`Lugar de residencia`<-  gsub("antioquia","",x2_4$`Lugar de residencia`)
  x2_4$`Lugar de residencia`<-  gsub("-","",x2_4$`Lugar de residencia`)
  x2_4$`Lugar de residencia` <- qdapRegex::rm_white(x2_4$`Lugar de residencia`)
  ####BD Municipios correccin rbin con x2_1
  x2_4_1<-x2_4%>% 
    subset((`Lugar de residencia`%in%fil))
  
  ####BD Municipios correccin rbin con x,x2_3
  x2_4_2<-x2_4%>% 
    subset(!(`Lugar de residencia`%in%fil))
  
  
  ####BD para geo
  BD_parageo<- rbind(x,x2_3,x2_4_2) %>% 
  dplyr::mutate(#MUNICIPIO = tidyr::replace_na(Hmisc::capitalize(convert_text(MUNICIPIO)), ""),
                DEPARTAMENTO = "Antioquia",#tidyr::replace_na(Hmisc::capitalize(convert_text(DEPARTAMENTO)), ""),
                C.DIRECCION = Convert_dir3(`Lugar de residencia`),
                address = stringr::str_c(C.DIRECCION, DEPARTAMENTO,"Colombia", sep = ", ")
  ) %>% 
  dplyr::mutate(address =ifelse(is.na(address),stringr::str_c(Convert_dirna(`Lugar de residencia`), DEPARTAMENTO,"Colombia", sep = ", "),address))
  ####BD para geo minicipio
  BD_paramuni<- rbind(x2_1,x2_4_1)%>% 
    dplyr::mutate(#MUNICIPIO = tidyr::replace_na(Hmisc::capitalize(convert_text(MUNICIPIO)), ""),
      DEPARTAMENTO = "Antioquia",#tidyr::replace_na(Hmisc::capitalize(convert_text(DEPARTAMENTO)), ""),
      C.DIRECCION = `Lugar de residencia`,
      address = stringr::str_c(C.DIRECCION, DEPARTAMENTO,"Colombia", sep = ", ")
    )
  rm(x,x2,x2_1,x2_2,x2_3,x2_4,x2_4_1,x2_4_2,fil,vec_antioquia)
  
  
  Data1<-rbind(BD_parageo,BD_paramuni)
  
 # lat,lon
  
  
  parageo <-NULL
  parageo <- dplyr::select(BD_parageo, DIRECCION=`Lugar de residencia`, DEPARTAMENTO=DEPARTAMENTO,C.DIRECCION,address);rm(BD_parageo) #%>% 
    # na.omit() %>% 
    # dplyr::mutate(MUNICIPIO = tidyr::replace_na(Hmisc::capitalize(convert_text(MUNICIPIO)), ""),
    #               DEPARTAMENTO = tidyr::replace_na(Hmisc::capitalize(convert_text(DEPARTAMENTO)), ""),
    #               C.DIRECCION = Convert_dir3(DIRECCION),
    #               address = stringr::str_c(C.DIRECCION, MUNICIPIO, DEPARTAMENTO,"Colombia", sep = ", ")
    # ) %>% 
    # dplyr::arrange(desc(MUNICIPIO));rm(Data)
  #names(parageo)
  paracorregir <- dplyr::filter(parageo, is.na(address))
  
  parageo <- dplyr::filter(parageo, !is.na(address))
  
  parageo <- dplyr::distinct(parageo, address, .keep_all = T)
  ####backap no Geo
  nongeo <- readRDS("./Nongeocoded/backup_old_nongeo.rds")
  
  parageo <- subset(parageo, !(address %in% c(nongeo$address)))
  ####Backap geo
  #setwd("~/Geolocalizador")
  geocoded <- readRDS("./Geocoded/backup_old.rds")
  parageo <- anti_join(parageo, geocoded, by = "address")#;rm(geocoded)
  # parageo1 <-parageo[1:500,]
  # parageo2 <-parageo[501:1000,]
  # parageo3 <-parageo[1001:1500,]
  # coincidencias<-parageo1$address%>%unique(.)
  # coincidencias1<-parageo2$address%>%unique(.)
  # coincidencias2<-parageo2$address%>%unique(.)
  # Data_r1<-parageo%>%
  #   subset(.,address %in% coincidencias)
  # Data_r2<-parageo%>%
  #   subset(.,address %in% coincidencias1)
  # Data_r3<-parageo%>%
  #   subset(.,address %in% coincidencias2)
  Basegraf <- NULL
  if(is.null(parageo) || nrow(parageo)== 0){
    geomuni<-BD_paramuni %>% 
      dplyr::select(DIRECCION=`Lugar de residencia`,DEPARTAMENTO,C.DIRECCION,address) %>% 
      left_join(subset(coordenadas_municipios,Departamento=="antioquia") %>% dplyr::select(C.DIRECCION=Municipio,lat=Latitud,lon=Longitud),by="C.DIRECCION");rm(BD_paramuni,coordenadas_municipios)
    geofinal<-NULL
    geofinal<-rbind(geomuni,geocoded)%>% 
      unique(.);geofinal <-geofinal[!duplicated(geofinal$address),];rm(geomuni,parageo)
    
    Basegraf<-dplyr::left_join(Data1,geofinal<-geofinal %>% dplyr::select(address,lat,lon),by = "address");rm(geocoded,geofinal) #%>% 
    # dplyr::mutate(key= paste(lon,lat, sep = "")) %>% 
    # dplyr::group_by(address,key) %>% 
    # dplyr::mutate(n = n()) %>% 
    # dplyr::arrange(address, n) %>%
    # dplyr::slice(n())
    return (Basegraf)
  }
  else if(!is.null(parageo) & nrow(parageo)!= 0){
    register_google(key ="AIzaSyBT-T-0jMN3empDdfjIYLh4SZQqJWwVb1U")#"AIzaSyBT-T-0jMN3empDdfjIYLh4SZQqJWwVb1U"emtelco
    parageo<-parageo %>% 
      mutate_geocode(address)#-> geocoded
    # result <- geocode(parageo$address, output = "all")
    # ### extracción de infomación del formato Json
    # for (k in 1:length(result)) {
    #   parageo$barrio[[k]] <- result[[k]][["results"]][[1]][["address_components"]][[3]][["long_name"]]
    #   parageo$barrio1[[k]] <- result[[k]][["results"]][[1]][["address_components"]][[3]][["short_name"]]
    #   parageo$lat[[k]] <- result[[k]][["results"]][[1]][["geometry"]][["location"]][["lat"]]
    #   parageo$lon[[k]] <- result[[k]][["results"]][[1]][["geometry"]][["location"]][["lng"]]
    # }
    ####Geo minicipio
    geomuni<-BD_paramuni %>% 
      dplyr::select(DIRECCION=`Lugar de residencia`,DEPARTAMENTO,C.DIRECCION,address) %>% 
      left_join(subset(coordenadas_municipios,Departamento=="antioquia") %>% dplyr::select(C.DIRECCION=Municipio,lat=Latitud,lon=Longitud),by="C.DIRECCION");rm(BD_paramuni,coordenadas_municipios)
    nongeocoded<-NULL
    geocoded1<-NULL
    nongeocoded <- subset(parageo, is.na(lon), select = address)
    geocoded1 <- subset(parageo, !is.na(lon))
    ### SAVE ####
    # Geocoded
    geocoded2 <- rbind(geocoded1, geocoded) %>% 
      unique()
    saveRDS(geocoded2, "./Geocoded/backup_old.rds")
    # NonGeo
    nongeocoded2<-rbind(nongeo, nongeocoded) %>% 
      unique() 
    saveRDS(nongeocoded2, "./Nongeocoded/backup_old_nongeo.rds");rm(nongeocoded,nongeo,nongeocoded2)
    geofinal<-NULL
    geofinal<-rbind(geomuni,parageo)%>% 
      unique(.);geofinal <-geofinal[!duplicated(geofinal$address),];rm(geomuni,parageo)
    
    Basegraf<-left_join(Data1,geofinal<-geofinal %>% dplyr::select(address,lat,lon),by = "address");rm(geocoded2)
    
    #saveRDS(Basegraf,"K:/01 Analytics/Daniel/Data_jesus_geo2.rds")
    return (Basegraf)
  }
  
  return (Basegraf)
  
}
