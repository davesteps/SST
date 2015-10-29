require("ggvis")
require('opendapR')
require(lubridate)
require(dplyr)
library(ggplot2)
require(ncdf)


#Sys.setenv(http_proxy='http://148.252.96.126:3128')
# 


popupString <- function(x){
  x1 <- paste(names(x),':',x,sep='',collapse = '<br/>')
  paste('<p>',x1,'</p>',sep='')}

stndf <- data.frame(id=1:3,lat=c(54.572,25.069,39.504),lon=c(-33.926,-42.2,5.977))

qry.sst <- function(r){
  
  s1 <- open.ncdf('http://data.ncof.co.uk/thredds/dodsC/METOFFICE-GLO-SST-L4-RAN-OBS-SST-MON')
  s2 <- open.ncdf('http://data.ncof.co.uk/thredds/dodsC/METOFFICE-GLO-SST-L4-NRT-OBS-SST-MON-V2')
  # save(s1,file='s1.rdata')
  # save(s2,file='s2.rdata')
  # load('s1.rdata')
  # load('s2.rdata')
  v1 <- s1$var[["analysed_sst"]]
  v2 <- s2$var[["analysed_sst"]]
  
  print(1)
  id <- as.numeric(r[1])#$id
  y <- as.numeric(r[2])#$lat
  x <- as.numeric(r[3])#$lon+180
  if(x<0){x<-360+x}
  
  print(2)
  q1 <- buildQuery(var = v1,xr = x,yr = y,
                   tr = as.Date(c('1985-01-15','2006-12-15'))) %>% 
    getQuery(s1,v1,.)
  q2 <- buildQuery(var = v2,xr = x,yr = y,
                   tr = as.Date(c('2007-01-01',as.character(Sys.Date())))) %>% 
    getQuery(s2,v2,.)
  print(3)
  df <- rbind(q1,q2)
  df$time <- as.Date(as.character(df$time))
  df$id <- id
  df$value <- df$value-272.15
  # df$lon <- df$lon-180
  print(4)
  str(df)
  names(df) <- c('x','y','date','celsius','id')
  print(5)
  df
}
# 
# "http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2?"
# "http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-RAN-OBS-ANOM?"
# str(TSdf)
# TSdf <- lapply(1:nrow(stndf), function(s) qry.sst(stndf[s,]))
# TSdf <- do.call(rbind,TSdf)
#  save(TSdf,file='TSdf.rdata')
load('TSdf.rdata')

function(input, output,session) {
  
  stndf <- stndf
  makeReactiveBinding('stndf')
  
  TSdf <- TSdf
  makeReactiveBinding('TSdf')
  
  output$map <- renderLeaflet({
    
    isolate(df <- stndf)
    
    leaflet() %>% addTiles() %>%
      addMarkers(lat=df$lat,
                 lng=df$lon,
                 layerId=(1:nrow(df)),
                 options=list('clickable'=T),
                 popup=as.character(apply(df,1,popupString)))%>% 
      setView(lng = -70,lat =  30, zoom = 3)
    
  })
  
  #   createAlert(session,anchorId = "a1", alertId="aa", 
  #              title = "Click on map to add marker", 
  #              content ='',
  #              style = "info")
  
  #   observe({
  #     
  # 
  #     input$slt_date
  #     
  #     isolate({
  #       str(TSdf)
  #     
  #       leafletProxy('map') %>%
  #         addMarkers(lat=stndf$lat,
  #                   lng=stndf$lon,
  #                   layerId=(1:nrow(stndf)),
  #                   options=list('clickable'=T),
  #                   popup=as.character(apply(stndf,1,popupString)))
  #     })
  #   })
  #   
  
  
  
  observeEvent(input$map_click,{
    
    closeAlert(session, 'aa')
    
    isolate({
      
      lat <- round(input$map_click$lat,3)
      lon <- round(input$map_click$lng,3)
      
      if(is.null(stndf)){
        stndf <<- data.frame(id=1,lat=lat,lon=lon)
      } else {
        if(nrow(stndf)<5){
          stndf <<- rbind(stndf,c(nrow(stndf)+1,lat,lon))
        } else {
          createAlert(session,anchorId = "a1", alertId="aa", 
                      title = "Maximum 5 markers allowed", 
                      content='',
                      style = "danger") 
        } 
      }
      
      leafletProxy('map') %>%
        clearMarkers() %>%
        addMarkers(lat=stndf$lat,
                   lng=stndf$lon,
                   layerId=(1:nrow(stndf)),
                   options=list('clickable'=T),
                   popup=as.character(apply(stndf,1,popupString)))
      
      
    })
  })
  
  
  observeEvent(input$btn_ext,{
    
    closeAlert(session, 'aa')
    withProgress(message = 'Extracting...',
                 detail = 'takes a few seconds...', value = 0, {
                   TSdf <- lapply(1:nrow(stndf), extTS ) %>% do.call(rbind,.)
                 })
    TSdf <<- TSdf
  })
  
  
  extTS <- function(i){
    incProgress(1/nrow(stndf))
    qry.sst(stndf[i,])}
  
  
  
  observeEvent(input$slt_date,{
    
    leafletProxy('map') %>%
      # removeTiles(layerId = 'WMS') %>%
      addWMSTiles(layerId = 'sst',layers = 'analysed_sst',
                  baseUrl = "http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2?",
                  attribution = HTML(paste(a('OSTIA SST',target='_blank',
                                             href='http://data.ncof.co.uk/thredds/catalog.html'),
                                           icon('copyright'),
                                           'Crown Copyright 2011, published by the Met Office.')),
                  options=WMSTileOptions(
                    time=paste(input$slt_date,'T12:00:00.000Z',sep=''),
                    nBands=255,
                    transparent=T,
                    format='image/png',
                    colorscalerange='272,310'))
    
  })
  
  observeEvent(input$btn_clr,{
    
    isolate({
      stndf <<- NULL
      TSdf <<- NULL
    })
    
    # closeAlert(session, 'aa')
    leafletProxy('map') %>%
      clearMarkers()
    
  })
  
  
  reactive({
    
    pt <- input$slt_ptype
    
    print('GVIS')
    str(TSdf)
    str(pt)
    
    if(is.null(TSdf)|is.null(pt)){
      p <- data.frame(x=1,y=1) %>%
        ggvis(~x,~y) %>%
        layer_points()
    } else {
      
      
      TSdf$id <- factor(TSdf$id)
      if(pt=='Timeseries'){
        
        p = TSdf %>% group_by(id) %>%
          ggvis(~date,~celsius,stroke=~id) %>%
          layer_lines() %>%
          layer_model_predictions(model='lm')
      }
      
      if(pt=='Kernel Density'){
        #TSdf$month <- month(TSdf$date)
        p = TSdf %>% group_by(id) %>%
          ggvis(~celsius,fill=~id) %>%
          layer_densities() 
        
      }
      
      if(pt=='Monthly Means'){
        TSdf$month <- month(TSdf$date)
        p = TSdf %>% group_by(month,id) %>%
          summarise(celsius=mean(celsius))%>%
          ggvis(~month,~celsius) %>%
          layer_points(fill=~id)
      }    
      
    }
    
    p %>% add_tooltip(tooltip, "hover")
  }) %>% bind_shiny(plot_id = "P")
  
  tooltip <- function(x) {
    ggvisclick <<- x
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br/>")}
  
  output$plot_type <- renderUI({
    
    if(is.null(TSdf))
      return(NULL)
    
    selectInput('slt_ptype',label = NULL,choices = c('Timeseries','Kernel Density','Monthly Means'),selectize = F)
    
  })
  
  output$plot_UI <- renderUI({
    
    if(is.null(TSdf))
      return(NULL)
    
    absolutePanel(id = "controls",
                  top = 140,
                  left = 10,
                  draggable = F,
                  width='auto',
                  height='auto',
                  ggvisOutput(plot_id = "P"))#plotOutput('TS_plot',width=600,height = 450))
    
  })
  
  ggvisclick <- NULL
  makeReactiveBinding('ggvisclick')
  
  observeEvent(ggvisclick,{
    
    print(ggvisclick$id)
    id<-as.numeric(ggvisclick$id)
    #map$clearPopups()
    
    leafletProxy('map') %>%
      setView(lat=stndf$lat[id]-10,3,#isolate(input$map_zoom),
              lng=stndf$lon[id]-15) %>%
      addCircleMarkers(lat=stndf$lat[id],fillOpacity = 0.5,
                       lng=stndf$lon[id],layerId = 'h',
                       stroke = F,color = 'black')
    
    # map$markerPopup(id)  
    
  })
  
  output$table <- renderDataTable({
    
    if(is.null(TSdf))
      return(NULL)
    
    TSdf
    
  })
  
  

  
  # updateDateInput(session,'slt_date',value = Sys.Date()-1)
  
}
# install.packages('dplyr')
