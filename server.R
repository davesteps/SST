
require("ggvis")
require(lubridate)
library(leaflet)
library(shiny)
require(shinyBS)
library(rWMS)
require(plyr)
require(dplyr)
library(ggplot2)



load(file='sst.rdata')
load(file='TSdf.rdata')


# Sys.setenv(http_proxy='http://148.252.96.126:3128')
# Sys.setenv(https_proxy='http://148.252.96.126:3128')


#stndf
popupString <- function(x){
  x1 <- paste(names(x),':',x,sep='',collapse = '<br/>')
  paste('<p>',x1,'</p>',sep='')}

sstdf <- function(id,lat,lon){
sst1.df<- TDSqueryAll(sst.wms[[1]],'analysed_sst',lat,lon,'month')
sst2.df<- TDSqueryAll(sst.wms[[2]],'analysed_sst',lat,lon,'month')
df<-rbind(sst1.df,sst2.df)
df$celcius <- df$values-272.15
df$id <- id
df}

#TSdf <- sstdf(stndf$id,stndf$lat,stndf$lon)
#save(TSdf,file='TSdf.rdata')
#load('TSdf.rdata')
shinybootstrap2::withBootstrap2({
shinyServer(function(input, output,session) {

  stndf <- rbind.data.frame(
    c(1, 54.572, -33.926),
    c(2, 13.069, -39.023),
    c(3, 39.504, 5.977))
  
  names(stndf)<-c('id','lat','lon')
  makeReactiveBinding('stndf')
  
  TSdf <- TSdf
  makeReactiveBinding('TSdf')
  
  map <- createLeafletMap(session, 'map')
  
  #createAlert(session,inputId = "a1", alertId="aa", 
  #            title = "Click on map to add marker", 
  #            message='',
  #            type = "info")

  observe({
    

    input$slt_date
    

    isolate({
      str(TSdf)
    map$addMarker(lat=stndf$lat,
                  lng=stndf$lon,
                  layerId=(1:nrow(stndf)),
                  options=list('clickable'=T),
                  popup=as.character(apply(stndf,1,popupString)))
    })
  })
  
  
  
  
  observe({

    if(is.null(input$map_click))
      return(NULL)
    
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
          createAlert(session,inputId = "a1", alertId="aa", 
                      title = "Maximum 5 markers allowed", 
                      message='',
                      type = "danger") 
        } 
      }
      
      map$addMarker(lat=stndf$lat,
                    lng=stndf$lon,
                    layerId=(1:nrow(stndf)),
                    options=list('clickable'=T),
                    popup=as.character(apply(stndf,1,popupString)))
      
      
    })
  })


  observe({
    
    if(input$btn_ext==0)
      return(NULL)
    
    
    closeAlert(session, 'aa')
    updateProgressBar(session,"TSpb", value=0,visible=T, animate=TRUE)
    isolate(TSdf <<- ddply(stndf,'id',.fun=extTS))
    updateProgressBar(session,"TSpb",visible=F)                    
    
    
    
  })
  
    extTS <- function(x){
      updateProgressBar(session,"TSpb", value=(x$id/nrow(stndf)*100))        
      sstdf(x$id,x$lat,x$lon)}
    

  
  observe({
    
    if(is.null(input$slt_date))
      return(NULL)
      
    map$clearWMS()     
    
    print(paste(sst.wms[[1]]@url,'scaleRange=270,310&',sep=''))
    t<-formattedDateString(sst.wms[[1]],lyr = 'analysed_sst',dates = input$slt_date)
#      map$addWMS(url=paste(sst.wms[[1]]@url,'colorscalerange=270,310&transparent=true&',sep=''),
    map$addWMS(url=sst.wms[[1]]@url,
               options=list(layers='analysed_sst',time=t,
                            nBands=255,
                            transparent=T,
                            format='image/png',
                            colorscalerange='270,310'))
      
  
    
  })
  
  observe({
    
    if(input$btn_clr==0)
      return(NULL)
    
    isolate({
      stndf <<- NULL
      TSdf <<- NULL
    })
    
    closeAlert(session, 'aa')
    map$clearMarkers()
    
  })
  
  
  reactive({
    
    
    pt <- input$slt_ptype

    print('GVIS')
    
    
    if(is.null(TSdf)|is.null(pt)){
      p <- data.frame(x=1,y=1) %>%
      ggvis(~x,~y) %>%
      layer_points()
    } else {
             
      
      TSdf$id <- factor(TSdf$id)
    if(pt=='Timeseries'){
      
      p = TSdf %>% group_by(id) %>%
        ggvis(~date,~celcius,stroke=~id) %>%
        layer_lines() %>%
        layer_model_predictions(model='lm')
    }

    if(pt=='Distribution'){
      #TSdf$month <- month(TSdf$date)
      p = TSdf %>% group_by(id) %>%
        ggvis(~celcius,fill=~id) %>%
        layer_densities() 
    
    }
    
    if(pt=='Monthly means'){
      TSdf$month <- month(TSdf$date)
      p = TSdf %>% group_by(month,id) %>%
        summarise(celcius=mean(celcius))%>%
      ggvis(~month,~celcius) %>%
        layer_points(fill=~id)
      
    }    

    }
    
    p %>% add_tooltip(tooltip, "hover")
    
  }) %>% bind_shiny("P")
  
  tooltip <- function(x) {
    ggvisclick <<- x
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br />")
  }
  
  output$plot_type <- renderUI({
  
    if(is.null(TSdf))
      return(NULL)
    
    selectInput('slt_ptype',label = NULL,choices = c('Timeseries','Distribution','Monthly means'),selectize = F)
    
  })
  
  output$plot_UI <- renderUI({
    
    
    if(is.null(TSdf))
      return(NULL)
    
    absolutePanel(id = "controls",
                  top = 100,
                  left = 10,
                  draggable = F,
                  width='auto',
                  height='auto',
                  ggvisOutput("P"))#plotOutput('TS_plot',width=600,height = 450))
    
    
    
  })
  
  ggvisclick <- NULL
  makeReactiveBinding('ggvisclick')
  
  observe({
    
    print(ggvisclick$id)
    id<-as.numeric(ggvisclick$id)
    #map$clearPopups()
  
    
    map$setView(lat=stndf$lat[id],3,#isolate(input$map_zoom),
                lng=stndf$lon[id]-15)
    map$markerPopup(id)  
    
  })
  
  output$table <- renderDataTable({
    
    if(is.null(TSdf))
      return(NULL)
    
    TSdf
    
  })
  
  updateDateInput(session,'slt_date',value = '2014-12-09')
  
})
})


