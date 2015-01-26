  
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#

# http://shiny.rstudio.com
##
#library(shinyIncubator)
library(shiny)
require(shinyBS)
require(leaflet)


shinyUI(navbarPage(title="Global Sea Surface Temperature 1985-2014",id = 'main',
                   inverse=T,
                   collapsable = T,
                   tabPanel(title = icon('globe'),
                            div(class="outer",
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css")
                                ),
                                leafletMap( width="100%",height='100%',
                                            "map",
                                            initialTileLayerAttribution = HTML('OSTIA SST data from <a href="http://www.myocean.eu">MyOcean</a>'),
                                            options=list(
                                              center = c(30, -35),
                                             zoom = 3)
                                            ),
#                                 absolutePanel(#id = "controls",
#                                   #class='modal',
#                                   fixed=T,
#                                   draggable = F,
#                                   top = 'auto',
#                                   left= 10,#'auto',
#                                   right = 'auto',
#                                   bottom=50,
#                                   height='auto',
#                                   width = 'auto',
#                                   textOutput('mouselatlon',)
#                                 ),
                                
                                absolutePanel(top = 10,
                                              left = 50,
                                              draggable = F,
                                              width='auto',
                                              height='auto',
                                              dateInput('slt_date',label=NULL,value = '2014-12-10',min = '1985-01-01',max='2014-12-12'),
                                              uiOutput('plot_type'),
                                              bsProgressBar("TSpb", visible=FALSE, striped=TRUE),
                                              bsTooltip('slt_date','Date of mapped layer'),
                                              bsTooltip('btn_ext','Extract SST data at markers'),
                                              bsTooltip('btn_clr','Clear Markers'),
                                              bsTooltip('slt_ptype','Change plot type'),
                                              bsAlert('a1')
                                              
                                              ),
                                absolutePanel(top = 10,
                                              left = 220,
                                              draggable = F,
                                              width='auto',
                                              height='auto',
                                              
                                              div(class='row-fluid',
                                                  div(class='span6',
                                              bsActionButton('btn_ext',label = icon('download'),style = 'danger')),
                                              div(class='span6',bsActionButton('btn_clr',label = icon('refresh'),style = 'danger')
                                              ))
                                              
                                              ),
                                          uiOutput('plot_UI')

                                
                                
                                              
                            )
                   ),
                   tabPanel(title = icon('map-marker'),
                            dataTableOutput('table')
                   )
                                                   
))
