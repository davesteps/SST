# 
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#

# http://shiny.rstudio.com
##
#library(shinyIncubator)


navbarPage(title="Global Sea Surface Temperature 1985-2015",
           id = 'main',
           theme = shinytheme("cosmo"),
           # inverse=T,
           collapsible = T,
           tabPanel(title = icon('globe'),
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        leafletOutput('map',width="100%",height='100%'),
                        
                        #                         absolutePanel(#id = "controls",
                        #                           #class='modal',
                        #                           fixed=T,
                        #                           draggable = F,
                        #                           top = 'auto',
                        #                           left= 10,#'auto',
                        #                           right = 'auto',
                        #                           bottom=50,
                        #                           height='auto',
                        #                           width = 'auto',
                        #                           textOutput('mouselatlon')
                        #                         ),
                        #                         
                        absolutePanel(top = 10,
                                      left = 50,
                                      draggable = F,
                                      width=140,
                                      height='auto',
                                      dateInput('slt_date',label=NULL,value = Sys.Date()-1,min = '2007-01-01',max=Sys.Date()-1),
                                      uiOutput('plot_type'),
                                      # bsProgressBar("TSpb", visible=FALSE, striped=TRUE),
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
                                      #                                       div(class='row-fluid',
                                      #                                           div(class='span6',
                                      actionButton('btn_ext',label = icon('download')),
                                      # ),
                                      # div(class='span6',
                                      actionButton('btn_clr',label = icon('refresh'))
                                      # )
                                      # )
                                      
                        ),
                        absolutePanel(bottom = 20,
                                      right = 10,
                                      draggable = F,
                                      width='auto',
                                      height='auto',
                                      a(icon('github fa-2x'),href='https://github.com/davesteps/sst',target='_blank'),
                                      a(icon('twitter fa-2x'),href='https://twitter.com/davesteps',target='_blank')
                        ),
                        uiOutput('plot_UI')
                    )
           ),
           tabPanel(title = icon('map-marker'),
                    dataTableOutput('table')
           )
           
           
)