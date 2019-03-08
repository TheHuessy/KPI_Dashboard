library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(dplyr)
library(lubridate)
library(plotly)

#setwd("H:/My Documents/Side Projects/Dashboard Demo/Dashboard")

shinyServer(function(input, output) {
  
##### TARGET LINE FUNCTIONS #####
  vline <- function(x = 0, color = "#FB4D42") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color)
    )
  }
  
  hline <- function(y = 0, color = "#FB4D42") {
    list(
      type = "line", 
      x0 = 0,
      x1 = 1, 
      xref = "paper",
      y0 = y, 
      y1 = y,
      line = list(color = color)
    )
  }
  

###########################      ADMINISTRATION AND FINANCE     #################################
  
  get.AF <- read.csv("www/Table Outputs/AF.csv")
  colnames(get.AF) <- gsub("\\.", " ", colnames(get.AF))
  colnames(get.AF) <- gsub("X", "", colnames(get.AF))
  AF <- get.AF
  
  output$AnF.TCTbl <- DT::renderDataTable({AF[,2:7] <- format(AF[,2:7], big.mark = ",")
  AF}, rownames = FALSE,
  selection = 'single',
  options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
      "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
      "$(this.api().table().footer()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
      "}"))
  )
  
  
  output$AnF.TCPlt <- renderPlotly({
    AnF.TC.P1 <- plot_ly(data = AF, 
                      x = factor(colnames(AF)[3:7], 
                                 levels = c(colnames(AF)[3], 
                                            colnames(AF)[4], 
                                            colnames(AF)[5], 
                                            colnames(AF)[6],
                                            colnames(AF)[7])
                      ),
                      y = as.numeric(AF[1,3:7]),
                      type = "bar",
                      name = as.character(AF[1,1]),
                      hoverinfo = 'text',
                      text = format(as.numeric(AF[1,3:7]), big.mark = ","),
                      textposition = 'top-center',
                      marker = list(color = c('#091F2F',
                                              '#091F2F',
                                              '#091F2F',
                                              '#45789C',
                                              '#091F2F'
                      )
                      ))     %>%
      layout(title = AF[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(AF[1,2])), 
               y1 = as.numeric(as.character(AF[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(AF[1,2])),
               text = paste("Target",format(as.numeric(as.character(AF[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(AF[1,2:7])*.95, digits = 0),
                         round(max(AF[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      ) %>% config(displayModeBar = FALSE)
    AnF.TC.P1
  })
  
  
  
  observeEvent(input$AnF.TCTbl_rows_selected,{
    AnF.TC.e <- reactive({input$AnF.TCTbl_rows_selected})
    if (is.null(AnF.TC.e()) == FALSE){
      output$AnF.TCPlt <- renderPlotly({
        AnF.TC.P1 <- plot_ly(data = AF, 
                          x = factor(colnames(AF)[3:7], 
                                     levels = c(colnames(AF)[3], 
                                                colnames(AF)[4], 
                                                colnames(AF)[5], 
                                                colnames(AF)[6],
                                                colnames(AF)[7])
                          ),
                          y = as.numeric(AF[AnF.TC.e(),3:7]),
                          type = "bar",
                          name = as.character(AF[AnF.TC.e(),1]),
                          hoverinfo = 'text',
                          text = format(as.numeric(AF[AnF.TC.e(),3:7]), big.mark = ","),
                          textposition = 'top-center',
                          marker = list(color = c('#091F2F',
                                                  '#091F2F',
                                                  '#091F2F',
                                                  '#45789C',
                                                  '#091F2F'
                          )
                          )) %>%
          layout(title = AF[AnF.TC.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(AF[AnF.TC.e(),2])), 
                   y1 = as.numeric(as.character(AF[AnF.TC.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(AF[AnF.TC.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(AF[AnF.TC.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(AF[AnF.TC.e(),2:7])*.95, digits = 0),
                             round(max(AF[AnF.TC.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        AnF.TC.P1
      })
    } else if (is.null(AnF.TC.e()) == TRUE){
      output$AnF.TCPlt <- renderPlotly({
        AnF.TC.P1 <- plot_ly(data = AF, 
                          x = factor(colnames(AF)[3:7], 
                                     levels = c(colnames(AF)[3], 
                                                colnames(AF)[4], 
                                                colnames(AF)[5], 
                                                colnames(AF)[6],
                                                colnames(AF)[7])
                          ),
                          y = as.numeric(AF[1,3:7]),
                          type = "bar",
                          name = as.character(AF[1,1]),
                          hoverinfo = 'text',
                          text = format(as.numeric(AF[1,3:7]), big.mark = ","),
                          textposition = 'top-center',
                          marker = list(color = c('#091F2F',
                                                  '#091F2F',
                                                  '#091F2F',
                                                  '#45789C',
                                                  '#091F2F'
                          )
                          )) %>%
          layout(title = AF[1,1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(AF[1,2])), 
                   y1 = as.numeric(as.character(AF[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(AF[1,2])),
                   text = paste("Target", format(as.numeric(as.character(AF[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(AF[1,2:7])*.95, digits = 0),
                             round(max(AF[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        AnF.TC.P1
      })
    }
  })
  
  
  ####### INFOBOXES ###
  
  
  output$AnF.TC.IB.1 <- renderInfoBox({
    AnF.TC.1.GT <- round(((AF[1,5]/AF[1,2])*100)-100, digits = 1)
    AnF.TC.1.GY <- round(((AF[1,5]/AF[1,6])*100)-100, digits = 1)
    AnF.TC.1.GYT <- round(((AF[1,5]/AF[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(AnF.TC.1.GT), if(AF[1,5]>AF[1,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(AnF.TC.1.GY), if(AF[1,5] >AF[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(AnF.TC.1.GYT), if(AF[1,5] >AF[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(AF)[5], AF$Metric[1]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("calendar-times", lib = "font-awesome"))
  })
  
  output$AnF.TC.IB.2 <- renderInfoBox({
    AnF.TC.2.GT <- round(((AF[2,5]/AF[2,2])*100)-100, digits = 1)
    AnF.TC.2.GY <- round(((AF[2,5]/AF[2,6])*100)-100, digits = 1)
    AnF.TC.2.GYT <- round(((AF[2,5]/AF[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(AnF.TC.2.GT), if(AF[2,5]>AF[2,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(AnF.TC.2.GY), if(AF[2,5] >AF[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(AnF.TC.2.GYT), if(AF[2,5] >AF[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(AF)[5], AF$Metric[2]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("file-invoice-dollar", lib = "font-awesome"))
  })
  
  output$AnF.TC.IB.3 <- renderInfoBox({
    AnF.TC.3.GT <- round(((AF[3,5]/AF[3,2])*100)-100, digits = 1)
    AnF.TC.3.GY <- round(((AF[3,5]/AF[3,6])*100)-100, digits = 1)
    AnF.TC.3.GYT <- round(((AF[3,5]/AF[3,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(AnF.TC.3.GT), if(AF[3,5]>AF[3,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(AnF.TC.3.GY), if(AF[3,5] >AF[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(AnF.TC.3.GYT), if(AF[3,5] >AF[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(AF)[5], AF$Metric[3]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("money-check-alt", lib = "font-awesome"))
  }) 
  
   
    
###########################      ARTS AND CULTURE     #################################
  ###########################      BPL     ###
 
  get.LibUsr <- read.csv("www/Table Outputs/AC.csv")
  colnames(get.LibUsr) <- gsub("\\.", " ", colnames(get.LibUsr))
  colnames(get.LibUsr) <- gsub("X", "", colnames(get.LibUsr))
  LibUsr <- get.LibUsr

  output$LUS <- DT::renderDataTable({LibUsr[,2:7] <- format(LibUsr[,2:7], big.mark = ",")
  LibUsr}, rownames = FALSE,
                                    selection = 'none',
                                    options = list(
                                      initComplete = JS(
                                        "function(settings, json) {",
                                        "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                        "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                        "$(this.api().table().footer()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                        "}"))
  )
                                      
  
  output$AC.LusPlt <- renderPlotly({
    BPL.P1 <- plot_ly(data = LibUsr, 
                      x = factor(colnames(LibUsr)[3:7], 
                                 levels = c(colnames(LibUsr)[3], 
                                            colnames(LibUsr)[4], 
                                            colnames(LibUsr)[5], 
                                            colnames(LibUsr)[6],
                                            colnames(LibUsr)[7])
                      ),
                      y = as.numeric(LibUsr[1,3:7]),
                      type = "bar",
                      name = as.character(LibUsr[1,1]),
                      hoverinfo = 'text',
                      text = format(as.numeric(LibUsr[1,3:7]), big.mark = ","),
                      textposition = 'top-center',
                      marker = list(color = c('#091F2F',
                                              '#091F2F',
                                              '#091F2F',
                                              '#45789C',
                                              '#091F2F'
                                              )
                      ))     %>%
      layout(title = LibUsr[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(LibUsr[1,2])), 
               y1 = as.numeric(as.character(LibUsr[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(LibUsr[1,2])),
               text = paste("Target",format(as.numeric(as.character(LibUsr[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(LibUsr[2:7])*.95, digits = 0),
                         round(max(LibUsr[2:7])*1.01, digits = 0) 
                         ),
               tickformat = ",d"
               )
      ) %>% config(displayModeBar = FALSE)
    BPL.P1
  })
  
  
  
  ####### INFOBOXES ###
  
  #Av Daily Library Users This Month
  output$AC.2.M1 <- renderInfoBox({
    BPL.GT <- round(((LibUsr[1,5]/LibUsr[1,2])*100)-100, digits = 1)
    BPL.GY <- round(((LibUsr[1,5]/LibUsr[1,6])*100)-100, digits = 1)
    BPL.GYT <- round(((LibUsr[1,5]/LibUsr[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(BPL.GT), if(LibUsr[1,5] >LibUsr[1,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(BPL.GY), if(LibUsr[1,5] >LibUsr[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(BPL.GYT), if(LibUsr[1,5] >LibUsr[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
             title = h5(paste(colnames(LibUsr)[5], "Average Daily Users")),
             color = "blue",
             fill = TRUE,
             icon = icon("book-reader", lib = "font-awesome"))
  })
    
###########################      BFD     #################################
  
  get.BFD <- read.csv("www/Table Outputs/BFD.csv")
  colnames(get.BFD) <- gsub("\\.", " ", colnames(get.BFD))
  colnames(get.BFD) <- gsub("X", "", colnames(get.BFD))
  BFDt <- get.BFD
  output$BFDTbl <- DT::renderDataTable({BFDt[,2:7] <- format(BFDt[,2:7], big.mark = ",")
    BFDt}, 
                                       rownames = FALSE,
                                       selection = 'single',
                                       options = list(
                                         initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                           "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                           "}")))

  output$BFDPlt <- renderPlotly({
    BFD.P1 <- plot_ly(data = BFDt, 
                      x = factor(colnames(BFDt)[3:7], 
                                 levels = c(colnames(BFDt)[3], 
                                            colnames(BFDt)[4], 
                                            colnames(BFDt)[5], 
                                            colnames(BFDt)[6],
                                            colnames(BFDt)[7])
                      ),
                      y = as.numeric(BFDt[1,3:7]),
                      type = "bar",
                      name = as.character(BFDt[1,1]),
                      hoverinfo = 'text',
                      text = format(as.numeric(BFDt[1,3:7]), big.mark = ","),
                      textposition = 'top-center',
                      marker = list(color = c('#091F2F',
                                              '#091F2F',
                                              '#091F2F',
                                              '#45789C',
                                              '#091F2F'
                      )
                      ))      %>%
      layout(title = BFDt[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(BFDt[1,2])), 
               y1 = as.numeric(as.character(BFDt[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(BFDt[1,2])),
               text = paste("Target", format(as.numeric(as.character(BFDt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(BFDt[1,2:7])*.95, digits = 0),
                         round(max(BFDt[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    BFD.P1
  })
  observeEvent(input$BFDTbl_rows_selected,{
    BFD.e <- reactive({input$BFDTbl_rows_selected})
    if (is.null(BFD.e()) == FALSE){
      output$BFDPlt <- renderPlotly({
        BFD.P1 <- plot_ly(data = BFDt, 
                x = factor(colnames(BFDt)[3:7], 
                           levels = c(colnames(BFDt)[3], 
                                      colnames(BFDt)[4], 
                                      colnames(BFDt)[5], 
                                      colnames(BFDt)[6],
                                      colnames(BFDt)[7])
                ),
                y = as.numeric(BFDt[BFD.e(),3:7]),
                type = "bar",
                name = as.character(BFDt[BFD.e(),1]),
                hoverinfo = 'text',
                text = format(as.numeric(BFDt[BFD.e(),3:7]), big.mark = ","),
                textposition = 'top-center',
                marker = list(color = c('#091F2F',
                                        '#091F2F',
                                        '#091F2F',
                                        '#45789C',
                                        '#091F2F'
                )
                )) %>%
          layout(title = BFDt[BFD.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(BFDt[BFD.e(),2])), 
                   y1 = as.numeric(as.character(BFDt[BFD.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(BFDt[BFD.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(BFDt[BFD.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(BFDt[BFD.e(),2:7])*.95, digits = 0),
                             round(max(BFDt[BFD.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        BFD.P1
      })
    } else if (is.null(BFD.e()) == TRUE){
      output$BFDPlt <- renderPlotly({
        BFD.P1 <- plot_ly(data = BFDt, 
                          x = factor(colnames(BFDt)[3:7], 
                                     levels = c(colnames(BFDt)[3], 
                                                colnames(BFDt)[4], 
                                                colnames(BFDt)[5], 
                                                colnames(BFDt)[6],
                                                colnames(BFDt)[7])
                          ),
                          y = as.numeric(BFDt[1,3:7]),
                          type = "bar",
                          name = as.character(BFDt[1,1]),
                          hoverinfo = 'text',
                          text = format(as.numeric(BFDt[1,3:7]), big.mark = ","),
                          textposition = 'top-center',
                          marker = list(color = c('#091F2F',
                                                  '#091F2F',
                                                  '#091F2F',
                                                  '#45789C',
                                                  '#091F2F'
                          )
                          )) %>%
          layout(title = BFDt[1,1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(BFDt[1,2])), 
                   y1 = as.numeric(as.character(BFDt[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(BFDt[1,2])),
                   text = paste("Target", format(as.numeric(as.character(BFDt[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(BFDt[1,2:7])*.95, digits = 0),
                             round(max(BFDt[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        BFD.P1
      })
    }
  })
  
  
  ####### INFOBOXES ###
  
  
  output$BFD.IB.1 <- renderInfoBox({
    BFD.1.GT <- round(((BFDt[1,5]/BFDt[1,2])*100)-100, digits = 1)
    BFD.1.GY <- round(((BFDt[1,5]/BFDt[1,6])*100)-100, digits = 1)
    BFD.1.GYT <- round(((BFDt[1,5]/BFDt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(BFD.1.GT), if(BFDt[1,5]>BFDt[1,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(BFD.1.GY), if(BFDt[1,5] >BFDt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(BFD.1.GYT), if(BFDt[1,5] >BFDt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(BFDt)[5], BFDt$Metric[1]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("ambulance", lib = "font-awesome"))
  })
  
  output$BFD.IB.2 <- renderInfoBox({
    BFD.2.GT <- round(((BFDt[2,5]/BFDt[2,2])*100)-100, digits = 1)
    BFD.2.GY <- round(((BFDt[2,5]/BFDt[2,6])*100)-100, digits = 1)
    BFD.2.GYT <- round(((BFDt[2,5]/BFDt[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(BFD.2.GT), if(BFDt[2,5]>BFDt[2,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(BFD.2.GY), if(BFDt[2,5] >BFDt[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(BFD.2.GYT), if(BFDt[2,5] >BFDt[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(BFDt)[5], BFDt$Metric[2]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("stopwatch", lib = "font-awesome"))
  })

  output$BFD.IB.3 <- renderInfoBox({
    BFD.3.GT <- round(((BFDt[3,5]/BFDt[3,2])*100)-100, digits = 1)
    BFD.3.GY <- round(((BFDt[3,5]/BFDt[3,6])*100)-100, digits = 1)
    BFD.3.GYT <- round(((BFDt[3,5]/BFDt[3,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(BFD.3.GT), if(BFDt[3,5]>BFDt[3,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(BFD.3.GY), if(BFDt[3,5] >BFDt[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(BFD.3.GYT), if(BFDt[3,5] >BFDt[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(BFDt)[5], BFDt$Metric[3]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("dashboard", lib = "glyphicon"))
  })  

    
###########################      BPD     #################################
  
  get.BPD <- read.csv("www/Table Outputs/BPD.csv", stringsAsFactors = FALSE)
  colnames(get.BPD) <- gsub("\\.", " ", colnames(get.BPD))
  colnames(get.BPD) <- gsub("X", "", colnames(get.BPD))
  BPDt <- get.BPD
  output$BPDTbl <- DT::renderDataTable({BPDt[,2:7] <- format(BPDt[,2:7], big.mark = ",")
    BPDt}, 
                                       rownames = FALSE,
                                       selection = 'single',
                                       options = list(
                                         initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                           "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                           "}")))
  
  output$BPDPlt <- renderPlotly({
    #Need to transpose it so that it's easier to read into plotly
   # trBPDt <- as.data.frame(t(BPDt))
    #The x axis needs to be factorized to tell plotly which order to display them in
    BPD.P1 <- plot_ly(data = BPDt, 
                      x = factor(colnames(BPDt)[3:7], 
                                 levels = c(colnames(BPDt)[3], 
                                            colnames(BPDt)[4], 
                                            colnames(BPDt)[5], 
                                            colnames(BPDt)[6],
                                            colnames(BPDt)[7]
                                            )
                      ),
                      y = as.numeric(BPDt[1,3:7]),
                      type = "bar",
                      name = as.character(BPDt[1,1]),
                      hoverinfo = 'text',
                      text = format(as.numeric(BPDt[1,3:7]), big.mark = ","),
                      textposition = 'top-center',
                      marker = list(color = c('#091F2F',
                                              '#091F2F',
                                              '#091F2F',
                                              '#45789C',
                                              '#091F2F'
                      )
                      )) %>%
      layout(title = BPDt[1,1],
             shapes = list(hline(as.numeric(as.character(BPDt[1,2])))),
             annotations = list(
               x = 0,
               y = as.numeric(as.character(BPDt[1,2])),
               text = paste("Target", format(as.numeric(as.character(BPDt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = TRUE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(BPDt[1,2:7])*.95, digits = 0),
                         round(max(BPDt[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
             )%>% config(displayModeBar = FALSE)
    BPD.P1
  })
  
observeEvent(input$BPDTbl_rows_selected,{
    bpd.e <- reactive({input$BPDTbl_rows_selected})
    if (length(bpd.e()) != 0){
  output$BPDPlt <- renderPlotly({
      trBPDt <- as.data.frame(t(BPDt))
      #The x axis needs to be factorized to tell plotly which order to display them in
      BPD.P1 <- plot_ly(data = BPDt, 
                        x = factor(colnames(BPDt)[3:7], 
                                   levels = c(colnames(BPDt)[3], 
                                              colnames(BPDt)[4], 
                                              colnames(BPDt)[5], 
                                              colnames(BPDt)[6],
                                              colnames(BPDt)[7])
                        ),
                        y = as.numeric(BPDt[bpd.e(),3:7]),
                        type = "bar",
                        name = as.character(BPDt[bpd.e(),1]),
                        hoverinfo = 'text',
                        text = format(as.numeric(BPDt[bpd.e(),3:7]), big.mark = ","),
                        textposition = 'top-center',
                        marker = list(color = c('#091F2F',
                                                '#091F2F',
                                                '#091F2F',
                                                '#45789C',
                                                '#091F2F'
                        )
                        ))        %>%
        layout(title = BPDt[bpd.e(),1],
               shapes = list(hline(as.numeric(as.character(BPDt[bpd.e(),2])))),
               annotations = list(
                 x = 0,
                 y = as.numeric(as.character(BPDt[bpd.e(),2])),
                 text = paste("Target",format(as.numeric(as.character(BPDt[bpd.e(),2])), big.mark = ",")),
                 xref = "x",
                 yref = "y",
                 showarrow = TRUE,
                 yanchor = "bottom",
                 ax = 0,
                 ay = 0
               ),
               font = list(
                 family = '"Lora", serif',
                 color = "#288BE4"
               ),
               yaxis = list(
                 range = c(round(min(BPDt[bpd.e(),2:7])*.95, digits = 0),
                           round(max(BPDt[bpd.e(),2:7])*1.01, digits = 0) 
                 ),
                 tickformat = ",d"
               )
               )%>% config(displayModeBar = FALSE)
      BPD.P1
  })
  } else if (length(bpd.e()) == 0){
    output$BPDPlt <- renderPlotly({
      #Need to transpose it so that it's easier to read into plotly
      #trBPDt <- as.data.frame(t(BPDt))
      #The x axis needs to be factorized to tell plotly which order to display them in
      BPD.P1 <- plot_ly(data = BPDt, 
                        x = factor(colnames(BPDt)[3:7], 
                                   levels = c(colnames(BPDt)[3], 
                                              colnames(BPDt)[4], 
                                              colnames(BPDt)[5], 
                                              colnames(BPDt)[6],
                                              colnames(BPDt)[7]
                                              )
                        ),
                        y = as.numeric(BPDt[1,3:7]),
                        type = "bar",
                        name = as.character(BPDt[1,1]),
                        hoverinfo = 'text',
                        text = format(as.numeric(BPDt[1,3:7]), big.mark = ","),
                        textposition = 'top-center',
                        marker = list(color = c('#091F2F',
                                                '#091F2F',
                                                '#091F2F',
                                                '#45789C',
                                                '#091F2F'
                        )
                        ))%>%
        layout(title = BPDt[1,1],
               shapes = list(hline(as.numeric(as.character(BPDt[1,2])))),
               annotations = list(
                 x = 0,
                 y = as.numeric(as.character(BPDt[1,2])),
                 text = paste("Target", format(as.numeric(as.character(BPDt[1,2])), big.mark = ",")),
                 xref = "x",
                 yref = "y",
                 showarrow = TRUE,
                 yanchor = "bottom",
                 ax = 0,
                 ay = 0
               ),
               font = list(
                 family = '"Lora", serif',
                 color = "#288BE4"
               ),
               yaxis = list(
                 range = c(round(min(BPDt[1,2:7])*.95, digits = 0),
                           round(max(BPDt[1,2:7])*1.01, digits = 0) 
                 ),
                 tickformat = ",d"
               )
               )%>% config(displayModeBar = FALSE)
      BPD.P1
        })
      }
  })

### Overtime ###

## HOURS ##
rawBPDOTH <- read.csv("www/Table Outputs/BPD-OTHU.csv", stringsAsFactors = FALSE)
colnames(rawBPDOTH) <- gsub("\\.", " ", colnames(rawBPDOTH))
colnames(rawBPDOTH) <- gsub("X", "", colnames(rawBPDOTH))
BPD2t <- rawBPDOTH
output$BPD_OTHTbl <- DT::renderDataTable({BPD2t[,2:7] <- format(BPD2t[,2:7], big.mark = ",")
  BPD2t}, 
                                     rownames = FALSE,
                                     selection = 'multiple',
                                     options = list(
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                         "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                         "}")))

output$BPD_OTHPlt <- renderPlotly({
  #Need to transpose it so that it's easier to read into plotly
  # trBPD2t <- as.data.frame(t(BPD2t))
  #The x axis needs to be factorized to tell plotly which order to display them in
  BPD.P2 <- plot_ly(data = BPD2t, 
                    x = factor(colnames(BPD2t)[3:7], 
                               levels = c(colnames(BPD2t)[3], 
                                          colnames(BPD2t)[4], 
                                          colnames(BPD2t)[5], 
                                          colnames(BPD2t)[6],
                                          colnames(BPD2t)[7]
                               )
                    ),
                    y = as.numeric(BPD2t[1,3:7]),
                    type = "bar",
                    name = as.character(BPD2t[1,1]),
                    hoverinfo = 'text',
                    text = format(as.numeric(BPD2t[1,3:7]), big.mark = ","),
                    textposition = 'top-center',
                    marker = list(color = c('#091F2F',
                                            '#091F2F',
                                            '#091F2F',
                                            '#45789C',
                                            '#091F2F'
                    )
                    )) %>%
    layout(title = BPD2t[1,1],
           shapes = list(hline(as.numeric(as.character(BPD2t[1,2])))),
           annotations = list(
             x = 0,
             y = as.numeric(as.character(BPD2t[1,2])),
             text = paste("Average", format(as.numeric(as.character(BPD2t[1,2])), big.mark = ",")),
             xref = "x",
             yref = "y",
             showarrow = TRUE,
             yanchor = "bottom",
             ax = 0,
             ay = 0
           ),
           font = list(
             family = '"Lora", serif',
             color = "#288BE4"
           ),
           yaxis = list(
             range = c(round(min(BPD2t[1,2:7])*.95, digits = 0),
                       round(max(BPD2t[1,2:7])*1.01, digits = 0) 
             ),
             tickformat = ",d"
           )
    )%>% config(displayModeBar = FALSE)
  BPD.P2
})


observeEvent(input$BPD_OTHTbl_rows_selected,{
  scs <- reactive({input$BPD_OTHTbl_rows_selected})
  if (length(scs()) != 0){
    output$BPD_OTHPlt <- renderPlotly({
      #Update by iterating over all schools selected
      
      #trBPDF <- as.data.frame(t(BPDF))
      #The x axis needs to be factorized to tell plotly which order to display them in
      BPD.P2 <- plot_ly(data = BPD2t,
                        type = "bar")
      for (i in scs()){
        BPD.P2 <- add_trace(BPD.P2,
                            x = factor(colnames(BPD2t)[3:7], 
                                       levels = c(colnames(BPD2t)[3], 
                                                  colnames(BPD2t)[4], 
                                                  colnames(BPD2t)[5], 
                                                  colnames(BPD2t)[6],
                                                  colnames(BPD2t)[7]#,
                                                  #colnames(BPD2t)[8]
                                       )
                            ),
                            y = as.numeric(BPD2t[i,3:7]),
                            type = "bar",
                            name = as.character(BPD2t[i,1]),
                            hoverinfo = 'text',
                            text = format(as.numeric(BPD2t[i,3:7]), big.mark = ","),
                            textposition = 'top-center'#,
                            #marker = list(color = '#091F2F')
        )
        
      }
      BPD.P2 <- BPD.P2 %>% layout(title = "Overtime Hours by Unit",
                                  shapes = list(hline(mean(as.numeric(as.character(BPD2t[scs(),2]))))),
                                  annotations = list(
                                    x = 0,
                                    y = mean(as.numeric(as.character(BPD2t[scs(),2]))),
                                    text = paste("Average", format(mean(as.numeric(as.character(BPD2t[scs(),2]))), big.mark = ",")),
                                    xref = "x",
                                    yref = "y",
                                    showarrow = TRUE,
                                    yanchor = "bottom",
                                    ax = 0,
                                    ay = 0
                                  ),
                                  font = list(
                                    family = '"Lora", serif',
                                    color = "#288BE4"
                                  )
      )%>% config(displayModeBar = FALSE)
      BPD.P2
    })
  } else if (length(scs()) == 0){
    output$BPD_OTHPlt <- renderPlotly({
      BPD.P2 <- plot_ly(data = BPDt, 
                        x = factor(colnames(BPDt)[3:7], 
                                   levels = c(colnames(BPDt)[3], 
                                              colnames(BPDt)[4], 
                                              colnames(BPDt)[5], 
                                              colnames(BPDt)[6],
                                              colnames(BPDt)[7]#,
                                              #colnames(BPDt)[8]
                                   )
                        ),
                        y = as.numeric(BPDt[1,3:7]),
                        type = "bar",
                        name = as.character(BPDt[1,1]),
                        hoverinfo = 'text',
                        text = format(as.numeric(BPDt[1,3:7]), big.mark = ","),
                        textposition = 'top-center',
                        marker = list(color = '#091F2F'
                        )) %>% layout(title = BPDt[1,1],
                                      shapes = list(hline(as.numeric(as.character(BPD2t[1,2])))),
                                      annotations = list(
                                        x = 0,
                                        y = as.numeric(as.character(BPD2t[1,2])),
                                        text = paste("Average", format(as.numeric(as.character(BPD2t[1,2])), big.mark = ",")),
                                        xref = "x",
                                        yref = "y",
                                        showarrow = TRUE,
                                        yanchor = "bottom",
                                        ax = 0,
                                        ay = 0
                                      ),
                                      font = list(
                                        family = '"Lora", serif',
                                        color = "#288BE4"
                                      ),
                                      yaxis = list(
                                        range = c(round(min(BPD2t[1,2:7])*.95, digits = 0),
                                                  round(max(BPD2t[1,2:7])*1.01, digits = 0) 
                                        ),
                                        tickformat = ",d"
                                      )
                        )%>% config(displayModeBar = FALSE)
      BPD.P2
    })
  }
})

## PAID ##

rawBPDOTP <- read.csv("www/Table Outputs/BPD-OTPU.csv", stringsAsFactors = FALSE)
colnames(rawBPDOTP) <- gsub("\\.", " ", colnames(rawBPDOTP))
colnames(rawBPDOTP) <- gsub("X", "", colnames(rawBPDOTP))
BPD2t <- rawBPDOTP
output$BPD_OTPTbl <- DT::renderDataTable({BPD2t[,2:7] <- format(BPD2t[,2:7], big.mark = ",")
  BPD2t}, 
                                         rownames = FALSE,
                                         selection = 'multiple',
                                         options = list(
                                           initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                             "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                             "}")))

output$BPD_OTPPlt <- renderPlotly({
  #Need to transpose it so that it's easier to read into plotly
  # trBPD2t <- as.data.frame(t(BPD2t))
  #The x axis needs to be factorized to tell plotly which order to display them in
  BPD.P3 <- plot_ly(data = BPD2t, 
                    x = factor(colnames(BPD2t)[3:7], 
                               levels = c(colnames(BPD2t)[3], 
                                          colnames(BPD2t)[4], 
                                          colnames(BPD2t)[5], 
                                          colnames(BPD2t)[6],
                                          colnames(BPD2t)[7]
                               )
                    ),
                    y = as.numeric(BPD2t[1,3:7]),
                    type = "bar",
                    name = as.character(BPD2t[1,1]),
                    hoverinfo = 'text',
                    text = format(as.numeric(BPD2t[1,3:7]), big.mark = ","),
                    textposition = 'top-center',
                    marker = list(color = c('#091F2F',
                                            '#091F2F',
                                            '#091F2F',
                                            '#45789C',
                                            '#091F2F'
                    )
                    )) %>%
    layout(title = BPD2t[1,1],
           shapes = list(hline(as.numeric(as.character(BPD2t[1,2])))),
           annotations = list(
             x = 0,
             y = as.numeric(as.character(BPD2t[1,2])),
             text = paste("Average", format(as.numeric(as.character(BPD2t[1,2])), big.mark = ",")),
             xref = "x",
             yref = "y",
             showarrow = TRUE,
             yanchor = "bottom",
             ax = 0,
             ay = 0
           ),
           font = list(
             family = '"Lora", serif',
             color = "#288BE4"
           ),
           yaxis = list(
             range = c(round(min(BPD2t[1,2:7])*.95, digits = 0),
                       round(max(BPD2t[1,2:7])*1.01, digits = 0) 
             ),
             tickformat = ",d"
           )
    )%>% config(displayModeBar = FALSE)
  BPD.P3
})


observeEvent(input$BPD_OTPTbl_rows_selected,{
  scs <- reactive({input$BPD_OTPTbl_rows_selected})
  if (length(scs()) != 0){
    output$BPD_OTPPlt <- renderPlotly({
      #Update by iterating over all schools selected
      
      #trBPDF <- as.data.frame(t(BPDF))
      #The x axis needs to be factorized to tell plotly which order to display them in
      BPD.P3 <- plot_ly(data = BPD2t,
                        type = "bar")
      for (i in scs()){
        BPD.P3 <- add_trace(BPD.P3,
                            x = factor(colnames(BPD2t)[3:7], 
                                       levels = c(colnames(BPD2t)[3], 
                                                  colnames(BPD2t)[4], 
                                                  colnames(BPD2t)[5], 
                                                  colnames(BPD2t)[6],
                                                  colnames(BPD2t)[7]#,
                                                  #colnames(BPD2t)[8]
                                       )
                            ),
                            y = as.numeric(BPD2t[i,3:7]),
                            type = "bar",
                            name = as.character(BPD2t[i,1]),
                            hoverinfo = 'text',
                            text = format(as.numeric(BPD2t[i,3:7]), big.mark = ","),
                            textposition = 'top-center'#,
                            #marker = list(color = '#091F2F')
        )
        
      }
      BPD.P3 <- BPD.P3 %>% layout(title = "Overtime Paid by Unit",
                                  shapes = list(hline(mean(as.numeric(as.character(BPD2t[scs(),2]))))),
                                  annotations = list(
                                    x = 0,
                                    y = mean(as.numeric(as.character(BPD2t[scs(),2]))),
                                    text = paste("Average", format(mean(as.numeric(as.character(BPD2t[scs(),2]))), big.mark = ",")),
                                    xref = "x",
                                    yref = "y",
                                    showarrow = TRUE,
                                    yanchor = "bottom",
                                    ax = 0,
                                    ay = 0
                                  ),
                                  font = list(
                                    family = '"Lora", serif',
                                    color = "#288BE4"
                                  )
      )%>% config(displayModeBar = FALSE)
      BPD.P3
    })
  } else if (length(scs()) == 0){
    output$BPD_OTPPlt <- renderPlotly({
      BPD.P3 <- plot_ly(data = BPDt, 
                        x = factor(colnames(BPDt)[3:7], 
                                   levels = c(colnames(BPDt)[3], 
                                              colnames(BPDt)[4], 
                                              colnames(BPDt)[5], 
                                              colnames(BPDt)[6],
                                              colnames(BPDt)[7]#,
                                              #colnames(BPDt)[8]
                                   )
                        ),
                        y = as.numeric(BPDt[1,3:7]),
                        type = "bar",
                        name = as.character(BPDt[1,1]),
                        hoverinfo = 'text',
                        text = format(as.numeric(BPDt[1,3:7]), big.mark = ","),
                        textposition = 'top-center',
                        marker = list(color = '#091F2F'
                        )) %>% layout(title = BPDt[1,1],
                                      shapes = list(hline(95)),
                                      annotations = list(
                                        x = 0,
                                        y = 95,
                                        text = paste("Average", format(as.numeric(as.character(BPD2t[1,2])), big.mark = ",")),
                                        xref = "x",
                                        yref = "y",
                                        showarrow = TRUE,
                                        yanchor = "bottom",
                                        ax = 0,
                                        ay = 0
                                      ),
                                      font = list(
                                        family = '"Lora", serif',
                                        color = "#288BE4"
                                      ),
                                      yaxis = list(
                                        range = c(round(min(BPD2t[1,2:7])*.95, digits = 0),
                                                  round(max(BPD2t[1,2:7])*1.01, digits = 0) 
                                        ),
                                        tickformat = ",d"
                                      )
                        )%>% config(displayModeBar = FALSE)
      BPD.P3
    })
  }
})


####### INFOBOXES ###


output$BPD.IB.1 <- renderInfoBox({
  BPD.1.GT <- round(((BPDt[1,5]/BPDt[1,2])*100)-100, digits = 1)
  BPD.1.GY <- round(((BPDt[1,5]/BPDt[1,6])*100)-100, digits = 1)
  BPD.1.GYT <- round(((BPDt[1,5]/BPDt[1,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.1.GT), if(BPDt[1,5]>BPDt[1,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.1.GY), if(BPDt[1,5] >BPDt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.1.GYT), if(BPDt[1,5] >BPDt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[1]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("running", lib = "font-awesome"))
})

output$BPD.IB.2 <- renderInfoBox({
  BPD.2.GT <- round(((BPDt[2,5]/BPDt[2,2])*100)-100, digits = 1)
  BPD.2.GY <- round(((BPDt[2,5]/BPDt[2,6])*100)-100, digits = 1)
  BPD.2.GYT <- round(((BPDt[2,5]/BPDt[2,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.2.GT), if(BPDt[2,5]>BPDt[2,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.2.GY), if(BPDt[2,5] >BPDt[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.2.GYT), if(BPDt[2,5] >BPDt[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[2]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("balance-scale", lib = "font-awesome"))
})

output$BPD.IB.3 <- renderInfoBox({
  BPD.3.GT <- round(((BPDt[3,5]/BPDt[3,2])*100)-100, digits = 1)
  BPD.3.GY <- round(((BPDt[3,5]/BPDt[3,6])*100)-100, digits = 1)
  BPD.3.GYT <- round(((BPDt[3,5]/BPDt[3,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.3.GT), if(BPDt[3,5]>BPDt[3,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.3.GY), if(BPDt[3,5] >BPDt[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.3.GYT), if(BPDt[3,5] >BPDt[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[3]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("user-slash", lib = "font-awesome"))
})  

output$BPD.IB.4 <- renderInfoBox({
  BPD.4.GT <- round(((BPDt[4,5]/BPDt[4,2])*100)-100, digits = 1)
  BPD.4.GY <- round(((BPDt[4,5]/BPDt[4,6])*100)-100, digits = 1)
  BPD.4.GYT <- round(((BPDt[4,5]/BPDt[4,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.4.GT), if(BPDt[4,5]>BPDt[4,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.4.GY), if(BPDt[4,5] >BPDt[4,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.4.GYT), if(BPDt[4,5] >BPDt[4,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[4]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("hand-point-right", lib = "font-awesome"))
})  

output$BPD.IB.5 <- renderInfoBox({
  BPD.5.GT <- round(((BPDt[5,5]/BPDt[5,2])*100)-100, digits = 1)
  BPD.5.GY <- round(((BPDt[5,5]/BPDt[5,6])*100)-100, digits = 1)
  BPD.5.GYT <- round(((BPDt[5,5]/BPDt[5,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.5.GT), if(BPDt[5,5]>BPDt[5,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.5.GY), if(BPDt[5,5] >BPDt[5,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.5.GYT), if(BPDt[5,5] >BPDt[5,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[5]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("cutlery", lib = "glyphicon"))
})  

output$BPD.IB.6 <- renderInfoBox({
  BPD.6.GT <- round(((BPDt[6,5]/BPDt[6,2])*100)-100, digits = 1)
  BPD.6.GY <- round(((BPDt[6,5]/BPDt[6,6])*100)-100, digits = 1)
  BPD.6.GYT <- round(((BPDt[6,5]/BPDt[6,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.6.GT), if(BPDt[6,5]>BPDt[6,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.6.GY), if(BPDt[6,5] >BPDt[6,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.6.GYT), if(BPDt[6,5] >BPDt[6,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[6]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("user-clock", lib = "font-awesome"))
})  


output$BPD.IB.7 <- renderInfoBox({
  BPD.7.GT <- round(((BPDt[7,5]/BPDt[7,2])*100)-100, digits = 1)
  BPD.7.GY <- round(((BPDt[7,5]/BPDt[7,6])*100)-100, digits = 1)
  BPD.7.GYT <- round(((BPDt[7,5]/BPDt[7,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.7.GT), if(BPDt[7,5]>BPDt[7,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.7.GY), if(BPDt[7,5] >BPDt[7,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.7.GYT), if(BPDt[7,5] >BPDt[7,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[7]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("hand-holding-usd", lib = "font-awesome"))
})  

output$BPD.IB.8 <- renderInfoBox({
  BPD.8.GT <- round(((BPDt[8,5]/BPDt[8,2])*100)-100, digits = 1)
  BPD.8.GY <- round(((BPDt[8,5]/BPDt[8,6])*100)-100, digits = 1)
  BPD.8.GYT <- round(((BPDt[8,5]/BPDt[8,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.8.GT), if(BPDt[8,5]>BPDt[8,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.8.GY), if(BPDt[8,5] >BPDt[8,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.8.GYT), if(BPDt[8,5] >BPDt[8,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[8]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("user-clock", lib = "font-awesome"))
})  

output$BPD.IB.9 <- renderInfoBox({
  BPD.9.GT <- round(((BPDt[9,5]/BPDt[9,2])*100)-100, digits = 1)
  BPD.9.GY <- round(((BPDt[9,5]/BPDt[9,6])*100)-100, digits = 1)
  BPD.9.GYT <- round(((BPDt[9,5]/BPDt[9,7])*100)-100, digits = 1)
  #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
  infoBox(value = shiny::HTML(paste(paste(abs(BPD.9.GT), if(BPDt[9,5]>BPDt[9,2]){"% Above 3 Month Trend"} else {"% Below 3 Month Trend"}),
                                    br(),
                                    paste(abs(BPD.9.GY), if(BPDt[9,5] >BPDt[9,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                    br(),
                                    paste(abs(BPD.9.GYT), if(BPDt[9,5] >BPDt[9,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
          title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPDt)[5], BPDt$Metric[9]), '</font>')),
          color = "blue",
          fill = TRUE,
          icon = icon("hand-holding-usd", lib = "font-awesome"))
})  
 
###########################      BPS     #################################
    
    ########### Student Attendance 

  get.BPS <- read.csv("www/Table Outputs/BPS.csv")
  colnames(get.BPS) <- gsub("\\.", " ", colnames(get.BPS))
  colnames(get.BPS) <- gsub("X", "", colnames(get.BPS))
  BPSt <- get.BPS
  output$SchoolGTbl <- DT::renderDataTable({BPSt[,2:7] <- format(BPSt[,2:7], big.mark = ",")
    BPSt}, rownames = FALSE,
                                           selection = 'none',
                                           options = list(
                                             initComplete = JS(
                                               "function(settings, json) {",
                                               "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                               "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                               "}")))

  output$BPSDaily <- renderPlotly({
    #Need to transpose it so that it's easier to read into plotly
    #trBPSt <- as.data.frame(t(BPSt))
    #The x axis needs to be factorized to tell plotly which order to display them in
    BPS.P1 <- plot_ly(data = BPSt, 
            x = factor(colnames(BPSt)[3:7], 
                       levels = c(colnames(BPSt)[3], 
                                  colnames(BPSt)[4], 
                                  colnames(BPSt)[5], 
                                  colnames(BPSt)[6],
                                  colnames(BPSt)[7]#,
                                  #colnames(BPSt)[8]
                                  )
            ),
            y = as.numeric(BPSt[1,3:7]),
            type = "bar",
            name = as.character(BPSt[1,1]),
            hoverinfo = 'text',
            text = format(as.numeric(BPSt[1,3:7]), big.mark = ","),
            textposition = 'top-center',
            marker = list(color = '#091F2F'
            )) %>%
      layout(title = BPSt[1,1],
             shapes = list(hline(95)),
             annotations = list(
               x = 0,
               y = 95,
               text = paste("Target", format(as.numeric(as.character(BPSt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = TRUE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(na.omit(as.numeric(BPSt[1,2:7])))*.95, digits = 0),
                         round(max(na.omit(as.numeric(BPSt[1,2:7])))*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
             )%>% config(displayModeBar = FALSE)
    BPS.P1
  })
  
  BPSF <- read.csv("www/Table Outputs/BPSFULL.csv", stringsAsFactors = FALSE)
  colnames(BPSF) <- gsub("\\.", " ", colnames(BPSF))
  colnames(BPSF) <- gsub("X", "", colnames(BPSF))
  
  output$SchoolTbl <- DT::renderDataTable({BPSF[,2:7] <- format(BPSF[,2:7], big.mark = ",")
    BPSF}, 
                                          rownames = FALSE,
                                          options = list(
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                              "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                              "}")))
  
  rs <- reactive({input$SchoolTbl_rows_selected})
  output$BPSALL <- renderPlotly({
    #SET INITIAL GRAPH OUTPUT
    # trBPSt <- as.data.frame(t(BPSt))
    #The x axis needs to be factorized to tell plotly which order to display them in
    BPS.P2 <- plot_ly(data = BPSt, 
            x = factor(colnames(BPSt)[3:7], 
                       levels = c(colnames(BPSt)[3], 
                                  colnames(BPSt)[4], 
                                  colnames(BPSt)[5], 
                                  colnames(BPSt)[6],
                                  colnames(BPSt)[7]#,
                                  #colnames(BPSt)[8]
                                  )
            ),
            y = as.numeric(BPSt[1,3:7]),
            type = "bar",
            name = as.character(BPSt[1,1]),
            hoverinfo = 'text',
            text = format(as.numeric(BPSt[1,3:7]), big.mark = ","),
            textposition = 'top-center' ,
             marker = list(color = '#091F2F'
            )) %>%
      layout(title = BPSt[1,1],
             shapes = list(hline(95)),
             annotations = list(
               x = 0,
               y = 95,
               text = paste("Target",format(as.numeric(as.character(BPSt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = TRUE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(na.omit(as.numeric(BPSt[1,2:7])))*.95, digits = 0),
                         round(max(na.omit(as.numeric(BPSt[1,2:7])))*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
             )%>% config(displayModeBar = FALSE)
    BPS.P2
  })
  observeEvent(input$SchoolTbl_rows_selected,{
    scs <- reactive({input$SchoolTbl_rows_selected})
    if (length(scs()) != 0){
    output$BPSALL <- renderPlotly({
      #Update by iterating over all schools selected

      #trBPSF <- as.data.frame(t(BPSF))
      #The x axis needs to be factorized to tell plotly which order to display them in
      BPS.P2 <- plot_ly(data = BPSF,
                        type = "bar")
      for (i in scs()){
        BPS.P2 <- add_trace(BPS.P2,
                            x = factor(colnames(BPSF)[3:7], 
                                       levels = c(colnames(BPSF)[3], 
                                                  colnames(BPSF)[4], 
                                                  colnames(BPSF)[5], 
                                                  colnames(BPSF)[6],
                                                  colnames(BPSF)[7]#,
                                                  #colnames(BPSF)[8]
                                                  )
                            ),
                            y = as.numeric(BPSF[i,3:7]),
                            type = "bar",
                            name = as.character(BPSF[i,1]),
                            hoverinfo = 'text',
                            text = format(as.numeric(BPSF[i,3:7]), big.mark = ","),
                            textposition = 'top-center'#,
                            #marker = list(color = '#091F2F')
                            )
                            
      }
      BPS.P2 <- BPS.P2 %>% layout(title = BPSF[1,1],
               shapes = list(hline(95)),
               annotations = list(
                 x = 0,
                 y = 95,
                 text = paste("Target", format(as.numeric(as.character(BPSF[1,2])), big.mark = ",")),
                 xref = "x",
                 yref = "y",
                 showarrow = TRUE,
                 yanchor = "bottom",
                 ax = 0,
                 ay = 0
               ),
               font = list(
                 family = '"Lora", serif',
                 color = "#288BE4"
               ),
               yaxis = list(
                 range = c(round(min(na.omit(t(BPSF[scs(),2:7])))*.95, digits = 0),
                           round(max(na.omit(t(BPSF[scs(),2:7])))*1.01, digits = 0) 
                 ),
                 tickformat = ",d"
               )
               )%>% config(displayModeBar = FALSE)
      BPS.P2
    })
    } else if (length(scs()) == 0){
      output$BPSALL <- renderPlotly({
      BPS.P2 <- plot_ly(data = BPSt, 
                        x = factor(colnames(BPSt)[3:7], 
                                   levels = c(colnames(BPSt)[3], 
                                              colnames(BPSt)[4], 
                                              colnames(BPSt)[5], 
                                              colnames(BPSt)[6],
                                              colnames(BPSt)[7]#,
                                              #colnames(BPSt)[8]
                                              )
                        ),
                        y = as.numeric(BPSt[1,3:7]),
                        type = "bar",
                        name = as.character(BPSt[1,1]),
                        hoverinfo = 'text',
                        text = format(as.numeric(BPSt[1,3:7]), big.mark = ","),
                        textposition = 'top-center',
                        marker = list(color = '#091F2F'
                        )) %>% layout(title = BPSt[1,1],
                  shapes = list(hline(95)),
                  annotations = list(
                    x = 0,
                    y = 95,
                    text = paste("Target", format(as.numeric(as.character(BPSF[1,2])), big.mark = ",")),
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    yanchor = "bottom",
                    ax = 0,
                    ay = 0
                  ),
                  font = list(
                    family = '"Lora", serif',
                    color = "#288BE4"
                  ),
                  yaxis = list(
                    range = c(round(min(na.omit(as.numeric(BPSF[2:7])))*.95, digits = 0),
                              round(max(na.omit(as.numeric(BPSF[2:7])))*1.01, digits = 0) 
                    ),
                    tickformat = ",d"
                  )
                  )%>% config(displayModeBar = FALSE)
      BPS.P2
    })
     }
   })
  
  ### INFO BOXES ###
  
  #Most recent attendance data
  output$BPS.IB.1 <- renderInfoBox({
    BPS.1.GT <- round(((BPSt[1,3]/BPSt[1,2])*100)-100, digits = 1)
    BPS.1.GY <- round(((BPSt[1,3]/BPSt[1,6])*100)-100, digits = 1)
    BPS.1.GYT <- round(((BPSt[1,3]/BPSt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(BPS.1.GT), if(BPSt[1,3]>BPSt[1,2]){"% Above Target"} else if(BPSt[1,3]<BPSt[1,2]){"% Below Target"} else if(BPSt[1,3]==BPSt[1,2]){"On Target!"} else {"No Data Currently Available!"}),
                                      br(),
                                      paste(abs(BPS.1.GY), if(BPSt[1,3] >BPSt[1,6]){paste("% Above", colnames(BPSt[6]))} else {paste("% Below", colnames(BPSt[6]))}),
                                      br(),
                                      paste(abs(BPS.1.GYT), if(BPSt[1,3] >BPSt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPSt)[3], BPSt$Metric[1]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("school", lib = "font-awesome"))
  })
  
  #Most recent full month of data
  output$BPS.IB.2 <- renderInfoBox({
    BPS.2.GT <- round(((BPSt[1,6]/BPSt[1,2])*100)-100, digits = 1)
    #BPS.2.GY <- round(((BPSt[1,6]/BPSt[2,6])*100)-100, digits = 1)
    BPS.2.GYT <- round(((BPSt[1,6]/BPSt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(BPS.2.GT), if(BPSt[1,6]>BPSt[1,2]){"% Above Target"} else if(BPSt[1,6]<BPSt[1,2]){"% Below Target"} else if(BPSt[1,6]==BPSt[1,2]){"On Target!"} else {"No Data Currently Available!"}),
                                      br(),
                                      #paste(abs(BPS.2.GY), if(BPSt[1,6] >BPSt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      #br(),
                                      paste(abs(BPS.2.GYT), if(BPSt[1,6] >BPSt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(BPSt)[6], BPSt$Metric[1]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("school", lib = "font-awesome"))
  })
  
  #Best school This month
  output$BPS.IB.3 <- renderInfoBox({
    B.Sch.m1 <- BPSF$School[which(BPSF[,6] == max(BPSF[,6]))]
    B.AtRt.m1 <- max(BPSF[,6])
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(B.Sch.m1),
                                      br(),
                                      paste(B.AtRt.m1, "% Present in ", colnames(BPSF)[6],   sep = "")
                                      )),
            title = shiny::HTML(paste('<font size="-1">', "Highest Attendance This Month", '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("education", lib = "glyphicon"))
  })
  
  #Best school this year
  output$BPS.IB.4 <- renderInfoBox({
    B.Sch.y <- BPSF$School[which(BPSF[,7] == max(BPSF[,7]))]
    B.AtRt.y <- max(BPSF[,7])
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(B.Sch.y),
                                      br(),
                                      paste(B.AtRt.y, "% Present in ", colnames(BPSF)[7],   sep = "")
    )),
    title = shiny::HTML(paste('<font size="-1">', "Highest Attendance This Year to Date", '</font>')),
    color = "blue",
    fill = TRUE,
    icon = icon("education", lib = "glyphicon"))
  })
  
    
###########################      CIVIC ENGAGEMENT     #################################
  
  get.CE <- read.csv("www/Table Outputs/CE.csv")
  colnames(get.CE) <- gsub("\\.", " ", colnames(get.CE))
  colnames(get.CE) <- gsub("X", "", colnames(get.CE))
  CEt <- get.CE
  output$CETbl <- DT::renderDataTable({CEt[,2:7] <- format(CEt[,2:7], big.mark = ",")
    CEt}, 
                                       rownames = FALSE,
                                       selection = 'single',
                                      options = list(
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                          "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                          "}")))
  
  output$CEPlt <- renderPlotly({
    CE.P1 <- plot_ly(data = CEt, 
                      x = factor(colnames(CEt)[3:7], 
                                 levels = c(colnames(CEt)[3], 
                                            colnames(CEt)[4], 
                                            colnames(CEt)[5], 
                                            colnames(CEt)[6],
                                            colnames(CEt)[7]
                                            )
                      ),
                      y = as.numeric(CEt[1,3:7]),
                      type = "bar",
                      name = as.character(CEt[1,1]),
                     hoverinfo = 'text',
                     text = format(as.numeric(CEt[1,3:7]), big.mark = ","),
                     textposition = 'top-center',
                     marker = list(color = c('#091F2F',
                                             '#091F2F',
                                             '#091F2F',
                                             '#45789C',
                                             '#091F2F'
                     )
                     )) %>%
      layout(title = CEt[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(CEt[1,2])), 
               y1 = as.numeric(as.character(CEt[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(CEt[1,2])),
               text = paste("Target", format(as.numeric(as.character(CEt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(CEt[1,2:7])*.95, digits = 0),
                         round(max(CEt[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    CE.P1
  })
  observeEvent(input$CETbl_rows_selected,{
    CE.e <- reactive({input$CETbl_rows_selected})
    if (is.null(CE.e()) == FALSE){
      output$CEPlt <- renderPlotly({
        CE.P1 <- plot_ly(data = CEt, 
                          x = factor(colnames(CEt)[3:7], 
                                     levels = c(colnames(CEt)[3], 
                                                colnames(CEt)[4], 
                                                colnames(CEt)[5], 
                                                colnames(CEt)[6],
                                                colnames(CEt)[7])
                          ),
                          y = as.numeric(CEt[CE.e(),3:7]),
                          type = "bar",
                          name = as.character(CEt[CE.e(),1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(CEt[CE.e(),3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
          layout(title = CEt[CE.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(CEt[CE.e(),2])), 
                   y1 = as.numeric(as.character(CEt[CE.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(CEt[CE.e(),2])),
                   text = paste("Target", format(as.numeric(as.character(CEt[CE.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(CEt[CE.e(),2:7])*.95, digits = 0),
                             round(max(CEt[CE.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        CE.P1
      })
    } else if (is.null(CE.e()) == TRUE){
      output$CEPlt <- renderPlotly({
        CE.P1 <- plot_ly(data = CEt, 
                          x = factor(colnames(CEt)[3:7], 
                                     levels = c(colnames(CEt)[3], 
                                                colnames(CEt)[4], 
                                                colnames(CEt)[5], 
                                                colnames(CEt)[6],
                                                colnames(CEt)[7])
                          ),
                          y = as.numeric(CEt[1,3:7]),
                          type = "bar",
                          name = as.character(CEt[1,1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(CEt[1,3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
          layout(title = CEt[1,1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(CEt[1,2])), 
                   y1 = as.numeric(as.character(CEt[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(CEt[1,2])),
                   text = paste("Target",format(as.numeric(as.character(CEt[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(CEt[1,2:7])*.95, digits = 0),
                             round(max(CEt[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        CE.P1
      })
    }
  })
  
  ####### INFOBOXES ###
  
  
  output$CE.IB.1 <- renderInfoBox({
    CE.1.GT <- round(((CEt[1,5]/CEt[1,2])*100)-100, digits = 1)
    CE.1.GY <- round(((CEt[1,5]/CEt[1,6])*100)-100, digits = 1)
    CE.1.GYT <- round(((CEt[1,5]/CEt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(CE.1.GT), if(CEt[1,5]>CEt[1,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(CE.1.GY), if(CEt[1,5] >CEt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(CE.1.GYT), if(CEt[1,5] >CEt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(CEt)[5], CEt$Metric[1]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("dashboard", lib = "glyphicon"))
  })
  
  output$CE.IB.2 <- renderInfoBox({
    CE.2.GT <- round(((CEt[2,5]/CEt[2,2])*100)-100, digits = 1)
    CE.2.GY <- round(((CEt[2,5]/CEt[2,6])*100)-100, digits = 1)
    CE.2.GYT <- round(((CEt[2,5]/CEt[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(CE.2.GT), if(CEt[2,5]>CEt[2,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(CE.2.GY), if(CEt[2,5] >CEt[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(CE.2.GYT), if(CEt[2,5] >CEt[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(CEt)[5], CEt$Metric[2]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("stopwatch", lib = "font-awesome"))
  })
  
  output$CE.IB.3 <- renderInfoBox({
    sr <- CEt[grep(pattern = "Source", CEt$Metric),]
    PopSrc <- sr$Metric[which(sr[,5] == max(sr[,5]))]
    PopAmt <- sr[which(sr[,5] == max(sr[,5])),5]
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(gsub("% Requests From Source: ", "", PopSrc)),
                                      br(),
                                      paste(PopAmt, "% of All Requests in ", colnames(sr)[5], sep = "")
                                      )),
            title = shiny::HTML(paste('<font size="-1">', "Most Popular Reporting Source", '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("inbox", lib = "glyphicon"))
  })    
  
  output$CE.IB.4 <- renderInfoBox({
    hd <- CEt[grep(pattern = "From:", CEt$Metric),]
    PopHd <- hd$Metric[which(hd[,5] == max(hd[,5]))]
    PopHAmt <- hd[which(hd[,5] == max(hd[,5])),5]
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(gsub("% Requests From: ", "", PopHd)),
                                      br(),
                                      paste(PopHAmt, "% of All Requests in ", colnames(hd)[5], sep = "")
    )),
    title = shiny::HTML(paste('<font size="-1">', "Most Popular Reporting Neighborhood", '</font>')),
    color = "blue",
    fill = TRUE,
    icon = icon("globe", lib = "glyphicon"))
  })    
  
  
###########################      ECONOMIC DEVELOPMENT     #################################
    
    
###########################      EEOS    #################################

   
  
  
  ####### PARKS ##
  
  get.EV.Prk <- read.csv("www/Table Outputs/EV-PARK.csv")
  colnames(get.EV.Prk) <- gsub("\\.", " ", colnames(get.EV.Prk))
  colnames(get.EV.Prk) <- gsub("X", "", colnames(get.EV.Prk))
  EV.Prkt <- get.EV.Prk
  output$EV.PrkTbl <- DT::renderDataTable({EV.Prkt[,2:7] <- format(EV.Prkt[,2:7], big.mark = ",")
    EV.Prkt}, 
                                          rownames = FALSE,
                                          selection = 'single',
                                          options = list(
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                              "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                              "}")))
  
  output$EV.PrkPlt <- renderPlotly({
    EV.Prk.P1 <- plot_ly(data = EV.Prkt, 
                         x = factor(colnames(EV.Prkt)[3:7], 
                                    levels = c(colnames(EV.Prkt)[3],
                                               colnames(EV.Prkt)[4], 
                                               colnames(EV.Prkt)[5], 
                                               colnames(EV.Prkt)[6], 
                                               colnames(EV.Prkt)[7])
                         ),
                         y = as.numeric(EV.Prkt[1,3:7]),
                         type = "bar",
                         name = as.character(EV.Prkt[1,1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(EV.Prkt[1,3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
      layout(title = EV.Prkt[1,1],
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(EV.Prkt[1,2])), 
               y1 = as.numeric(as.character(EV.Prkt[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(EV.Prkt[1,2])),
               text = paste("Target",format(as.numeric(as.character(EV.Prkt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(EV.Prkt[1,2:7])*.95, digits = 0),
                         round(max(EV.Prkt[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    EV.Prk.P1
  })
  observeEvent(input$EV.PrkTbl_rows_selected,{
    EV.Prk.e <- reactive({input$EV.PrkTbl_rows_selected})
    if (is.null(EV.Prk.e()) == FALSE){
      output$EV.PrkPlt <- renderPlotly({
        EV.Prk.P1 <- plot_ly(data = EV.Prkt, 
                             x = factor(colnames(EV.Prkt)[3:7], 
                                        levels = c(colnames(EV.Prkt)[3],
                                                   colnames(EV.Prkt)[4], 
                                                   colnames(EV.Prkt)[5], 
                                                   colnames(EV.Prkt)[6], 
                                                   colnames(EV.Prkt)[7])
                             ),
                             y = as.numeric(EV.Prkt[EV.Prk.e(),3:7]),
                             type = "bar",
                             name = as.character(EV.Prkt[EV.Prk.e(),1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(EV.Prkt[1,3:7]), big.mark = ","),
                             textposition = 'top-center',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             )) %>%
          layout(title = EV.Prkt[EV.Prk.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(EV.Prkt[EV.Prk.e(),2])), 
                   y1 = as.numeric(as.character(EV.Prkt[EV.Prk.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(EV.Prkt[EV.Prk.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(EV.Prkt[EV.Prk.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(EV.Prkt[EV.Prk.e(),2:7])*.95, digits = 0),
                             round(max(EV.Prkt[EV.Prk.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        EV.Prk.P1
      })
    } else if (is.null(EV.Prk.e()) == TRUE){
      output$EV.PrkPlt <- renderPlotly({
        EV.Prk.P1 <- plot_ly(data = EV.Prkt, 
                             x = factor(colnames(EV.Prkt)[3:7], 
                                        levels = c(colnames(EV.Prkt)[3],
                                                   colnames(EV.Prkt)[4], 
                                                   colnames(EV.Prkt)[5], 
                                                   colnames(EV.Prkt)[6], 
                                                   colnames(EV.Prkt)[7])
                             ),
                             y = as.numeric(EV.Prkt[1,3:7]),
                             type = "bar",
                             name = as.character(EV.Prkt[1,1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(EV.Prkt[1,3:7]), big.mark = ","),
                             textposition = 'top-center',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             )) %>%
          layout(title = EV.Prkt[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(EV.Prkt[1,2])), 
                   y1 = as.numeric(as.character(EV.Prkt[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(EV.Prkt[1,2])),
                   text = paste("Target",format(as.numeric(as.character(EV.Prkt[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(EV.Prkt[1,2:7])*.95, digits = 0),
                             round(max(EV.Prkt[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        EV.Prk.P1
      })
    }
  })
  
  ### INFO BOXES ###
  
  output$EV.Prk.IB.1 <- renderInfoBox({
    EV.Prk.1.GT <- round(((EV.Prkt[1,5]/EV.Prkt[1,2])*100)-100, digits = 1)
    EV.Prk.1.GY <- round(((EV.Prkt[1,5]/EV.Prkt[1,6])*100)-100, digits = 1)
    EV.Prk.1.GYT <- round(((EV.Prkt[1,5]/EV.Prkt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(EV.Prk.1.GT), if(EV.Prkt[1,5]>EV.Prkt[1,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(EV.Prk.1.GY), if(EV.Prkt[1,5] >EV.Prkt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(EV.Prk.1.GYT), if(EV.Prkt[1,5] >EV.Prkt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(EV.Prkt)[5], EV.Prkt$Metric[1]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("user-clock", lib = "font-awesome"))
  })
  
  output$EV.Prk.IB.2 <- renderInfoBox({
    EV.Prk.2.GT <- round(((EV.Prkt[2,5]/EV.Prkt[2,2])*100)-100, digits = 1)
    EV.Prk.2.GY <- round(((EV.Prkt[2,5]/EV.Prkt[2,6])*100)-100, digits = 1)
    EV.Prk.2.GYT <- round(((EV.Prkt[2,5]/EV.Prkt[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(EV.Prk.2.GT), if(EV.Prkt[2,5]>EV.Prkt[2,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(EV.Prk.2.GY), if(EV.Prkt[2,5] >EV.Prkt[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(EV.Prk.2.GYT), if(EV.Prkt[2,5] >EV.Prkt[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(EV.Prkt)[5], EV.Prkt$Metric[2]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("hand-holding-usd", lib = "font-awesome"))
  })
  
    
###########################      HEALTH AND HUMAN SERVICES     #################################
  
  ##### BCYF ###  
  
  
  ##### BPHC ###  
  get.HHS.Bph <- read.csv("www/Table Outputs/HHS-BPHC.csv")
  colnames(get.HHS.Bph) <- gsub("\\.", " ", colnames(get.HHS.Bph))
  colnames(get.HHS.Bph) <- gsub("X", "", colnames(get.HHS.Bph))
  HHS.Bpht <- get.HHS.Bph
  output$HHS.BphTbl <- DT::renderDataTable({HHS.Bpht[,2:7] <- format(HHS.Bpht[,2:7], big.mark = ",")
    HHS.Bpht}, 
                                       rownames = FALSE,
                                       selection = 'single',
                                       options = list(
                                         initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                           "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                           "}")))
  
  output$HHS.BphPlt <- renderPlotly({
    HHS.Bph.P1 <- plot_ly(data = HHS.Bpht, 
                      x = factor(colnames(HHS.Bpht)[3:7], 
                                 levels = c(colnames(HHS.Bpht)[3], 
                                            colnames(HHS.Bpht)[4], 
                                            colnames(HHS.Bpht)[5], 
                                            colnames(HHS.Bpht)[6],
                                            colnames(HHS.Bpht)[7])
                      ),
                      y = as.numeric(HHS.Bpht[1,3:7]),
                      type = "bar",
                      name = as.character(HHS.Bpht[1,1]),
                      hoverinfo = 'text',
                      text = format(as.numeric(HHS.Bpht[1,3:7]), big.mark = ","),
                      textposition = 'top-center',
                      marker = list(color = c('#091F2F',
                                              '#091F2F',
                                              '#091F2F',
                                              '#45789C',
                                              '#091F2F'
                      )
                      )) %>%
      layout(title = HHS.Bpht[1,1],
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(HHS.Bpht[1,2])), 
               y1 = as.numeric(as.character(HHS.Bpht[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(HHS.Bpht[1,2])),
               text = paste("Target",format(as.numeric(as.character(HHS.Bpht[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(HHS.Bpht[1,2:7])*.95, digits = 0),
                         round(max(HHS.Bpht[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    HHS.Bph.P1
  })
  observeEvent(input$HHS.BphTbl_rows_selected,{
    HHS.Bph.e <- reactive({input$HHS.BphTbl_rows_selected})
    if (is.null(HHS.Bph.e()) == FALSE){
      output$HHS.BphPlt <- renderPlotly({
        HHS.Bph.P1 <- plot_ly(data = HHS.Bpht, 
                          x = factor(colnames(HHS.Bpht)[3:7], 
                                     levels = c(colnames(HHS.Bpht)[3], 
                                                colnames(HHS.Bpht)[4], 
                                                colnames(HHS.Bpht)[5], 
                                                colnames(HHS.Bpht)[6],
                                                colnames(HHS.Bpht)[7])
                          ),
                          y = as.numeric(HHS.Bpht[HHS.Bph.e(),3:7]),
                          type = "bar",
                          name = as.character(HHS.Bpht[HHS.Bph.e(),1]),
                          hoverinfo = 'text',
                          text = format(as.numeric(HHS.Bpht[HHS.Bph.e(),3:7]), big.mark = ","),
                          textposition = 'top-center',
                          marker = list(color = c('#091F2F',
                                                  '#091F2F',
                                                  '#091F2F',
                                                  '#45789C',
                                                  '#091F2F'
                          )
                          )) %>%
          layout(title = HHS.Bpht[HHS.Bph.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(HHS.Bpht[HHS.Bph.e(),2])), 
                   y1 = as.numeric(as.character(HHS.Bpht[HHS.Bph.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(HHS.Bpht[HHS.Bph.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(HHS.Bpht[HHS.Bph.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(HHS.Bpht[HHS.Bph.e(),2:7])*.95, digits = 0),
                             round(max(HHS.Bpht[HHS.Bph.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        HHS.Bph.P1
      })
    } else if (is.null(HHS.Bph.e()) == TRUE){
      output$HHS.BphPlt <- renderPlotly({
        HHS.Bph.P1 <- plot_ly(data = HHS.Bpht, 
                          x = factor(colnames(HHS.Bpht)[3:7], 
                                     levels = c(colnames(HHS.Bpht)[3], 
                                                colnames(HHS.Bpht)[4], 
                                                colnames(HHS.Bpht)[5], 
                                                colnames(HHS.Bpht)[6],
                                                colnames(HHS.Bpht)[7])
                          ),
                          y = as.numeric(HHS.Bpht[1,3:7]),
                          type = "bar",
                          name = as.character(HHS.Bpht[1,1]),
                          hoverinfo = 'text',
                          text = format(as.numeric(HHS.Bpht[1,3:7]), big.mark = ","),
                          textposition = 'top-center',
                          marker = list(color = c('#091F2F',
                                                  '#091F2F',
                                                  '#091F2F',
                                                  '#45789C',
                                                  '#091F2F'
                          )
                          )) %>%
          layout(title = HHS.Bpht[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(HHS.Bpht[1,2])), 
                   y1 = as.numeric(as.character(HHS.Bpht[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(HHS.Bpht[1,2])),
                   text = paste("Target",format(as.numeric(as.character(HHS.Bpht[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(HHS.Bpht[1,2:7])*.95, digits = 0),
                             round(max(HHS.Bpht[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        HHS.Bph.P1
      })
    }
  })
  
  ### INFO BOXES ###
  
  output$HHS.Bph.IB.1 <- renderInfoBox({
    HHS.Bph.1.GT <- round(((HHS.Bpht[1,5]/HHS.Bpht[1,2])*100)-100, digits = 1)
    HHS.Bph.1.GY <- round(((HHS.Bpht[1,5]/HHS.Bpht[1,6])*100)-100, digits = 1)
    HHS.Bph.1.GYT <- round(((HHS.Bpht[1,5]/HHS.Bpht[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (HHS.Bpht[1,5] != HHS.Bpht[1,2]){paste(abs(HHS.Bph.1.GT), if(HHS.Bpht[1,5]>HHS.Bpht[1,2]){"% Above Target"} else if(HHS.Bpht[1,5]<HHS.Bpht[1,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(HHS.Bph.1.GY), if(HHS.Bpht[1,5] >HHS.Bpht[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(HHS.Bph.1.GYT), if(HHS.Bpht[1,5] >HHS.Bpht[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(HHS.Bpht)[5], HHS.Bpht$Metric[1]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("ambulance", lib = "font-awesome"))
  })
  
  output$HHS.Bph.IB.2 <- renderInfoBox({
    HHS.Bph.2.GT <- round(((HHS.Bpht[2,5]/HHS.Bpht[2,2])*100)-100, digits = 1)
    HHS.Bph.2.GY <- round(((HHS.Bpht[2,5]/HHS.Bpht[2,6])*100)-100, digits = 1)
    HHS.Bph.2.GYT <- round(((HHS.Bpht[2,5]/HHS.Bpht[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (HHS.Bpht[2,5] != HHS.Bpht[2,2]){paste(abs(HHS.Bph.2.GT), if(HHS.Bpht[2,5]>HHS.Bpht[2,2]){"% Above Target"} else if(HHS.Bpht[2,5]<HHS.Bpht[2,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(HHS.Bph.2.GY), if(HHS.Bpht[2,5] >HHS.Bpht[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(HHS.Bph.2.GYT), if(HHS.Bpht[2,5] >HHS.Bpht[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(HHS.Bpht)[5], HHS.Bpht$Metric[2]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("medkit", lib = "font-awesome"))
  })
  
  output$HHS.Bph.IB.3 <- renderInfoBox({
    HHS.Bph.3.GT <- round(((HHS.Bpht[3,5]/HHS.Bpht[3,2])*100)-100, digits = 1)
    HHS.Bph.3.GY <- round(((HHS.Bpht[3,5]/HHS.Bpht[3,6])*100)-100, digits = 1)
    HHS.Bph.3.GYT <- round(((HHS.Bpht[3,5]/HHS.Bpht[3,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (HHS.Bpht[3,5] != HHS.Bpht[3,2]){paste(abs(HHS.Bph.3.GT), if(HHS.Bpht[3,5]>HHS.Bpht[3,2]){"% Above Target"} else if(HHS.Bpht[3,5]<HHS.Bpht[3,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(HHS.Bph.3.GY), if(HHS.Bpht[3,5] >HHS.Bpht[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(HHS.Bph.3.GYT), if(HHS.Bpht[3,5] >HHS.Bpht[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(HHS.Bpht)[5], HHS.Bpht$Metric[3]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("band-aid", lib = "font-awesome"))
  })
    
###########################      HOUSING     #################################
    
  get.IT <- read.csv("www/Table Outputs/HOU.csv")
  colnames(get.IT) <- gsub("\\.", " ", colnames(get.IT))
  colnames(get.IT) <- gsub("X", "", colnames(get.IT))
  HOUt <- get.IT
  output$HOUTbl <- DT::renderDataTable({HOUt[,2:7] <- format(HOUt[,2:7], big.mark = ",")
  HOUt}, 
  rownames = FALSE,
  selection = 'none',
  options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
      "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
      "}")))
  
  output$HOUPlt <- renderPlotly({
    HOU.P1 <- plot_ly(data = HOUt, 
                     x = factor(colnames(HOUt)[3:7], 
                                levels = c(colnames(HOUt)[3], 
                                           colnames(HOUt)[4], 
                                           colnames(HOUt)[5], 
                                           colnames(HOUt)[6],
                                           colnames(HOUt)[7])
                     ),
                     y = as.numeric(HOUt[1,3:7]),
                     type = "bar",
                     name = as.character(HOUt[1,1]),
                     hoverinfo = 'text',
                     text = format(as.numeric(HOUt[1,3:7]), big.mark = ","),
                     textposition = 'top-center',
                     marker = list(color = c('#091F2F',
                                             '#091F2F',
                                             '#091F2F',
                                             '#45789C',
                                             '#091F2F'
                     )
                     )) %>%
      layout(title = HOUt[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(HOUt[1,2])), 
               y1 = as.numeric(as.character(HOUt[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(HOUt[1,2])),
               text = paste("Target",format(as.numeric(as.character(HOUt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(HOUt[1,2:7])*.95, digits = 0),
                         round(max(HOUt[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    HOU.P1
  })
  observeEvent(input$HOUTbl_rows_selected,{
    HOU.e <- reactive({input$HOUTbl_rows_selected})
    if (is.null(HOU.e()) == FALSE){
      output$HOUPlt <- renderPlotly({
        HOU.P1 <- plot_ly(data = HOUt, 
                         x = factor(colnames(HOUt)[3:7], 
                                    levels = c(colnames(HOUt)[3], 
                                               colnames(HOUt)[4], 
                                               colnames(HOUt)[5], 
                                               colnames(HOUt)[6],
                                               colnames(HOUt)[7])
                         ),
                         y = as.numeric(HOUt[HOU.e(),3:7]),
                         type = "bar",
                         name = as.character(HOUt[HOU.e(),1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(HOUt[HOU.e(),3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
          layout(title = HOUt[HOU.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(HOUt[HOU.e(),2])), 
                   y1 = as.numeric(as.character(HOUt[HOU.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(HOUt[HOU.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(HOUt[HOU.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(HOUt[HOU.e(),2:7])*.95, digits = 0),
                             round(max(HOUt[HOU.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        HOU.P1
      })
    } else if (is.null(HOU.e()) == TRUE){
      output$HOUPlt <- renderPlotly({
        HOU.P1 <- plot_ly(data = HOUt, 
                         x = factor(colnames(HOUt)[3:7], 
                                    levels = c(colnames(HOUt)[3], 
                                               colnames(HOUt)[4], 
                                               colnames(HOUt)[5], 
                                               colnames(HOUt)[6],
                                               colnames(HOUt)[7])
                         ),
                         y = as.numeric(HOUt[1,3:7]),
                         type = "bar",
                         name = as.character(HOUt[1,1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(HOUt[1,3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
          layout(title = HOUt[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(HOUt[1,2])), 
                   y1 = as.numeric(as.character(HOUt[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(HOUt[1,2])),
                   text = paste("Target",format(as.numeric(as.character(HOUt[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(HOUt[1,2:7])*.95, digits = 0),
                             round(max(HOUt[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        HOU.P1
      })
    }
  }) 
  
  ### INFO BOXES ###
  
  output$HOU.IB.1 <- renderInfoBox({
    HOU.1.GT <- round(((HOUt[1,5]/HOUt[1,2])*100)-100, digits = 1)
    HOU.1.GY <- round(((HOUt[1,5]/HOUt[1,6])*100)-100, digits = 1)
    HOU.1.GYT <- round(((HOUt[1,5]/HOUt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (HOUt[1,5] != HOUt[1,2]){paste(abs(HOU.1.GT), if(HOUt[1,5]>HOUt[1,2]){"% Above Target"} else if(HOUt[1,5]<HOUt[1,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(HOU.1.GY), if(HOUt[1,5] >HOUt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(HOU.1.GYT), if(HOUt[1,5] >HOUt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(HOUt)[5], HOUt$Metric[1]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("house-damage", lib = "font-awesome"))
  })
  
    
###########################      DOIT     #################################
   
  get.IT <- read.csv("www/Table Outputs/DOIT.csv")
  colnames(get.IT) <- gsub("\\.", " ", colnames(get.IT))
  colnames(get.IT) <- gsub("X", "", colnames(get.IT))
  ITt <- get.IT
  output$DoitTbl <- DT::renderDataTable({ITt[,2:7] <- format(ITt[,2:7], big.mark = ",")
    ITt}, 
                                           rownames = FALSE,
                                           selection = 'none',
                                        options = list(
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                            "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                            "}")))
  
  output$DoitPlt <- renderPlotly({
    IT.P1 <- plot_ly(data = ITt, 
                          x = factor(colnames(ITt)[3:7], 
                                     levels = c(colnames(ITt)[3], 
                                                colnames(ITt)[4], 
                                                colnames(ITt)[5], 
                                                colnames(ITt)[6],
                                                colnames(ITt)[7])
                          ),
                          y = as.numeric(ITt[1,3:7]),
                          type = "bar",
                          name = as.character(ITt[1,1]),
                     hoverinfo = 'text',
                     text = format(as.numeric(ITt[1,3:7]), big.mark = ","),
                     textposition = 'top-center',
                     marker = list(color = c('#091F2F',
                                             '#091F2F',
                                             '#091F2F',
                                             '#45789C',
                                             '#091F2F'
                     )
                     )) %>%
      layout(title = ITt[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(ITt[1,2])), 
               y1 = as.numeric(as.character(ITt[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(ITt[1,2])),
               text = paste("Target",format(as.numeric(as.character(ITt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(ITt[1,2:7])*.95, digits = 0),
                         round(max(ITt[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    IT.P1
  })
  observeEvent(input$DoitTbl_rows_selected,{
    IT.e <- reactive({input$DoitTbl_rows_selected})
    if (is.null(IT.e()) == FALSE){
      output$DoitPlt <- renderPlotly({
        IT.P1 <- plot_ly(data = ITt, 
                              x = factor(colnames(ITt)[3:7], 
                                         levels = c(colnames(ITt)[3], 
                                                    colnames(ITt)[4], 
                                                    colnames(ITt)[5], 
                                                    colnames(ITt)[6],
                                                    colnames(ITt)[7])
                              ),
                              y = as.numeric(ITt[IT.e(),3:7]),
                              type = "bar",
                              name = as.character(ITt[IT.e(),1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(ITt[IT.e(),3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
          layout(title = ITt[IT.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(ITt[IT.e(),2])), 
                   y1 = as.numeric(as.character(ITt[IT.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(ITt[IT.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(ITt[IT.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(ITt[IT.e(),2:7])*.95, digits = 0),
                             round(max(ITt[IT.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        IT.P1
      })
    } else if (is.null(IT.e()) == TRUE){
      output$DoitPlt <- renderPlotly({
        IT.P1 <- plot_ly(data = ITt, 
                              x = factor(colnames(ITt)[3:7], 
                                         levels = c(colnames(ITt)[3], 
                                                    colnames(ITt)[4], 
                                                    colnames(ITt)[5], 
                                                    colnames(ITt)[6],
                                                    colnames(ITt)[7])
                              ),
                              y = as.numeric(ITt[1,3:7]),
                              type = "bar",
                              name = as.character(ITt[1,1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(ITt[1,3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
          layout(title = ITt[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(ITt[1,2])), 
                   y1 = as.numeric(as.character(ITt[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(ITt[1,2])),
                   text = paste("Target",format(as.numeric(as.character(ITt[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(ITt[1,2:7])*.95, digits = 0),
                             round(max(ITt[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        IT.P1
      })
    }
  }) 
  
  ### INFO BOXES ###
  
  output$IT.IB.1 <- renderInfoBox({
    IT.1.GT <- round(((ITt[1,5]/ITt[1,2])*100)-100, digits = 1)
    IT.1.GY <- round(((ITt[1,5]/ITt[1,6])*100)-100, digits = 1)
    IT.1.GYT <- round(((ITt[1,5]/ITt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ITt[1,5] != ITt[1,2]){paste(abs(IT.1.GT), if(ITt[1,5]>ITt[1,2]){"% Above Target"} else if(ITt[1,5]<ITt[1,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(IT.1.GY), if(ITt[1,5] >ITt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(IT.1.GYT), if(ITt[1,5] >ITt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ITt)[5], ITt$Metric[1]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("phone", lib = "glyphicon"))
  })
    
###########################      MAYOR'S OFFICE     #################################
    
    
###########################      OPERATIONS    #################################
  get.Ops <- read.csv("www/Table Outputs/OP.csv")
  colnames(get.Ops) <- gsub("\\.", " ", colnames(get.Ops))
  colnames(get.Ops) <- gsub("X", "", colnames(get.Ops))
  Opst <- get.Ops
  output$OpsTbl <- DT::renderDataTable({Opst[,2:7] <- format(Opst[,2:7], big.mark = ",")
    Opst}, 
                                          rownames = FALSE,
                                          selection = 'single',
                                       options = list(
                                         initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                           "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                           "}")))
  
  output$OpsPlt <- renderPlotly({
    Ops.P1 <- plot_ly(data = Opst, 
                         x = factor(colnames(Opst)[3:7], 
                                    levels = c(colnames(Opst)[3], 
                                               colnames(Opst)[4], 
                                               colnames(Opst)[5], 
                                               colnames(Opst)[6],
                                               colnames(Opst)[7])
                         ),
                         y = as.numeric(Opst[1,3:7]),
                         type = "bar",
                         name = as.character(Opst[1,1]),
                      hoverinfo = 'text',
                      text = format(as.numeric(Opst[1,3:7]), big.mark = ","),
                      textposition = 'top-center',
                      marker = list(color = c('#091F2F',
                                              '#091F2F',
                                              '#091F2F',
                                              '#45789C',
                                              '#091F2F'
                      )
                      )) %>%
      layout(title = Opst[1,1],
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(Opst[1,2])), 
               y1 = as.numeric(as.character(Opst[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(Opst[1,2])),
               text = paste("Target",format(as.numeric(as.character(Opst[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(Opst[1,2:7])*.95, digits = 0),
                         round(max(Opst[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    Ops.P1
  })
  observeEvent(input$OpsTbl_rows_selected,{
    Ops.e <- reactive({input$OpsTbl_rows_selected})
    if (is.null(Ops.e()) == FALSE){
      output$OpsPlt <- renderPlotly({
        Ops.P1 <- plot_ly(data = Opst, 
                             x = factor(colnames(Opst)[3:7], 
                                        levels = c(colnames(Opst)[3], 
                                                   colnames(Opst)[4], 
                                                   colnames(Opst)[5], 
                                                   colnames(Opst)[6],
                                                   colnames(Opst)[7])
                             ),
                             y = as.numeric(Opst[Ops.e(),3:7]),
                             type = "bar",
                             name = as.character(Opst[Ops.e(),1]),
                          hoverinfo = 'text',
                          text = format(as.numeric(Opst[Ops.e(),3:7]), big.mark = ","),
                          textposition = 'top-center',
                          marker = list(color = c('#091F2F',
                                                  '#091F2F',
                                                  '#091F2F',
                                                  '#45789C',
                                                  '#091F2F'
                          )
                          )) %>%
          layout(title = Opst[Ops.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(Opst[Ops.e(),2])), 
                   y1 = as.numeric(as.character(Opst[Ops.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(Opst[Ops.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(Opst[Ops.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(Opst[Ops.e(),2:7])*.95, digits = 0),
                             round(max(Opst[Ops.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        Ops.P1
      })
    } else if (is.null(Ops.e()) == TRUE){
      output$OpsPlt <- renderPlotly({
        Ops.P1 <- plot_ly(data = Opst, 
                             x = factor(colnames(Opst)[3:7], 
                                        levels = c(colnames(Opst)[3], 
                                                   colnames(Opst)[4], 
                                                   colnames(Opst)[5], 
                                                   colnames(Opst)[6],
                                                   colnames(Opst)[7])
                             ),
                             y = as.numeric(Opst[1,3:7]),
                             type = "bar",
                             name = as.character(Opst[1,1]),
                          hoverinfo = 'text',
                          text = format(as.numeric(Opst[1,3:7]), big.mark = ","),
                          textposition = 'top-center',
                          marker = list(color = c('#091F2F',
                                                  '#091F2F',
                                                  '#091F2F',
                                                  '#45789C',
                                                  '#091F2F'
                          )
                          )) %>%
          layout(title = Opst[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(Opst[1,2])), 
                   y1 = as.numeric(as.character(Opst[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(Opst[1,2])),
                   text = paste("Target",format(as.numeric(as.character(Opst[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(Opst[1,2:7])*.95, digits = 0),
                             round(max(Opst[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        Ops.P1
      })
    }
  })  
  
  
  output$Ops.IB.1 <- renderInfoBox({
    Ops.1.GT <- round(((Opst[1,5]/Opst[1,2])*100)-100, digits = 1)
    Ops.1.GY <- round(((Opst[1,5]/Opst[1,6])*100)-100, digits = 1)
    Ops.1.GYT <- round(((Opst[1,5]/Opst[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (Opst[1,5] != Opst[1,2]){paste(abs(Ops.1.GT), if(Opst[1,5]>Opst[1,2]){"% Above Target"} else if(Opst[1,5]<Opst[1,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(Ops.1.GY), if(Opst[1,5] >Opst[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(Ops.1.GYT), if(Opst[1,5] >Opst[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(Opst)[5], Opst$Metric[1]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            # icon = icon("pencil", lib = "glyphicon")
            icon = icon("spray-can", lib = "font-awesome")
            )
  })
  
  output$Ops.IB.2 <- renderInfoBox({
    Ops.2.GT <- round(((Opst[2,5]/Opst[2,2])*100)-100, digits = 1)
    Ops.2.GY <- round(((Opst[2,5]/Opst[2,6])*100)-100, digits = 1)
    Ops.2.GYT <- round(((Opst[2,5]/Opst[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (Opst[2,5] != Opst[2,2]){paste(abs(Ops.2.GT), if(Opst[2,5]>Opst[2,2]){"% Above Target"} else if(Opst[2,5]<Opst[2,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(Ops.2.GY), if(Opst[2,5] >Opst[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(Ops.2.GYT), if(Opst[2,5] >Opst[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(Opst)[5], Opst$Metric[2]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("hand-holding-usd", lib = "font-awesome"))
  })
  
  output$Ops.IB.3 <- renderInfoBox({
    Ops.3.GT <- round(((Opst[3,5]/Opst[3,2])*100)-100, digits = 1)
    Ops.3.GY <- round(((Opst[3,5]/Opst[3,6])*100)-100, digits = 1)
    Ops.3.GYT <- round(((Opst[3,5]/Opst[3,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (Opst[3,5] != Opst[3,2]){paste(abs(Ops.3.GT), if(Opst[3,5]>Opst[3,2]){"% Above Target"} else if(Opst[3,5]<Opst[3,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(Ops.3.GY), if(Opst[3,5] >Opst[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(Ops.3.GYT), if(Opst[3,5] >Opst[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(Opst)[5], Opst$Metric[3]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("user-clock", lib = "font-awesome"))
  })
  
  ###### ISD ###
  
  
  get.OP.Ins <- read.csv("www/Table Outputs/EV-ISD.csv") #< this says EV because I moved ISD while running the table pull and am too lazy to rename the file it's writing to
  colnames(get.OP.Ins) <- gsub("\\.", " ", colnames(get.OP.Ins))
  colnames(get.OP.Ins) <- gsub("X", "", colnames(get.OP.Ins))
  OP.Inst <- get.OP.Ins
  output$OP.InsTbl <- DT::renderDataTable({OP.Inst[,2:7] <- format(OP.Inst[,2:7], big.mark = ",")
  OP.Inst}, 
  rownames = FALSE,
  selection = 'single',
  options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
      "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
      "}")))
  
  output$OP.InsPlt <- renderPlotly({
    OP.Ins.P1 <- plot_ly(data = OP.Inst, 
                         x = factor(colnames(OP.Inst)[3:7], 
                                    levels = c(colnames(OP.Inst)[3], 
                                               colnames(OP.Inst)[4], 
                                               colnames(OP.Inst)[5], 
                                               colnames(OP.Inst)[6],
                                               colnames(OP.Inst)[7])
                         ),
                         y = as.numeric(OP.Inst[1,3:7]),
                         type = "bar",
                         name = as.character(OP.Inst[1,1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(OP.Inst[1,3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
      layout(title = OP.Inst[1,1],
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(OP.Inst[1,2])), 
               y1 = as.numeric(as.character(OP.Inst[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(OP.Inst[1,2])),
               text = paste("Target", format(as.numeric(as.character(OP.Inst[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(OP.Inst[1,2:7])*.95, digits = 0),
                         round(max(OP.Inst[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    OP.Ins.P1
  })
  observeEvent(input$OP.InsTbl_rows_selected,{
    OP.Ins.e <- reactive({input$OP.InsTbl_rows_selected})
    if (is.null(OP.Ins.e()) == FALSE){
      output$OP.InsPlt <- renderPlotly({
        OP.Ins.P1 <- plot_ly(data = OP.Inst, 
                             x = factor(colnames(OP.Inst)[3:7], 
                                        levels = c(colnames(OP.Inst)[3], 
                                                   colnames(OP.Inst)[4], 
                                                   colnames(OP.Inst)[5], 
                                                   colnames(OP.Inst)[6],
                                                   colnames(OP.Inst)[7])
                             ),
                             y = as.numeric(OP.Inst[OP.Ins.e(),3:7]),
                             type = "bar",
                             name = as.character(OP.Inst[OP.Ins.e(),1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(OP.Inst[OP.Ins.e(),3:7]), big.mark = ","),
                             textposition = 'top-center',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             ))  %>%
          layout(title = OP.Inst[OP.Ins.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(OP.Inst[OP.Ins.e(),2])), 
                   y1 = as.numeric(as.character(OP.Inst[OP.Ins.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(OP.Inst[OP.Ins.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(OP.Inst[OP.Ins.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(OP.Inst[OP.Ins.e(),2:7])*.95, digits = 0),
                             round(max(OP.Inst[OP.Ins.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        OP.Ins.P1
      })
    } else if (is.null(OP.Ins.e()) == TRUE){
      output$OP.InsPlt <- renderPlotly({
        OP.Ins.P1 <- plot_ly(data = OP.Inst, 
                             x = factor(colnames(OP.Inst)[3:7], 
                                        levels = c(colnames(OP.Inst)[3], 
                                                   colnames(OP.Inst)[4], 
                                                   colnames(OP.Inst)[5], 
                                                   colnames(OP.Inst)[6],
                                                   colnames(OP.Inst)[7])
                             ),
                             y = as.numeric(OP.Inst[1,3:7]),
                             type = "bar",
                             name = as.character(OP.Inst[1,1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(OP.Inst[1,3:7]), big.mark = ","),
                             textposition = 'top-center',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             )) %>%
          layout(title = OP.Inst[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(OP.Inst[1,2])), 
                   y1 = as.numeric(as.character(OP.Inst[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(OP.Inst[1,2])),
                   text = paste("Target",format(as.numeric(as.character(OP.Inst[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(OP.Inst[1,2:7])*.95, digits = 0),
                             round(max(OP.Inst[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        OP.Ins.P1
      })
    }
  })
  
  ### INFO BOXES ###
  
  output$OP.Ins.IB.1 <- renderInfoBox({
    OP.Ins.1.GT <- round(((OP.Inst[1,5]/OP.Inst[1,2])*100)-100, digits = 1)
    OP.Ins.1.GY <- round(((OP.Inst[1,5]/OP.Inst[1,6])*100)-100, digits = 1)
    OP.Ins.1.GYT <- round(((OP.Inst[1,5]/OP.Inst[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(OP.Ins.1.GT), if(OP.Inst[1,5]>OP.Inst[1,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(OP.Ins.1.GY), if(OP.Inst[1,5] >OP.Inst[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(OP.Ins.1.GYT), if(OP.Inst[1,5] >OP.Inst[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(OP.Inst)[5], OP.Inst$Metric[1]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("list-alt", lib = "glyphicon"))
  })
  
  output$OP.Ins.IB.2 <- renderInfoBox({
    OP.Ins.2.GT <- round(((OP.Inst[2,5]/OP.Inst[2,2])*100)-100, digits = 1)
    OP.Ins.2.GY <- round(((OP.Inst[2,5]/OP.Inst[2,6])*100)-100, digits = 1)
    OP.Ins.2.GYT <- round(((OP.Inst[2,5]/OP.Inst[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(OP.Ins.2.GT), if(OP.Inst[2,5]>OP.Inst[2,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(OP.Ins.2.GY), if(OP.Inst[2,5] >OP.Inst[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(OP.Ins.2.GYT), if(OP.Inst[2,5] >OP.Inst[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">',paste(strwrap(paste(colnames(OP.Inst)[5], OP.Inst$Metric[2]),57), collapse="<br>"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("thermometer-quarter", lib = "font-awesome"))
  })
  
  output$OP.Ins.IB.3 <- renderInfoBox({
    OP.Ins.3.GT <- round(((OP.Inst[3,5]/OP.Inst[3,2])*100)-100, digits = 1)
    OP.Ins.3.GY <- round(((OP.Inst[3,5]/OP.Inst[3,6])*100)-100, digits = 1)
    OP.Ins.3.GYT <- round(((OP.Inst[3,5]/OP.Inst[3,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(paste(abs(OP.Ins.3.GT), if(OP.Inst[3,5]>OP.Inst[3,2]){"% Above Target"} else {"% Below Target"}),
                                      br(),
                                      paste(abs(OP.Ins.3.GY), if(OP.Inst[3,5] >OP.Inst[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(OP.Ins.3.GYT), if(OP.Inst[3,5] >OP.Inst[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(colnames(OP.Inst)[5], OP.Inst$Metric[3]), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("dashboard", lib = "glyphicon"))
  })  
  
    
###########################      STREETS     #################################
  
  ##### TRANSPORTATION ###
  get.ST.Tra <- read.csv("www/Table Outputs/STR-TRAN.csv")
  colnames(get.ST.Tra) <- gsub("\\.", " ", colnames(get.ST.Tra))
  colnames(get.ST.Tra) <- gsub("X", "", colnames(get.ST.Tra))
  ST.Trat <- get.ST.Tra
  output$ST.TraTbl <- DT::renderDataTable({ST.Trat[,2:7] <- format(ST.Trat[,2:7], big.mark = ",")
    ST.Trat}, 
                                           rownames = FALSE,
                                           selection = 'single',
                                          options = list(
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                              "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                              "}")))
  
  output$ST.TraPlt <- renderPlotly({
    ST.Tra.P1 <- plot_ly(data = ST.Trat, 
                          x = factor(colnames(ST.Trat)[3:7], 
                                     levels = c(colnames(ST.Trat)[3], 
                                                colnames(ST.Trat)[4], 
                                                colnames(ST.Trat)[5], 
                                                colnames(ST.Trat)[6],
                                                colnames(ST.Trat)[7])
                          ),
                          y = as.numeric(ST.Trat[1,3:7]),
                          type = "bar",
                          name = as.character(ST.Trat[1,1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(ST.Trat[1,3:7]), big.mark = ","),
                         textposition = 'top-center',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                         )) %>%
      layout(title = ST.Trat[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(ST.Trat[1,2])), 
               y1 = as.numeric(as.character(ST.Trat[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(ST.Trat[1,2])),
               text = paste("Target",format(as.numeric(as.character(ST.Trat[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(ST.Trat[1,2:7])*.95, digits = 0),
                         round(max(ST.Trat[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    ST.Tra.P1
  })
  observeEvent(input$ST.TraTbl_rows_selected,{
    ST.Tra.e <- reactive({input$ST.TraTbl_rows_selected})
    if (is.null(ST.Tra.e()) == FALSE){
      output$ST.TraPlt <- renderPlotly({
        ST.Tra.P1 <- plot_ly(data = ST.Trat, 
                              x = factor(colnames(ST.Trat)[3:7], 
                                         levels = c(colnames(ST.Trat)[3], 
                                                    colnames(ST.Trat)[4], 
                                                    colnames(ST.Trat)[5], 
                                                    colnames(ST.Trat)[6],
                                                    colnames(ST.Trat)[7])
                              ),
                              y = as.numeric(ST.Trat[ST.Tra.e(),3:7]),
                              type = "bar",
                              name = as.character(ST.Trat[ST.Tra.e(),1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(ST.Trat[ST.Tra.e(),3:7]), big.mark = ","),
                             textposition = 'top-center',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             ))  %>%
          layout(title = ST.Trat[ST.Tra.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(ST.Trat[ST.Tra.e(),2])), 
                   y1 = as.numeric(as.character(ST.Trat[ST.Tra.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(ST.Trat[ST.Tra.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(ST.Trat[ST.Tra.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(ST.Trat[ST.Tra.e(),2:7])*.95, digits = 0),
                             round(max(ST.Trat[ST.Tra.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        ST.Tra.P1
      })
    } else if (is.null(ST.Tra.e()) == TRUE){
      output$ST.TraPlt <- renderPlotly({
        ST.Tra.P1 <- plot_ly(data = ST.Trat, 
                              x = factor(colnames(ST.Trat)[3:7], 
                                         levels = c(colnames(ST.Trat)[3], 
                                                    colnames(ST.Trat)[4], 
                                                    colnames(ST.Trat)[5], 
                                                    colnames(ST.Trat)[6],
                                                    colnames(ST.Trat)[7])
                              ),
                              y = as.numeric(ST.Trat[1,3:7]),
                              type = "bar",
                              name = as.character(ST.Trat[1,1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(ST.Trat[1,3:7]), big.mark = ","),
                             textposition = 'top-center',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             ))  %>%
          layout(title = ST.Trat[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(ST.Trat[1,2])), 
                   y1 = as.numeric(as.character(ST.Trat[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(ST.Trat[1,2])),
                   text = paste("Target",format(as.numeric(as.character(ST.Trat[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(ST.Trat[1,2:7])*.95, digits = 0),
                             round(max(ST.Trat[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        ST.Tra.P1
      })
    }
  })
  
  output$ST.Tra.IB.1 <- renderInfoBox({
    ST.Tra.1.GT <- round(((ST.Trat[1,5]/ST.Trat[1,2])*100)-100, digits = 1)
    ST.Tra.1.GY <- round(((ST.Trat[1,5]/ST.Trat[1,6])*100)-100, digits = 1)
    ST.Tra.1.GYT <- round(((ST.Trat[1,5]/ST.Trat[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Trat[1,5] != ST.Trat[1,2]){paste(abs(ST.Tra.1.GT), if(ST.Trat[1,5]>ST.Trat[1,2]){"% Above Target"} else if(ST.Trat[1,5]<ST.Trat[1,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Tra.1.GY), if(ST.Trat[1,5] >ST.Trat[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Tra.1.GYT), if(ST.Trat[1,5] >ST.Trat[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Trat)[5], ST.Trat$Metric[1]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            #icon = icon("ok", lib = "glyphicon")
            icon = icon("stopwatch", lib = "font-awesome")
    )
  })
  
  output$ST.Tra.IB.2 <- renderInfoBox({
    ST.Tra.2.GT <- round(((ST.Trat[2,5]/ST.Trat[2,2])*100)-100, digits = 1)
    ST.Tra.2.GY <- round(((ST.Trat[2,5]/ST.Trat[2,6])*100)-100, digits = 1)
    ST.Tra.2.GYT <- round(((ST.Trat[2,5]/ST.Trat[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Trat[2,5] != ST.Trat[2,2]){paste(abs(ST.Tra.2.GT), if(ST.Trat[2,5]>ST.Trat[2,2]){"% Above Target"} else if(ST.Trat[2,5]<ST.Trat[2,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Tra.2.GY), if(ST.Trat[2,5] >ST.Trat[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Tra.2.GYT), if(ST.Trat[2,5] >ST.Trat[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Trat)[5], ST.Trat$Metric[2]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("car-crash", lib = "font-awesome"))
  })
  
  output$ST.Tra.IB.3 <- renderInfoBox({
    ST.Tra.3.GT <- round(((ST.Trat[3,5]/ST.Trat[3,2])*100)-100, digits = 1)
    ST.Tra.3.GY <- round(((ST.Trat[3,5]/ST.Trat[3,6])*100)-100, digits = 1)
    ST.Tra.3.GYT <- round(((ST.Trat[3,5]/ST.Trat[3,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Trat[3,5] != ST.Trat[3,2]){paste(abs(ST.Tra.3.GT), if(ST.Trat[3,5]>ST.Trat[3,2]){"% Above Target"} else if(ST.Trat[3,5]<ST.Trat[3,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Tra.3.GY), if(ST.Trat[3,5] >ST.Trat[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Tra.3.GYT), if(ST.Trat[3,5] >ST.Trat[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Trat)[5], ST.Trat$Metric[3]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("hand-holding-usd", lib = "font-awesome"))
  })
  
  output$ST.Tra.IB.4 <- renderInfoBox({
    ST.Tra.4.GT <- round(((ST.Trat[4,5]/ST.Trat[4,2])*100)-100, digits = 1)
    ST.Tra.4.GY <- round(((ST.Trat[4,5]/ST.Trat[4,6])*100)-100, digits = 1)
    ST.Tra.4.GYT <- round(((ST.Trat[4,5]/ST.Trat[4,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Trat[4,5] != ST.Trat[4,2]){paste(abs(ST.Tra.4.GT), if(ST.Trat[4,5]>ST.Trat[4,2]){"% Above Target"} else if(ST.Trat[4,5]<ST.Trat[4,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Tra.4.GY), if(ST.Trat[4,5] >ST.Trat[4,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Tra.4.GYT), if(ST.Trat[4,5] >ST.Trat[4,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Trat)[5], ST.Trat$Metric[4]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("user-clock", lib = "font-awesome"))
  })
  
  ##### DEPARTMENT OF PUBLIC WORKS ###
  
  get.ST.Dpw <- read.csv("www/Table Outputs/STR-DPW.csv")
  colnames(get.ST.Dpw) <- gsub("\\.", " ", colnames(get.ST.Dpw))
  colnames(get.ST.Dpw) <- gsub("X", "", colnames(get.ST.Dpw))
  ST.Dpwt <- get.ST.Dpw
  output$ST.DpwTbl <- DT::renderDataTable({ST.Dpwt[,2:7] <- format(ST.Dpwt[,2:7], big.mark = ",")
    ST.Dpwt}, 
                                          rownames = FALSE,
                                          selection = 'single',
                                          options = list(
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#091F2F', 'color': '#f2f2f2', 'font-family': 'Lora'});",
                                              "$(this.api().table().body()).css({'background-color': '#f2f2f2', 'color': '#091F2F', 'font-family': 'Lora'});",
                                              "}")))
  
  output$ST.DpwPlt <- renderPlotly({
    ST.Dpw.P1 <- plot_ly(data = ST.Dpwt, 
                         x = factor(colnames(ST.Dpwt)[3:7], 
                                    levels = c(colnames(ST.Dpwt)[3], 
                                               colnames(ST.Dpwt)[4], 
                                               colnames(ST.Dpwt)[5], 
                                               colnames(ST.Dpwt)[6],
                                               colnames(ST.Dpwt)[7])
                         ),
                         y = as.numeric(ST.Dpwt[1,3:7]),
                         type = "bar",
                         name = as.character(ST.Dpwt[1,1]),
                         hoverinfo = 'text',
                         text = format(as.numeric(ST.Dpwt[1,3:7]), big.mark = ","), 
                         textposition = 'top',
                         marker = list(color = c('#091F2F',
                                                 '#091F2F',
                                                 '#091F2F',
                                                 '#45789C',
                                                 '#091F2F'
                         )
                                       )) %>% 
      layout(title = ST.Dpwt[1,1] ,
             shapes = list(
               type = "line", 
               x0 = 0,
               x1 = 1, 
               xref = "paper",
               y0 = as.numeric(as.character(ST.Dpwt[1,2])), 
               y1 = as.numeric(as.character(ST.Dpwt[1,2])),
               line = list(color = "#FB4D42")) ,
             annotations = list(
               x = 0,
               y = as.numeric(as.character(ST.Dpwt[1,2])),
               text = paste("Target",format(as.numeric(as.character(ST.Dpwt[1,2])), big.mark = ",")),
               xref = "x",
               yref = "y",
               showarrow = FALSE,
               yanchor = "bottom",
               ax = 0,
               ay = 0
             ),
             font = list(
               family = '"Lora", serif',
               color = "#288BE4"
             ),
             yaxis = list(
               range = c(round(min(ST.Dpwt[1,2:7])*.95, digits = 0),
                         round(max(ST.Dpwt[1,2:7])*1.01, digits = 0) 
               ),
               tickformat = ",d"
             )
      )%>% config(displayModeBar = FALSE)
    ST.Dpw.P1
  })
  observeEvent(input$ST.DpwTbl_rows_selected,{
    ST.Dpw.e <- reactive({input$ST.DpwTbl_rows_selected})
    if (is.null(ST.Dpw.e()) == FALSE){
      output$ST.DpwPlt <- renderPlotly({
        ST.Dpw.P1 <- plot_ly(data = ST.Dpwt, 
                             x = factor(colnames(ST.Dpwt)[3:7], 
                                        levels = c(colnames(ST.Dpwt)[3], 
                                                   colnames(ST.Dpwt)[4], 
                                                   colnames(ST.Dpwt)[5], 
                                                   colnames(ST.Dpwt)[6],
                                                   colnames(ST.Dpwt)[7])
                             ),
                             y = as.numeric(ST.Dpwt[ST.Dpw.e(),3:7]),
                             type = "bar",
                             name = as.character(ST.Dpwt[ST.Dpw.e(),1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(ST.Dpwt[ST.Dpw.e(),3:7]), big.mark = ","),
                             textposition = 'top-center',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             )) %>%
          layout(title = ST.Dpwt[ST.Dpw.e(),1] ,
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(ST.Dpwt[ST.Dpw.e(),2])), 
                   y1 = as.numeric(as.character(ST.Dpwt[ST.Dpw.e(),2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(ST.Dpwt[ST.Dpw.e(),2])),
                   text = paste("Target",format(as.numeric(as.character(ST.Dpwt[ST.Dpw.e(),2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(ST.Dpwt[ST.Dpw.e(),2:7])*.95, digits = 0),
                             round(max(ST.Dpwt[ST.Dpw.e(),2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )%>% config(displayModeBar = FALSE)
        ST.Dpw.P1
      })
    } else if (is.null(ST.Dpw.e()) == TRUE){
      output$ST.DpwPlt <- renderPlotly({
        ST.Dpw.P1 <- plot_ly(data = ST.Dpwt, 
                             x = factor(colnames(ST.Dpwt)[3:7], 
                                        levels = c(colnames(ST.Dpwt)[3], 
                                                   colnames(ST.Dpwt)[4], 
                                                   colnames(ST.Dpwt)[5], 
                                                   colnames(ST.Dpwt)[6],
                                                   colnames(ST.Dpwt)[7])
                             ),
                             y = as.numeric(ST.Dpwt[1,3:7]),
                             type = "bar",
                             name = as.character(ST.Dpwt[1,1]),
                             hoverinfo = 'text',
                             text = format(as.numeric(ST.Dpwt[1,3:7]), big.mark = ","), 
                             textposition = 'top',
                             marker = list(color = c('#091F2F',
                                                     '#091F2F',
                                                     '#091F2F',
                                                     '#45789C',
                                                     '#091F2F'
                             )
                             )) %>%
          layout(title = ST.Dpwt[1,1],
                 shapes = list(
                   type = "line", 
                   x0 = 0,
                   x1 = 1, 
                   xref = "paper",
                   y0 = as.numeric(as.character(ST.Dpwt[1,2])), 
                   y1 = as.numeric(as.character(ST.Dpwt[1,2])),
                   line = list(color = "#FB4D42")) ,
                 annotations = list(
                   x = 0,
                   y = as.numeric(as.character(ST.Dpwt[1,2])),
                   text = paste("Target",format(as.numeric(as.character(ST.Dpwt[1,2])), big.mark = ",")),
                   xref = "x",
                   yref = "y",
                   showarrow = FALSE,
                   yanchor = "bottom",
                   ax = 0,
                   ay = 0
                 ),
                 font = list(
                   family = '"Lora", serif',
                   color = "#288BE4"
                 ),
                 yaxis = list(
                   range = c(round(min(ST.Dpwt[1,2:7])*.95, digits = 0),
                             round(max(ST.Dpwt[1,2:7])*1.01, digits = 0) 
                   ),
                   tickformat = ",d"
                 )
          )  %>% config(displayModeBar = FALSE)
        ST.Dpw.P1
      })
    }
  })
  
  output$ST.Dpw.IB.1 <- renderInfoBox({
    ST.Dpw.1.GT <- round(((ST.Dpwt[1,5]/ST.Dpwt[1,2])*100)-100, digits = 1)
    ST.Dpw.1.GY <- round(((ST.Dpwt[1,5]/ST.Dpwt[1,6])*100)-100, digits = 1)
    ST.Dpw.1.GYT <- round(((ST.Dpwt[1,5]/ST.Dpwt[1,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Dpwt[1,5] != ST.Dpwt[1,2]){paste(abs(ST.Dpw.1.GT), if(ST.Dpwt[1,5]>ST.Dpwt[1,2]){"% Above Target"} else if(ST.Dpwt[1,5]<ST.Dpwt[1,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Dpw.1.GY), if(ST.Dpwt[1,5] >ST.Dpwt[1,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Dpw.1.GYT), if(ST.Dpwt[1,5] >ST.Dpwt[1,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Dpwt)[5], ST.Dpwt$Metric[1]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("stopwatch", lib = "font-awesome")
    )
  })
  
  output$ST.Dpw.IB.2 <- renderInfoBox({
    ST.Dpw.2.GT <- round(((ST.Dpwt[2,5]/ST.Dpwt[2,2])*100)-100, digits = 1)
    ST.Dpw.2.GY <- round(((ST.Dpwt[2,5]/ST.Dpwt[2,6])*100)-100, digits = 1)
    ST.Dpw.2.GYT <- round(((ST.Dpwt[2,5]/ST.Dpwt[2,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Dpwt[2,5] != ST.Dpwt[2,2]){paste(abs(ST.Dpw.2.GT), if(ST.Dpwt[2,5]>ST.Dpwt[2,2]){"% Above Target"} else if(ST.Dpwt[2,5]<ST.Dpwt[2,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Dpw.2.GY), if(ST.Dpwt[2,5] >ST.Dpwt[2,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Dpw.2.GYT), if(ST.Dpwt[2,5] >ST.Dpwt[2,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Dpwt)[5], ST.Dpwt$Metric[2]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("envelope", lib = "font-awesome"))
  })
  
  output$ST.Dpw.IB.3 <- renderInfoBox({
    ST.Dpw.3.GT <- round(((ST.Dpwt[3,5]/ST.Dpwt[3,2])*100)-100, digits = 1)
    ST.Dpw.3.GY <- round(((ST.Dpwt[3,5]/ST.Dpwt[3,6])*100)-100, digits = 1)
    ST.Dpw.3.GYT <- round(((ST.Dpwt[3,5]/ST.Dpwt[3,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Dpwt[3,5] != ST.Dpwt[3,2]){paste(abs(ST.Dpw.3.GT), if(ST.Dpwt[3,5]>ST.Dpwt[3,2]){"% Above Target"} else if(ST.Dpwt[3,5]<ST.Dpwt[3,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Dpw.3.GY), if(ST.Dpwt[3,5] >ST.Dpwt[3,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Dpw.3.GYT), if(ST.Dpwt[3,5] >ST.Dpwt[3,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Dpwt)[5], ST.Dpwt$Metric[3]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("trash-alt", lib = "font-awesome"))
  })
  
  output$ST.Dpw.IB.4 <- renderInfoBox({
    ST.Dpw.4.GT <- round(((ST.Dpwt[4,5]/ST.Dpwt[4,2])*100)-100, digits = 1)
    ST.Dpw.4.GY <- round(((ST.Dpwt[4,5]/ST.Dpwt[4,6])*100)-100, digits = 1)
    ST.Dpw.4.GYT <- round(((ST.Dpwt[4,5]/ST.Dpwt[4,7])*100)-100, digits = 1)
    #The only way to get all of these values to show up in multiple lines per bos is to wrap things in HTML() and specify br() for each line break
    infoBox(value = shiny::HTML(paste(if (ST.Dpwt[4,5] != ST.Dpwt[4,2]){paste(abs(ST.Dpw.4.GT), if(ST.Dpwt[4,5]>ST.Dpwt[4,2]){"% Above Target"} else if(ST.Dpwt[4,5]<ST.Dpwt[4,2]){"% Below Target"})} else {paste("On Target!")},
                                      br(),
                                      paste(abs(ST.Dpw.4.GY), if(ST.Dpwt[4,5] >ST.Dpwt[4,6]){"% Above This Month Last Year"} else {"% Below This Month Last Year"}),
                                      br(),
                                      paste(abs(ST.Dpw.4.GYT), if(ST.Dpwt[4,5] >ST.Dpwt[4,7]){"% Above This Year to Date"} else {"% Below This Year to Date"}))),
            title = shiny::HTML(paste('<font size="-1">', paste(strwrap(paste(colnames(ST.Dpwt)[5], ST.Dpwt$Metric[4]),57), collapse="\n"), '</font>')),
            color = "blue",
            fill = TRUE,
            icon = icon("lightbulb", lib = "font-awesome"))
  })
})