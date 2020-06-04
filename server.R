#For ABS COD 2018 data received in Sept 2019
#N. Man
library(shiny)
#library(shinyTree)
#library(shinythemes)
library(ggplot2)
#library(ggthemes)
#library(dplyr)
#library(tidyverse) #for str_to_title but not really needed
library(plotly)
library(shinycustomloader)
#for drug type plots
#https://stackoverflow.com/questions/47062532/multiple-lines-for-text-per-legend-label-in-ggplot2
#library(stringr) #messes up ordering - not needed if already coded with \n in data

#reading RDS file format is apparently more efficient
#https://appsilon.com/fast-data-loading-from-files-to-r/
load("death_2018.RData")
#df <- readRDS("ABS_COD2018_All.rds")

PlotFn <- function(df=sub,Yr=yr,col=agecodcols) {
#  table(df$year)
  ggplot(df) + geom_line() + labs(x="Year") +
      aes(x=df[,year], group=1) +
      scale_colour_manual(values=col) +
      scale_x_continuous(breaks=seq(Yr[[1]],Yr[[2]],2) )
}
PlotY <- function(P=p,Yax=yax) {
  print(Yax)
    if (Yax=="num") {
      P <- P + aes(y=n, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", intent,
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$n, 250)))+
        labs(y="Number of deaths")
    }

    else if (Yax=="r5" | Yax=="r5ci") {
      P <- P + aes(y=rate_ht, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", intent,
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (Yax=="r5ci") {
        P <- P + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }

    else if (Yax=="r6" | Yax=="r6ci") {
      P <- P + aes(y=rate_m, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", intent,
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (Yax=="r6ci") {
        P <- P + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
}  
# Allow for site's state to be bookmarked via the url
# See https://shiny.rstudio.com/articles/bookmarking-state.html for details
enableBookmarking("url")

server <- function(input, output, session) {

  # Allow direct linking to specific tabs (with default configs)  
  observe({
# https://shiny.rstudio.com/reference/shiny/0.14/parseQueryString.html
  # query <- parseQueryString(session$clientData$url_search)
  # query1 <- paste(names(query), query, sep="=", collapse=", ")
  # print(query1)
  # https://stackoverflow.com/questions/32872222/how-do-you-pass-parameters-to-a-shiny-app-via-url
#     if (!is.null(input$Bulletin)) {
# #      updateTextInput(session, "text", value=query[2])
#     }
#can try when there is time???
    # for (i in 1:(length(reactiveValuesToList(input)))) {
    #   nameval=names(reactiveValuesToList(input)[i])
    #   valuetoupdate=query[[nameval]]
    #   
    #   if (!is.null(query[[nameval]])) {
    #     if (is.na(as.numeric(valuetoupdate))) {
    #       updateTextInput(session, nameval, value=valuetoupdate)
    #     }
    #     else {
    #       updateTextInput(session, nameval, value=as.numeric(valuetoupdate))
    #     }
    #   }
    # }

  # https://github.com/daattali/advanced-shiny/blob/master/url-inputs
    # if (!is.null(query[['Plot']])) {
    #   updateTextInput(session, "Plot", value=query[['Plot']])
    # }
  # Trigger this observer every time an input changes
    reactiveValuesToList(input)
#    print(input)
#    print(input$Plot)
#    print(input$showSidebar[[1]])
    #Shorten URL - https://shiny.rstudio.com/reference/shiny/latest/setBookmarkExclude.html
    setBookmarkExclude(c("plotly_hover-A","plotly_afterplot-A",
           ".clientValue-default-plotlyCrosstalkOpts","DTdrugA","ageAllA"))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
#trying to hide side panel - but messes up on update###############
  observeEvent(input$showSidebar, {
  #   if (input$showSidebar[[1]]%%2==0) {
  #     shinyjs::show(id="Sidebar")
  #   }
  #   else {
  #     shinyjs::hide(id="Sidebar")
  #   }
    shinyjs::toggle(id="Sidebar")
  })
  observeEvent( eventExpr=input$DTdrugA, ignoreInit=TRUE , handlerExpr={
      if (input$DTdrugA==TRUE) {
        updateCheckboxGroupInput(session,"DTdrug",NULL,choices=c(
            "OPIOIDS",
            "heroin",
            "natural & semi-synthetic opioids",
            "methadone",
            "synthetic opioids",
            "ALCOHOL",
            "AMPHETAMINES",
            "ANTIDEPRESSANTS",
            "tricyclic & tetracyclic antidepressants",
            "other & unspecified antidepressants",
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "barbiturates",
            "benzodiazepines",
            "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
            ="antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "ANTIPSYCHOTICS & NEUROLEPTICS",
            "other & unspecified antipsychotics (e.g. quetiapine)"
            ="other & unspecified antipsychotics",
            "CANNABINOIDS",
            "COCAINE",
            "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
            "other nonsteroidal anti-inflammatory drugs"
          ),selected=c(
            "OPIOIDS",
            "heroin",
            "natural & semi-synthetic opioids",
            "methadone",
            "synthetic opioids",
            "ALCOHOL",
            "AMPHETAMINES",
            "ANTIDEPRESSANTS",
            "tricyclic & tetracyclic antidepressants",
            "other & unspecified antidepressants",
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "barbiturates",
            "benzodiazepines",
            "antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "ANTIPSYCHOTICS & NEUROLEPTICS",
            "other & unspecified antipsychotics",
            "CANNABINOIDS",
            "COCAINE",
            "NONOPIOID ANALGESICS",
            "4-aminophenol derivatives",
            "other nonsteroidal anti-inflammatory drugs"
          ))
      }
      else if (input$DTdrugA==FALSE) {
        updateCheckboxGroupInput(session,"DTdrug",NULL,
          choices=c(
            "OPIOIDS",
            "heroin",
            "natural & semi-synthetic opioids",
            "methadone",
            "synthetic opioids",
            "ALCOHOL",
            "AMPHETAMINES",
            "ANTIDEPRESSANTS",
            "tricyclic & tetracyclic antidepressants",
            "other & unspecified antidepressants",
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "barbiturates",
            "benzodiazepines",
            "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
            ="antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "ANTIPSYCHOTICS & NEUROLEPTICS",
            "other & unspecified antipsychotics (e.g. quetiapine)"
            ="other & unspecified antipsychotics",
            "CANNABINOIDS",
            "COCAINE",
            "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
            "other nonsteroidal anti-inflammatory drugs"
          ),
          selected=character(0)
        )
      }
  })
  observeEvent( eventExpr=input$ageAllA, ignoreInit=TRUE , handlerExpr={
      if (input$ageAllA==TRUE) {
        updateCheckboxGroupInput(session,"ageAll",NULL,choices=c(
            "15 to 24"="15-24",
            "25 to 34"="25-34",
            "35 to 44"="35-44",
            "45 to 54"="45-54",
            "55 to 64"="55-64",
            "65 to 74"="65-74",
            "75 to 84"="75-84",
            "All ages",
            "15 to 64"="15-64"
          ),
          selected=c(
            "15-24",
            "25-34",
            "35-44",
            "45-54",
            "55-64",
            "65-74",
            "75-84",
            "All ages",
            "15-64"
          ))
      }
      else if (input$ageAllA==FALSE) {
        updateCheckboxGroupInput(session,"ageAll",NULL,choices=c(
            "15 to 24"="15-24",
            "25 to 34"="25-34",
            "35 to 44"="35-44",
            "45 to 54"="45-54",
            "55 to 64"="55-64",
            "65 to 74"="65-74",
            "75 to 84"="75-84",
            "All ages",
            "15 to 64"="15-64"
          ),
          selected=character(0)
        )
      }
  })

  # Radio buttons for dropdown list-------------------------------------------------------------------------
  #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
  #To try??? https://stackoverflow.com/questions/42169380/shiny-renderui-with-multiple-inputs
  #To try??? https://shiny.rstudio.com/gallery/dynamic-ui.html
  # observe({
  #   if (input$dropSI=="Intent")
  #     x <- selectInput("codE9I", label=NULL,
  #                      c("All", "Accidental", "Intentional", "Undetermined", "Other") )
  # 
  #   if (input$dropSI=="Sex")
  #     y <- checkboxGroupInput(
  #       "codE9S", "Intent:",
  #       c("All", "Accidental", "Intentional", "Undetermined", "Other"),
  #       selected=c("All", "Accidental", "Intentional", "Undetermined", "Other")  )
  #   if (input$dropSI=="Intent")
  #     y <- checkboxGroupInput("sexE9S", "Sex:",
  #                             choices=c("Male", "Female", "All"),
  #                             selected=c("Male", "Female", "All"))
  # 
  #   output$Control9 <- renderUI({
  #     tagList(x,y)
  #   })
  # })

####For shinyTree - need to work out how to set default values
#   output$ageA <- renderTree({
#     list(
#       "10-year age groups"=list("15-24"="",
#         "25-34"="", "35-44"="", "45-54"="",
#         "55-64"="", "65-74"="", "75-84"=""),
#       "15-64"="",
#       "All ages"="")
#   })
#   output$codA <- renderTree({
#     # intA=list("Intent:"=list(
#     #   'All'='1',
#     #   'Accidental'='2') )
#     intA=list(
#       'All'='1',
#       'Accidental'='2')
# #Trying: https://stackoverflow.com/questions/55258868/shinytree-with-default-selected-value
#      attr(intA[[1]]['All'],"stselected")=TRUE #doesn't work for selecting default but doesn't break it either
#      attr(intA[[1]],"stopened")=TRUE
#      intA
#   })

  # Amphetamine plot (Table 2) --------------------------------------------------------
  output$AmPlot <- renderPlotly({
#    df_Stim <- readRDS("ABS_COD2018_Stim.rds")
####For shinyTree
#    print( dim.data.frame(get_selected(input$ageAll,c("classid"))) )
#    print( get_selected(input$cod2C) )
  # ageA <- get_selected(input$ageAll,c("classid"))
  # codA <- get_selected(input$cod2C,c("classid"))
    sub <- subset(ABS_COD2018_Stim, subset=(jurisdiction=="Australia" & sex=="All" & 
           drug=="Amphetamines" & nature=="Underlying" & intent %in% input$cod2C & 
           age_group %in% input$ageAll & (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
  ####For user-defined year intervals
  #     yr <- as.numeric(input$xax)
  #     yr <- (input$yr97[[2]]-input$yr97[[1]])/yr
    yr <- input$yr97
    # print(yr[[1]])
    # print(yr[[2]])
    # p <- PlotFn()
    p <- ggplot(sub) + geom_line() + labs(x="Year") +
      aes(x=year, group=1) +
      scale_colour_manual(values=agecodcols) +
  ####For user-defined year intervals
  #     function(x) unique(floor( pretty(x,n=yr) ) )
      scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
    if (dim.data.frame(input$cod2C)[2]==1) {
      p <- p + aes(colour=age_group) +
        labs(title=paste0("Intent: ",input$cod2C) )
      Legend <- "Age"
    }
    else {
      p <- p + aes(colour=age_intent, linetype=age_intent) +
        scale_linetype_manual(values=agecodtype)
      Legend <- "Age by intent"
    }

    yax <- input$yax
    # PlotY()
    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", intent,
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$n, 250)))+
        labs(y="Number of deaths")
    }

    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", intent,
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }

    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", intent,
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$CPlot <- renderPlotly({
#    df_Stim <- readRDS("ABS_COD2018_Stim.rds")
    sub <- subset(ABS_COD2018_Stim, subset=(drug=="Cocaine" & intent %in% input$cod2C &
        nature=="Underlying" & age_group %in% input$Cage & sex=="All" &
        jurisdiction=="Australia" & (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
    
    p <- ggplot(sub) + aes(x=year, linetype=age_intent, colour=age_intent, group=1) +
      geom_line() + labs(x="Year") +
      scale_linetype_manual(values=agecodtype) +
      scale_colour_manual(values=agecodcols) +
      scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
    
    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Intent: ", intent,
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      )
      ) +
        scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }
    
    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Intent: ", intent,
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    
    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", intent,
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text="Age by intent", xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  ##WIP: html download - only static if html only##########################
  #output$AllDrugHtml <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename="AllDrug.html",
  #   content=function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite=TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(n=input$slider)
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file=file,
  #                       params=params,
  #                       envir=new.env(parent=globalenv())
  #     )
  # All drug deaths by jurisdiction, intent, sex & age (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$AllPlot <- renderPlotly({
    #Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html  
    if (input$dropSI=="Intent") {
      sub <- subset(ABS_COD2018_All, age_group %in% input$ageAll & jurisdiction==input$jur &
                                  intent==input$AllIcod & sex %in% input$sexC &
                                  (year>=input$yr97[[1]] & year<=input$yr97[[2]]) )
      p <- ggplot(sub) + aes(x=year) + geom_line() +
        scale_colour_manual(values=agecodcols) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) ) #function(x) unique(floor(pretty(x,high.u.bias=0,u5.bias=.1))))
      if (dim.data.frame(input$sexC)[2]==1) {
        p <- p + aes(colour=age_group, group=1) + 
          labs(title=paste0(input$jur,", Intent: ",input$AllIcod,", Sex: ",input$sexC) )
        Legend <- "Age"
      }
      else {
        p <- p + aes(colour=age_sex, linetype=age_sex, group=1) +
          scale_linetype_manual(values=agecodtype) +
          labs(title=paste0(input$jur,", Intent: ",input$AllIcod) )
        Legend <- "Age & sex"
      }
    }
    else if (input$dropSI=="Sex") {
      if (input$sex4R != "MF") {
        sub <- subset(ABS_COD2018_All, subset=(age_group %in% input$ageAll & jurisdiction==input$jur &
                                      intent %in% input$AllScod & sex==input$sex4R &
                                      (year>=input$yr97[[1]] & year<=input$yr97[[2]]) ) )
        p <- ggplot(sub)
        Title <- paste0(input$jur,", Sex: ",input$sex4R)
      }
      else {
        sub <- subset(ABS_COD2018_All, subset=(age_group %in% input$ageAll & jurisdiction==input$jur &
                                      intent %in% input$AllScod & sex != "All" &
                                      (year>=input$yr97[[1]] & year<=input$yr97[[2]]) ) )
        p <- ggplot(sub) + facet_grid(cols=vars(sex))
        Title <- input$jur
      }

      if (dim.data.frame(input$AllScod)[2]==1) {
        p <- p + aes(colour=age_group, group=1) + 
          scale_colour_manual(values=agecodcols) +
          labs(title=paste0(Title,", Intent: ",input$AllScod) )
        Legend <- "Age"
      }
      else if (dim.data.frame(input$ageAll)[2]==1) {
        p <- p + aes(linetype=intent, group=1) + 
          scale_linetype_manual(values=agecodtype) +
          labs(title=paste0(Title,", Age: ",input$ageAll) )
        Legend <- "Intent"
      }
      else {
        p <- p + aes(colour=age_intent, linetype=age_intent, group=1) + 
          scale_colour_manual(values=agecodcols) +
          scale_linetype_manual(values=agecodtype) +
          labs(title= Title )
        Legend <- "Age by intent"
      }
      p <- p + aes(x=year) + geom_line() +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2))
    }

    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
              "Year: ", year,
              "<br>Deaths: ", n,
              "<br>Intent: ", intent,
#             "<br>Jurisdiction: ", location,
              "<br>Age: ", age_group,
              "<br>Sex: ", sex
            )
      ) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }
    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht,
             text=paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Intent: ", intent, 
  #            "<br>Jurisdiction: ", location,
               "<br>Age: ", age_group,
               "<br>Sex: ", sex
             )
      ) + scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Intent: ", intent, 
    #          "<br>Jurisdiction: ", location,
               "<br>Age: ", age_group,
               "<br>Sex: ", sex
             )
      ) + scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + labs(x="Year") + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank(),
      strip.background=element_rect(fill="#a9e7bb"),
      strip.text=element_text(color="#000000", face="bold") )
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="#000000")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  # Remoteness by jurisdiction, intent and sex (Table R) ------------------------------------------
  output$RAPlot <- renderPlotly({
#    df_R <- readRDS("ABS_COD2018_Rem.rds")
    if (input$jurR=="Australia") {
      regR <- input$RAra
    }
    else {
      regR <- input$Rra
    }
    sub <- subset(ABS_COD2018_Rem, jurisdiction==input$jurR & age_group=="All ages" &
                    sex=="All" & intent %in% input$cod2C & region %in% regR &
                    (year>=input$yr11[[1]] & year<=input$yr11[[2]]) )

    p <- ggplot(sub) + aes(x=year, colour=reg_intent, linetype=reg_intent , group=1) +
        geom_line() + labs(x="Year", title=paste0(input$jurR,", All ages") ) +
        scale_colour_manual(values=regcodcols) +
        scale_linetype_manual(values=regcodtype) +
        scale_x_continuous(breaks=seq(input$yr11[[1]],input$yr11[[2]],2) )
      Legend <- "Region by intent"

    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Region: ", region,
        "<br>Intent: ", intent
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }
    
    if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Region: ", region,
        "<br>Intent: ", intent
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    
    if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), "% (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Region: ", region,
        "<br>Intent: ", intent
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits=c(0, max(sub$rate_m_ucl,.1))) +
        labs(y="Percentage of drug-induced deaths among all deaths")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$RPPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
#    df_R <- readRDS("ABS_COD2018_Rem.rds")
#    sub <- subset(df_R,DID>=50)
    if (input$jurR=="Australia") {
      sub <- subset(ABS_COD2018_Rem, intent==input$codR & 
            age_group==input$ageR &
            (year>=input$yr11[[1]] & year<=input$yr11[[2]]) & 
            sex==input$sexR & jurisdiction==input$jurR)
    }
    else {
      sub <- subset(ABS_COD2018_Rem, intent==input$codR & 
            age_group==input$ageR & 
            (year>=input$yr11[[1]] & year<=input$yr11[[2]]) & 
            sex=="All" & jurisdiction==input$jurR)
    }

#    if (input$yax != "num") {
      if (input$jurR=="Australia" & input$sexR=="All" & input$ageR=="All ages" ) {
        sub <- filter(sub, region!="Regional and Remote") %>%
          group_by(year, intent, sex, jurisdiction, age_group) %>% 
          distinct() %>%
          mutate(#alldeaths=sum(n),
                 percent=round(n/sum(n)*100, 2),
                 region=factor(region, levels=c( "Remote and Very Remote",
                   "Outer Regional", "Inner Regional", "Major Cities"
                 )))
      }
      else {
        sub <- filter(sub, region=="Regional and Remote" | region=="Major Cities" ) %>%
          group_by(year, intent, sex, jurisdiction, age_group) %>% 
          distinct() %>%
          mutate(#alldeaths=sum(n),
                 percent=round(n/sum(n)*100, 2),
                 region=factor(region, levels=c( "Regional and Remote",
                                                     "Major Cities"
                 )))
      }
      
      p <- ggplot(sub, aes(x=year, y=percent, fill=region, group=1, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Percent: ", percent, "%",
        "<br>Area: ", region,
        "<br>Jurisdiction: ", jurisdiction,
        "<br>Age: ", age_group,
        "<br>Sex: ", sex,
        "<br>Intent: ", intent
      ))) + geom_area() + labs(y="Percent of drug-induced deaths") +
      scale_fill_manual(values=regcols) #c("#d3d798", "#b3bd50", "#95a327", "#6a7d14"))
#     scale_fill_manual(values=c("#c1c870", "#748a34", "#465d02", "#1f3300"))
    # }
    # else {
    #   p <- ggplot(sub) + aes(x=year, colour=age_group, linetype=region , group=1) +
    #     geom_line() + labs(x="Year", title=paste0(input$jurR,", intent: ") ) +
    #     scale_colour_manual(values=agecodcols) +
    #     scale_linetype_manual(values=regtype) +
    #     scale_x_continuous(breaks=function(x) unique(floor(pretty(x))))
    #   Legend <- "Region by age"
    #   
    #   if (input$yaxR=="num") {
    #     p <- p + aes(y=n, text=paste0(
    #       "Year: ", year,
    #       "<br>Deaths: ", n,
    #       "<br>Region: ", region,
    #       "<br>Intent: ", intent
    #       #        ,"<br>Sex: ", sex
    #     )) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
    #       labs(y="Number of deaths")
    #   }
    #   
    # }
    p <- p + labs(x="Year") +
      scale_x_continuous(breaks=seq(input$yr11[[1]],input$yr11[[2]],2) )
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    ggplotly(p,  tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text="Remoteness area", xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
    
  })

  # All drugs by type, jurisdiction, intent and/or sex (Table 12, 12b & 12c) ----------------------------------------------------------
  output$DTJPlot <- renderPlotly({
#    df_DT <- readRDS("ABS_COD2018_DT.rds")
#  print(str_wrap(dtcols,50))
#  dtcols <- str_wrap(dtcols,50)
#  print(str_wrap("ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",50))
    LO <- "v"
    LY <- 0.99
    if (input$DTJdrop=="IntSx") {
      if (input$DTjur=="Australia") {
          if (input$DTIsex=="All") {
            DTIcod=input$DTIcod
          }
          else {
            DTIcod=input$DTIcod2
          }
          if (input$DTIsex!="MF") {
            sub <- subset(ABS_COD2018_DT, subset=(intent==DTIcod & drug %in% input$DTdrug
                          & age_group==input$DTage & sex==input$DTIsex & jurisdiction==input$DTjur
                          & (year>=input$yr97[[1]] & year<=input$yr97[[2]] ) ) )
            p <- ggplot(sub)
            Title <- paste0(input$DTjur,", Age: ",input$DTage,", Sex: ",input$DTIsex,", Intent: ",DTIcod)
          }
          else {
            sub <- subset(ABS_COD2018_DT, subset=(intent==DTIcod & drug %in% input$DTdrug
                          & age_group==input$DTage & sex != "All" & jurisdiction==input$DTjur
                          & (year>=input$yr97[[1]] & year<=input$yr97[[2]] ) ) )
            p <- ggplot(sub) + facet_grid(cols=vars(sex) )
            Title <- paste0(input$DTjur,", Age: ",input$DTage,", Intent: ",DTIcod)
            LO <- "h"
            LY <- -0.15
          }
      }
      if (input$DTjur != "Australia") {
          sub <- subset(ABS_COD2018_DT, subset=(intent==input$DTIcod2 & drug %in% input$DTdrug
                             & age_group=="All ages" & sex=="All" & jurisdiction==input$DTjur
                             & (year>=input$yr97[[1]] & year<=input$yr97[[2]] ) ) )
          p <- ggplot(sub)
          Title <- paste0(input$DTjur,", Age: All ages, Sex: All persons, Intent: ",input$DTIcod2)
      }
      p <- p + aes(x=year, colour=drug, linetype=drug, group=1) +
      geom_line() +
      scale_colour_manual(values=dtcols) +
      scale_linetype_manual(values=dttype) +
      scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Legend <- "" #"Related drug"
      
      if (input$DTIsex!="All") {
        validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
      }
    }
    
    if (input$DTJdrop=="Drug") {
      if (input$DTjur=="Australia") {
        sub <- subset(ABS_COD2018_DT, subset=(intent %in% input$cod4C & drug==input$DTdrugD & 
                    age_group==input$DTage & sex %in% input$sexC & jurisdiction==input$DTjur &
                    (year>=input$yr97[[1]] & year<=input$yr97[[2]]) ) )
        p <- ggplot(sub) + aes(x=year, group=1) + # colour=sex_intent, linetype=sex_intent,
          scale_colour_manual(values=sexcols)
        Title <- paste0(input$DTjur,", Age: ",input$DTage,", Drug: ",input$DTdrugD)

        if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
          p <- p + aes(colour=sex)
          Legend <- "Sex"
          if (dim.data.frame(input$cod4C)[2]==1) {
            Title <- paste0(Title,"; Intent: ",input$cod4C)
          }
          if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
            p <- p + facet_grid(cols=vars(intent) )
            LO <- "h"
            LY <- -0.15
          }
        }
        else {
          p <- p + aes(colour=sex_intent, linetype=sex_intent) +
            scale_linetype_manual(values=sexcodtype)
          Legend <- "Sex by intent"
        }

        if (input$sexC[[1]]!="All") {
          validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
        }
      }
      if (input$DTjur != "Australia") {
        sub <- subset(ABS_COD2018_DT, subset=(intent %in% input$cod2C & drug==input$DTdrugD & 
                     age_group=="All ages" & sex=="All" & jurisdiction==input$DTjur &
                     (year>=input$yr97[[1]] & year<=input$yr97[[2]]) ) )
        p <- ggplot(sub) + aes(x=year, linetype=intent, group=1) +
          scale_linetype_manual(values=codtype)
        Title <- paste0(input$DTjur,", Age: All ages, Sex: All persons, Drug: ",input$DTdrugD)
        Legend <- "Intent"
      }
      p <- p + geom_line() +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
    }
    
    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", toupper(substring(drug, 1,1)), tolower(substring(drug, 2)),
        "<br>Intent: ", intent,
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }
    
    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Drug: ", toupper(substring(drug, 1,1)), tolower(substring(drug, 2)),
        "<br>Intent: ", intent,
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    
    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Drug: ", drug,
        "<br>Intent: ", intent,
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + labs(x="Year", title=Title) + theme_light() +
      theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(fill="#a9e7bb"),
        strip.text=element_text(color="#000000", face="bold") )
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=.99, yanchor="bottom",
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      #      layout(legend=list(orientation="h", y=-0.15, yanchor="top"), margin=list(b=100, l=100)) %>% 
      layout(legend=list(orientation=LO, y=LY, yanchor="top"), margin=list(b=80) ) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  # All drugs by type, age & intent (Tables 12 & 12a) ----------------------------------------------------------
  output$DTAPlot <- renderPlotly({
#    df_DT <- readRDS("ABS_COD2018_DT.rds")

#Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
    if (input$DTAdrop=="Age_Intent") {
      sub <- subset(ABS_COD2018_DT, subset=(jurisdiction=="Australia" & sex=="All"
          & age_group==input$DTAIage & intent==input$DTAIcod & nature=="Underlying"
          & drug %in% input$DTdrug & (year>=input$yr97[[1]] & year<=input$yr97[[2]] ) ) )
      p <- ggplot(sub) + aes(x=year, colour=drug, linetype=drug, group=1) +
        geom_line() + labs(title=paste0("Age: ",input$DTAIage,"; intent: ",input$DTAIcod)) +
        scale_colour_manual(values=dtcols) +
        scale_linetype_manual(values=dttype) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Legend <- "Drug involved"
      LO <- "v"
      LY <- 0.99
#    LO <- "h"
#    LY <- -0.15
    }
    if (input$DTAdrop=="Drug") {
      sub <- subset(ABS_COD2018_DT, subset=(intent %in% input$cod2C & nature=="Underlying" & drug==input$DTdrugD &
          age_group %in% input$ageAll & sex=="All" & jurisdiction=="Australia" &
          (year>=input$yr97[[1]] & year<=input$yr97[[2]]) ) )
      p <- ggplot(sub) + aes(x=year, colour=age_intent, linetype=age_intent, group=1) +
        geom_line() + labs(title=paste0("Drug-induced deaths involving ",input$DTdrugD)) +
        scale_colour_manual(values=agecodcols) +
        scale_linetype_manual(values=agecodtype) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Legend <- "Age by intent"
      LO <- "v"
      LY <- 0.99
    }

    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", toupper(substring(drug, 1,1)), tolower(substring(drug, 2)),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", intent,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }

    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Drug: ", toupper(substring(drug, 1,1)), tolower(substring(drug, 2)),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", intent,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }

    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Drug: ", drug,
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", intent,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Remove vertical gridlines
    p <- p + labs(x="Year") + theme_light() +
        theme(legend.title=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank())

    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper",
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99 , yanchor="bottom",
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
#      layout(legend=list(orientation="h", y=-0.15, yanchor="top"), margin=list(b=100, l=100)) %>%
      layout(legend=list(orientation=LO, y=LY, yanchor="top"), margin=list(b=80) ) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  # O4 - by opioid, age & intent (Table 4) -----------------------------------------------------------------
  output$O4Plot <- renderPlotly({
#    df_Op <- readRDS("ABS_COD2018_Op.rds")
    if (input$dropOA=="Drug") {
      sub <- subset(ABS_COD2018_Op, subset=(sex=="All" & location=="Aus" & drug==input$OdrugO &
                 intent %in% input$cod4C & age_group %in% input$ageAll &
                 (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
      p <- ggplot(sub) + aes(x=year, group=1) +
        geom_line() + scale_colour_manual(values=agecodcols) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Title <- input$OdrugO

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        p <- p + aes(colour=age_group)
        Legend <- "Age"
      }
      else {
        p <- p + aes(colour=age_intent, linetype=age_intent) +
          scale_linetype_manual(values=agecodtype)
        Legend <- "Age by intent"
      }
    }

    else if (input$dropOA=="Age") {
      sub <- subset(ABS_COD2018_Op, subset=(sex=="All" & location=="Aus" & drug %in% input$OdrugC &
        intent %in% input$cod4C & age_group==input$ageOAA &
        (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
      p <- ggplot(sub) + aes(x=year, group=1) +
        geom_line() + scale_colour_manual(values=opcodcols) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
  #     function(x) unique(floor( pretty(x,n=yr) ) )
      Title <- paste0("Age: ",input$ageOAA)

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        p <- p + aes(colour=drug)
        Legend <- "Drug"
      }
      else {
        p <- p + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opcodtype)
        Legend <- "Drug by intent"
      }
    }

    if (dim.data.frame(input$cod4C)[2]==1) {
      Title <- paste0(Title,"; Intent: ",input$cod4C)
    }
    if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
      p <- p + facet_grid(cols=vars(intent) )
    }

    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
         "Year: ", year,
         "<br>Deaths: ", n,
         "<br>Opioid: ", drug,
         "<br>Intent: ", intent,
         "<br>Age group: ", age_group)
      ) +  scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }
    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Opioid: ", drug,
        "<br>Intent: ", intent,
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Opioid: ", drug,
        "<br>Intent: ", intent,
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + labs(x= "Year", title=Title) + theme_light() +
      theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(fill="#a9e7bb"),
        strip.text=element_text(color="#000000", face="bold") )
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  # O5 - by opioid, intent & sex (Table 5)-----------------------------------------------------------------
  output$O5Plot <- renderPlotly({
#    df_Op <- readRDS("ABS_COD2018_Op.rds")
    # sub <- subset(df_Op, subset=(age_group==input$ageR & location=="Aus" &
    #    drug %in% input$O5drug & intent==input$O5cod & sex %in% input$O5sex  &
    #     (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
    
    if (input$O5drop=="Opioid") {
      sub <- subset(ABS_COD2018_Op, subset=(age_group==input$ageR & 
          drug==input$OdrugO & intent %in% input$cod4C & sex %in% input$sexC &
          location=="Aus" & (year>=input$yr97[[1]] & year<=input$yr97[[2]])))

      p <- ggplot(sub) + aes(x=year, group=1) +
        geom_line() +
        scale_colour_manual(values=sexcols) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Title <- input$OdrugO
      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        p <- p + aes(colour=sex)
        Legend <- "Sex"
      }
      else {
        p <- p + aes(colour=sex_intent, linetype=sex_intent) +
          scale_linetype_manual(values=sexcodtype)
        Legend <- "Sex by intent"
      }
    }
    else if (input$O5drop=="Intent") {
      sub <- subset(ABS_COD2018_Op, subset=(age_group==input$ageR &
          drug %in% input$OdrugC & sex %in% input$sexC & intent ==input$cod4R & 
          location=="Aus" & (year>=input$yr97[[1]] & year<=input$yr97[[2]])))

      p <- ggplot(sub) + aes(x=year, colour=op_sex, linetype=op_sex, group=1) +
        geom_line() +
        scale_colour_manual(values=opcodcols) +
        scale_linetype_manual(values=opcodtype) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Legend <- "Drug by sex"
      Title <- paste0("Intent: ",input$OdrugO)
    }
    else if (input$O5drop=="Sex") {
      Legend <- "Drug by intent"
      if (input$sex4R != "MF") {
        sub <- subset(ABS_COD2018_Op, subset=(age_group==input$ageR & location=="Aus" &
             drug %in% input$OdrugC & intent %in% input$cod4C & sex==input$sex4R &
             (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
        
        p <- ggplot(sub)
        Title <- paste0("Sex: ",input$sex4R)
        if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
          p <- p + aes(colour=drug)
          Legend <- "Drug"
        }
        else {
          p <- p + aes(colour=op_intent, linetype=op_intent) +
            scale_linetype_manual(values=opcodtype)
        }
      }
      else {
        sub <- subset(ABS_COD2018_Op, subset=(age_group==input$ageR & location=="Aus" &
             drug %in% input$OdrugC & intent %in% input$cod4C & sex != "All" &
             (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
        
        p <- ggplot(sub) + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opcodtype) +
          facet_grid(cols=vars(sex) )

        Title <- ""
      }
      p <- p + aes(x=year, group=1) +
        geom_line() + scale_colour_manual(values=opcodcols) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
    }

    if (input$O5drop != "Intent") {
      if (dim.data.frame(input$cod4C)[2]==1) {
        Title <- paste0(Title,"; Intent: ",input$cod4C)
      }
      if ( dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2 &
        ((input$O5drop=="Sex" & input$sex4R != "MF") | input$O5drop=="Opioid") ) {
        p <- p + facet_grid(cols=vars(intent) )
      }
    }

    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Age group: ", age_group,
          "<br>Intent: ", intent,
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(x="Year", y="Number of deaths")
    }
    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Age group: ", age_group,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", intent,
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x="Year", y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Age group: ", age_group,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", intent,
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        ) ) +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(x="Year", y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + labs(x= "Year", title=Title) + theme_light() +
      theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(fill="#a9e7bb"),
        strip.text=element_text(color="#000000", face="bold") )
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })
  
  # O6 - by jurisdiction, sex & intent (Table 6) -----------------------------------------------------------------
  output$O6Plot <- renderPlotly({
#    df_Op <- readRDS("ABS_COD2018_Op.rds")
    sub <- subset(ABS_COD2018_Op, subset=(age_group==input$ageR & drug=="All opioids" & jurisdiction==input$jur &
                                     intent %in% input$cod2C & sex %in% input$sexC &
                                     (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
    #    sub$sex_intent <- paste(sub$sex,sub$intent,sep=",")
    
    p <- ggplot(sub) + aes(x=year, 
        colour=sex_intent, linetype=sex_intent, group=1) + #colour=location, linetype=sex,
      geom_line() + labs(x="Year", title=paste0(input$jur,", ",input$ageR) ) +
      scale_colour_manual(values=sexcols) + #statecols
      scale_linetype_manual(values=sexcodtype) + #sextype
      scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
    
    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", intent, 
          "<br>Jurisdiction: ", location,
          "<br>Sex: ", sex
        )) + labs(y="Number of deaths") +
        scale_y_continuous(limits=c(0, max(sub$n, 250)))
    }
    
    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", intent, 
          "<br>Jurisdiction: ", location,
          "<br>Sex: ", sex
        )) + labs(y="Deaths per 100,000") +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5)))
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    
    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", intent, 
        "<br>Jurisdiction: ", location,
        "<br>Sex: ", sex
      )
      ) + labs(y="Deaths per 1,000,000") +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25)))
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text="Sex by intent", xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })
  
  # Opioids with other drugs by age & intent (Table 7) -----------------------------------------------------------------
  output$W7Plot <- renderPlotly({
#    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    sub <- subset(ABS_COD2018_OpW, #jurisdiction=="Australia" & word(drug, start=1, end=3)=="All opioids with" &
            intent %in% input$cod4C & (year>=input$yr97[[1]] & year<=input$yr97[[2]]) & sex=="All")
    if ( is.null(input$Wshow) ) {
      sub <- subset(sub, set=="OpioidW")
    }

    if (input$dropOA=="Drug") {
      sub <- subset(sub, drug==input$W7Ddrug & age_group %in% input$ageAll)
  
      p <- ggplot(sub) + aes(x=year, group=1) + # colour=age_intent, linetype=age_intent,
        labs(x="Year") + geom_line() +
        scale_colour_manual(values=agecodcols) + #scale_linetype_manual(values=agecodtype) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Title <- paste0("All opioids with ",input$W7Ddrug)

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        p <- p + aes(colour=age_group)
        Legend <- "Age"
      }
      else {
        p <- p + aes(colour=age_intent, linetype=age_intent) +
          scale_linetype_manual(values=agecodtype)
        Legend <- "Age by intent"
      }
    }
    if (input$dropOA=="Age") {
      sub <- subset(sub, drug %in% input$Wdrug & age_group==input$ageOAA)
      
      p <- ggplot(sub) + aes(x=year, group=1) + #, colour=op_intent, linetype=op_intent
        labs(x="Year") + geom_line() +
        scale_colour_manual(values=opWcodcols) + #scale_linetype_manual(values=opWcodtype) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Title <- paste0("Age: ",input$ageOAA)

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        p <- p + aes(colour=drug)
        Legend <- "Drug"
      }
      else {
        p <- p + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opWcodtype)
        Legend <- "Drug by intent"
      }
    }
    
    if ( is.character(input$Wshow) ) {
      p <- p + aes(alpha=primary) +
        scale_alpha_manual(values=c(0.3 , 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }
    if (dim.data.frame(input$cod4C)[2]==1) {
      Title <- paste0(Title,"; Intent: ",input$cod4C)
    }
    if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
      p <- p + facet_grid(cols=vars(intent) )
    }

    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Sex: ", sex,
          "<br>Age group: ", age_group ) ) + 
        scale_y_continuous(limits=c(0, max(sub$n, 250) ) ) +
        labs(y="Number of deaths")
    }
    if (input$yax=="r5" | input$yax=="r5ci") {
        p <- p + aes(y=rate_ht, text=paste0(
            "Year: ", year,
            "<br>Deaths: ", n,
            "<br>Rate: ", round(rate_ht, 2)," (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
            "<br>Drug: ", drug,
            "<br>Intent: ", intent,
            "<br>Sex: ", sex,
            "<br>Age group: ", age_group)) + 
          scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
          labs(y="Deaths per 100,000")

        if (input$yax=="r5ci") {
          p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
        }
    }
    if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Sex: ", sex,
          "<br>Age group: ", age_group)) +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
        if (input$yax=="r6ci") {
          p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + labs(title=Title) + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(fill="#a9e7bb"),
        strip.text=element_text(color="#000000", face="bold") )

    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99 , yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })
  
  # Opioids and other drugs by sex (Table 8) ------------------------------------------
  output$W8Plot <- renderPlotly({
#    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    sub <- subset(ABS_COD2018_OpW, drug %in% input$Wdrug & age_group==input$ageR & 
          (year>=input$yr97[[1]] & year<=input$yr97[[2]]) )
    if ( is.null(input$Wshow) ) {
      sub <- subset(sub, set=="OpioidW")
    }

    if (input$dropSI=="Sex") {
      Legend <- "Drug by intent"
      if (input$sex4R != "MF") {
        sub <- subset(sub, intent %in% input$cod4C & sex==input$sex4R )
        Title <- paste0("Age group:",input$ageR,"  Sex: ",input$sex4R)
        p <- ggplot(sub) + aes(x=year, group=1) + geom_line() +
          scale_colour_manual(values=opWcodcols) +
          scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
        
        if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
          p <- p + aes(colour=drug)
          Legend <- "Drug"
          if (dim.data.frame(input$cod4C)[2]==1) {
            Title <- paste0(Title,"; Intent: ",input$cod4C)
          }
          if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
            p <- p + facet_grid(cols=vars(intent) )
          }
        }
        else {
          p <- p + aes(colour=op_intent, linetype=op_intent) +
            scale_linetype_manual(values=opWcodtype)
        }

      }
      else {
        sub <- subset(sub, intent %in% input$cod4C & sex != "All" )
        p <- ggplot(sub) + aes(x=year, colour=op_intent,
            linetype=op_intent, group=1) + 
          scale_linetype_manual(values=opWcodtype) +
          facet_grid(cols=vars(sex))
        Title=paste0("Age group:",input$ageR)
      }
    }
    if (input$dropSI=="Intent") {
      sub <- subset(sub, intent==input$cod4R & sex %in% input$sexC )
      p <- ggplot(sub) + aes(x=year, colour=op_sex, linetype=op_sex, group=1) + 
        geom_line() + 
        scale_colour_manual(values=opWcodcols) +
        scale_linetype_manual(values=opWcodtype) +
        scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )
      Legend <- "Drug by sex"
      Title <- paste0("Age group:",input$ageR,"  Intent: ",input$cod4R)
    }

    if ( is.character(input$Wshow) ) {
      p <- p + aes(alpha=primary) +
        scale_alpha_manual(values=c(0.3 , 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Age group: ", age_group,
          "<br>Sex: ", sex)) +
        scale_y_continuous(limits=c(0, max(sub$n, 250))) + labs(y="Number of deaths")
    }
    if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Age group: ", age_group,
          "<br>Sex: ", sex
        )) + geom_line() +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2)," (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Sex: ", sex
        )) + geom_line() +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
        if (input$yax=="r6ci") {
          p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + labs(x="Year", title=Title) +
      theme_light() + theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(fill="#a9e7bb"),
        strip.text=element_text(color="#000000", face="bold") )
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
    
  })

  # Exclusive Opioids by age and intent (Table 10) -----------------------------------------------------------------
  output$E0Plot <- renderPlotly({
#    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
      if (input$dropOA=="Drug") {
      sub <- filter(ABS_COD2018_OpE, sex=="All" & location=="Aus" &
          drug==input$E0Odrug & intent %in% input$cod3C & age_group %in% input$ageAll &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]])) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all=TRUE)
#        sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      
      p <- ggplot(sub) + aes(x=year, colour=age_intent, linetype=age_intent, group=1) +
        geom_line() + labs(x="Year", title=input$E0Odrug) +
        scale_colour_manual(values=agecodcols) +
        scale_linetype_manual(values=agecodtype) +
        scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) )
      Legend <- "Age by intent"
    }
    
    else if (input$dropOA=="Age") {
      sub <- subset(ABS_COD2018_OpE, subset=(sex=="All" & location=="Aus" &
          drug %in% input$Edrug & intent %in% input$cod3C & age_group==input$ageOAA &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]))) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all=TRUE)
      
      p <- ggplot(sub) + aes(x=year, colour=op_intent, linetype=op_intent, group=1) +
        geom_line() + labs(x="Year", title=paste0("Age: ",input$ageOAA)) +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype) +
        scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) )
      Legend <- "Drug by intent"
    }
    
    if (input$yax=="num") {
      p <- p + aes(y=n,
        text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Opioid: ", drug,
          "<br>Intent: ", intent,
          "<br>Age group: ", age_group)) +
        scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }
    
    else if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Opioid: ", drug,
          "<br>Intent: ", intent,
          "<br>Age group: ", age_group)) +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }
    
    else if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Opioid: ", drug,
          "<br>Intent: ", intent,
          "<br>Age group: ", age_group)) +
        scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })

  # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11) ------------------------------------------
  output$E9Plot <- renderPlotly({
#    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
    if (input$dropSI=="Intent") {
      sub <- filter(ABS_COD2018_OpE, jurisdiction==input$jur & age_group==input$ageR &
          sex %in% input$sexC & intent==input$E9Icod & drug %in% input$Edrug & 
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all=TRUE)

      p <- ggplot(sub) + aes(x=year, colour=op_sex, linetype=op_sex, group=1) +
        geom_line() + labs(x="Year", title=paste0(input$jur,", Age: ",input$ageR," Intent: ",input$E9Icod) ) +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype) +
        scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) )
      Legend <- "Drug by sex"
    }
    if (input$dropSI=="Sex") {
      if (input$sex4R != "MF") {
        sub <- filter(ABS_COD2018_OpE, jurisdiction==input$jur & age_group==input$ageR &
            sex==input$sex4R & intent %in% input$cod3C & drug %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all=TRUE)
        p <- ggplot(sub) + labs(x="Year", title=paste0(
          input$jur,", Age: ",input$ageR," Sex: ",input$sex4R) )
      }
      else {
        sub <- filter(ABS_COD2018_OpE, jurisdiction==input$jur & age_group==input$ageR &
            sex != "All" & intent %in% input$cod3C & drug %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all=TRUE)
        p <- ggplot(sub) + facet_grid(cols=vars(sex)) +
          labs(x="Year", title=paste0(input$jur,", Age: ",input$ageR) )
      }
      p <- p + aes(x=year, colour=op_intent, linetype=op_intent, group=1) +
        geom_line() +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype) +
        scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) )
      Legend <- "Drug by intent"
    }


    if (input$yax=="num") {
      p <- p + aes(y=n, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Sex: ", sex
        )) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
        labs(y="Number of deaths")
    }
    
    if (input$yax=="r5" | input$yax=="r5ci") {
      p <- p + aes(y=rate_ht, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2)," (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Sex: ", sex)) +
        scale_y_continuous(limits=c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y="Deaths per 100,000")
      if (input$yax=="r5ci") {
        p <- p + geom_ribbon(aes(ymin=rate_ht_lcl, ymax=rate_ht_ucl), alpha=0.1, size=0)
      }
    }

    if (input$yax=="r6" | input$yax=="r6ci") {
      p <- p + aes(y=rate_m, text=paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Drug: ", drug,
          "<br>Intent: ", intent,
          "<br>Sex: ", sex
        )) + scale_y_continuous(limits=c(0, max(sub$rate_m_ucl, 25))) +
        labs(y="Deaths per 1,000,000")
      if (input$yax=="r6ci") {
        p <- p + geom_ribbon(aes(ymin=rate_m_lcl, ymax=rate_m_ucl), alpha=0.1, size=0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
  # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title=element_blank()) +
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank(),
            strip.background=element_rect(fill="#a9e7bb"),
            strip.text=element_text(color="#000000", face="bold") )
    
    ggplotly(p, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text=Legend, xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$EPPlot <- renderPlotly({
    #needs to be sorted [order(...)]
    #weird proportions plot from 2015 onwards because of duplicates by AUS
    #- need to make distinct
#    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
    sub <- filter(ABS_COD2018_OpE, drug %in%
          c("Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids",
            "Illicit & pharmaceutical opioids",
            "Other & unspecified opioids") &
        intent==input$codR & age_group==input$ageR & 
        (year>=input$yr07[[1]] & year<=input$yr07[[2]]) & 
        sex==input$sexR & jurisdiction==input$jur) %>% # & table!="10" & table!="18"
      group_by(year, intent, sex, jurisdiction, age_group) %>% 
      distinct() %>%
      mutate(#alldeaths=sum(n),
        percent=round(n/sum(n)*100, 2),
        drug=factor(drug, levels=c("Other & unspecified opioids",
                                   "Illicit & pharmaceutical opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Exclusive illicit opioids")))

    p <- ggplot(sub, aes(x=year, y=percent, fill=drug, group=1, text=paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Percent: ", percent,"%",
        "<br>Drug: ", drug,
        "<br>Intent: ", intent,
        "<br>Sex: ", sex))) +
      geom_area() +
      labs(x="Year", y="Percent of opioid induced deaths") +
      theme_light() + 
      theme(legend.title=element_blank()) + 
      scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) ) +
      scale_fill_manual(values=opEcols) #c("#d3d798", "#b3bd50", "#95a327", "#6a7d14"))
#      scale_fill_manual(values=c("#c1c870", "#748a34", "#465d02", "#1f3300"))
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x=element_blank(),
                   panel.grid.major.x=element_blank())

    ggplotly(p,  tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper", 
          xanchor="left", yanchor="bottom"
        ))  %>%
      add_annotations(
        text="Drug", xref="paper", yref="paper",
        x=1.02, xanchor="left",
        y=0.99, yanchor="bottom", # Same y as legend below
        legendtitle=TRUE, showarrow=FALSE
      ) %>%
      layout(legend=list(y=0.99, yanchor="top"), margin=list(b=80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
    
  })
  
  #  output$AmphOD <- renderTable(readRDS("Amph_OD.rds"))
  #  output$CocaineOD <- renderTable(readRDS("Cocaine_OD.rds"))
  #  output$AllOD <- renderTable(readRDS("All_OD.rds"))
  #  output$RemOD <- renderTable(readRDS("Rem_OD.rds"))
  #  output$Rem_OD <- renderTable(readRDS("Rem_OD.rds"))
  #  output$DTOD <- renderTable(readRDS("DT_OD.rds"), sanitize.text.function=function(x){x})
  #  output$DT_OD <- renderTable(readRDS("DT_OD.rds"), sanitize.text.function=function(x){x})
  
}
