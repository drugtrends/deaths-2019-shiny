#For ABS COD 2019 data received in Dec 2020
#N. Man
#library(shiny) #already loaded in UI
library(ggplot2)
#library(plotly) #already loaded in UI
library(shinycustomloader)

#https://appsilon.com/fast-data-loading-from-files-to-r/
# load("death_2018.Rdata")
load("death_2019.Rdata")

PlotFn <- function(Pd,Gp,Yax,Yr,Varc="age",Varl="intent",Labc="<br>Age: ",Labl="<br>Intent: ",CrudeCol=0,Crude=F) {
  # print(Yax)
  if (Yax=="num") {
    Gp <- Gp + geom_line() + aes(y=n, text=paste0(
        "Year: ",year,
        "<br>Deaths: ",n,
        Labl,get(Varl),
        Labc,get(Varc)
      )) +
      scale_y_continuous(limits=c(0, max(Pd$n, 250))) +
      labs(y="Number of deaths")
  }

  else if (Yax=="cr" | Yax=="crci") {
    if (Yax=="cr") {
      Gp <- Gp + geom_line() + aes(y=cr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n,
          "<br>Crude rate: ",round(cr, 2)," (", round(cr_lci, 2),",", round(cr_uci, 2),")",
          Labl,get(Varl),
          Labc,get(Varc)
        ))
    }
    else {
#"Warning: Ignoring unknown aesthetics: text" because it is used with geom_line but we don't want text box with the ribbon
      Gp <- Gp + geom_line(aes(y=cr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n,
          "<br>Crude rate: ",round(cr, 2)," (", round(cr_lci, 2),",",round(cr_uci, 2),")",
          Labl,get(Varl),
          Labc,get(Varc)
        ))) +
        geom_ribbon(aes(ymin=cr_lci,ymax=cr_uci), alpha=0.1,size=0)
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$cr_uci, 2.5))) +
      labs(y="Crude mortality rate per 100,000")
  }

  else if (Yax=="sr" | Yax=="srci") {
    # Pd <- subset(Pd,age=="All ages")
    if (Yax=="sr") {
      if (Crude) {
        if (CrudeCol==1) {
          # if (nrow(unique(Pd[,Varl]))==1) {
          Gp <- Gp + geom_line( aes(y=sr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),color="Age standardised") ) +
            geom_line( aes(y=cr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),color="Crude") )
        }
        else if (CrudeCol==2) {
          Gp <- Gp + geom_line( aes(y=sr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),color=paste("Age standardised",get(Varl),sep=","),linetype=paste("Age standardised",get(Varl),sep=",")) ) +
            geom_line( aes(y=cr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),color=paste("Crude",get(Varl),sep=","),linetype=paste("Crude",get(Varl),sep=",")) )
        }
        else if (CrudeCol==3) {
          Gp <- Gp + geom_line( aes(y=sr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),color=paste("Age standardised",get(Varc),sep=","),linetype=paste("Age standardised",get(Varc),sep=",")) ) +
            geom_line( aes(y=cr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),color=paste("Crude",get(Varc),sep=","),linetype=paste("Crude",get(Varc),sep=",")) )
        # }
        }
        else {
          Gp <- Gp + geom_line( aes(y=sr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),alpha="Age standardised") ) +
            geom_line(aes(y=cr, text=paste0(
              "Year: ",year,
              "<br>Deaths: ",n,
              "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
              Labl,get(Varl), Labc,get(Varc)
            ),alpha="Crude")) +
            scale_alpha_manual(values=c(1,.3))
        }
        Gp <- Gp + labs(y="Mortality rate per 100,000")
      }
      else {
        Gp <- Gp + geom_line() + aes(y=sr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
            Labl,get(Varl), Labc,get(Varc)
          ))
      }
    }
    else {
      Gp <- Gp + geom_line( aes(y=sr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n,
          "<br>Rate: ",round(sr, 2)," (", round(sr_lci, 2),",", round(sr_uci, 2),")",
          Labl,get(Varl),Labc,get(Varc)
        )) ) + geom_ribbon(aes(ymin=sr_lci, ymax=sr_uci), alpha=0.1, size=0)
    }
    if (Crude==F | Yax=="srci") {
      Gp <- Gp + labs(y="Age standardised mortality rate per 100,000")
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$sr_uci,Pd$cr_uci,2.5)))
  }

  Gp <- Gp + aes(x=year, group=1) + labs(x="Year") +
    scale_x_continuous(breaks=seq(Yr[[1]],Yr[[2]],2)) +
    theme_light() + theme(
      legend.title=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank())
  return(Gp)
}

SplitFn <- function(Pd,Gp,Vars,PageDim) {
  if (PageDim<992) {
    Gp <- Gp + facet_grid(rows=vars(get(Vars)))
  }
  else {
    Gp <- Gp + facet_grid(cols=vars(get(Vars)))
    if (PageDim<1200) {
      Gp <- Gp + theme(
        axis.text.x=element_text(angle=90),
        # axis.title.x=element_text() # vjust & lineheight not working
      )
    #   LX(0)
    #   LO("h")
    #   LY(-.2)
    }
  }
  # NB: need to have grid codes last
  Gp <- Gp + theme(strip.background=element_rect(fill="#dbefd4"),
      strip.text=element_text(color="#000000", face="bold") )
  return(Gp)
}

PlyFn <- function(Gp=gp,Lt=Legend,O=LO(),X=LX(),Y=LY()) {
    ggplotly(Gp, tooltip="text") %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2019">DrugTrends</a>, NDARC',
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
          xref="paper", yref="paper"
        ))  %>%
      add_annotations(
        text=Lt, xref="paper", yref="paper",
        x=X, xanchor="left",
        y=Y, yanchor="bottom",
        legendtitle=T, showarrow=F
      ) %>%
      layout(legend=list(orientation=O, y=Y, yanchor="top"), margin=list(b=80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
}
# Allow for site's state to be bookmarked via the url
# See https://shiny.rstudio.com/articles/bookmarking-state.html for details
enableBookmarking("url")

server <- function(input, output, session) {

  # Allow direct linking to specific tabs (with default configs)  
  observe({
  # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    #Shorten URL - https://shiny.rstudio.com/reference/shiny/latest/setBookmarkExclude.html
    setBookmarkExclude(c("plotly_hover-A","plotly_afterplot-A",
           ".clientValue-default-plotlyCrosstalkOpts","dimension","DTdrugA","ageAllA"))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  LX <- reactiveVal(1.02)
  LO <- reactiveVal("v")
  LY <- reactiveVal(0.99)
  observeEvent( eventExpr=input$dimension,handlerExpr={
    if (input$dimension<992) {
      LO("h")
      LY(-.2)
      LX(0)
    }
    else {
      LO("v")
      LY(0.99)
      LX(1.02)
    }
  })
  observeEvent( eventExpr=input$DTdrugA, ignoreInit=T, handlerExpr={
      if (input$DTdrugA==T) {
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
      else if (input$DTdrugA==F) {
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
  observeEvent( eventExpr=input$ageAllA, ignoreInit=T, handlerExpr={
      if (input$ageAllA==T) {
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
      else if (input$ageAllA==F) {
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

  # Amphetamine plot (Table 2) --------------------------------------------------------
  output$AmPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    crudeCol <- 0
    Age <- input$ageAll
    if (yax=="sr" | yax=="srci") Age <- "All ages"
    varl <- "intent"
    labl <- "<br>Intent: "
    varc <- "age"
    labc <- "<br>Age: "

    sub <- subset(COD2019_Stim, jurisdiction=="Australia" & sex=="People" & 
           drug=="Amphetamines" & intent %in% input$cod2C & #nature=="Underlying" & 
           age %in% Age & (year>=yr[1] & year<=yr[2]))
    if (input$Ashow==F | yax=="crci" | yax=="srci") {
      sub <- subset(sub, primary=="amphetamine-induced")
    }

    gp <- ggplot(sub) + scale_colour_manual(values=get("agecodcols"))
    if (length(input$cod2C)==1 | (length(input$cod2C)==2 & input$codS==2)) {
      if (length(input$cod2C)==1) gp <- gp + labs(title=paste0("Intent: ",input$cod2C) )
      if (yax=="sr" & crude) {
        # gp <- gp + scale_linetype_manual(values=agecodtype)
        Legend <- "Estimate type"
        crudeCol <- 1
      }
      else {
        gp <- gp + aes(colour=age)
        Legend <- "Age"
      }
    }
    else {
      gp <- gp + scale_linetype_manual(values=agecodtype)
      if (yax=="sr" & crude) {
        Legend <- "Estimate type by intent"
        crudeCol <- 2
      }
      else {
        gp <- gp + aes(colour=age_intent, linetype=age_intent)
        Legend <- "Age by intent"
      }
    }
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | yax=="sr") ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,.3))
      Legend <- paste0(Legend,"<br> by death data type")
      LY(.9)
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
  ####For user-defined year intervals
  #     xax <- as.numeric(input$xax)
  #     xax <- (input$yr97[[2]]-input$yr97[[1]])/xax
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,CrudeCol=crudeCol,Crude=crude) #+
  ####For user-defined year intervals
  #     function(x) unique(floor( pretty(x,n=xax) ) )
  #     scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )

    if (length(input$cod2C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
      # if (input$dimension<992) {
      #   gp <- gp + facet_grid(rows=vars(intent))
      # }
      # else {
      #   gp <- gp + facet_grid(cols=vars(intent))
      #   if (input$dimension<1200) {
      #     LX(0)
      #     LO("h")
      #     LY(-.2)
      #   }
      # }
    }
    # # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    # gp <- gp +
    #   theme(strip.background=element_rect(fill="#dbefd4"),
    #     strip.text=element_text(color="#000000", face="bold") )

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$CPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    crudeCol <- 0
    Age <- input$Cage
    if (yax=="sr" | yax=="srci") Age <- "All ages"

    sub <- subset(COD2019_Stim, drug=="Cocaine" & intent %in% input$cod2C &
        age %in% Age & sex=="People" & #nature=="Underlying" &
        jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]))
    if ( input$Ashow==F | yax=="crci" | yax=="srci" ) {
      sub <- subset(sub, primary=="cocaine-induced")
    }
    
    if (length(input$cod2C)==1 | (length(input$cod2C)==2 & input$codS==2)) {
      if (length(Age)==1) {
        gp <- ggplot(sub) + labs(title=paste0("Age: ",Age, ", Intent: ",input$cod2C))
        if (yax=="sr" & crude) {
          gp <- gp + scale_color_manual(values=age2codcols)
          Legend <- "Estimate type"
          crudeCol <- 1
        }
        else {
          Legend <- ""
        }
      }
      else {
        gp <- ggplot(sub) + aes(colour=age) + 
          scale_colour_manual(values=age2codcols)
        if (length(input$cod2C)==1) gp <- gp + labs(title=paste0("Intent: ",input$cod2C))
          Legend <- "Age"
      }
      # if (length(input$cod2C)==2 & input$codS==2) {
      #   if (input$dimension<992) {
      #     gp <- gp + facet_grid(rows=vars(intent))
      #   }
      #   else {
      #     gp <- gp + facet_grid(cols=vars(intent))
      #     if (input$dimension<1200) {
      #       LX(0)
      #       LO("h")
      #       LY(-.2)
      #     }
      #   }
      # }
    }
    else if (length(Age)==1) {
      gp <- ggplot(sub) + aes(linetype=intent) + 
        scale_linetype_manual(values=agecodtype) +
        labs(title=paste0("Age: ",Age))
      if (yax=="sr" & crude) {
        gp <- gp + scale_color_manual(values=age2codcols)
        Legend <- "Estimate type by Intent"
        crudeCol <- 2
      }
      else {
        Legend <- "Intent"
      }
    }
    else {
      gp <- ggplot(sub) + aes(colour=age_intent, linetype=age_intent) + 
        scale_colour_manual(values=age2codcols) +
        scale_linetype_manual(values=agecodtype)
      Legend <- "Age by intent"
    }
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | yax=="sr") ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,.3))
      Legend <- paste0(Legend,"<br> by death data type")
      LY(.9)
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    varl <- "intent"
    labl <- "<br>Intent: "
    varc <- "age"
    labc <- "<br>Age: "
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,CrudeCol=crudeCol,Crude=crude)

    if (length(input$cod2C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
    }
    # # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    # gp <- gp +
    #   theme(strip.background=element_rect(fill="#dbefd4"),
    #     strip.text=element_text(color="#000000", face="bold") )

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

 # All drug deaths by jurisdiction, intent, sex & age (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$AllPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    crudeCol <- 0
    Age <- input$ageAll
    if (yax=="sr" | yax=="srci") Age <- "All ages"

    if (input$dropSI=="Intent") {
      sub <- subset(COD2019_All, age %in% Age & jurisdiction==input$jur &
        intent==input$AllIcod & sex %in% input$sexC &
        (year>=yr[1] & year<=yr[2]) )
      gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
      if (length(input$sexC)==1) {
        if (yax=="sr" & crude) {
          gp <- gp +
            labs(title=paste0(input$jur,", ",input$sexC,", All ages, Intent: ",input$AllIcod) )
          Legend <- "Estimate type"
          crudeCol <- 1
        }
        else {
          gp <- gp + aes(colour=age) + 
            labs(title=paste0(input$jur,", ",input$sexC,", Intent: ",input$AllIcod) )
          Legend <- "Age"
        }
      }
      else {
        gp <- gp + scale_linetype_manual(values=agecodtype)
        if (yax=="sr" & crude) {
          gp <- gp + 
            labs(title=paste0(input$jur,", All ages, Intent: ",input$AllIcod) )
          Legend <- "Estimate type & sex"
          crudeCol <- 2
        }
        else {
          gp <- gp + aes(colour=age_sex, linetype=age_sex) +
            labs(title=paste0(input$jur,", Intent: ",input$AllIcod) )
          Legend <- "Age & sex"
        }
      }
      varl <- "sex"
      labl <- "<br>Sex: "
      varc <- "age"
      labc <- "<br>Age: "
    }
    else if (input$dropSI=="Sex") {
      if (input$sex4R!="MF") {
        sub <- subset(COD2019_All, subset=(
          age %in% Age & jurisdiction==input$jur &
          intent %in% input$AllScod & sex==input$sex4R &
          (year>=yr[1] & year<=yr[2]) ) )
        gp <- ggplot(sub)
        Title <- paste0(input$jur,", ",input$sex4R)
      }
      else {
        sub <- subset(COD2019_All, subset=(
          age %in% Age & jurisdiction==input$jur &
          intent %in% input$AllScod & sex!="People" &
          (year>=yr[1] & year<=yr[2]) ) )
        gp <- ggplot(sub)
        # if (input$dimension<992) {
        #   gp <- gp + facet_grid(rows=vars(sex))
        # }
        # else {
        #   gp <- gp + facet_grid(cols=vars(sex))
        #   if (input$dimension<1200) {
        #     LX(0)
        #     LO("h")
        #     LY(-.2)
        #   }
        # }
        Title <- input$jur
      }
      if (length(input$AllScod)==1 | (length(input$AllScod)==2 & input$codS==2 & input$sex4R!="MF")) {
        gp <- gp + scale_colour_manual(values=agecodcols)
        if (yax=="sr" & crude) {
          Title <- paste0(Title,", All ages")
          Legend <- "Estimate type"
          crudeCol <- 1
        }
        else {
          gp <- gp + aes(colour=age) 
          Legend <- "Age"
        }
        if (length(input$AllScod)==1) Title <- paste0(Title,", Intent: ",input$AllScod)
      }
      else {
        gp <- gp + scale_linetype_manual(values=agecodtype)
        if (yax=="sr" & crude) {
          gp <- gp + scale_colour_manual(values=agecodcols)
          Title <- paste0(Title,", All ages")
          Legend <- "Estimate type by intent"
          crudeCol <- 2
        }
        else if (length(Age)==1) {
          gp <- gp + aes(linetype=intent)
          Title <- paste0(Title,", Age: ",Age)
          Legend <- "Intent"
        }
        else {
          gp <- gp + aes(colour=age_intent, linetype=age_intent) + 
            scale_colour_manual(values=agecodcols)
          Legend <- "Age by intent"
        }
        gp <- gp + labs(title= Title)
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age"
      labc <- "<br>Age: "
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,CrudeCol=crudeCol,Crude=crude)

    if (input$dropSI=="Sex") {
      if (input$sex4R=="MF") {
        pageDim <- input$dimension
        gp <- SplitFn(Pd=sub,Gp=gp,Vars="sex",PageDim=pageDim)
      }
      else if ( length(input$AllScod)==2 & input$codS==2 ) {
        pageDim <- input$dimension
        gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
      }
    }
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Remoteness by jurisdiction, intent and sex (Table R) ------------------------------------------
  output$RAPlot <- renderPlotly({
    yr <- input$yr11
    yax <- input$yax
    crude <- input$crude
    if (input$jurR=="Australia") {
      regR <- input$RAra
    }
    else {
      regR <- input$Rra
    }
    sub <- subset(COD2019_Rem, jurisdiction==input$jurR & age=="All ages" &
      sex=="People" & intent %in% input$cod2C & region %in% regR &
      (year>=input$yr11[[1]] & year<=input$yr11[[2]]) )

    gp <- ggplot(sub) + aes(colour=reg_intent, linetype=reg_intent) +
        labs(title=paste0(input$jurR,", All ages") ) +
        scale_colour_manual(values=regcodcols) +
        scale_linetype_manual(values=regcodtype)
    Legend <- "Region by intent"

    varl <- "intent"
    labl <- "<br>Intent: "
    varc <- "region"
    labc <- "<br>Region: "
    validate(need(nrow(sub) > 0, "No data selected"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,Crude=crude)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$RPPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    if (input$jurR=="Australia") {
      sub <- subset(COD2019_Rem, intent==input$codR & 
            age==input$ageR &
            (year>=input$yr11[[1]] & year<=input$yr11[[2]]) & 
            sex==input$sexR & jurisdiction==input$jurR)
    }
    else {
      sub <- subset(COD2019_Rem, intent==input$codR & 
            age==input$ageR & 
            (year>=input$yr11[[1]] & year<=input$yr11[[2]]) & 
            sex=="People" & jurisdiction==input$jurR)
    }

    if (input$jurR=="Australia" & input$sexR=="People" & input$ageR=="All ages" ) {
      sub <- filter(sub, region!="Regional and Remote") %>%
        group_by(year, intent, sex, jurisdiction, age) %>% 
        distinct() %>%
        mutate(percent=round(n/sum(n)*100, 2),
          region=factor(region, levels=c( "Remote and Very Remote",
            "Outer Regional", "Inner Regional", "Major Cities"
          )))
    }
    else {
      sub <- filter(sub, region=="Regional and Remote" | region=="Major Cities" ) %>%
        group_by(year, intent, sex, jurisdiction, age) %>% 
        distinct() %>%
        mutate(percent=round(n/sum(n)*100, 2),
          region=factor(region, levels=c( "Regional and Remote",
            "Major Cities"
          )))
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    gp <- ggplot(sub, aes(x=year, y=percent, fill=region, group=1, text=paste0(
        "Year: ",year,
        "<br>Deaths: ",n,
        "<br>Percent: ",percent, "%",
        "<br>Area: ",region,
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Age: ",age,
        "<br>Sex: ",sex,
        "<br>Intent: ",intent
      ))) + geom_area() +
      labs(x="Year",y="Percent of drug-induced deaths") +
      scale_fill_manual(values=regcols) +
      scale_x_continuous(breaks=seq(input$yr11[[1]],input$yr11[[2]],2) )

    gp <- gp + theme_light() + theme(legend.title=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    PlyFn(Gp=gp,Lt="Remoteness area",X=LX(),Y=LY(),O=LO())
  })

  # All drugs by type, jurisdiction, intent and/or sex (Table 12, 12b & 12c) ------------------------------------
  output$DTJPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    Age <- input$DTage
    if (yax=="sr" | yax=="srci") Age <- "All ages"

    if (input$DTJdrop=="IntSx") {
      if (input$DTjur=="Australia") {
        if (input$DTIsex=="People") {
          DTIcod=input$DTIcod
        }
        else {
          DTIcod=input$DTIcod2
        }
        if (input$DTIsex!="MF") {
          sub <- subset(COD2019_DT, intent==DTIcod & drug %in% input$DTdrug
            & age==Age & sex==input$DTIsex & jurisdiction==input$DTjur
            & (year>=yr[1] & year<=yr[2] ))
          gp <- ggplot(sub)
          Title <- paste0(input$DTjur,", Age: ",Age,", Sex: ",input$DTIsex,", Intent: ",DTIcod)
        }
        else {
          sub <- subset(COD2019_DT, intent==DTIcod & drug %in% input$DTdrug
            & age==Age & sex!="People" & jurisdiction==input$DTjur
            & (year>=yr[1] & year<=yr[2]))
          gp <- ggplot(sub)
          # if (input$dimension<992) {
          #   gp <- gp + facet_grid(rows=vars(sex))
          # }
          # else {
          #   gp <- gp + facet_grid(cols=vars(sex))
          #   if (input$dimension<1200) {
          #     LX(0)
          #     LO("h")
          #     LY(-.2)
          #   }
          # }
          Title <- paste0(input$DTjur,", Age: ",Age,", Intent: ",DTIcod)
        }
      }
      if (input$DTjur!="Australia") {
        sub <- subset(COD2019_DT, intent==input$DTIcod2 & drug %in% input$DTdrug
          & age=="All ages" & sex=="People" & jurisdiction==input$DTjur
          & (year>=yr[1] & year<=yr[2]))
        gp <- ggplot(sub)
        Title <- paste0(input$DTjur,", Age: All ages, Sex: All persons, Intent: ",input$DTIcod2)
      }
      gp <- gp + aes(colour=drug, linetype=drug) +
        scale_colour_manual(values=dtcols) +
        scale_linetype_manual(values=dttype)
      Legend <- "Drug involved"
      
      if (input$DTIsex!="People") {
        validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Drug involved: "
    }
    
    if (input$DTJdrop=="Drug") {
      if (input$DTjur=="Australia") {
        sub <- subset(COD2019_DT, intent %in% input$cod4C & drug==input$DTdrugD & 
          age==Age & sex %in% input$sexC & jurisdiction==input$DTjur &
          (year>=yr[1] & year<=yr[2]))
        gp <- ggplot(sub) +
          scale_colour_manual(values=sexcols)
        Title <- paste0(input$DTjur,", Age: ",Age,", Drug: ",input$DTdrugD)

        if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          gp <- gp + aes(colour=sex)
          Legend <- "Sex"
          if (length(input$cod4C)==1) {
            Title <- paste0(Title,", Intent: ",input$cod4C)
          }
        }
        else {
          gp <- gp + aes(colour=sex_intent, linetype=sex_intent) +
            scale_linetype_manual(values=sexcodtype)
          Legend <- "Sex by intent"
        }

        if (input$sexC[[1]]!="People") {
          validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
        }
      }
      if (input$DTjur!="Australia") {
        sub <- subset(COD2019_DT, intent %in% input$cod2C & drug==input$DTdrugD & 
          age=="All ages" & sex=="People" & jurisdiction==input$DTjur &
          (year>=yr[1] & year<=yr[2]))
        gp <- ggplot(sub) + aes(linetype=intent) +
          scale_linetype_manual(values=agecodtype)
        Title <- paste0(input$DTjur,", Age: All ages, Sex: All persons, Drug: ",input$DTdrugD)
        Legend <- "Intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "sex"
      labc <- "<br>Sex: "
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:' to show all drugs involved.)"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,Crude=crude)

    if (input$DTJdrop=="IntSx" & input$DTjur=="Australia" & input$DTIsex=="MF") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="sex",PageDim=pageDim)
    }
    if (input$DTJdrop=="Drug" & length(input$cod4C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # All drugs by type, age & intent (Tables 12 & 12a) ----------------------------------------------------------
  output$DTAPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    crudeCol <- 0

    if (input$DTAdrop=="Age_Intent") {
      Age <- input$DTAIage
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- subset(COD2019_DT, sex=="People" & age==Age &
          drug %in% input$DTdrug & intent==input$DTAIcod & #nature=="Underlying" &
          jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]) )
      gp <- ggplot(sub) + aes(colour=drug, linetype=drug) +
        scale_colour_manual(values=dtcols) +
        scale_linetype_manual(values=dttype) +
        labs(title=paste0("Age: ",Age,", intent: ",input$DTAIcod))
      Legend <- "Drug involved"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Drug involved: "
    }
    if (input$DTAdrop=="Drug") {
      Age <- input$ageAll
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- subset(COD2019_DT, age %in% Age & sex=="People" &
          intent %in% input$cod2C & drug==input$DTdrugD & #nature=="Underlying" &
          jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]) )

      if (length(input$cod2C)==2 & input$codS==2) {
        gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
        if (yax=="sr" & crude) {
          gp <- gp +
            labs(title=paste0("Drug-induced deaths involving ",input$DTdrugD,", All ages") )
          Legend <- "Estimate type"
          crudeCol <- 1
        }
        else {
          gp <- gp + aes(colour=age_intent) +
            labs(title=paste0("Drug-induced deaths involving ",input$DTdrugD))
          Legend <- "Age"
        }
      }
      else if (length(Age)==1) {
        gp <- ggplot(sub) + scale_linetype_manual(values=agecodtype)
        if (yax=="sr" & crude) {
          gp <- gp + scale_colour_manual(values=agecodcols) +
            labs(title=paste0("Drug-induced deaths involving ",input$DTdrugD,", All ages") )
          Legend <- "Estimate type by intent"
          crudeCol <- 2
        }
        else {
          gp <- gp + aes(linetype=intent) +
            labs(title=paste0("Drug-induced deaths involving ",input$DTdrugD,", Age: ",Age))
          Legend <- "Intent"
        }
      }
      else {
        gp <- ggplot(sub) + aes(colour=age_intent, linetype=age_intent) +
          scale_colour_manual(values=agecodcols) +
          scale_linetype_manual(values=agecodtype) +
          labs(title=paste0("Drug-induced deaths involving ",input$DTdrugD))
        Legend <- "Age by intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age"
      labc <- "<br>Age: "
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:'/'Age:' to show all drugs involved/age.)"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,CrudeCol=crudeCol,Crude=crude)

    if (input$DTAdrop=="Drug" & length(input$cod2C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
    }
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O4 - by opioid, age & intent (Table 4) -----------------------------------------------------------------
  output$O4Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    crudeCol <- 0

    if (input$dropOA=="Drug") {
      Age <- input$ageAll
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- subset(COD2019_Op, subset=(sex=="People" & location=="Aus" & drug==input$OdrugO &
         intent %in% input$cod4C & age %in% Age &
         (year>=yr[1] & year<=yr[2])))
      gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
      Title <- input$OdrugO

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        if (yax=="sr" & crude) {
          # gp <- gp + scale_linetype_manual(values=agecodtype)
          Legend <- "Estimate type"
          crudeCol <- 1
        }
        else {
          gp <- gp + aes(colour=age)
          Legend <- "Age"
        }
      }
      else {
        gp <- gp + scale_linetype_manual(values=agecodtype)
        if (yax=="sr" & crude) {
          Legend <- "Estimate type by intent"
          crudeCol <- 2
        }
        else {
          gp <- gp + aes(colour=age_intent, linetype=age_intent)
          Legend <- "Age by intent"
        }
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age"
      labc <- "<br>Age: "
    }

    else if (input$dropOA=="Age") {
      Age <- input$ageOAA
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- subset(COD2019_Op, subset=(sex=="People" & location=="Aus" & drug %in% input$OdrugC &
        intent %in% input$cod4C & age==Age &
        (year>=yr[1] & year<=yr[2])))
      gp <- ggplot(sub) + scale_colour_manual(values=opcodcols)
      Title <- paste0("Age: ",Age)

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        gp <- gp + aes(colour=drug)
        Legend <- "Drug"
      }
      else {
        gp <- gp + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opcodtype)
        Legend <- "Drug by intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Opioid: "
    }

    if (length(input$cod4C)==1) {
      Title <- paste0(Title,", Intent: ",input$cod4C)
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,CrudeCol=crudeCol,Crude=crude)

    if (length(input$cod4C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
    }
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O5 - by opioid, intent & sex (Table 5)-----------------------------------------------------------------
  output$O5Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    Age <- input$ageR
    if (yax=="sr" | yax=="srci") Age <- "All ages"

    if (input$O5drop=="Opioid") {
      sub <- subset(COD2019_Op, age==Age & 
          drug==input$OdrugO & intent %in% input$cod4C & sex %in% input$sexC &
          location=="Aus" & (year>=yr[1] & year<=yr[2]))

      gp <- ggplot(sub) + scale_colour_manual(values=sexcols)
      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        gp <- gp + aes(colour=sex)
        Legend <- "Sex"
      }
      else {
        gp <- gp + aes(colour=sex_intent, linetype=sex_intent) +
          scale_linetype_manual(values=sexcodtype)
        Legend <- "Sex by intent"
      }
      Title <- input$OdrugO
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "sex"
      labc <- "<br>Sex: "
    }
    else if (input$O5drop=="Intent") {
      sub <- subset(COD2019_Op, subset=(age==Age &
        drug %in% input$OdrugC & sex %in% input$sexC & intent ==input$cod4R &
        location=="Aus" & (year>=yr[1] & year<=yr[2])))

      gp <- ggplot(sub) + aes(colour=op_sex, linetype=op_sex) +
        scale_colour_manual(values=opcodcols) +
        scale_linetype_manual(values=opcodtype)
      Legend <- "Drug by sex"
      Title <- paste0("Intent: ",input$cod4R)
      varc <- "drug"
      labc <- "<br>Opioid: "
      varl <- "sex"
      labl <- "<br>Sex: "
    }
    else if (input$O5drop=="Sex") {
      Legend <- "Drug by intent"
      if (input$sex4R!="MF") {
        sub <- subset(COD2019_Op, age==Age & location=="Aus" &
            drug %in% input$OdrugC & intent %in% input$cod4C & sex==input$sex4R &
            (year>=yr[1] & year<=yr[2]))
        
        gp <- ggplot(sub)
        Title <- paste0("Sex: ",input$sex4R)
        if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          gp <- gp + aes(colour=drug)
          Legend <- "Drug"
        }
        else {
          gp <- gp + aes(colour=op_intent, linetype=op_intent) +
            scale_linetype_manual(values=opcodtype)
        }
      }
      else {
        sub <- subset(COD2019_Op, age==Age & location=="Aus" &
            drug %in% input$OdrugC & intent %in% input$cod4C & sex!="People" &
            (year>=yr[1] & year<=yr[2]))
        
        gp <- ggplot(sub) + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opcodtype)
        Title <- "Females & Males"
        # if (input$dimension<992) {
        #   gp <- gp + facet_grid(rows=vars(sex))
        # }
        # else {
        #   gp <- gp + facet_grid(cols=vars(sex))
        #   if (input$dimension<1200) {
        #     LX(0)
        #     LO("h")
        #     LY(-.2)
        #   }
        # }
      }
      gp <- gp + scale_colour_manual(values=opcodcols)
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Opioid: "
    }

    if (input$O5drop!="Intent") {
      if (length(input$cod4C)==1) {
        Title <- paste0(Title,", Intent: ",input$cod4C)
      }
      # else if ( length(input$cod4C)==2 & input$codS==2 &
      #   ((input$O5drop=="Sex" & input$sex4R!="MF") | input$O5drop=="Opioid") ) {
      #   if (input$dimension<992) {
      #     gp <- gp + facet_grid(rows=vars(intent))
      #   }
      #   else {
      #     gp <- gp + facet_grid(cols=vars(intent))
      #     if (input$dimension<1200) {
      #       LX(0)
      #       LO("h")
      #       LY(-.2)
      #     }
      #   }
      # }
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,Crude=crude)

    Title <- paste0(Title,", Age: ",Age)
    gp <- gp + labs(title=Title) #+
      # theme(strip.background=element_rect(fill="#dbefd4"),
      #   strip.text=element_text(color="#000000", face="bold") )
    if (input$O5drop=="Sex" & input$sex4R=="MF") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="sex",PageDim=pageDim)
    }
    else if ( input$O5drop!="Intent" & length(input$cod4C)==2 & input$codS==2 ) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # O6 - by jurisdiction, sex & intent (Table 6) -------------------------------------------
  output$O6Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    Age <- input$ageR
    if (yax=="sr" | yax=="srci") Age <- "All ages"

    sub <- subset(COD2019_Op, age==Age & drug=="All opioids" &
        jurisdiction==input$jur & intent %in% input$cod2C & sex %in% input$sexC &
        (year>=yr[1] & year<=yr[2]))

    gp <- ggplot(sub) +
      aes(colour=sex_intent, linetype=sex_intent) +
      labs(title=paste0(input$jur,", ",Age) ) +
      scale_colour_manual(values=sexcols) +
      scale_linetype_manual(values=sexcodtype)

    varl <- "intent"
    labl <- "<br>Intent: "
    varc <- "sex"
    labc <- "<br>Sex: "

    validate(need(nrow(sub) > 0, "No data selected"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,Crude=crude)

    PlyFn(Gp=gp,Lt="Sex by intent",X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids with other drugs by age & intent (Table 7) ----------------------------------------------------------
  output$W7Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    crudeCol <- 0

    sub <- subset(COD2019_OpW, intent %in% input$cod4C &
      (year>=yr[1] & year<=yr[2]) & sex=="People")
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | ( yax=="sr" &
      input$dropOA=="Age" & (length(input$cod4C)>2 | (input$codS==1 & length(input$cod4C)==2)) ) ) {
      sub <- subset(sub, primary=="opioid-induced")
    }

    if (input$dropOA=="Drug") {
      Age <- input$ageAll
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- subset(sub, drug==input$W7Ddrug & age %in% Age)
  
      gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
      Title <- paste0("All opioids with ",input$W7Ddrug)

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        if (yax=="sr" & crude) {
          # gp <- gp + scale_linetype_manual(values=agecodtype)
          Legend <- "Estimate type"
          crudeCol <- 1
        }
        else {
          gp <- gp + aes(colour=age)
          Legend <- "Age"
        }
      }
      else {
        gp <- gp + scale_linetype_manual(values=agecodtype)
        if (yax=="sr" & crude) {
          Legend <- "Estimate type by intent"
          crudeCol <- 2
        }
        else {
          gp <- gp + aes(colour=age_intent, linetype=age_intent)
          Legend <- "Age by intent"
        }
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age"
      labc <- "<br>Age: "
    }
    if (input$dropOA=="Age") {
      Age <- input$ageOAA
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- subset(sub, drug %in% input$Wdrug & age==Age)
      
      gp <- ggplot(sub) + scale_colour_manual(values=opWcodcols)
      Title <- paste0("Age: ",Age)

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        if (yax=="sr" & crude) {
          gp <- gp + scale_linetype_manual(values=opWcodtype)
          Legend <- "Estimate type by drug involved"
          crudeCol <- 3
        }
        else {
          gp <- gp + aes(colour=drug)
          Legend <- "Drug involved"
        }
      }
      else {
        gp <- gp + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opWcodtype)
        Legend <- "Drug involved by intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Drug involved: "
    }
    
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | ( yax=="sr" &
      (input$dropOA=="Drug" | length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2)) )) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0(Legend,"<br> by death data type")
    }
    if (length(input$cod4C)==1) {
      Title <- paste0(Title,", Intent: ",input$cod4C)
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,CrudeCol=crudeCol,Crude=crude)

    gp <- gp + labs(title=Title) #+
      # theme(strip.background=element_rect(fill="#dbefd4"),
      #   strip.text=element_text(color="#000000", face="bold") )
    if (length(input$cod4C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
      # if (input$dimension<992) {
      #   gp <- gp + facet_grid(rows=vars(intent))
      # }
      # else {
      #   gp <- gp + facet_grid(cols=vars(intent))
      #   if (input$dimension<1200) {
      #     LX(0)
      #     LO("h")
      #     LY(-.2)
      #   }
      # }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids and other drugs by sex (Table 8) ------------------------------------------
  output$W8Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    crude <- input$crude
    crudeCol <- 0
    Age <- input$ageR
    if (yax=="sr" | yax=="srci") Age <- "All ages"

    sub <- subset(COD2019_OpW, drug %in% input$Wdrug & age==Age & 
          (year>=yr[1] & year<=yr[2]) )
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" &
      ( input$dropSI=="Intent" | (length(input$cod4C)>2 | (length(input$cod4C)==2 & (input$codS==1 | input$sex4R=="MF"))) ) ) ) {
      sub <- subset(sub, primary=="opioid-induced")
    }

    if (input$dropSI=="Sex") {
      Legend <- "Drug by intent"
      if (input$sex4R!="MF") {
        sub <- subset(sub, intent %in% input$cod4C & sex==input$sex4R )
        Title <- paste0("Age group: ",Age,",  Sex: ",input$sex4R)
        gp <- ggplot(sub) + scale_colour_manual(values=opWcodcols)
        
        if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          if (yax=="sr" & crude) {
            gp <- gp + scale_linetype_manual(values=opWcodtype)
            Legend <- "Estimate type by drug involved"
            crudeCol <- 3
          }
          else {
            gp <- gp + aes(colour=drug)
            Legend <- "Drug involved"
          }
          if (length(input$cod4C)==1) {
            Title <- paste0(Title,", Intent: ",input$cod4C)
          }
          # if (length(input$cod4C)==2 & input$codS==2) {
          #   if (input$dimension<992) {
          #     gp <- gp + facet_grid(rows=vars(intent))
          #   }
          #   else {
          #     gp <- gp + facet_grid(cols=vars(intent))
          #     if (input$dimension<1200) {
          #       LX(0)
          #       LO("h")
          #       LY(-.2)
          #     }
          #   }
          # }
        }
        else {
          gp <- gp + aes(colour=op_intent, linetype=op_intent) +
            scale_linetype_manual(values=opWcodtype)
        }
      }
      else {
        sub <- subset(sub, intent %in% input$cod4C & sex!="People" )
        if (length(input$cod4C)==1) {
          if (yax=="sr" & crude) {
            gp <- ggplot(sub) + scale_linetype_manual(values=opWcodtype)
            Legend <- "Estimate type by drug involved"
            crudeCol <- 3
          }
          else {
            gp <- ggplot(sub) + aes(colour=drug)
            Legend <- "Drug involved"
          }
        }
        else {
          gp <- ggplot(sub) + aes(colour=op_intent, linetype=op_intent) +
            scale_colour_manual(values=opWcodcols) +
            scale_linetype_manual(values=opWcodtype)
        }
        # if (input$dimension<992) {
        #   gp <- gp + facet_grid(rows=vars(sex))
        # }
        # else {
        #   gp <- gp + facet_grid(cols=vars(sex))
        #   if (input$dimension<1200) {
        #     LX(0)
        #     LO("h")
        #     LY(-.2)
        #   }
        # }
        Title=paste0("Age group: ",Age)
      }
      varc <- "drug"
      labc <- "<br>Drug: "
      varl <- "intent"
      labl <- "<br>Intent: "
    }
    if (input$dropSI=="Intent") {
      sub <- subset(sub, intent==input$cod4R & sex %in% input$sexC )
      gp <- ggplot(sub) + aes(colour=op_sex, linetype=op_sex) + 
        scale_colour_manual(values=opWcodcols) +
        scale_linetype_manual(values=opWcodtype)
      Legend <- "Drug by sex"
      Title <- paste0("Age group: ",Age,",  Intent: ",input$cod4R)
      varc <- "drug"
      labc <- "<br>Drug: "
      varl <- "sex"
      labl <- "<br>Sex: "
    }

    if ( input$Ashow==T & (yax=="num" | yax=="cr" | ( yax=="sr" &
      input$dropSI=="Sex" & length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2 & input$sex4R!="MF") )) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0(Legend,"<br> by death data type")
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,CrudeCol=crudeCol,Crude=crude)

    gp <- gp + labs(title=Title) #+
      # theme(strip.background=element_rect(fill="#dbefd4"),
      #   strip.text=element_text(color="#000000", face="bold") )
    if (input$dropSI=="Sex") {
      if (input$sex4R=="MF") {
        pageDim <- input$dimension
        gp <- SplitFn(Pd=sub,Gp=gp,Vars="sex",PageDim=pageDim)
      }
      else if ( length(input$cod4C)==2 & input$codS==2 ) {
        pageDim <- input$dimension
        gp <- SplitFn(Pd=sub,Gp=gp,Vars="intent",PageDim=pageDim)
      }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive Opioids by age and intent (Table 10) --------------------------------------------------------------
  output$E0Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    crude <- input$crude

    if (input$dropOA=="Drug") {
      Age <- input$ageAll
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- filter(COD2019_OpE, sex=="People" & location=="Aus" &
          drug==input$E0Odrug & intent %in% input$cod3C & age %in% Age &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]])) %>%
        distinct(drug, year, intent, sex, location, age, .keep_all=T)

      gp <- ggplot(sub) + aes(colour=age_intent, linetype=age_intent) +
        labs(title=input$E0Odrug) +
        scale_colour_manual(values=agecodcols) +
        scale_linetype_manual(values=agecodtype)
      Legend <- "Age by intent"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age"
      labc <- "<br>Age: "
    }

    else if (input$dropOA=="Age") {
      Age <- input$ageOAA
      if (yax=="sr" | yax=="srci") Age <- "All ages"
      sub <- subset(COD2019_OpE, subset=(sex=="People" & location=="Aus" &
          drug %in% input$Edrug & intent %in% input$cod3C & age==Age &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]))) %>%
        distinct(drug, year, intent, sex, location, age, .keep_all=T)
      
      gp <- ggplot(sub) + aes(colour=op_intent, linetype=op_intent) +
        labs(title=paste0("Age: ",Age)) +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype)
      Legend <- "Drug by intent"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Opioid type: "
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,Crude=crude)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11) -----------------------
  output$E9Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    crude <- input$crude
    Age <- input$ageR
    if (yax=="sr" | yax=="srci") Age <- "All ages"

    if (input$dropSI=="Intent") {
      sub <- filter(COD2019_OpE, jurisdiction==input$jur & age==Age &
          sex %in% input$sexC & intent==input$E9Icod & drug %in% input$Edrug & 
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
        distinct(drug, year, intent, sex, location, age, .keep_all=T)

      gp <- ggplot(sub) + aes(colour=op_sex, linetype=op_sex) +
        labs(title=paste0(input$jur,", Age: ",Age,", Intent: ",input$E9Icod) ) +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype)
      Legend <- "Drug by sex"
      varl <- "drug"
      labl <- "<br>Opioid type: "
      varc <- "sex"
      labc <- "<br>Sex: "
    }
    if (input$dropSI=="Sex") {
      if (input$sex4R!="MF") {
        sub <- filter(COD2019_OpE, jurisdiction==input$jur & age==Age &
            sex==input$sex4R & intent %in% input$cod3C & drug %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age, .keep_all=T)
        gp <- ggplot(sub) + labs(title=paste0(
          input$jur,", Age: ",Age,", Sex: ",input$sex4R) )
      }
      else {
        sub <- filter(COD2019_OpE, jurisdiction==input$jur & age==Age &
            sex!="People" & intent %in% input$cod3C & drug %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age, .keep_all=T)
        gp <- ggplot(sub) +
          labs(title=paste0(input$jur,", Age: ",Age) )
        if (input$dimension<992) {
          gp <- gp + facet_grid(rows=vars(sex))
        }
        else {
          gp <- gp + facet_grid(cols=vars(sex))
          if (input$dimension<1200) {
            LX(0)
            LO("h")
            LY(-.2)
          }
        }
      }
      gp <- gp + aes(colour=op_intent, linetype=op_intent) +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype)
      Legend <- "Drug by intent"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Opioid type: "
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=varl,Varc=varc,Labl=labl,Labc=labc,Crude=crude)

    gp <- gp + theme(strip.background=element_rect(fill="#dbefd4"),
            strip.text=element_text(color="#000000", face="bold") )
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$EPPlot <- renderPlotly({
    #needs to be sorted [order(...)]
    #weird proportions plot from 2015 onwards because of duplicates by AUS
    #- need to make distinct
    sub <- filter(COD2019_OpE, drug %in%
          c("Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids",
            "Illicit & pharmaceutical opioids",
            "Other & unspecified opioids") &
        intent==input$codR & age==input$ageR & 
        (year>=input$yr07[[1]] & year<=input$yr07[[2]]) & 
        sex==input$sexR & jurisdiction==input$jur) %>%
      group_by(year, intent, sex, jurisdiction, age) %>% 
      distinct() %>%
      mutate(
        percent=round(n/sum(n)*100, 2),
        drug=factor(drug, levels=c("Other & unspecified opioids",
                                   "Illicit & pharmaceutical opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Exclusive illicit opioids")))

    gp <- ggplot(sub, aes(x=year, y=percent, fill=drug, group=1, text=paste0(
        "Year: ",year,
        "<br>Deaths: ",n,
        "<br>Percent: ",percent,"%",
        "<br>Drug: ",drug,
        "<br>Intent: ",intent,
        "<br>Sex: ",sex))) +
      geom_area() +
      labs(x="Year", y="Percent of opioid induced deaths") +
      scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) ) +
      scale_fill_manual(values=opEcols)
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    gp <- gp + theme_light() + theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())

    PlyFn(Gp=gp,Lt="Drug",X=LX(),Y=LY(),O=LO())
  })

}
