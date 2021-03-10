#For ABS COD 2019 data received in Dec 2020
#N. Man
#library(shiny) #already loaded in UI
library(ggplot2)
#library(plotly) #already loaded in UI
library(shinycustomloader)

#https://appsilon.com/fast-data-loading-from-files-to-r/
# load("death_2018.Rdata")
load("death_2019.Rdata")

ColtypFn <- function(Pd,Gp,Yax,Varc="Age",Vart="Intent",Split="",EstForm="",RVData=0) {
  print(paste(EstForm,EstForm>"Colo",Varc,Vart,EstForm,RVData))
  # Gp <- ggplot(data=Pd)
  if (( (Varc!="" & Vart!="") | EstForm>"Colo" ) & Split!=Varc & Split!=Vart) {
    Var <- paste0(Varc,Vart)
    if (Varc==Vart) Var <- Varc
    Gp <- Gp + scale_colour_manual( values=get(paste0(Var,"cols")) ) +
      scale_linetype_manual( values=get(paste0(Var,"type")) )
    if (EstForm<="Alph") {
      Gp <- Gp + aes(colour=get(Var), linetype=get(Var))
    }
  }
  else if (( Varc!="" | EstForm=="Colo" ) & Split!=Varc) {
    Gp <- Gp + scale_colour_manual( values=get(paste0(Varc,"cols")) )
    if (Varc!="") Gp <- Gp + aes(colour=get(Varc))
  }
  else if ( (Vart!="" ) & Split!=Vart ) {
    Gp <- Gp + aes(linetype=get(Vart)) +
      scale_linetype_manual( values=get(paste0(Vart,"type")) )
  }
  else if (EstForm=="" & RVData>0) {
    if (RVData==2) {
      Gp <- Gp + aes(color=Release) +
        scale_color_manual(values=Notecolo)
    }
    Gp <- Gp + scale_shape_manual(values=Noteshap) +
      geom_point(data=subset(Pd,Note!=""),aes(shape=Note),size=2)
  }
  return(Gp)
}

PlotFn <- function(Pd,Gp,Yax,Yr,Varc="",Vart="",Labc,Labt,EstForm="") {
  if (Yax=="num") {
    Gp <- Gp + geom_line() + aes(y=n, text=paste0(
        "Year: ",year,
        "<br>Deaths: ",n,
        "<br>",Labt,": ",get(Labt),
        "<br>",Labc,": ",get(Labc)
      )) +
      scale_y_continuous(limits=c(0, max(Pd$n, 250))) +
      labs(y="Number of deaths")
  }

  else if (Yax=="cr" | Yax=="crci") {
    if (Yax=="cr") {
      Gp <- Gp + geom_line() + aes(y=cr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n, " (last: ",n_2018,")",
          "<br>Crude rate: ",round(cr, 2)," (", round(cr_lci, 2),",", round(cr_uci, 2),")",
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc)
        ))
    }
    else {
#"Warning: Ignoring unknown aesthetics: text" because it is used with geom_line but we don't want text box with the ribbon
      Gp <- Gp + geom_line(aes(y=cr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n, " (last: ",n_2018,")",
          "<br>Crude rate: ",round(cr, 2)," (", round(cr_lci, 2),",",round(cr_uci, 2),")",
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc)
        ))) +
        geom_ribbon(aes(ymin=cr_lci,ymax=cr_uci), alpha=0.1,size=0)
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$cr_uci, 2.5))) +
      labs(y="Crude mortality rate per 100,000")
  }

  else if (Yax=="sr" | Yax=="srci") {
    # Pd <- subset(Pd,Age=="All ages")
    if (Yax=="sr") {
      if (EstForm=="Alph") {
        Gp <- Gp + geom_line( aes(y=sr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),alpha="Age standardised") ) +
          geom_line(aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),alpha="Crude")) +
          scale_alpha_manual(values=c(1,.3))
      }
      else if (EstForm=="Colo") {
        # if (nrow(unique(Pd[,Labt]))==1) {
        Gp <- Gp + geom_line( aes(y=sr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color="Age standardised") ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color="Crude") )
      }
      else if (EstForm=="Colo2") {
        Gp <- Gp + geom_line( aes(y=sr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Age standardised",get(Labt),sep=","),linetype=paste("Age standardised",get(Labt),sep=",")) ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Crude",get(Labt),sep=","),linetype=paste("Crude",get(Labt),sep=",")) )
      }
      else if (EstForm=="Type2") {
        Gp <- Gp + geom_line( aes(y=sr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Age standardised",get(Labc),sep=","),linetype=paste("Age standardised",get(Labc),sep=",")) ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Crude rate: ",round(cr,2)," (", round(cr_lci,2),",",round(cr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Crude",get(Labc),sep=","),linetype=paste("Crude",get(Labc),sep=",")) )
      }
      else {
        Gp <- Gp + geom_line() + aes(y=sr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, " (last: ",n_2018,")",
            "<br>Age standardised rate: ",round(sr,2)," (",round(sr_lci,2),",",round(sr_uci,2),")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ))
      }
    }
    else {
      Gp <- Gp + geom_line( aes(y=sr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n, " (last: ",n_2018,")",
          "<br>Age standardised rate: ",round(sr, 2)," (", round(sr_lci, 2),",", round(sr_uci, 2),")",
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc)
        )) ) + geom_ribbon(aes(ymin=sr_lci, ymax=sr_uci), alpha=0.1, size=0)
    }
    if (EstForm=="") {
      Gp <- Gp + labs(y="Age standardised mortality rate per 100,000")
    }
    else {
      Gp <- Gp + labs(y="Mortality rate per 100,000")
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$sr_uci,Pd$cr_uci,2.5)))
  }

  Gp <- Gp + labs(x="Year") + aes(x=year, group=1) +
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
        axis.text.x=element_text(angle=90)
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
      # if (Split!="" & input$dimension<1200) {
      #   LX(0)
      #   LO("h")
      #   LY(-.2)
      # }
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
          "85+"="85+",
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
          "85+"="85+",
          "All ages",
          "15-64"
        )
      )
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
          "85+"="85+",
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
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageAll
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    INTENT <- input$cod2C

    pd <- subset(COD2019_Stim, jurisdiction=="Australia" & Sex=="People" & 
           Drug=="Amphetamines" & Intent %in% INTENT & #nature=="Underlying" & 
           Age %in% AGE & (year>=yr[1] & year<=yr[2]))
    if (input$Ashow==F | yax=="crci" | yax=="srci") {
      pd <- subset(pd, primary=="amphetamine-induced")
    }

    Title <- "Australia"
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    gp <- ggplot(pd) + labs(title=Title)

    varc <- "Age"
    labc <- "Age"
    vart <- "Intent"
    labt <- "Intent"
    Split <- ""
    if (length(AGE)==1 & estimateForm=="") varc <- ""
    # gp <- ggplot(pd) + scale_colour_manual(values=get("AgeIntentcols"))
    if (length(INTENT)==1 | (length(INTENT)==2 & input$codS==2)) {
      if (length(INTENT)==1) {
        vart <- ""
      } else {
        Split <- vart
      }
      if (estimateForm!="") {
        # gp <- gp + scale_linetype_manual(values=AgeIntenttype)
        Legend <- "Estimate type"
        estimateForm <- "Colo"
      }
      else {
        # gp <- gp + aes(colour=Age)
        Legend <- "Age"
      }
    }
    else {
      # gp <- gp + scale_linetype_manual(values=AgeIntenttype)
      if (estimateForm!="") {
        Legend <- "Estimate type by Intent"
        estimateForm <- "Colo2"
      }
      else {
        # gp <- gp + aes(colour=AgeIntent, linetype=AgeIntent)
        Legend <- "Age by Intent"
      }
    }

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | yax=="sr") ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,.3))
      Legend <- paste0(Legend,"<br> by death data type")
      LY(.9)
      rvData <- 0
    }
    if (varc=="" & vart=="" & estimateForm=="" & rvData==1) {
      Legend <- "Revision\nversion"
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm,RVData=rvData)
  ####For user-defined year intervals
  #     xax <- as.numeric(input$xax)
  #     xax <- (input$yr97[[2]]-input$yr97[[1]])/xax
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm) #+
  ####For user-defined year intervals
  #     function(x) unique(floor( pretty(x,n=xax) ) )
  #     scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )

    if (Split!="") {
      pageDim <- input$dimension
      # print(Split)
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
      # if (input$dimension<992) {
      #   gp <- gp + facet_grid(rows=vars(Intent))
      # }
      # else {
      #   gp <- gp + facet_grid(cols=vars(Intent))
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
    # ggplotly(gp, tooltip="text")
  })

  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$CPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$Cage
    if (yax=="sr" | yax=="srci") AGE <- " All ages"
    INTENT <- input$cod2C

    pd <- subset(COD2019_Stim, Drug=="Cocaine" & Intent %in% INTENT &
        Age %in% AGE & Sex=="People" & #nature=="Underlying" &
        jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]))
    if ( input$Ashow==F | yax=="crci" | yax=="srci" ) {
      pd <- subset(pd, primary=="cocaine-induced")
    }

    Title <- "Australia"
    if (length(AGE)==1) Title <- paste0(Title,", Age:",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    gp <- ggplot(pd) + labs(title=Title)

    varc <- "Age"
    labc <- "Age"
    vart <- "Intent"
    labt <- "Intent"
    if (length(AGE)==1 & estimateForm=="") varc <- ""
    Split <- ""
    Legend <- ""
    if (length(INTENT)==1 | (length(INTENT)==2 & input$codS==2)) {
      if (length(INTENT)>1) {
        Split <- vart
      }
      # else {
      # }
      vart <- ""
      if (estimateForm!="") {
        # gp <- gp + scale_color_manual(values=AgeIntentcols)
        Legend <- "Estimate type"
        estimateForm <- "Colo"
      }
      else if (length(AGE)>1) {
        # gp <- gp + aes(colour=Age) + 
        #   scale_colour_manual(values=AgeIntentcols)
        Legend <- "Age"
      }
    }
    else {
      Legend <- "Intent"
      if (estimateForm!="") {
        # varc <- ""
        # gp <- gp + scale_color_manual(values=AgeIntentcols)
        Legend <- "Estimate type by Intent"
        estimateForm <- "Colo2"
      }
      else if (length(AGE)>1) {
        Legend <- "Age by Intent"
      }
    }

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | yax=="sr") ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,.3))
      Legend <- paste0(Legend,"<br> by death data type")
      LY(.9)
      rvData <- 0
    }
    if (varc=="" & vart=="" & estimateForm=="" & rvData==1) {
      Legend <- "Release\nversion"
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    # vart <- "Intent"
    # labt <- "Intent"
    # varc <- "Age"
    # labc <- "Age"
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm,RVData=rvData)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm)

    if (length(INTENT)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars="Intent",PageDim=pageDim)
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

 # All drug deaths by jurisdiction, Intent, Sex & Age (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$AllPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageAll
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    if (input$dropSI=="Intent") {
      INTENT <- input$AllIcod
      SEX <- input$sexC
    }
    else if (input$dropSI=="Sex") {
      INTENT <- input$AllScod
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
    }

    Title <- input$jur
    if (length(SEX)==1) Title <- paste0(Title,", ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)

    varc <- "Age"
    labc <- "Age"
    if (length(AGE)==1 & estimateForm=="") varc <- ""
    if (input$dropSI=="Intent") {
      vart <- "Sex"
      labt <- "Sex"
      # gp <- ggplot(pd) + scale_colour_manual(values=AgeSexcols)
      if (length(SEX)==1) {
        vart <- ""
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else if (length(AGE)==1) {
          Legend <- ""
        }
        else {
          # gp <- gp + aes(colour=Age)
          Legend <- "Age"
        }
      }
      else {
        # gp <- gp + scale_linetype_manual(values=AgeSextype)
        if (estimateForm!="") {
          Legend <- "Estimate type & Sex"
          estimateForm <- "Colo2"
        }
        else if (length(AGE)==1) {
          Legend <- "Sex"
        }
        else {
          # gp <- gp + aes(colour=AgeSex, linetype=AgeSex)
          Legend <- "Age & Sex"
        }
      }
    }
    else if (input$dropSI=="Sex") {
      vart <- "Intent"
      labt <- "Intent"
      # if (input$sex4R!="MF") {
      # }
      # else {
      # }
      if (length(input$AllScod)==1 | (length(input$AllScod)==2 & input$codS==2 & input$sex4R!="MF")) {
        vart <- ""
        # gp <- gp + scale_colour_manual(values=AgeIntentcols)
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else if (length(AGE)==1) {
          Legend <- ""
        }
        else {
          # gp <- gp + aes(colour=Age) 
          Legend <- "Age"
        }
        if (length(input$AllScod)>1 & Split=="") {
          Split <- "Intent"
        }
      }
      else {
        # gp <- gp + scale_linetype_manual(values=AgeIntenttype)
        if (estimateForm!="") {
          # gp <- gp + scale_colour_manual(values=AgeIntentcols)
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
        }
        else if (length(AGE)==1) {
          # gp <- gp + aes(linetype=Intent)
          Legend <- "Intent"
        }
        else {
          # gp <- gp + aes(colour=AgeIntent, linetype=AgeIntent) + 
          #   scale_colour_manual(values=AgeIntentcols)
          Legend <- "Age by Intent"
        }
      }
    }

    pd <- subset( COD2019_All, jurisdiction==input$jur &
      Sex %in% SEX & Age %in% AGE & Intent %in% INTENT &
      (year>=yr[1] & year<=yr[2]) )

    rvData <- 1
    if (varc!="" | vart!="" | estimateForm!="" | yax=="srci" | yax=="sr") {
      pd <- subset( pd, Release=="Dec 2020")
      if (varc!="" | vart!="" | estimateForm!="") {
        rvData <- 0
      }
      else {
        Legend <- "Release\nversion"
      }
    }
    else if (yax!="srci" & yax!="sr") {
      rvData <- 2
      Legend <- "Release date\nby version"
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm,RVData=rvData)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm)

    # if (input$dropSI=="Sex") {
    #   if (input$sex4R=="MF") {
    #     pageDim <- input$dimension
    #     gp <- SplitFn(Pd=pd,Gp=gp,Vars="Sex",PageDim=pageDim)
    #   }
    #   else if ( length(input$AllScod)==2 & input$codS==2 ) {
    #     pageDim <- input$dimension
    #     gp <- SplitFn(Pd=pd,Gp=gp,Vars="Intent",PageDim=pageDim)
    #   }
    # }
    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
    }
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Remoteness by jurisdiction, Intent and Sex (Table R) ------------------------------------------
  output$RAPlot <- renderPlotly({
    yr <- input$yr11
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""
    INTENT <- input$cod2C

    if (input$jurR=="Australia") {
      regR <- input$RAra
    }
    else {
      regR <- input$Rra
    }
    pd <- subset(COD2019_Rem, jurisdiction==input$jurR & Age=="All ages" &
      Sex=="People" & Intent %in% INTENT & Region %in% regR &
      (year>=input$yr11[[1]] & year<=input$yr11[[2]]) )

    gp <- ggplot(pd) + aes(colour=RegionIntent, linetype=RegionIntent) +
        labs(title=paste0(input$jurR,", All ages") ) +
        scale_colour_manual(values=RegionIntentcols) +
        scale_linetype_manual(values=RegionIntenttype)
    Legend <- "Region by Intent"

    vart <- "Intent"
    labt <- "Intent"
    varc <- "Region"
    labc <- "Region"
    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$RPPlot <- renderPlotly({
    Split <- ""
    #needs to be sorted [order(...)] & made distinct
    if (input$jurR=="Australia") {
      pd <- subset(COD2019_Rem, Intent==input$codR & 
            Age==input$ageR &
            (year>=input$yr11[[1]] & year<=input$yr11[[2]]) & 
            Sex==input$sexR & jurisdiction==input$jurR)
    }
    else {
      pd <- subset(COD2019_Rem, Intent==input$codR & 
            Age==input$ageR & 
            (year>=input$yr11[[1]] & year<=input$yr11[[2]]) & 
            Sex=="People" & jurisdiction==input$jurR)
    }

    if (input$jurR=="Australia" & input$sexR=="People" & input$ageR=="All ages" ) {
      pd <- filter(pd, Region!="Regional and Remote") %>%
        group_by(year, Intent, Sex, jurisdiction, Age) %>% 
        distinct() %>%
        mutate(percent=round(n/sum(n)*100, 2),
          Region=factor(Region, levels=c( "Remote and Very Remote",
            "Outer Regional", "Inner Regional", "Major Cities"
          )))
    }
    else {
      pd <- filter(pd, Region=="Regional and Remote" | Region=="Major Cities" ) %>%
        group_by(year, Intent, Sex, jurisdiction, Age) %>% 
        distinct() %>%
        mutate(percent=round(n/sum(n)*100, 2),
          Region=factor(Region, levels=c( "Regional and Remote",
            "Major Cities"
          )))
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd, aes(x=year, y=percent, fill=Region, group=1, text=paste0(
        "Year: ",year,
        "<br>Deaths: ",n,
        "<br>Percent: ",percent, "%",
        "<br>Area: ",Region,
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Age: ",Age,
        "<br>Sex: ",Sex,
        "<br>Intent: ",Intent
      ))) + geom_area() +
      labs(x="Year",y="Percent of drug-induced deaths") +
      scale_fill_manual(values=Regioncols) +
      scale_x_continuous(breaks=seq(input$yr11[[1]],input$yr11[[2]],2) )

    gp <- gp + theme_light() + theme(legend.title=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    PlyFn(Gp=gp,Lt="Remoteness area",X=LX(),Y=LY(),O=LO())
  })

  # All drugs by type, jurisdiction, Intent and/or Sex (Table 12, 12b & 12c) ------------------------------------
  output$DTJPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    AGE <- input$DTage
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    if (input$DTjur!="Australia") {
      AGE <- "All ages"
      SEX <- "People"
      INTENT <- input$cod2C
    }
    if (input$DTjur=="Australia") {
      if (input$DTJdrop=="IntSx") {
        SEX <- input$DTIsex
        if (input$DTIsex=="MF") {
          SEX <- c("Female","Male")
          Split <- "Sex"
        }
        if (input$DTIsex=="People") {
          INTENT <- input$DTIcod
        }
        else {
          INTENT <- input$DTIcod2
        }
      }
      else if (input$DTJdrop=="Drug") {
        INTENT <- input$cod4C
        SEX <- input$sexC
      }
    }
    if (input$DTJdrop=="IntSx") {
      DRUG <- input$DTdrug
    }
    else if (input$DTJdrop=="Drug") {
      DRUG <- input$DTdrugD
    }

    Title <- paste0(input$DTjur,", Age: ",AGE)
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(DRUG)==1) Title <- paste0(Title,", Drug involved: ",DRUG)

    pd <- subset(COD2019_DT, Intent %in% INTENT & Drug %in% DRUG
      & Age==AGE & Sex %in% SEX & jurisdiction==input$DTjur
      & (year>=yr[1] & year<=yr[2] ))

    if (input$DTJdrop=="IntSx") {
      vart <- "Drug"
      labt <- "Intent"
      varc <- "Drug"
      labc <- "Drug"
      if (input$DTjur=="Australia") {
        if (input$DTIsex!="MF") {
          # gp <- ggplot(pd)
        }
        else {
          # gp <- ggplot(pd)
        }
      }
      if (input$DTjur!="Australia") {
        # gp <- ggplot(pd)
      }
      # gp <- gp + aes(colour=Drug, linetype=Drug) +
      #   scale_colour_manual(values=Drugcols) +
      #   scale_linetype_manual(values=Drugtype)
      Legend <- "Drug involved"
      
      if (input$DTIsex!="People") {
        validate(need(nrow(pd) > 0, "Please select All ages for age range for data by male and/or female."))
      }
    }
    
    if (input$DTJdrop=="Drug") {
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Sex"
      labc <- "Sex"
      Legend <- ""
      if (length(SEX)==1) {
        varc <- ""
        if (estimateForm!="") {
          varc <- "Age"
          estimateForm <- "Colo"
          Legend <- "Estimate type"
        }
      }
      if (input$DTjur=="Australia") {
        # gp <- ggplot(pd) +
        #   scale_colour_manual(values=SexIntentcols)
        if (length(INTENT)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          # gp <- gp + aes(colour=Sex)
          vart <- ""
          if (length(INTENT)==2 & input$codS==2) Split <- "Intent"
          if (length(SEX)>1) {
            Legend <- "Sex"
          }
        }
        else if (length(SEX)>1) {
          # gp <- gp + aes(colour=SexIntent, linetype=SexIntent) +
          #   scale_linetype_manual(values=SexIntenttype)
          Legend <- "Sex by Intent"
        }
        else if (estimateForm!="") {
          estimateForm <- "Colo2"
          Legend <- "Estimate type by Intent"
        }
        else {
          Legend <- "Intent"
        }

        if (input$sexC[[1]]!="People") {
          validate(need(nrow(pd) > 0, "Please select All ages for age range for data by male and/or female."))
        }
      }
      if (input$DTjur!="Australia") {
        # gp <- ggplot(pd) + aes(linetype=Intent) +
        #   scale_linetype_manual(values=AgeIntenttype)
        if (length(INTENT)==1) {
          vart <- ""
          Legend <- ""
        }
        else {
          Legend <- "Intent"
          if (estimateForm!="") {
            estimateForm <- "Colo2"
            Legend <- "Estimate type by Intent"
          }
        }
      }
    }

    rvData <- 1
    if (varc!="" | vart!="" | estimateForm!="" | yax=="srci" | yax=="sr") {
      pd <- subset( pd, Release=="Dec 2020")
      if (varc!="" | vart!="" | estimateForm!="") {
        rvData <- 0
      }
      else {
        Legend <- "Release\nversion"
      }
    }
    else if (yax!="srci" & yax!="sr") {
      rvData <- 2
      Legend <- "Release date\nby version"
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:' to show all drugs involved.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm,RVData=rvData)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm)

    # if (input$DTJdrop=="IntSx" & input$DTjur=="Australia" & input$DTIsex=="MF") {
    #   pageDim <- input$dimension
    #   gp <- SplitFn(Pd=pd,Gp=gp,Vars="Sex",PageDim=pageDim)
    # }
    # if (input$DTJdrop=="Drug" & length(input$cod4C)==2 & input$codS==2) {
    #   pageDim <- input$dimension
    #   gp <- SplitFn(Pd=pd,Gp=gp,Vars="Intent",PageDim=pageDim)
    # }
    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # All drugs by type, Age & Intent (Tables 12 & 12a) ----------------------------------------------------------
  output$DTAPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$DTAdrop=="Drug") {
      AGE <- input$ageAll
      INTENT <- input$cod2C
      DRUG <- input$DTdrugD
      if (length(input$cod2C)==2 & input$codS==2) Split <- "Intent"
    }
    else {
      AGE <- input$DTAIage
      INTENT <- input$DTAIcod
      DRUG <- input$DTdrug
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- "Australia"
    if (length(AGE)==1) Title <- paste0(input$DTjur,", Age: ",AGE)
    # if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(DRUG)==1) Title <- paste0(Title,", Drug involved: ",DRUG)

    if (input$DTAdrop=="Age_Intent") {
      # gp <- ggplot(pd) + aes(colour=Drug, linetype=Drug) +
        # scale_colour_manual(values=Drugcols) +
        # scale_linetype_manual(values=Drugtype) +
        # labs(title=paste0("Age: ",AGE,", Intent: ",input$DTAIcod))
      Legend <- "Drug involved"
      vart <- "Drug"
      labt <- "Intent"
      varc <- "Drug"
      labc <- "Drug"
    }
    if (input$DTAdrop=="Drug") {
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Age"
      labc <- "Age"
      if (length(AGE)==1 & estimateForm=="") varc <- ""
      if ( length(input$cod2C)==1 | (length(input$cod2C)==2 & input$codS==2)) {
        vart <- ""
        # if (length(input$cod2C)==2) Split <- "Intent"
        # gp <- ggplot(pd) + scale_colour_manual(values=AgeIntentcols)
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else if (length(AGE)==1) {
          Legend <- ""
        }
        else {
          # gp <- gp + aes(colour=AgeIntent)
          Legend <- "Age"
        }
      }
      else if (estimateForm!="") {
          # gp <- gp + scale_colour_manual(values=AgeIntentcols)
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
      }
      else if (length(AGE)==1) {
        # gp <- ggplot(pd) + scale_linetype_manual(values=AgeIntenttype)
          # gp <- gp + aes(linetype=Intent)
          Legend <- "Intent"
      }
      else {
        # gp <- ggplot(pd) + aes(colour=AgeIntent, linetype=AgeIntent) +
        #   scale_colour_manual(values=AgeIntentcols) +
        #   scale_linetype_manual(values=AgeIntenttype)
        Legend <- "Age by Intent"
      }
    }

    pd <- subset( COD2019_DT, jurisdiction=="Australia" &
        Sex=="People" & Age %in% AGE & Intent %in% INTENT &
        Drug %in% DRUG & #nature=="Underlying" &
        (year>=yr[1] & year<=yr[2]) )
    rvData <- 1
    if (varc!="" | vart!="" | estimateForm!="" | yax=="srci" | yax=="sr") {
      pd <- subset( pd, Release=="Dec 2020")
      if (varc!="" | vart!="" | estimateForm!="") {
        rvData <- 0
      }
      else {
        Legend <- "Release\nversion"
      }
    }
    else if (yax!="srci" & yax!="sr") {
      rvData <- 2
      Legend <- "Release date\nby version"
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:'/'Age:' to show all drugs involved/age.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm,RVData=rvData)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
    }
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O4 - by opioid, Age & Intent (Table 4) -----------------------------------------------------------------
  output$O4Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      if (yax=="sr" | yax=="srci") AGE <- "All ages"
      pd <- subset(COD2019_Op, subset=(Sex=="People" & jurisdiction=="Australia" & Opioid==input$OdrugO &
         Intent %in% input$cod4C & Age %in% AGE &
         (year>=yr[1] & year<=yr[2])))
      gp <- ggplot(pd) + scale_colour_manual(values=AgeIntentcols)
      Title <- input$OdrugO

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        if (estimateForm!="") {
          # gp <- gp + scale_linetype_manual(values=AgeIntenttype)
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else {
          gp <- gp + aes(colour=Age)
          Legend <- "Age"
        }
      }
      else {
        gp <- gp + scale_linetype_manual(values=AgeIntenttype)
        if (estimateForm!="") {
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
        }
        else {
          gp <- gp + aes(colour=AgeIntent, linetype=AgeIntent)
          Legend <- "Age by Intent"
        }
      }
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Age"
      labc <- "Age"
    }

    else if (input$dropOA=="Age") {
      AGE <- input$ageOAA
      if (yax=="sr" | yax=="srci") AGE <- "All ages"
      pd <- subset(COD2019_Op, subset=(Sex=="People" & jurisdiction=="Australia" & Opioid %in% input$OdrugC &
        Intent %in% input$cod4C & Age==AGE &
        (year>=yr[1] & year<=yr[2])))
      gp <- ggplot(pd) + scale_colour_manual(values=OpioidIntentcols)
      Title <- paste0("Age: ",AGE)

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        gp <- gp + aes(colour=Opioid)
        Legend <- "Opioid"
      }
      else {
        gp <- gp + aes(colour=OpioidIntent, linetype=OpioidIntent) +
          scale_linetype_manual(values=OpioidIntenttype)
        Legend <- "Opioid by Intent"
      }
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Opioid"
      labc <- "Opioid"
    }

    if (length(input$cod4C)==1) {
      Title <- paste0(Title,", Intent: ",input$cod4C)
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    if (length(input$cod4C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars="Intent",PageDim=pageDim)
    }
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O5 - by opioid, Intent & Sex (Table 5)-----------------------------------------------------------------
  output$O5Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    if (input$O5drop=="Opioid") {
      pd <- subset(COD2019_Op, Age==AGE & 
          Opioid==input$OdrugO & Intent %in% input$cod4C & Sex %in% input$sexC &
          jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]))

      gp <- ggplot(pd) + scale_colour_manual(values=sexcols)
      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        gp <- gp + aes(colour=Sex)
        Legend <- "Sex"
      }
      else {
        gp <- gp + aes(colour=SexIntent, linetype=SexIntent) +
          scale_linetype_manual(values=sexIntenttype)
        Legend <- "Sex by Intent"
      }
      Title <- input$OdrugO
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Sex"
      labc <- "Sex"
    }
    else if (input$O5drop=="Intent") {
      pd <- subset(COD2019_Op, subset=(Age==AGE &
        Opioid %in% input$OdrugC & Sex %in% input$sexC & Intent==input$cod4R &
        jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2])))

      gp <- ggplot(pd) + aes(colour=OpioidSex, linetype=OpioidSex) +
        scale_colour_manual(values=OpioidSexcols) +
        scale_linetype_manual(values=OpioidSextype)
      Legend <- "Opioid by Sex"
      Title <- paste0("Intent: ",input$cod4R)
      vart <- "Sex"
      labt <- "Sex"
      varc <- "Opioid"
      labc <- "Opioid"
    }
    else if (input$O5drop=="Sex") {
      Legend <- "Opioid by Intent"
      if (input$sex4R!="MF") {
        pd <- subset(COD2019_Op, Age==AGE & jurisdiction=="Australia" &
            Opioid %in% input$OdrugC & Intent %in% input$cod4C & Sex==input$sex4R &
            (year>=yr[1] & year<=yr[2]))
        
        gp <- ggplot(pd)
        Title <- paste0("Sex: ",input$sex4R)
        if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          gp <- gp + aes(colour=Opioid)
          Legend <- "Opioid"
        }
        else {
          gp <- gp + aes(colour=OpioidIntent, linetype=OpioidIntent) +
            scale_linetype_manual(values=OpioidIntenttype)
        }
      }
      else {
        pd <- subset(COD2019_Op, Age==AGE & jurisdiction=="Australia" &
            Opioid %in% input$OdrugC & Intent %in% input$cod4C & Sex!="People" &
            (year>=yr[1] & year<=yr[2]))
        
        gp <- ggplot(pd) + aes(colour=OpioidIntent, linetype=OpioidIntent) +
          scale_linetype_manual(values=OpioidIntenttype)
        Title <- "Females & Males"
        # if (input$dimension<992) {
        #   gp <- gp + facet_grid(rows=vars(Sex))
        # }
        # else {
        #   gp <- gp + facet_grid(cols=vars(Sex))
        #   if (input$dimension<1200) {
        #     LX(0)
        #     LO("h")
        #     LY(-.2)
        #   }
        # }
      }
      gp <- gp + scale_colour_manual(values=OpioidIntentcols)
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Opioid"
      labc <- "Opioid"
    }

    if (input$O5drop!="Intent") {
      if (length(input$cod4C)==1) {
        Title <- paste0(Title,", Intent: ",input$cod4C)
      }
      # else if ( length(input$cod4C)==2 & input$codS==2 &
      #   ((input$O5drop=="Sex" & input$sex4R!="MF") | input$O5drop=="Opioid") ) {
      #   if (input$dimension<992) {
      #     gp <- gp + facet_grid(rows=vars(Intent))
      #   }
      #   else {
      #     gp <- gp + facet_grid(cols=vars(Intent))
      #     if (input$dimension<1200) {
      #       LX(0)
      #       LO("h")
      #       LY(-.2)
      #     }
      #   }
      # }
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    Title <- paste0(Title,", Age: ",AGE)
    gp <- gp + labs(title=Title) #+
      # theme(strip.background=element_rect(fill="#dbefd4"),
      #   strip.text=element_text(color="#000000", face="bold") )
    if (input$O5drop=="Sex" & input$sex4R=="MF") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars="Sex",PageDim=pageDim)
    }
    else if ( input$O5drop!="Intent" & length(input$cod4C)==2 & input$codS==2 ) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars="Intent",PageDim=pageDim)
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # O6 - by jurisdiction, Sex & Intent (Table 6) -------------------------------------------
  output$O6Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    pd <- subset(COD2019_Op, Age==AGE & Opioid=="All opioids" &
        jurisdiction==input$jur & Intent %in% input$cod2C & Sex %in% input$sexC &
        (year>=yr[1] & year<=yr[2]))

    gp <- ggplot(pd) +
      aes(colour=SexIntent, linetype=SexIntent) +
      labs(title=paste0(input$jur,", Age: ",AGE) ) +
      scale_colour_manual(values=sexcols) +
      scale_linetype_manual(values=sexIntenttype)

    vart <- "Intent"
    labt <- "Intent"
    varc <- "Sex"
    labc <- "Sex"

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    PlyFn(Gp=gp,Lt="Sex by Intent",X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids with other drugs by Age & Intent (Table 7) ----------------------------------------------------------
  output$W7Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    pd <- subset(COD2019_OpW, Intent %in% input$cod4C &
      (year>=yr[1] & year<=yr[2]) & Sex=="People")
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | ( yax=="sr" &
      input$dropOA=="Age" & (length(input$cod4C)>2 | (input$codS==1 & length(input$cod4C)==2)) ) ) {
      pd <- subset(pd, primary=="opioid-induced")
    }

    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      if (yax=="sr" | yax=="srci") AGE <- "All ages"
      pd <- subset(pd, Drug==input$W7Ddrug & Age %in% AGE)
  
      gp <- ggplot(pd) + scale_colour_manual(values=AgeIntentcols)
      Title <- paste0("All opioids with ",input$W7Ddrug)

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        if (estimateForm!="") {
          # gp <- gp + scale_linetype_manual(values=AgeIntenttype)
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else {
          gp <- gp + aes(colour=Age)
          Legend <- "Age"
        }
      }
      else {
        gp <- gp + scale_linetype_manual(values=AgeIntenttype)
        if (estimateForm!="") {
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
        }
        else {
          gp <- gp + aes(colour=AgeIntent, linetype=AgeIntent)
          Legend <- "Age by Intent"
        }
      }
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Age"
      labc <- "Age"
    }
    if (input$dropOA=="Age") {
      AGE <- input$ageOAA
      if (yax=="sr" | yax=="srci") AGE <- "All ages"
      pd <- subset(pd, Drug %in% input$Wdrug & Age==AGE)
      
      gp <- ggplot(pd) + scale_colour_manual(values=opWIntentcols)
      Title <- paste0("Age: ",AGE)

      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
        if (estimateForm!="") {
          gp <- gp + scale_linetype_manual(values=opWIntenttype)
          Legend <- "Estimate type by Drug involved"
          estimateForm <- "Type2"
        }
        else {
          gp <- gp + aes(colour=Drug)
          Legend <- "Drug involved"
        }
      }
      else {
        gp <- gp + aes(colour=DrugIntent, linetype=DrugIntent) +
          scale_linetype_manual(values=opWIntenttype)
        Legend <- "Drug involved by Intent"
      }
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Drug"
      labc <- "Drug"
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

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    gp <- gp + labs(title=Title) #+
      # theme(strip.background=element_rect(fill="#dbefd4"),
      #   strip.text=element_text(color="#000000", face="bold") )
    if (length(input$cod4C)==2 & input$codS==2) {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars="Intent",PageDim=pageDim)
      # if (input$dimension<992) {
      #   gp <- gp + facet_grid(rows=vars(Intent))
      # }
      # else {
      #   gp <- gp + facet_grid(cols=vars(Intent))
      #   if (input$dimension<1200) {
      #     LX(0)
      #     LO("h")
      #     LY(-.2)
      #   }
      # }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids and other drugs by Sex (Table 8) ------------------------------------------
  output$W8Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    if (input$dropSI=="Sex") {
      INTENT <- input$cod4C
      SEX <- input$sex4R
      if (input$sex4R=="MF") SEX <- c("Female","Male")
    }
    else {
      INTENT <- input$cod4R
      SEX <- input$sexC
    }
    pd <- subset(COD2019_OpW, Drug %in% input$Wdrug &
      Age==AGE & Intent %in% INTENT & Sex %in% SEX &
      (year>=yr[1] & year<=yr[2]) )
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" &
      ( input$dropSI=="Intent" | (length(input$cod4C)>2 |
      (length(input$cod4C)==2 & (input$codS==1 | input$sex4R=="MF"))) ) ) ) {
      pd <- subset(pd, primary=="opioid-induced")
    }

    if (input$dropSI=="Sex") {
      Legend <- "Drug by Intent"
      gp <- ggplot(pd) + scale_colour_manual(values=opWIntentcols)
      if (input$sex4R!="MF") {
        # pd <- subset(pd, Intent %in% input$cod4C & Sex==input$sex4R )
        # gp <- ggplot(pd) + scale_colour_manual(values=opWIntentcols)
        Title <- paste0("Age: ",AGE,",  Sex: ",input$sex4R)
        
        if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          if (estimateForm!="") {
            gp <- gp + scale_linetype_manual(values=opWIntenttype)
            Legend <- "Estimate type by Drug involved"
            estimateForm <- "Type2"
          }
          else {
            gp <- gp + aes(colour=Drug)
            Legend <- "Drug involved"
          }
          if (length(input$cod4C)==1) {
            Title <- paste0(Title,", Intent: ",input$cod4C)
          }
          # if (length(input$cod4C)==2 & input$codS==2) {
          #   if (input$dimension<992) {
          #     gp <- gp + facet_grid(rows=vars(Intent))
          #   }
          #   else {
          #     gp <- gp + facet_grid(cols=vars(Intent))
          #     if (input$dimension<1200) {
          #       LX(0)
          #       LO("h")
          #       LY(-.2)
          #     }
          #   }
          # }
        }
        else {
          gp <- gp + aes(colour=DrugIntent, linetype=DrugIntent) +
            scale_linetype_manual(values=opWIntenttype)
        }
      }
      else {
        # pd <- subset(pd, Intent %in% input$cod4C & Sex!="People" )
        # gp <- ggplot(pd) +
        #     scale_colour_manual(values=opWIntentcols)
        if (length(input$cod4C)==1) {
          if (estimateForm!="") {
            gp <- gp + scale_linetype_manual(values=opWIntenttype)
            Legend <- "Estimate type by Drug involved"
            estimateForm <- "Type2"
          }
          else {
            gp <- gp + aes(colour=Drug)
            Legend <- "Drug involved"
          }
        }
        else {
          gp <- gp + aes(colour=DrugIntent, linetype=DrugIntent) +
            scale_linetype_manual(values=opWIntenttype)
        }
        # if (input$dimension<992) {
        #   gp <- gp + facet_grid(rows=vars(Sex))
        # }
        # else {
        #   gp <- gp + facet_grid(cols=vars(Sex))
        #   if (input$dimension<1200) {
        #     LX(0)
        #     LO("h")
        #     LY(-.2)
        #   }
        # }
        Title=paste0("Age: ",AGE)
      }
      varc <- "Drug"
      labc <- "Drug"
      vart <- "Intent"
      labt <- "Intent"
    }
    if (input$dropSI=="Intent") {
      # pd <- subset(pd, Intent==input$cod4R & Sex %in% input$sexC)
      gp <- ggplot(pd) + aes(colour=DrugSex, linetype=DrugSex) + 
        scale_colour_manual(values=opWIntentcols) +
        scale_linetype_manual(values=opWIntenttype)
      Legend <- "Drug by Sex"
      Title <- paste0("Age: ",AGE,",  Intent: ",input$cod4R)
      varc <- "Drug"
      labc <- "Drug"
      vart <- "Sex"
      labt <- "Sex"
    }

    if ( input$Ashow==T & (yax=="num" | yax=="cr" | ( yax=="sr" &
      input$dropSI=="Sex" & length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2 & input$sex4R!="MF") )) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0(Legend,"<br> by death data type")
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    gp <- gp + labs(title=Title) #+
      # theme(strip.background=element_rect(fill="#dbefd4"),
      #   strip.text=element_text(color="#000000", face="bold") )
    if (input$dropSI=="Sex") {
      if (input$sex4R=="MF") {
        pageDim <- input$dimension
        gp <- SplitFn(Pd=pd,Gp=gp,Vars="Sex",PageDim=pageDim)
      }
      else if ( length(input$cod4C)==2 & input$codS==2 ) {
        pageDim <- input$dimension
        gp <- SplitFn(Pd=pd,Gp=gp,Vars="Intent",PageDim=pageDim)
      }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive Opioids by Age and Intent (Table 10) --------------------------------------------------------------
  output$E0Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      if (yax=="sr" | yax=="srci") AGE <- "All ages"
      pd <- filter(COD2019_OpE, Sex=="People" & jurisdiction=="Australia" &
          Opioid==input$E0Odrug & Intent %in% input$cod3C & Age %in% AGE &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]])) %>%
        distinct(Opioid, year, Intent, Sex, jurisdiction, Age, .keep_all=T)

      gp <- ggplot(pd) + aes(colour=AgeIntent, linetype=AgeIntent) +
        labs(title=input$E0Odrug) +
        scale_colour_manual(values=AgeIntentcols) +
        scale_linetype_manual(values=AgeIntenttype)
      Legend <- "Age by Intent"
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Age"
      labc <- "Age"
    }

    else if (input$dropOA=="Age") {
      AGE <- input$ageOAA
      if (yax=="sr" | yax=="srci") AGE <- "All ages"
      pd <- subset(COD2019_OpE, subset=(Sex=="People" & jurisdiction=="Australia" &
          Opioid %in% input$Edrug & Intent %in% input$cod3C & Age==AGE &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]))) %>%
        distinct(Opioid, year, Intent, Sex, jurisdiction, Age, .keep_all=T)
      
      gp <- ggplot(pd) + aes(colour=OpioidIntent, linetype=OpioidIntent) +
        labs(title=paste0("Age: ",AGE)) +
        scale_colour_manual(values=OpioidIntentcols) +
        scale_linetype_manual(values=OpioidIntenttype)
      Legend <- "Opioid by Intent"
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Opioid"
      labc <- "Opioid"
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive opioids by jurisdiction, Intent and Sex (Table 9 & 11) -----------------------
  output$E9Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    # crude <- input$crude
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    if (input$dropSI=="Intent") {
      pd <- filter(COD2019_OpE, jurisdiction==input$jur & Age==AGE &
          Sex %in% input$sexC & Intent==input$E9Icod & Opioid %in% input$Edrug & 
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
        distinct(Opioid, year, Intent, Sex, jurisdiction, Age, .keep_all=T)

      gp <- ggplot(pd) + aes(colour=OpioidSex, linetype=OpioidSex) +
        labs(title=paste0(input$jur,", Age: ",AGE,", Intent: ",input$E9Icod) ) +
        scale_colour_manual(values=OpioidSexcols) +
        scale_linetype_manual(values=OpioidSextype)
      Legend <- "Opioid by Sex"
      vart <- "Opioid"
      labt <- "Opioid"
      varc <- "Sex"
      labc <- "Sex"
    }
    if (input$dropSI=="Sex") {
      if (input$sex4R!="MF") {
        pd <- filter(COD2019_OpE, jurisdiction==input$jur & Age==AGE &
            Sex==input$sex4R & Intent %in% input$cod3C & Opioid %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(Opioid, year, Intent, Sex, jurisdiction, Age, .keep_all=T)
        gp <- ggplot(pd) + labs(title=paste0(
          input$jur,", Age: ",AGE,", Sex: ",input$sex4R) )
      }
      else {
        pd <- filter(COD2019_OpE, jurisdiction==input$jur & Age==AGE &
            Sex!="People" & Intent %in% input$cod3C & Opioid %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(Opioid, year, Intent, Sex, jurisdiction, Age, .keep_all=T)
        gp <- ggplot(pd) +
          labs(title=paste0(input$jur,", Age: ",AGE) )
        if (input$dimension<992) {
          gp <- gp + facet_grid(rows=vars(Sex))
        }
        else {
          gp <- gp + facet_grid(cols=vars(Sex))
          if (input$dimension<1200) {
            LX(0)
            LO("h")
            LY(-.2)
          }
        }
      }
      gp <- gp + aes(colour=OpioidIntent, linetype=OpioidIntent) +
        scale_colour_manual(values=OpioidIntentcols) +
        scale_linetype_manual(values=OpioidIntenttype)
      Legend <- "Opioid by Intent"
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Opioid"
      labc <- "Opioid"
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Vart=vart,Varc=varc,Labt=labt,Labc=labc,EstForm=estimateForm)

    gp <- gp + theme(strip.background=element_rect(fill="#dbefd4"),
            strip.text=element_text(color="#000000", face="bold") )
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$EPPlot <- renderPlotly({
    Split <- ""
    #needs to be sorted [order(...)]
    #weird proportions plot from 2015 onwards because of duplicates by AUS
    #- need to make distinct
    pd <- filter(COD2019_OpE, Opioid %in%
          c("Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids",
            "Illicit & pharmaceutical opioids",
            "Other & unspecified opioids") &
        Intent==input$codR & Age==input$ageR & 
        (year>=input$yr07[[1]] & year<=input$yr07[[2]]) & 
        Sex==input$sexR & jurisdiction==input$jur) %>%
      group_by(year, Intent, Sex, jurisdiction, Age) %>% 
      distinct() %>%
      mutate(
        percent=round(n/sum(n)*100, 2),
        Opioid=factor(Opioid, levels=c("Other & unspecified opioids",
                                   "Illicit & pharmaceutical opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Exclusive illicit opioids")))

    gp <- ggplot(pd, aes(x=year, y=percent, fill=Opioid, group=1, text=paste0(
        "Year: ",year,
        "<br>Deaths: ",n,
        "<br>Percent: ",percent,"%",
        "<br>Opioid: ",Opioid,
        "<br>Intent: ",Intent,
        "<br>Sex: ",Sex))) +
      geom_area() +
      labs(x="Year", y="Percent of opioid induced deaths") +
      scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) ) +
      scale_fill_manual(values=opEcols)
    
    validate(need(nrow(pd) > 0, "No data selected"))
    
    gp <- gp + theme_light() + theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())

    PlyFn(Gp=gp,Lt="Opioid",X=LX(),Y=LY(),O=LO())
  })

}
