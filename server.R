#For ABS COD 2019 data received in Dec 2020
#N. Man
#library(shiny) #already loaded in UI
library(ggplot2)
#library(plotly) #already loaded in UI
library(shinycustomloader)

#https://appsilon.com/fast-data-loading-from-files-to-r/
# load("death_2018.Rdata")
load("death_2019.Rdata")

ColtypFn <- function(Pd,Gp,Yax=yax,Varc="Age",Vart="Intent",Split="",EstForm="") {
  # print(paste(EstForm,( (Varc!="" & Vart!="") | EstForm>"Colo" ) & Split!=Varc & (Split!=Vart | EstForm=="Type2") ,Varc,Vart,RVData,Split))
  # Gp <- ggplot(data=Pd)
  if (( (Varc!="" & Vart!="") | EstForm>"Colo" ) & Split!=Varc & (Split!=Vart | EstForm=="Type2") ) {
    Var <- paste0(Varc,Vart)
    if (Varc==Vart) Var <- Varc
    Gp <- Gp + scale_colour_manual( values=get(paste0(Var,"cols")) ) +
      scale_linetype_manual( values=get(paste0(Var,"type")) )
    if (Yax=="srci" | Yax=="crci") Gp <- Gp + scale_fill_manual( values=get(paste0(Var,"cols")) )
    if (EstForm<="Alph") {
      Gp <- Gp + aes(colour=get(Var), linetype=get(Var))
      if (Yax=="srci" | Yax=="crci") Gp <- Gp + aes(fill=get(Var))
    }
  }
  else if (( Varc!="" | EstForm=="Colo" ) & Split!=Varc) {
    Gp <- Gp + scale_colour_manual( values=get(paste0(Varc,"cols")) )
    if (Yax=="srci" | Yax=="crci") Gp <- Gp + scale_fill_manual( values=get(paste0(Varc,"cols")) )
    if (Varc!="") {
      Gp <- Gp + aes(colour=get(Varc))
      if (Yax=="srci" | Yax=="crci") Gp <- Gp + aes(fill=get(Varc))
    }
  }
  else if ( Vart!="" & Split!=Vart ) {
    Gp <- Gp + aes(linetype=get(Vart)) +
      scale_linetype_manual( values=get(paste0(Vart,"type")) )
  }
  # else if (EstForm=="" & RVData>0) {
  #   if (RVData==2) {
  #     Gp <- Gp + aes(color=Release) +
  #       scale_color_manual(values=Notecolo)
  #   }
    # Gp <- Gp + scale_shape_manual(values=Noteshap) +
    #   geom_point(data=subset(Pd,Note!=""),aes(shape=Note,text=NULL),size=2) #,show.legend=F) - does nothing
  # }
  # Gp <- Gp + scale_shape_manual(values=Noteshap) +
  #   # geom_point(data=subset(Pd,Note=="revised"),aes(text=NULL),shape=1,size=2) + #,show.legend=F) +
  #   # geom_point(data=subset(Pd,Note=="prelim."),aes(text=NULL),shape=4,size=2) #,show.legend=F)
  #   geom_point(data=subset(Pd,Note!=""),aes(shape=Note,text=NULL),size=2,show.legend=F)
  return(Gp)
}

PlotFn <- function(Pd,Gp,Yax,Yr,Labc,Labt,EstForm="",RVData=1) {
  if (Yax=="num") {
    Gp <- Gp + geom_line() + aes(y=n, text=paste0(
        "Year: ",year,
        "<br>Deaths: ",n," (last: ",n_2018,")",
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
          "<br>Crude rate: ",cr," (",cr_lci,",", cr_uci,")", # rounded off in dataset instead to avoid potential confidentiality issue with small numbers being discoverable
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc)
        ))
    }
    else {
#"Warning: Ignoring unknown aesthetics: text" because it is used with geom_line but we don't want text box with the ribbon
      Gp <- Gp + aes(y=cr) + geom_line(aes(text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n, " (last: ",n_2018,")",
          "<br>Crude rate: ",cr," (",cr_lci,",", cr_uci,")",
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc)
        ))) +
        geom_ribbon(aes(ymin=cr_lci,ymax=cr_uci), alpha=0.1,size=0)
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$cr_uci, 2.5,na.rm=T))) +
      labs(y="Crude mortality rate per 100,000")
  }

  else if (Yax=="sr" | Yax=="srci") {
    # Pd <- subset(Pd,Age=="All ages")
    Gp <- Gp + aes(y=sr)
    if (Yax=="sr") {
      if (EstForm=="Alph") {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Age standardised rate: ",sr," (",sr_lci,",", sr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),alpha="Age standardised") ) +
          geom_line(aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Crude rate: ",cr," (",cr_lci,",", cr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),alpha="Crude")) +
          scale_alpha_manual(values=c(1,.3))
      }
      else if (EstForm=="Colo") {
        # if (nrow(unique(Pd[,Labt]))==1) {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n, 
            "<br>Age standardised rate: ",sr," (",sr_lci,",", sr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color="Age standardised") ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Crude rate: ",cr," (",cr_lci,",", cr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color="Crude") )
      }
      else if (EstForm=="Colo2") {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Age standardised rate: ",sr," (",sr_lci,",", sr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Age standardised",get(Labt),sep=","),linetype=paste("Age standardised",get(Labt),sep=",")) ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Crude rate: ",cr," (",cr_lci,",", cr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Crude",get(Labt),sep=","),linetype=paste("Crude",get(Labt),sep=",")) )
      }
      else if (EstForm=="Type2") {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Age standardised rate: ",sr," (",sr_lci,",", sr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Age standardised",get(Labc),sep=","),linetype=paste("Age standardised",get(Labc),sep=",")) ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Crude rate: ",cr," (",cr_lci,",", cr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ),color=paste("Crude",get(Labc),sep=","),linetype=paste("Crude",get(Labc),sep=",")) )
      }
      else {
        Gp <- Gp + geom_line() + aes(text=paste0(
            "Year: ",year,
            "<br>Deaths: ",n,
            "<br>Age standardised rate: ",sr," (",sr_lci,",", sr_uci,")",
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc)
          ))
      }
    }
    else {
      Gp <- Gp + geom_line( aes(text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n,
          "<br>Age standardised rate: ",sr," (",sr_lci,",", sr_uci,")",
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
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$sr_uci,Pd$cr_uci,2.5,na.rm=T)))
  }
  if (RVData>0) {
    if (Yax!="sr" | EstForm=="" | EstForm=="Colo2")
    Gp <- Gp + #scale_shape_manual(values=Noteshap) +
      geom_point(data=subset(Pd,Note=="revised"),aes(x=year+.1),shape=1,size=1.5,show.legend=F) +
      geom_point(data=subset(Pd,Note=="prelim."),aes(x=year+.1),shape=4,size=1.5,show.legend=F)
    # geom_point(data=subset(Pd,Note!=""),aes(text=NULL),show.legend=F)
    if (Yax=="sr") {
      if (EstForm=="Colo2") { # | EstForm=="Colo"
        Gp <- Gp +
        geom_point(data=subset(Pd,Note=="revised"),aes(x=year+.1,y=cr),shape=1,size=1.5,color="red") +
        geom_point(data=subset(Pd,Note=="prelim."),aes(x=year+.1,y=cr),shape=4,size=1.5,color="red")
      }
      # else if (EstForm=="Alph") {
      #   Gp <- Gp +
      #   geom_point(data=subset(Pd,Note=="revised"),aes(x=year+.1,y=cr,text=NULL),shape=1,alpha=0.3,show.legend=F) +
      #   geom_point(data=subset(Pd,Note=="prelim."),aes(x=year+.1,y=cr,text=NULL),shape=4,alpha=0.3,show.legend=F)
      # }
      # else if (EstForm!="") {
      #   Gp <- Gp +
      #   geom_point(data=subset(Pd,Note=="revised"),aes(x=year+.1,y=cr,text=NULL),shape=1,show.legend=F) +
      #   geom_point(data=subset(Pd,Note=="prelim."),aes(x=year+.1,y=cr,text=NULL),shape=4,show.legend=F)
      # }
    }
    else {
      if (EstForm=="" & RVData==2) {
        Gp <- Gp + aes(color=Release) +
          scale_color_manual(values=Notecolo)
        if (Yax=="srci" | Yax=="crci")
          Gp <- Gp + aes(fill=Release) +
            scale_fill_manual(values=Notecolo)
      }
    }
  }

  Gp <- Gp + labs(x="Year") + aes(x=year, group=1) +
    scale_x_continuous(breaks=seq(Yr[1],Yr[2],2)) +
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
    #   LO("h")
    #   LX(0)
    #   LY(-.2)
    }
  }
  # NB: need to have grid codes last
  Gp <- Gp + theme(strip.background=element_rect(fill="#dbefd4"),
      strip.text=element_text(color="#000000", face="bold") )
  return(Gp)
}

PlyFn <- function(Gp,Lt,O=LO(),X=LX(),Y=LY()) {  #Pd,Yax,
  # pr <- as.data.frame(select(subset(Pd,Note=="prelim."),c("year",Yax)))
  # print(pr)
    ggplotly(Gp, tooltip="text") %>%
      # add_markers(data=pr,
      #   # type="scatter",
      #   # mode="markers",
      #   y=get(Yax),
      #   x=year,
      #   marker=list(
      #     color='grey',
      #     symbol="cross"
      # )) %>%
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
      #   LO("h")
      #   LX(0)
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
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    AGE <- input$ageAll
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    INTENT <- input$cod2C

    pd <- subset(COD2019_Stim, jurisdiction=="Australia" & Sex=="People" & 
           Drug=="Amphetamines" & Intent %in% INTENT & #nature=="Underlying" & 
           Age %in% AGE & (year>=yr[1] & year<=yr[2]))

    Title <- "Australia"
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)

    varc <- "Age"
    labc <- "Age"
    vart <- "Intent"
    labt <- "Intent"
    if (length(AGE)==1 & estimateForm=="") varc <- ""
    # gp <- ggplot(pd) + scale_colour_manual(values=get("AgeIntentcols"))
    if (length(INTENT)==1 | (length(INTENT)==2 & input$codS==2)) {
      if (length(INTENT)==2) Split <- labt
      vart <- ""
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

    if (input$Ashow==F | yax=="crci" | yax=="srci") {
      pd <- subset(pd, primary=="Amphetamine-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)
    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | yax=="sr") ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,.3))
      Legend <- paste0("Death data type by<br>",Legend)
      LY(.9)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    }
    # if (varc=="" & vart=="" & estimateForm=="" & rvData==1) {
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    if (Split!="") {
      pageDim <- input$dimension
      # print(Split)
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
      # if (input$dimension<992) {
      #   gp <- gp + facet_grid(rows=vars(Intent))
      # }
      # else {
      #   gp <- gp + facet_grid(cols=vars(Intent))
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
      # }
    }
  ####For user-defined year intervals
  #     xax <- as.numeric(input$xax)
  #     xax <- (input$yr97[[2]]-input$yr97[[1]])/xax
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData) #+
  ####For user-defined year intervals
  #     function(x) unique(floor( pretty(x,n=xax) ) )
  #     scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )

    # # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    gp <- gp +
      theme(strip.background=element_rect(fill="#dbefd4"),
        strip.text=element_text(color="#000000", face="bold") )

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
    # ggplotly(gp, tooltip="text")
  })

  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$CPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    AGE <- input$Cage
    if (yax=="sr" | yax=="srci") AGE <- " All ages"
    INTENT <- input$cod2C

    pd <- subset(COD2019_Stim, Drug=="Cocaine" & Intent %in% INTENT &
        Age %in% AGE & Sex=="People" & #nature=="Underlying" &
        jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]))

    Title <- "Australia"
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)

    varc <- "Age"
    labc <- "Age"
    vart <- "Intent"
    labt <- "Intent"
    if (length(AGE)==1 & estimateForm=="") varc <- ""
    Legend <- ""
    if (length(INTENT)==1 | (length(INTENT)==2 & input$codS==2)) {
      vart <- ""
      if (length(INTENT)==2) Split <- labt
      if (estimateForm!="") {
        Legend <- "Estimate type"
        estimateForm <- "Colo"
      }
      else if (length(AGE)>1) {
        Legend <- "Age"
      }
    }
    else {
      Legend <- "Intent"
      if (estimateForm!="") {
        # varc <- "" #need AgeIntent cols & typ
        Legend <- "Estimate type by Intent"
        estimateForm <- "Colo2"
      }
      else if (length(AGE)>1) {
        Legend <- "Age by Intent"
      }
    }

    if ( input$Ashow==F | yax=="crci" | yax=="srci" ) {
      pd <- subset(pd, primary=="Cocaine-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)
    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | yax=="sr") ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,.3))
      Legend <- paste0("Death data type by<br>",Legend)
      LY(.9)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    }
    # if (varc=="" & vart=="" & estimateForm=="" & rvData==1) {
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

 # All drug deaths by jurisdiction, Intent, Sex & Age (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$AllPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    AGE <- input$ageAll
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    if (input$dropSI=="Intent") {
      INTENT <- input$AllIcod
      SEX <- input$sexC
      labt <- "Sex"
    }
    else if (input$dropSI=="Sex") {
      INTENT <- input$AllScod
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
      labt <- "Intent"
    }
    labc <- "Age"

    Title <- input$jur
    if (length(SEX)==1) Title <- paste0(Title,", ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)

    varc <- labc
    vart <- labt
    if (length(AGE)==1 & estimateForm=="") {
      varc <- ""
      Legend <- labt
    }
    if (input$dropSI=="Intent") {
      if (length(SEX)==1) {
        vart <- ""
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else {
          Legend <- varc
        }
      }
      else {
        if (estimateForm!="") {
          Legend <- "Estimate type by Sex"
          estimateForm <- "Colo2"
        }
        else if (length(AGE)>1) {
          Legend <- "Age by Sex"
        }
      }
    }
    else if (input$dropSI=="Sex") {
      if (length(input$AllScod)==1 | (length(input$AllScod)==2 & input$codS==2 & Split=="")) {
        vart <- ""
        if (length(input$AllScod)==2) Split <- labt
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else {
          Legend <- varc
        }
      }
      else {
        if (estimateForm!="") {
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
        }
        else if (length(AGE)>1) {
          Legend <- "Age by Intent"
        }
      }
    }

    pd <- subset( COD2019_All, jurisdiction==input$jur &
      Sex %in% SEX & Age %in% AGE & Intent %in% INTENT &
      (year>=yr[1] & year<=yr[2]) )

    rvData <- 1
    if (varc!="" | yax=="srci" | yax=="sr" | input$Rshow==F) { #| vart!="" | estimateForm!=""
      pd <- subset( pd, Release=="Dec 2020")
      # if (varc!="" | vart!="" | estimateForm!="") {
      #   rvData <- 0
      # }
      # else {
      #   Legend <- "Release\nversion"
      # }
    }
    else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
      rvData <- 2
      if (vart=="") {
        Legend <- "Release date"#\nby version"
      }
      else {
        Legend <- paste(Legend,"by Release date")
      }
    }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO()) #Pd=pd,Yax=yax,
  })

  # Remoteness by jurisdiction, Intent and Sex (Table R) ------------------------------------------
  output$RAPlot <- renderPlotly({
    yr <- input$yr09
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    INTENT <- input$cod2C
    if (input$jurR=="Australia") {
      REGION <- input$RAra
    }
    else {
      REGION <- input$Rra
    }

    Title <- paste0(input$jurR,", All ages") 

    pd <- subset(COD2019_Rem, jurisdiction==input$jurR & Age=="All ages" &
      Sex=="People" & Intent %in% INTENT & Region %in% REGION &
      (year>=yr[1] & year<=yr[2]) )

    # gp <- ggplot(pd) + aes(colour=RegionIntent, linetype=RegionIntent) +
    #     labs(title=paste0(input$jurR,", All ages") ) +
    #     scale_colour_manual(values=RegionIntentcols) +
    #     scale_linetype_manual(values=RegionIntenttype)

    vart <- "Intent"
    labt <- "Intent"
    varc <- "Region"
    labc <- "Region"

    if (length(INTENT)==1 & length(REGION)==1) {
      vart <- ""
      varc <- ""
      Legend <- ""
      Title <- paste0(Title,", Region: ",REGION,", Intent:", INTENT) 
    }
    else if (length(INTENT)==1) {
      vart <- ""
      Legend <- "Region"
      Title <- paste0(Title,", Intent:", INTENT) 
    }
    else if (length(REGION)==1) {
      varc <- ""
      Legend <- "Intent"
      Title <- paste0(Title,", Region: ",REGION) 
    }
    else {
    Legend <- "Region by Intent"
    }

    rvData <- 1
    if (varc!="" | yax=="srci" | yax=="sr" | input$Rshow==F) { #| vart!="" | estimateForm!=""
      pd <- subset( pd, Release=="Dec 2020")
      # if (varc!="" | vart!="" | estimateForm!="") {
      #   rvData <- 0
      # }
      # else {
      #   Legend <- "Release\nversion"
      # }
    }
    else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
      rvData <- 2
      if (vart=="") {
        Legend <- "Release date"#\nby version"
      }
      else {
        Legend <- paste(Legend,"by Release date")
      }
    }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$RPPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    pd <- subset(COD2019_Rem,Release=="Dec 2020")
    if (input$jurR=="Australia") {
      pd <- subset(pd, Intent==input$codR & 
            Age==input$ageR &
            (year>=input$yr09[[1]] & year<=input$yr09[[2]]) & 
            Sex==input$sexR & jurisdiction==input$jurR)
    }
    else {
      pd <- subset(pd, Intent==input$codR & 
            Age==input$ageR & 
            (year>=input$yr09[[1]] & year<=input$yr09[[2]]) & 
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
      scale_x_continuous(breaks=seq(input$yr09[[1]],input$yr09[[2]],2) )

    gp <- gp + theme_light() + theme(legend.title=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
    
    PlyFn(Gp=gp,Lt="Remoteness area",X=LX(),Y=LY(),O=LO())
  })

  # All drugs by type, jurisdiction, Intent and/or Sex (Table 12, 12b & 12c) ------------------------------------
  output$DTJPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
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
      labt <- "Intent"
      labc <- "Drug"
      if (length(DRUG)==1) {
        vart <- ""
        varc <- ""
        Legend <- ""
      }
      else {
        vart <- "Drug"
        varc <- "Drug"
        Legend <- "Drug involved"
      }

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
        if (length(INTENT)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          vart <- ""
          if (length(INTENT)==2 & input$codS==2) Split <- labt
          if (length(SEX)>1) {
            Legend <- "Sex"
          }
        }
        else if (length(SEX)>1) {
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
    if (varc!="" | yax=="srci" | yax=="sr" | input$Rshow==F) { #| vart!="" | estimateForm!=""
      pd <- subset( pd, Release=="Dec 2020")
      # if (varc!="" | vart!="" | estimateForm!="") {
      #   rvData <- 0
      # }
      # else {
      #   Legend <- "Release\nversion"
      # }
    }
    else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
      rvData <- 2
      Legend <- "Release date"#\nby version"
    }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:' to show all drugs involved.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # All drugs by type, Age & Intent (Tables 12 & 12a) ----------------------------------------------------------
  output$DTAPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$DTAdrop=="Drug") {
      AGE <- input$ageAll
      INTENT <- input$cod2C
      DRUG <- input$DTdrugD
      if (length(input$cod2C)==2 & input$codS==2) Split <- "Intent"
    }
    else if (input$DTAdrop=="Age_Intent") {
      AGE <- input$DTAIage
      INTENT <- input$DTAIcod
      DRUG <- input$DTdrug
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- "Australia"
    # if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(DRUG)==1) Title <- paste0(Title,", Drug involved: ",DRUG)

    if (input$DTAdrop=="Age_Intent") {
      labt <- "Intent"
      labc <- "Drug"
      if (length(DRUG)==1) {
        vart <- ""
        varc <- ""
        Legend <- ""
        if (estimateForm!="") {
          varc <- "Age" # dummy for colors with Drug as default varc
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
      }
      else {
        vart <- "Drug"
        varc <- "Drug"
        Legend <- "Drug involved"
      }
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
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else if (length(AGE)==1) {
          Legend <- ""
        }
        else {
          Legend <- "Age"
        }
      }
      else if (estimateForm!="") {
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
      }
      else if (length(AGE)==1) {
          Legend <- "Intent"
      }
      else {
        Legend <- "Age by Intent"
      }
    }

    pd <- subset( COD2019_DT, jurisdiction=="Australia" &
        Sex=="People" & Age %in% AGE & Intent %in% INTENT &
        Drug %in% DRUG & #nature=="Underlying" &
        (year>=yr[1] & year<=yr[2]) )
    rvData <- 1
    if (varc!="" | yax=="srci" | yax=="sr" | input$Rshow==F) { #| vart!="" | estimateForm!=""
      pd <- subset( pd, Release=="Dec 2020")
      # if (varc!="" | vart!="" | estimateForm!="") {
      #   rvData <- 0
      # }
      # else {
      #   Legend <- "Release\nversion"
      # }
    }
    else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
      rvData <- 2
      Legend <- "Release date"#\nby version"
    }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:'/'Age:' to show all drugs involved/age.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O4 - by opioid, Age & Intent (Table 4) -----------------------------------------------------------------
  output$O4Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    INTENT <- input$cod4C
    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      OPIOID <- input$OdrugO
      labc <- "Age"
    }
    else if (input$dropOA=="Age") {
      AGE <- input$ageOAA
      OPIOID <- input$OdrugC
      labc <- "Opioid"
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- "Australia"
    # if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(OPIOID)==1) Title <- paste0(Title,", Opioid involved: ",OPIOID)

    pd <- subset(COD2019_Op, subset=(jurisdiction=="Australia" &
      Sex=="People" & Intent %in% INTENT & Age %in% AGE &
      Opioid %in% OPIOID & (year>=yr[1] & year<=yr[2])))

    vart <- "Intent"
    labt <- "Intent"
    varc <- labc
    if (length( get(toupper(labc)) )==1) varc <- ""
    if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
      vart <- ""
      if (length(input$cod4C)==2) Split <- labt
      if (estimateForm!="") {
        if (varc!="Opioid") {
          varc <- "Age" # dummy for colors
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else {
          vart <- labt
          Legend <- "Estimate type by Opioid"
          estimateForm <- "Type2"
        }
      }
      else {
        Legend <- varc
      }
    }
    else if (estimateForm!="" & varc!="Opioid") {
      varc <- "Age" # dummy for colors
      Legend <- "Estimate type by Intent"
      estimateForm <- "Colo2"
    }
    else {
      Legend <- paste(labc,"by Intent")
    }
    if (length( get(toupper(labc)) )==1 & estimateForm<"Colo") Legend <- vart

    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & ( input$dropOA=="Age" &
      (length(input$cod4C)>2 | (length(input$cod4C)==2 & input$codS==1) ) )) ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & ( input$dropOA=="Drug" |
      length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) )) ) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    }
    # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O5 - by opioid, Intent & Sex (Table 5) -----------------------------------------------------------------
  output$O5Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""
    Legend <- ""

    SEX <- input$sexC
    INTENT <- input$cod4C
    OPIOID <- input$OdrugC
    if (input$O5drop=="Opioid") {
      OPIOID <- input$OdrugO
      labt <- "Intent"
      labc <- "Sex"
    }
    else if (input$O5drop=="Intent") {
      INTENT <- input$cod4R
      labt <- "Sex"
      labc <- "Opioid"
    }
    else if (input$O5drop=="Sex") {
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
      labt <- "Intent"
      labc <- "Opioid"
    }

    Title <- "Australia"
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(OPIOID)==1) Title <- paste0(Title,", Opioid involved: ",OPIOID)

    pd <- subset(COD2019_Op, Age==AGE & 
      Opioid %in% OPIOID & Intent %in% INTENT & Sex %in% SEX &
      jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]))

    vart <- labt
    varc <- labc
    if (length(get(toupper(varc)))==1) {
      if (estimateForm!="") {
        varc <- "Age" # dummy for colors
        estimateForm <- "Colo2"
        Legend <- paste("Estimate type by",vart)
      }
      else {
        varc <- ""
        Legend <- vart
      }
    }
    if (input$O5drop!="Intent") {
      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2 & Split=="") ) {
        if (length(input$cod4C)==2) Split <- labt
        if (varc==labc & estimateForm!="") {
            estimateForm <- "Type2"
            Legend <- paste("Estimate type by",varc)
        }
        else {
          vart <- ""
          if (estimateForm!="" & varc=="Age") {
            estimateForm <- "Colo"
            Legend <- "Estimate type"
          }
          else {
            Legend <- varc
          }
        }
      }
      else if (varc==labc) {
        Legend <- paste(varc,"by",vart)
      }
    }
    else if (input$O5drop=="Intent") {
      # vart <- "Sex"
      # varc <- "Opioid"
      if (length(SEX)==1) {
        if (varc==labc & estimateForm!="") {
            vart <- "Intent" # dummy for linetype
            estimateForm <- "Type2"
            Legend <- paste("Estimate type by",varc)
        }
        else {
          vart <- ""
          if (estimateForm!="" & varc=="Age") {
            estimateForm <- "Colo"
            Legend <- "Estimate type"
          }
          else {
            Legend <- varc
          }
        }
      }
      else if (varc==labc) {
        Legend <- paste(varc,"by",vart)
      }
    }

    # rvData <- 0
    # if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & ( (input$O5drop!='Intent' & (length(input$cod4C)>2 | (length(input$cod4C)==2 & input$codS==1))) + 
            (input$O5drop!='Sex' & length(input$sexC)>1) + (input$O5drop!='Opioid' & length(input$OdrugC)>1) )>1)) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & ( (input$O5drop!='Intent' & (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2))) | 
            (input$O5drop!='Sex' & length(input$sexC)==1) | (input$O5drop!='Opioid' & length(input$OdrugC)==1) ))) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    }
    # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected"))
    # gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # O6 - by jurisdiction, Sex & Intent (Table 6) -------------------------------------------
  output$O6Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    pd <- subset(COD2019_Op, Age==AGE & Opioid=="All opioids" &
        jurisdiction==input$jur & Intent %in% input$cod2C & Sex %in% input$sexC &
        (year>=yr[1] & year<=yr[2]))

    # gp <- ggplot(pd) +
    #   aes(colour=SexIntent, linetype=SexIntent) +
    #   labs(title=paste0(input$jur,", Age: ",AGE) ) +
    #   scale_colour_manual(values=SexIntentcols) +
    #   scale_linetype_manual(values=SexIntenttype)

    Title <- paste0(input$jur,", Age: ",AGE)
    vart <- "Intent"
    labt <- "Intent"
    varc <- "Sex"
    labc <- "Sex"
    Legend <- "Sex by Intent"
    if (length(input$sexC)==1) {
      Title <- paste0(Title,", Sex: ",input$sexC)
      if (estimateForm!="") {
        estimateForm <- "Colo2"
        Legend <- "Estimate type by Intent"
      }
      else {
        varc <- ""
        Legend <- vart
      }
    }
    if (length(input$cod2C)==1) {
      Title <- paste0(Title,", Intent: ",input$cod2C)
      if (estimateForm!="") {
        estimateForm <- "Type2"
        Legend <- "Estimate type by Sex"
      }
      else {
        vart <- ""
        Legend <- varc
      }
    }
    if (vart=="" & varc=="" & estimateForm!="") {
      varc <- "Age" #dummy for color
      estimateForm <- "Colo"
      Legend <- "Estimate type"
    }

    # rvData <- 0
    # if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" &  length(input$sexC)>1 & length(input$cod2C)>1) ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & (length(input$sexC)==1 | length(input$cod2C)==1))) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    }
    # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected"))
    # gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids with other drugs by Age & Intent (Table 7) ----------------------------------------------------------
  output$W7Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    INTENT <- input$cod4C
    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      DRUG <- input$W7Ddrug
      labc <- "Age"
    }
    else if (input$dropOA=="Age") {
      AGE <- input$ageOAA
      DRUG <- input$Wdrug
      labc <- "Drug"
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- "Australia"
    # if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(DRUG)==1) Title <- paste0(Title,", Drug involved: ",DRUG)

    pd <- subset(COD2019_OpW, subset=(#jurisdiction=="Australia" &
      Sex=="People" & Intent %in% INTENT & Age %in% AGE &
      Drug %in% DRUG & (year>=yr[1] & year<=yr[2])))

    vart <- "Intent"
    labt <- "Intent"
    varc <- labc
    if (length( get(toupper(labc)) )==1) varc <- ""
    if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
      vart <- ""
      if (length(input$cod4C)==2) Split <- labt
      if (estimateForm!="") {
        if (varc!="Drug") {
          varc <- "Age" # dummy for colors
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
        else {
          vart <- labt
          Legend <- "Estimate type by Drug"
          estimateForm <- "Type2"
        }
      }
      else {
        Legend <- varc
      }
    }
    else if (estimateForm!="" & varc!="Drug") {
      varc <- "Age" # dummy for colors
      Legend <- "Estimate type by Intent"
      estimateForm <- "Colo2"
    }
    else {
      Legend <- paste(labc,"by Intent")
    }
    if (length(get(toupper(labc)))==1 & estimateForm<"Colo") Legend <- vart

    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & ( input$dropOA=="Age" &
      (length(input$cod4C)>2 | (length(input$cod4C)==2 & input$codS==1)) )) ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | ( yax=="sr" & (input$dropOA=="Drug" |
      length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2)) )) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    }
    # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids and other drugs by Sex (Table 8) ------------------------------------------
  output$W8Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    if (input$dropSI=="Sex") {
      INTENT <- input$cod4C
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
      labt <- "Intent"
    }
    else if (input$dropSI=="Intent") {
      INTENT <- input$cod4R
      SEX <- input$sexC
      labt <- "Sex"
    }

    Title <- paste0("Australia, Age: ",AGE)
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(input$Wdrug)==1) Title <- paste0(Title,", Drug involved: ",input$Wdrug)

    pd <- subset(COD2019_OpW, Drug %in% input$Wdrug &
      Age==AGE & Intent %in% INTENT & Sex %in% SEX &
      (year>=yr[1] & year<=yr[2]) )

    varc <- "Drug"
    labc <- "Drug"
    Legend <- paste(labc,"by",labt)
    if (length(input$Wdrug)==1) {
      varc <- ""
      Legend <- labt
      if (estimateForm!="") {
        varc <- "Age" # dummy for colors with Drug as default varc
        # if (vart=="") {
        #   Legend <- paste("Estimate type")
        #   estimateForm <- "Colo"
        # }
        # else {
          Legend <- paste("Estimate type by",labt)
          estimateForm <- "Colo2"
        # }
      }
    }
    if (input$dropSI=="Sex") {
      vart <- "Intent"
      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2 & Split=="") ) {
        if (estimateForm!="") {
          if (varc=="Drug") {
            Legend <- "Estimate type by Drug"
            estimateForm <- "Type2"
          }
          else {
            vart <- ""
            Legend <- paste("Estimate type")
            estimateForm <- "Colo"
          }
        }
        else {
          vart <- ""
          Legend <- varc
        }
        if (length(input$cod4C)==2) Split <- labt
      }
    }
    else if (input$dropSI=="Intent") {
      vart <- "Sex"
      if (length(SEX)==1) {
        if (estimateForm!="") {
          if (varc=="Drug") {
            Legend <- "Estimate type by Drug"
            estimateForm <- "Type2"
          }
          else {
            vart <- ""
            Legend <- paste("Estimate type")
            estimateForm <- "Colo"
          }
        }
        else {
          vart <- ""
          Legend <- varc
        }
      }
    }

    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & estimateForm=="Alph") ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & estimateForm!="Alph")) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    }
    # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive Opioids by Age and Intent (Table 10) --------------------------------------------------------------
  output$E0Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      OPIOID <- input$E0Odrug
      labc <- "Age"
    }
    else if (input$dropOA=="Age") {
      AGE <- input$ageOAA
      OPIOID <- input$Edrug
      labc <- "Opioid"
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- paste0("Australia, People")
    if (length(OPIOID)==1) Title <- paste0(Title,", Opioid involved: ",OPIOID)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(input$cod3C)==1) Title <- paste0(Title,", Intent: ",input$cod3C)

    pd <- filter(COD2019_OpE, Sex=="People" & jurisdiction=="Australia" &
        Opioid %in% OPIOID & Intent %in% input$cod3C & Age %in% AGE &
        (year>=yr[1] & year<=yr[2]))
      
    vart <- "Intent"
    labt <- "Intent"
    if (length(input$cod3C)==1 | (length(input$cod3C)==2 & input$codS==2) ) {
      vart <- ""
      if (length(input$cod3C)==2) Split <- labt
      Legend <- labc
    }
    # if (length(input$cod3C)==1) vart <- ""
    if (input$dropOA=="Drug") {
      varc <- "Age"
      # labc <- "Age"
      # AGE <- input$ageAll
      # if (yax=="sr" | yax=="srci") AGE <- "All ages"
      if (estimateForm!="") {
        if (vart=="") {
          Legend <- paste("Estimate type")
          estimateForm <- "Colo"
        }
        else {
          Legend <- paste("Estimate type by",labt)
          estimateForm <- "Colo2"
        }
      }
      else if (length(AGE)==1) {
        varc <- ""
        Legend <- vart
      }
      else {
        Legend <- "Age by Intent"
      }
    }
    else if (input$dropOA=="Age") {
      varc <- "Opioid"
      # labc <- "Opioid"
      # AGE <- input$ageOAA
      # if (yax=="sr" | yax=="srci") AGE <- "All ages"
      if (length(OPIOID)==1) {
        if (estimateForm!="") {
          varc <- "Age" # dummy for colors with Opioid as default varc
          if (vart=="") {
            Legend <- paste("Estimate type")
            estimateForm <- "Colo"
          }
          else {
            Legend <- paste("Estimate type by",labt)
            estimateForm <- "Colo2"
          }
        }
        else {
          varc <- ""
          Legend <- vart
        }
      }
      else {
        Legend <- "Opioid by Intent"
      }
    }

    # rvData <- 0
    # if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive opioids by jurisdiction, Intent and Sex (Table 9 & 11) -----------------------
  output$E9Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- input$ageR
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    # OPIOID <- input$Edrug
    if (input$dropSI=="Intent") {
      INTENT <- input$E9Icod
      SEX <- input$sexC
    }
    else if (input$dropSI=="Sex") {
      INTENT <- input$cod3C
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
    }

    Title <- paste0(input$jur,", Age: ",AGE)
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    
    pd <- filter(COD2019_OpE, jurisdiction==input$jur & Age==AGE &
        Sex %in% SEX & Intent %in% INTENT & Opioid %in% input$Edrug & 
          (year>=yr[1] & year<=yr[2]) )

    varc <- "Opioid"
    labc <- "Opioid"
    if (input$dropSI=="Sex") {
      vart <- "Intent"
      labt <- "Intent"
      Legend <- "Opioid by Intent"
      if (length(INTENT)==1 | (length(input$cod3C)==2 & input$codS==2 & Split=="") ) {
        vart <- ""
        if (length(input$cod3C)==2) Split <- labt
        Legend <- labc
      }
    }
    else if (input$dropSI=="Intent") {
      vart <- "Sex"
      labt <- "Sex"
      Legend <- "Opioid by Sex"
      if (length(SEX)==1) {
        vart <- ""
        Legend <- labc
      }
    }
    if (length(input$Edrug)==1) {
      varc <- ""
      Legend <- vart
      if (estimateForm!="") {
        varc <- "Age" # dummy for colors with Opioid as default varc
        if (vart=="") {
          Legend <- paste("Estimate type")
          estimateForm <- "Colo"
        }
        else {
          Legend <- paste("Estimate type by",labt)
          estimateForm <- "Colo2"
        }
      }
    }

    # rvData <- 0
    # if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$EPPlot <- renderPlotly({
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
      scale_fill_manual(values=Opioidcols)
    
    validate(need(nrow(pd) > 0, "No data selected"))
    
    gp <- gp + theme_light() + theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())

    PlyFn(Gp=gp,Lt="Opioid",X=LX(),Y=LY(),O=LO())
  })

}
