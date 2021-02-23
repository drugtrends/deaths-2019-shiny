#For ABS COD 2018 data received in Sept 2019
#N. Man
#library(shiny) #already loaded in UI
library(ggplot2)
#library(plotly) #already loaded in UI
library(shinycustomloader)

#https://appsilon.com/fast-data-loading-from-files-to-r/
# load("death_2018.Rdata")
load("death_2019.Rdata")

PlotFn <- function(Pd,Gp,Yax,Yr,Varc="age_group",Varl="intent",Labc="<br>Age: ",Labl="<br>Intent: ") {
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
          "<br>Crude rate: ",round(cr, 2)," (", round(cr_lci, 2),",", round(cr_uci, 2),")",
          Labl,get(Varl),
          Labc,get(Varc)
        ))) +
        geom_ribbon(aes(ymin=cr_lci, ymax=cr_uci), alpha=0.1, size=0)
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$cr_uci, 2.5))) +
      labs(y="Crude mortality rate per 100,000")
  }

  else if (Yax=="sr" | Yax=="srci") {
    if (Yax=="sr") {
      Gp <- Gp + geom_line() + aes(y=sr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n,
          "<br>Age standardised rate: ",round(sr, 2)," (", round(sr_lci, 2),",", round(sr_uci, 2),")",
          Labl,get(Varl),
          Labc,get(Varc)
        ))
    }
    else {
      Gp <- Gp + geom_line(aes(y=sr, text=paste0(
          "Year: ",year,
          "<br>Deaths: ",n,
          "<br>Rate: ",round(sr, 2)," (", round(sr_lci, 2),",", round(sr_uci, 2),")",
          Labl,get(Varl),
          Labc,get(Varc)
        ))) + geom_ribbon(aes(ymin=sr_lci, ymax=sr_uci), alpha=0.1, size=0)
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$sr_uci, 2.5))) +
      labs(y="Age standardised mortality rate per 100,000")
  }

  Gp <- Gp + aes(x=year, group=1) + labs(x="Year") +
    scale_x_continuous(breaks=seq(Yr[[1]],Yr[[2]],2)) +
    theme_light() + theme(
      legend.title=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank())
  return(Gp)
}

PlyFn <- function(Gp=gp,Lt=Legend,O=LO(),X=LX(),Y=LY()) {
    ggplotly(Gp, tooltip="text") %>%
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
    sub <- subset(ABS_COD2018_Stim, jurisdiction=="Australia" & sex=="All" & 
           drug=="Amphetamines" & nature=="Underlying" & intent %in% input$cod2C & 
           age_group %in% input$ageAll & (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
    if ( input$Ashow==F ) {
      sub <- subset(sub, set=="Stimulants")
    }

    gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
    if (dim.data.frame(input$cod2C)[2]==1) {
      gp <- gp + aes(colour=age_group) +
        labs(title=paste0("Intent: ",input$cod2C) )
      Legend <- "Age"
    }
    else {
      gp <- gp + aes(colour=age_intent, linetype=age_intent) +
        scale_linetype_manual(values=agecodtype)
      Legend <- "Age by intent"
    }
    if ( input$Ashow==T ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,0.3) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    varl <- "intent"
    labl <- "<br>Intent: "
    varc <- "age_group"
    labc <- "<br>Age: "
    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    yax <- input$yax
  ####For user-defined year intervals
  #     yr <- as.numeric(input$xax)
  #     yr <- (input$yr97[[2]]-input$yr97[[1]])/yr
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc) #+
  ####For user-defined year intervals
  #     function(x) unique(floor( pretty(x,n=yr) ) )
  #     scale_x_continuous(breaks=seq(input$yr97[[1]],input$yr97[[2]],2) )

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$CPlot <- renderPlotly({
    sub <- subset(ABS_COD2018_Stim, drug=="Cocaine" & intent %in% input$cod2C &
        nature=="Underlying" & age_group %in% input$Cage & sex=="All" &
        jurisdiction=="Australia" & (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
    if ( input$Ashow==F ) {
      sub <- subset(sub, set=="Stimulants")
    }
    
    if (dim.data.frame(input$cod2C)[2]==1 & dim.data.frame(input$Cage)[2]==1) {
      gp <- ggplot(sub) + labs(title="Age: ",input$Cage, ", Intent: ",input$cod2C )
      Legend <- "Age"
    }
    else if (dim.data.frame(input$cod2C)[2]==1) {
      gp <- ggplot(sub) + aes(colour=age_group) + 
        scale_colour_manual(values=age2codcols) +
        labs(title="Intent: ",input$cod2C )
      Legend <- "Age"
    }
    else if (dim.data.frame(input$Cage)[2]==1) {
      gp <- ggplot(sub) + aes(linetype=intent) + 
        scale_linetype_manual(values=agecodtype) +
        labs(title="Age: ",input$Cage )
      Legend <- "Intent"
    }
    else {
      gp <- ggplot(sub) + aes(colour=age_intent, linetype=age_intent) + 
        scale_colour_manual(values=age2codcols) +
        scale_linetype_manual(values=agecodtype)
      Legend <- "Age by intent"
    }
    if ( input$Ashow==T ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(1,0.3) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    varl <- "intent"
    labl <- "<br>Intent: "
    varc <- "age_group"
    labc <- "<br>Age: "
    validate(need(nrow(sub) > 0, "No data selected"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

 # All drug deaths by jurisdiction, intent, sex & age (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$AllPlot <- renderPlotly({
    if (input$dropSI=="Intent") {
      sub <- subset(ABS_COD2019_All, age_group %in% input$ageAll & jurisdiction==input$jur &
        intent==input$AllIcod & sex %in% input$sexC &
        (year>=input$yr97[[1]] & year<=input$yr97[[2]]) )
      gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
      if (dim.data.frame(input$sexC)[2]==1) {
        gp <- gp + aes(colour=age_group) + 
          labs(title=paste0(input$jur,", Intent: ",input$AllIcod,", Sex: ",input$sexC) )
        Legend <- "Age"
      }
      else {
        gp <- gp + aes(colour=age_sex, linetype=age_sex) +
          scale_linetype_manual(values=agecodtype) +
          labs(title=paste0(input$jur,", Intent: ",input$AllIcod) )
        Legend <- "Age & sex"
      }
      varl <- "sex"
      labl <- "<br>Sex: "
      varc <- "age_group"
      labc <- "<br>Age: "
    }
    else if (input$dropSI=="Sex") {
      if (input$sex4R != "MF") {
        sub <- subset(ABS_COD2019_All, subset=(
          age_group %in% input$ageAll & jurisdiction==input$jur &
          intent %in% input$AllScod & sex==input$sex4R &
          (year>=input$yr97[[1]] & year<=input$yr97[[2]]) ) )
        gp <- ggplot(sub)
        Title <- paste0(input$jur,", Sex: ",input$sex4R)
      }
      else {
        sub <- subset(ABS_COD2019_All, subset=(
          age_group %in% input$ageAll & jurisdiction==input$jur &
          intent %in% input$AllScod & sex != "All" &
          (year>=input$yr97[[1]] & year<=input$yr97[[2]]) ) )
        
        if (input$dimension<992) {
          gp <- ggplot(sub) + facet_grid(rows=vars(sex))
        }
        else {
          gp <- ggplot(sub) + facet_grid(cols=vars(sex))
          if (input$dimension<1200) {
            LX(0)
            LO("h")
            LY(-.2)
          }
        }
        Title <- input$jur
      }

      if (dim.data.frame(input$AllScod)[2]==1) {
        gp <- gp + aes(colour=age_group) + 
          scale_colour_manual(values=agecodcols) +
          labs(title=paste0(Title,", Intent: ",input$AllScod) )
        Legend <- "Age"
      }
      else if (dim.data.frame(input$ageAll)[2]==1) {
        gp <- gp + aes(linetype=intent) + 
          scale_linetype_manual(values=agecodtype) +
          labs(title=paste0(Title,", Age: ",input$ageAll) )
        Legend <- "Intent"
      }
      else {
        gp <- gp + aes(colour=age_intent, linetype=age_intent) + 
          scale_colour_manual(values=agecodcols) +
          scale_linetype_manual(values=agecodtype) +
          labs(title= Title)
        Legend <- "Age by intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age_group"
      labc <- "<br>Age: "
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)

    gp <- gp + theme(strip.background=element_rect(fill="#dbefd4"),
      strip.text=element_text(color="#000000", face="bold") )
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Remoteness by jurisdiction, intent and sex (Table R) ------------------------------------------
  output$RAPlot <- renderPlotly({
    if (input$jurR=="Australia") {
      regR <- input$RAra
    }
    else {
      regR <- input$Rra
    }
    sub <- subset(ABS_COD2018_Rem, jurisdiction==input$jurR & age_group=="All ages" &
      sex=="All" & intent %in% input$cod2C & region %in% regR &
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
    yax <- input$yax
    yr <- input$yr11
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$RPPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
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

    if (input$jurR=="Australia" & input$sexR=="All" & input$ageR=="All ages" ) {
      sub <- filter(sub, region!="Regional and Remote") %>%
        group_by(year, intent, sex, jurisdiction, age_group) %>% 
        distinct() %>%
        mutate(percent=round(n/sum(n)*100, 2),
          region=factor(region, levels=c( "Remote and Very Remote",
            "Outer Regional", "Inner Regional", "Major Cities"
          )))
    }
    else {
      sub <- filter(sub, region=="Regional and Remote" | region=="Major Cities" ) %>%
        group_by(year, intent, sex, jurisdiction, age_group) %>% 
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
        "<br>Age: ",age_group,
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
    if (input$DTJdrop=="IntSx") {
      if (input$DTjur=="Australia") {
        if (input$DTIsex=="All") {
          DTIcod=input$DTIcod
        }
        else {
          DTIcod=input$DTIcod2
        }
        if (input$DTIsex!="MF") {
          sub <- subset(ABS_COD2019_DT, intent==DTIcod & drug %in% input$DTdrug
            & age_group==input$DTage & sex==input$DTIsex & jurisdiction==input$DTjur
            & (year>=input$yr97[[1]] & year<=input$yr97[[2]] ))
          gp <- ggplot(sub)
          Title <- paste0(input$DTjur,", Age: ",input$DTage,", Sex: ",input$DTIsex,", Intent: ",DTIcod)
        }
        else {
          sub <- subset(ABS_COD2019_DT, intent==DTIcod & drug %in% input$DTdrug
            & age_group==input$DTage & sex != "All" & jurisdiction==input$DTjur
            & (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
          if (input$dimension<992) {
            gp <- ggplot(sub) + facet_grid(rows=vars(sex))
          }
          else {
            gp <- ggplot(sub) + facet_grid(cols=vars(sex))
            if (input$dimension<1200) {
              LX(0)
              LO("h")
              LY(-.2)
            }
          }
          Title <- paste0(input$DTjur,", Age: ",input$DTage,", Intent: ",DTIcod)
        }
      }
      if (input$DTjur != "Australia") {
        sub <- subset(ABS_COD2019_DT, intent==input$DTIcod2 & drug %in% input$DTdrug
          & age_group=="All ages" & sex=="All" & jurisdiction==input$DTjur
          & (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
        gp <- ggplot(sub)
        Title <- paste0(input$DTjur,", Age: All ages, Sex: All persons, Intent: ",input$DTIcod2)
      }
      gp <- gp + aes(colour=drug, linetype=drug) +
        scale_colour_manual(values=dtcols) +
        scale_linetype_manual(values=dttype)
      Legend <- "Drug involved"
      
      if (input$DTIsex!="All") {
        validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Drug involved: "
    }
    
    if (input$DTJdrop=="Drug") {
      if (input$DTjur=="Australia") {
        sub <- subset(ABS_COD2019_DT, intent %in% input$cod4C & drug==input$DTdrugD & 
          age_group==input$DTage & sex %in% input$sexC & jurisdiction==input$DTjur &
          (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
        gp <- ggplot(sub) +
          scale_colour_manual(values=sexcols)
        Title <- paste0(input$DTjur,", Age: ",input$DTage,", Drug: ",input$DTdrugD)

        if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
          gp <- gp + aes(colour=sex)
          Legend <- "Sex"
          if (dim.data.frame(input$cod4C)[2]==1) {
            Title <- paste0(Title,", Intent: ",input$cod4C)
          }
          if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
            if (input$dimension<992) {
              gp <- gp + facet_grid(rows=vars(intent))
            }
            else {
              gp <- gp + facet_grid(cols=vars(intent))
              if (input$dimension<1200) {
                LX(0)
                LO("h")
                LY(-.2)
              }
            }
          }
        }
        else {
          gp <- gp + aes(colour=sex_intent, linetype=sex_intent) +
            scale_linetype_manual(values=sexcodtype)
          Legend <- "Sex by intent"
        }

        if (input$sexC[[1]]!="All") {
          validate(need(nrow(sub) > 0, "Please select All ages for age range for data by male and/or female."))
        }
      }
      if (input$DTjur != "Australia") {
        sub <- subset(ABS_COD2019_DT, intent %in% input$cod2C & drug==input$DTdrugD & 
          age_group=="All ages" & sex=="All" & jurisdiction==input$DTjur &
          (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
        gp <- ggplot(sub) + aes(linetype=intent) +
          scale_linetype_manual(values=codtype)
        Title <- paste0(input$DTjur,", Age: All ages, Sex: All persons, Drug: ",input$DTdrugD)
        Legend <- "Intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "sex"
      labc <- "<br>Sex: "
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:' to show all drugs involved.)"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)
    # if (input$yax=="num") {
    #   gp <- gp + aes(y=n, text=paste0(
    #       "Year: ",year,
    #       "<br>Deaths: ",n,
    #       "<br>Drug: ",toupper(substring(drug, 1,1)), tolower(substring(drug, 2)),
    #       "<br>Intent: ",intent,
    #       "<br>Sex: ",sex,
    #       "<br>Age: ",age_group
    #     )
    #     ) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
    #     labs(y="Number of deaths")
    # }

    # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    gp <- gp + labs(title=Title) +
      theme(strip.background=element_rect(fill="#dbefd4"),
        strip.text=element_text(color="#000000", face="bold") )

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # All drugs by type, age & intent (Tables 12 & 12a) ----------------------------------------------------------
  output$DTAPlot <- renderPlotly({
    if (input$DTAdrop=="Age_Intent") {
      sub <- subset(ABS_COD2019_DT, sex=="All" & age_group==input$DTAIage &
          drug %in% input$DTdrug & intent==input$DTAIcod & nature=="Underlying" &
          jurisdiction=="Australia" & (year>=input$yr97[[1]] & year<=input$yr97[[2]]) )
      gp <- ggplot(sub) + aes(colour=drug, linetype=drug) +
        labs(title=paste0("Age: ",input$DTAIage,", intent: ",input$DTAIcod)) +
        scale_colour_manual(values=dtcols) +
        scale_linetype_manual(values=dttype)
      Legend <- "Drug involved"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Drug involved: "
    }
    if (input$DTAdrop=="Drug") {
      sub <- subset(ABS_COD2019_DT, age_group %in% input$ageAll & sex=="All" &
          intent %in% input$cod2C & nature=="Underlying" & drug==input$DTdrugD &
          jurisdiction=="Australia" & (year>=input$yr97[[1]] & year<=input$yr97[[2]]) )
      gp <- ggplot(sub) + aes(colour=age_intent, linetype=age_intent) +
        labs(title=paste0("Drug-induced deaths involving ",input$DTdrugD)) +
        scale_colour_manual(values=agecodcols) +
        scale_linetype_manual(values=agecodtype)
      Legend <- "Age by intent"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age_group"
      labc <- "<br>Age: "
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:'/'Age:' to show all drugs involved/age.)"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)
    # if (input$yax=="num") {
    #   gp <- gp + aes(y=n, text=paste0(
    #     "Year: ",year,
    #     "<br>Deaths: ",n,
    #     "<br>Drug: ",toupper(substring(drug, 1,1)), tolower(substring(drug, 2)),
    #     "<br>Intent: ",intent,
    #     "<br>Age: ",age_group
    #   )
    #   ) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
    #     labs(y="Number of deaths")
    # }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O4 - by opioid, age & intent (Table 4) -----------------------------------------------------------------
  output$O4Plot <- renderPlotly({
    if (input$dropOA=="Drug") {
      sub <- subset(ABS_COD2019_Op, subset=(sex=="All" & location=="Aus" & drug==input$OdrugO &
                 intent %in% input$cod4C & age_group %in% input$ageAll &
                 (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
      gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
      Title <- input$OdrugO

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        gp <- gp + aes(colour=age_group)
        Legend <- "Age"
     }
      else {
        gp <- gp + aes(colour=age_intent, linetype=age_intent) +
          scale_linetype_manual(values=agecodtype)
        Legend <- "Age by intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age_group"
      labc <- "<br>Age: "
    }

    else if (input$dropOA=="Age") {
      sub <- subset(ABS_COD2019_Op, subset=(sex=="All" & location=="Aus" & drug %in% input$OdrugC &
        intent %in% input$cod4C & age_group==input$ageOAA &
        (year>=input$yr97[[1]] & year<=input$yr97[[2]])))
      gp <- ggplot(sub) + scale_colour_manual(values=opcodcols)
      Title <- paste0("Age: ",input$ageOAA)

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
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

    if (dim.data.frame(input$cod4C)[2]==1) {
      Title <- paste0(Title,", Intent: ",input$cod4C)
    }
    if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
       if (input$dimension<992) {
        gp <- gp + facet_grid(rows=vars(intent))
      }
      else {
        gp <- gp + facet_grid(cols=vars(intent))
        if (input$dimension<1200) {
          LX(0)
          LO("h")
          LY(-.2)
        }
      }
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)
    
    gp <- gp + labs(title=Title) +
      theme(strip.background=element_rect(fill="#dbefd4"),
        strip.text=element_text(color="#000000", face="bold") )
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # O5 - by opioid, intent & sex (Table 5)-----------------------------------------------------------------
  output$O5Plot <- renderPlotly({
    if (input$O5drop=="Opioid") {
      sub <- subset(ABS_COD2019_Op, age_group==input$ageR & 
          drug==input$OdrugO & intent %in% input$cod4C & sex %in% input$sexC &
          location=="Aus" & (year>=input$yr97[[1]] & year<=input$yr97[[2]]))

      gp <- ggplot(sub) + scale_colour_manual(values=sexcols)
      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
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
      sub <- subset(ABS_COD2019_Op, subset=(age_group==input$ageR &
        drug %in% input$OdrugC & sex %in% input$sexC & intent ==input$cod4R & 
        location=="Aus" & (year>=input$yr97[[1]] & year<=input$yr97[[2]])))

      gp <- ggplot(sub) + aes(colour=op_sex, linetype=op_sex) +
        scale_colour_manual(values=opcodcols) +
        scale_linetype_manual(values=opcodtype)
      Legend <- "Drug by sex"
      Title <- paste0("Intent: ",input$cod4R)
      varl <- "drug"
      labl <- "<br>Opioid: "
      varc <- "sex"
      labc <- "<br>Sex: "
    }
    else if (input$O5drop=="Sex") {
      Legend <- "Drug by intent"
      if (input$sex4R != "MF") {
        sub <- subset(ABS_COD2019_Op, age_group==input$ageR & location=="Aus" &
            drug %in% input$OdrugC & intent %in% input$cod4C & sex==input$sex4R &
            (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
        
        gp <- ggplot(sub)
        Title <- paste0("Sex: ",input$sex4R)
        if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
          gp <- gp + aes(colour=drug)
          Legend <- "Drug"
        }
        else {
          gp <- gp + aes(colour=op_intent, linetype=op_intent) +
            scale_linetype_manual(values=opcodtype)
        }
      }
      else {
        sub <- subset(ABS_COD2019_Op, age_group==input$ageR & location=="Aus" &
            drug %in% input$OdrugC & intent %in% input$cod4C & sex != "All" &
            (year>=input$yr97[[1]] & year<=input$yr97[[2]]))
        
        gp <- ggplot(sub) + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opcodtype)
        Title <- "Females & Males"
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
      gp <- gp + scale_colour_manual(values=opcodcols)
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Opioid: "
    }

    if (input$O5drop != "Intent") {
      if (dim.data.frame(input$cod4C)[2]==1) {
        Title <- paste0(Title,", Intent: ",input$cod4C)
      }
      if ( dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2 &
        ((input$O5drop=="Sex" & input$sex4R != "MF") | input$O5drop=="Opioid") ) {
        if (input$dimension<992) {
          gp <- gp + facet_grid(rows=vars(intent))
        }
        else {
          gp <- gp + facet_grid(cols=vars(intent))
          if (input$dimension<1200) {
            LX(0)
            LO("h")
            LY(-.2)
          }
        }
      }
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)

    Title <- paste0(Title,", Age: ",input$ageR)
    gp <- gp + labs(title=Title) +
      theme(strip.background=element_rect(fill="#dbefd4"),
        strip.text=element_text(color="#000000", face="bold") )
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # O6 - by jurisdiction, sex & intent (Table 6) -------------------------------------------
  output$O6Plot <- renderPlotly({
    sub <- subset(ABS_COD2019_Op, age_group==input$ageR & drug=="All opioids" &
        jurisdiction==input$jur & intent %in% input$cod2C & sex %in% input$sexC &
        (year>=input$yr97[[1]] & year<=input$yr97[[2]]))

    gp <- ggplot(sub) +
      aes(colour=sex_intent, linetype=sex_intent) +
      labs(title=paste0(input$jur,", ",input$ageR) ) +
      scale_colour_manual(values=sexcols) +
      scale_linetype_manual(values=sexcodtype)

    varl <- "intent"
    labl <- "<br>Intent: "
    varc <- "sex"
    labc <- "<br>Sex: "

    validate(need(nrow(sub) > 0, "No data selected"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)

    PlyFn(Gp=gp,Lt="Sex by intent",X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids with other drugs by age & intent (Table 7) ----------------------------------------------------------
  output$W7Plot <- renderPlotly({
    sub <- subset(ABS_COD2019_OpW, intent %in% input$cod4C &
        (year>=input$yr97[[1]] & year<=input$yr97[[2]]) & sex=="All")
    if ( input$Ashow==F ) {
      sub <- subset(sub, set=="OpioidW")
    }

    if (input$dropOA=="Drug") {
      sub <- subset(sub, drug==input$W7Ddrug & age_group %in% input$ageAll)
  
      gp <- ggplot(sub) + scale_colour_manual(values=agecodcols)
      Title <- paste0("All opioids with ",input$W7Ddrug)

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        gp <- gp + aes(colour=age_group)
        Legend <- "Age"
      }
      else {
        gp <- gp + aes(colour=age_intent, linetype=age_intent) +
          scale_linetype_manual(values=agecodtype)
        Legend <- "Age by intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age_group"
      labc <- "<br>Age: "
    }
    if (input$dropOA=="Age") {
      sub <- subset(sub, drug %in% input$Wdrug & age_group==input$ageOAA)
      
      gp <- ggplot(sub) + scale_colour_manual(values=opWcodcols)
      Title <- paste0("Age: ",input$ageOAA)

      if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
        gp <- gp + aes(colour=drug)
        Legend <- "Drug"
      }
      else {
        gp <- gp + aes(colour=op_intent, linetype=op_intent) +
          scale_linetype_manual(values=opWcodtype)
        Legend <- "Drug by intent"
      }
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Drug involved: "
    }
    
    if ( input$Ashow==T ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(0.3, 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }
    if (dim.data.frame(input$cod4C)[2]==1) {
      Title <- paste0(Title,", Intent: ",input$cod4C)
    }
    if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
      if (input$dimension<992) {
        gp <- gp + facet_grid(rows=vars(intent))
      }
      else {
        gp <- gp + facet_grid(cols=vars(intent))
        if (input$dimension<1200) {
          LX(0)
          LO("h")
          LY(-.2)
        }
      }
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)
    # if (input$yax=="num") {
    #   gp <- gp + aes(y=n, text=paste0(
    #       "Year: ",year,
    #       "<br>Deaths: ",n,
    #       "<br>Drug: ",drug,
    #       "<br>Intent: ",intent,
    #       "<br>Sex: ",sex,
    #       "<br>Age group: ",age_group ) ) + 
    #     scale_y_continuous(limits=c(0, max(sub$n, 250) ) ) +
    #     labs(y="Number of deaths")
    # }

    gp <- gp + labs(title=Title) +
      theme(strip.background=element_rect(fill="#dbefd4"),
        strip.text=element_text(color="#000000", face="bold") )

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids and other drugs by sex (Table 8) ------------------------------------------
  output$W8Plot <- renderPlotly({
    sub <- subset(ABS_COD2019_OpW, drug %in% input$Wdrug & age_group==input$ageR & 
          (year>=input$yr97[[1]] & year<=input$yr97[[2]]) )
    if ( input$Ashow==F ) {
      sub <- subset(sub, set=="OpioidW")
    }

    if (input$dropSI=="Sex") {
      Legend <- "Drug by intent"
      if (input$sex4R != "MF") {
        sub <- subset(sub, intent %in% input$cod4C & sex==input$sex4R )
        Title <- paste0("Age group: ",input$ageR,",  Sex: ",input$sex4R)
        gp <- ggplot(sub) + scale_colour_manual(values=opWcodcols)
        
        if (dim.data.frame(input$cod4C)[2]==1 | (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) ) {
          gp <- gp + aes(colour=drug)
          Legend <- "Drug"
          if (dim.data.frame(input$cod4C)[2]==1) {
            Title <- paste0(Title,", Intent: ",input$cod4C)
          }
          if (dim.data.frame(input$cod4C)[2]==2 & input$cod4C2==2) {
            if (input$dimension<992) {
              gp <- gp + facet_grid(rows=vars(intent))
            }
            else {
              gp <- gp + facet_grid(cols=vars(intent))
              if (input$dimension<1200) {
                LX(0)
                LO("h")
                LY(-.2)
              }
            }
          }
        }
        else {
          gp <- gp + aes(colour=op_intent, linetype=op_intent) +
            scale_linetype_manual(values=opWcodtype)
        }
      }
      else {
        sub <- subset(sub, intent %in% input$cod4C & sex != "All" )
        gp <- ggplot(sub) + aes(colour=op_intent,
            linetype=op_intent) + 
          scale_linetype_manual(values=opWcodtype)
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
        Title=paste0("Age group: ",input$ageR)
      }
      varl <- "drug"
      labl <- "<br>Drug: "
      varc <- "intent"
      labc <- "<br>Intent: "
    }
    if (input$dropSI=="Intent") {
      sub <- subset(sub, intent==input$cod4R & sex %in% input$sexC )
      gp <- ggplot(sub) + aes(colour=op_sex, linetype=op_sex) + 
        scale_colour_manual(values=opWcodcols) +
        scale_linetype_manual(values=opWcodtype)
      Legend <- "Drug by sex"
      Title <- paste0("Age group: ",input$ageR,",  Intent: ",input$cod4R)
      varl <- "drug"
      labl <- "<br>Drug: "
      varc <- "sex"
      labc <- "<br>Sex: "
    }

    if ( input$Ashow==T ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(0.3, 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    yax <- input$yax
    yr <- input$yr97
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)
    # if (input$yax=="num") {
    #   gp <- gp + aes(y=n, text=paste0(
    #       "Year: ",year,
    #       "<br>Deaths: ",n,
    #       "<br>Drug: ",drug,
    #       "<br>Intent: ",intent,
    #       "<br>Age group: ",age_group,
    #       "<br>Sex: ",sex)) +
    #     scale_y_continuous(limits=c(0, max(sub$n, 250))) + labs(y="Number of deaths")
    # }

    gp <- gp + labs(title=Title) +
      theme(strip.background=element_rect(fill="#dbefd4"),
        strip.text=element_text(color="#000000", face="bold") )
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive Opioids by age and intent (Table 10) --------------------------------------------------------------
  output$E0Plot <- renderPlotly({
      if (input$dropOA=="Drug") {
      sub <- filter(ABS_COD2018_OpE, sex=="All" & location=="Aus" &
          drug==input$E0Odrug & intent %in% input$cod3C & age_group %in% input$ageAll &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]])) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all=T)

      gp <- ggplot(sub) + aes(colour=age_intent, linetype=age_intent) +
        labs(title=input$E0Odrug) +
        scale_colour_manual(values=agecodcols) +
        scale_linetype_manual(values=agecodtype)
      Legend <- "Age by intent"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "age_group"
      labc <- "<br>Age: "
    }
    
    else if (input$dropOA=="Age") {
      sub <- subset(ABS_COD2018_OpE, subset=(sex=="All" & location=="Aus" &
          drug %in% input$Edrug & intent %in% input$cod3C & age_group==input$ageOAA &
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]))) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all=T)
      
      gp <- ggplot(sub) + aes(colour=op_intent, linetype=op_intent) +
        labs(title=paste0("Age: ",input$ageOAA)) +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype)
      Legend <- "Drug by intent"
      varl <- "intent"
      labl <- "<br>Intent: "
      varc <- "drug"
      labc <- "<br>Opioid type: "
    }

    validate(need(nrow(sub) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    yax <- input$yax
    yr <- input$yr07
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)
    # if (input$yax=="num") {
    #   gp <- gp + aes(y=n,
    #     text=paste0(
    #       "Year: ",year,
    #       "<br>Deaths: ",n,
    #       "<br>Opioid: ",drug,
    #       "<br>Intent: ",intent,
    #       "<br>Age group: ",age_group)) +
    #     scale_y_continuous(limits=c(0, max(sub$n, 250))) +
    #     labs(y="Number of deaths")
    # }
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })

  # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11) -----------------------
  output$E9Plot <- renderPlotly({
    if (input$dropSI=="Intent") {
      sub <- filter(ABS_COD2018_OpE, jurisdiction==input$jur & age_group==input$ageR &
          sex %in% input$sexC & intent==input$E9Icod & drug %in% input$Edrug & 
          (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all=T)

      gp <- ggplot(sub) + aes(colour=op_sex, linetype=op_sex) +
        labs(title=paste0(input$jur,", Age: ",input$ageR,", Intent: ",input$E9Icod) ) +
        scale_colour_manual(values=opEcodcols) +
        scale_linetype_manual(values=opEcodtype)
      Legend <- "Drug by sex"
      varl <- "drug"
      labl <- "<br>Opioid type: "
      varc <- "sex"
      labc <- "<br>Sex: "
    }
    if (input$dropSI=="Sex") {
      if (input$sex4R != "MF") {
        sub <- filter(ABS_COD2018_OpE, jurisdiction==input$jur & age_group==input$ageR &
            sex==input$sex4R & intent %in% input$cod3C & drug %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all=T)
        gp <- ggplot(sub) + labs(title=paste0(
          input$jur,", Age: ",input$ageR,", Sex: ",input$sex4R) )
      }
      else {
        sub <- filter(ABS_COD2018_OpE, jurisdiction==input$jur & age_group==input$ageR &
            sex != "All" & intent %in% input$cod3C & drug %in% input$Edrug & 
            (year>=input$yr07[[1]] & year<=input$yr07[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all=T)
        gp <- ggplot(sub) +
          labs(title=paste0(input$jur,", Age: ",input$ageR) )
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
    yax <- input$yax
    yr <- input$yr07
    gp <- PlotFn(Pd=sub,Gp=gp,Yax=yax,Yr=yr,Varl=,varl,Varc=varc,Labl=labl,Labc=labc)
    # if (input$yax=="num") {
    #   gp <- gp + aes(y=n, text=paste0(
    #       "Year: ",year,
    #       "<br>Deaths: ",n,
    #       "<br>Drug: ",drug,
    #       "<br>Intent: ",intent,
    #       "<br>Sex: ",sex
    #     )) + scale_y_continuous(limits=c(0, max(sub$n, 250))) +
    #     labs(y="Number of deaths")
    # }

    gp <- gp + theme(strip.background=element_rect(fill="#dbefd4"),
            strip.text=element_text(color="#000000", face="bold") )
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$EPPlot <- renderPlotly({
    #needs to be sorted [order(...)]
    #weird proportions plot from 2015 onwards because of duplicates by AUS
    #- need to make distinct
    sub <- filter(ABS_COD2018_OpE, drug %in%
          c("Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids",
            "Illicit & pharmaceutical opioids",
            "Other & unspecified opioids") &
        intent==input$codR & age_group==input$ageR & 
        (year>=input$yr07[[1]] & year<=input$yr07[[2]]) & 
        sex==input$sexR & jurisdiction==input$jur) %>%
      group_by(year, intent, sex, jurisdiction, age_group) %>% 
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
