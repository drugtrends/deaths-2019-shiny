#For CODURF 2019 data received in Dec 2020
#N. Man
library(shiny)
library(shinydashboard)
library(plotly)
library(shinycustomloader)
# library(shinyjs)

ui <- function(request) {
  bootstrapPage('',
    tags$head(
      includeScript("google_analytics.js"),
# https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
      tags$script(src="dimension.js"),
#https://stackoverflow.com/questions/30096187/favicon-in-shiny
#https://www.w3.org/2005/10/howto-favicon
      tags$link(rel="icon", type="image/png", href="favicon.png")
    ),
    theme="DT-theme.css",
# Navigation bar ---------------------------------------------------------------
mainPanel(width=9,
  navbarPage(
    title= "Deaths induced by:",
    id="Plot",
  # All drugs menu tab ---------------------------------------------------------------
    navbarMenu("All drugs",
      
    # All drugs by jurisdiction, intent, age and sex (Tables 1a, 1b & 1c) -------------------
      tabPanel(value="AllPage",
        "Drug-induced deaths by jurisdiction, age and sex",
        h2("Drug-induced deaths by jurisdiction, intent, age and sex"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("AllPlot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteAll.html"))
          ),
          tabPanel("Notes", includeHTML("notesAll.html"))
        )
      ),

    # Remoteness by jurisdiction and intent (Tables R1, R4 & R5) ----------------------------
      tabPanel(value="RAPage",
        "Drug-induced deaths by jurisdiction and remoteness area",
        h2("Drug-induced deaths by jurisdiction, remoteness area and intent"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("RAPlot", width="100%", height="600px"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteRem.html")
          ),
          tabPanel("Notes", includeHTML("notesRem.html"))
        )
      ),

    # Remoteness percents (Tables R) -----------------------------------------
      tabPanel(value="RPPage",
        "Percentages of drug-induced deaths by remoteness area",
        h2("Percentages of drug-induced deaths by remoteness area"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("RPPlot", width="100%", height="600px"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteRem.html")
          ),
          tabPanel("Notes", includeHTML("notesRem.html"))
        )
      ),

    # All drug-induced deaths by drug, jurisdiction, intent and/or sex (Table 12, 12b & 12c)-------------------------
      tabPanel(value="DTJPage",
        "Drug-induced deaths by drug, jurisdiction and/or sex",
        h2("Drug-induced deaths by drug, jurisdiction, intent and/or sex"),
      
        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("DTJPlot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteDTJ.html")
           ),
           tabPanel("Notes", includeHTML("notesDT.html"))
         )
      ),


    # All drug-induced deaths by drug, age and intent (Table 12 & 12a)-----------------------
      tabPanel(value="DTAPage",
        "Drug-induced deaths by drug and age",
        h2("Drug-induced deaths by drug, age and intent"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("DTAPlot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteDTA.html")
           ),
          tabPanel("Notes", includeHTML("notesDT.html"))
        )
      )
    ),


  # Opioids menu tab -------------------------------------------------------------
    navbarMenu("Opioids",

    # Opioids by opioid, age and intent (Table 4) -----------------------------------------
      tabPanel(value="O4Page",
        "By opioid and age",
        h2("Opioid-induced deaths by opioid, age and intent"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("O4Plot", width="100%", height="600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp4.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html"))
        )
      ),

    # Opioids by intent, opioid and sex (Table 5) -----------------------------------------
      tabPanel(value="O5Page",
        "By opioid and sex",
        h2("Opioid-induced deaths by opioid, intent and sex"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("O5Plot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp5.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html"))
        )
      ),

    # Opioids by intent, jurisdiction and sex (Table 6) ---------------------------------
      tabPanel(value="O6Page",
        "By sex and jurisdiction",
        h2("Opioid-induced deaths by intent, jurisdiction and sex"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("O6Plot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp6.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html"))
        )
      ),
  
    # Exclusive opioids by age and intent (Table 10)-----------------------------------------
      tabPanel(value="E0Page",
        "By exclusive opioid type and age",
        h2("Opioid-induced deaths by exclusive opioid type, age and intent"),
        
        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("E0Plot", width="100%", height="600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteEOp.html")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html"))
        )
      ),
      
    # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11)-----------------------
      tabPanel(value="E9Page",
        "By exclusive opioid type, jurisdiction and sex",
        h2("Opioid-induced deaths by exclusive opioid type, jurisdiction, intent and sex"),
        
        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("E9Plot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteEOp.html")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html"))
        )
      ),
      
    # Exclusive opioids percents (Tables 10 & 11) -----------------------------------------
      tabPanel(value="EPPage",
        "Exclusive opioid types as percentages",
        h2("Opioid-induced deaths by exclusive opioid type as percentages of all opioid-induced deaths"),
        
        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("EPPlot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html"))
        )
      ),
  
    # Opioids with other drugs by age and intent (Table 7)-----------------------------------
    tabPanel(value="W7Page",
      "Opioids with other drugs, by age",
      h2("Opioid-induced deaths by other drugs with opioids, age and intent"),
  
      tabsetPanel(type="tabs",id="Tab",
        tabPanel("Plot",
            withLoader(plotlyOutput("W7Plot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
            includeHTML("fnoteWOp.html")
        ),
        tabPanel("Notes", includeHTML("notesWOp.html"))
      )
    ),
  
    # Opioids with other drugs by sex and intent (Table 8) ----------------------------------
    tabPanel(value="W8Page",
      "Opioids with other drugs, by sex",
      h2("Opioid-induced deaths by other drugs with opioids, sex and intent"),
  
        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
            # mainPanel(width=9,
              withLoader(plotlyOutput("W8Plot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteWOp.html")
          ),
          tabPanel("Notes", includeHTML("notesWOp.html"))
        )
      )
    ),


  # Stimulants menu tab ---------------------------------------------------------------
    navbarMenu("Stimulants",
  
    # Amphetamines tab (Table 2) --------------------------------------------------------
      tabPanel(value="AmPage",
        "Amphetamine-induced deaths",
        h2("Amphetamine-induced deaths"),

        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
              withLoader(plotlyOutput("AmPlot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteAms.html")
          ),
          tabPanel("Notes", includeHTML("notesAms.html"))
        )
      ),

    # Cocaine tab (Table 3) -------------------------------------------------------------
      tabPanel(value="CPage",
        "Cocaine-induced deaths",
        h2("Cocaine-induced deaths"),
  
        tabsetPanel(type="tabs",id="Tab",
          tabPanel("Plot",
            withLoader(plotlyOutput("CPlot", width="100%", height="600px"),
              type="image", loader="DT_NIDIP_tween.gif"),
            includeHTML("fnoteCoke.html")
          ),
          tabPanel("Notes", includeHTML("notesCoke.html"))
        )
      )
    ),

  # Explanatory Notes tab ---------------------------------------------------------------
    tabPanel(value="Method",
      "Explanatory notes", includeHTML("notesExplanatory.html")
    ),
  # Citation tab ------------------------------------------------------------
    tabPanel(value="Cites",
      "Citation & acknowledgements",includeHTML("Citation.html")
    )
  )
  ),
  conditionalPanel(condition="input.dimension>1199",
# https://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding - not for narrow screens
    HTML("<div class='col-sm-0' style='padding-top: 50px;
      background-image:
        linear-gradient(to bottom,
          rgba(106,125,20, 1) 0%,
          rgba(106,125,20, 1) 100%),
        linear-gradient(to bottom,
          rgba(106,125,20, 1) 0%,
          rgba(106,125,20, 1) 100%);
      background-clip: content-box, padding-box'>"),
    HTML("</div>"),
  ),
  conditionalPanel(condition="input.dimension>991 & input.dimension<1200",
    HTML("<div class='col-sm-0' style='padding-top: 100px;
      background-image:
        linear-gradient(to bottom,
          rgba(106,125,20, 1) 0%,
          rgba(106,125,20, 1) 100%),
        linear-gradient(to bottom,
          rgba(106,125,20, 1) 0%,
          rgba(106,125,20, 1) 100%);
      background-clip: content-box, padding-box'>"),
    HTML("</div>"),
  ),
  conditionalPanel(condition="input.dimension>767 & input.dimension<992",
    HTML("<div class='col-sm-0' style='padding-top: 150px;
      background-image:
        linear-gradient(to bottom,
          rgba(106,125,20, 1) 0%,
          rgba(106,125,20, 1) 100%),
        linear-gradient(to bottom,
          rgba(106,125,20, 1) 0%,
          rgba(106,125,20, 1) 100%);
      background-clip: content-box, padding-box'>"),
    HTML("</div>"),
  ),
  conditionalPanel(condition="input.Plot!='Method' & input.Plot!='Cites' & input.Tab=='Plot'",
    sidebarPanel(id="Sidebar",width=3,
    # year slider###############
      conditionalPanel(
        condition="input.Plot!='RAPage' & input.Plot!='RPPage' &
        input.Plot!='E9Page' & input.Plot!='E0Page' & input.Plot!='EPPage'",
        sliderInput("yr97", "Period",
          min=1997, max=2019,
          value=c(1997, 2019), sep=""
        )
      ),
      conditionalPanel(
        condition="input.Plot=='E9Page' | input.Plot=='E0Page' | input.Plot=='EPPage'",
        sliderInput("yr07", "Period",
          min=2007, max=2019,
          value=c(2007, 2019), sep=""
        )
      ),
      conditionalPanel(
        condition="input.Plot=='RAPage' | input.Plot=='RPPage'",
        sliderInput("yr09", "Period",
          min=2009, max=2019,
          value=c(2009, 2019), sep=""
        )
      ),
  ####For user-defined year intervals
    # radioButtons("xax", "Interval between years:",
    #   choices=c(1, 2, 5), inline=T, selected=2
    # ),
###HTML info on width of plot for WIP: <rect class="nsewdrag drag" width="854" height="474" ;"></rect>

    # y-variable list###############
      conditionalPanel(
        condition="input.Plot!='RPPage' & input.Plot!='EPPage'",
        selectInput("yax", "Plot:",
           c(
             "Number of deaths"="num",
             "Crude mortality rate"="cr",
             "Crude mortality rate (95% CI)"="crci",
             "Age standardised mortality rate"="sr",
             "Age standardised mortality rate (95% CI)"="srci"
           ),
           selected="cr"
        )
      ),
      conditionalPanel(condition="input.yax=='sr' | input.yax=='srci'",
        HTML("<i>NB: Age-standardised rates are only calculated for all ages.</i><p></p>")
      ),

    # geographical lists###############
      conditionalPanel(
        condition="input.Plot=='AllPage' | input.Plot=='O6Page'
        | input.Plot=='E9Page' | input.Plot=='EPPage'",
        selectInput("jur", "Jurisdiction:",
        c("Australia",
          "New South Wales",
          "Victoria",
          "Queensland",
          "South Australia",
          "Western Australia",
          "Tasmania",
          "Northern Territory",
          "Australian Capital Territory" )
        )
      ),
      conditionalPanel(
        condition="input.Plot=='RAPage' | input.Plot=='RPPage'",
        selectInput("jurR", "Jurisdiction:",
          c(
            "Australia",
            "New South Wales",
            "Victoria",
            "Queensland",
            "South Australia",
            "Western Australia"
          ),
          selected="Australia"
        )
      ),

      conditionalPanel(
        condition="input.Plot=='RAPage' & input.jurR=='Australia'",
        checkboxGroupInput("RAra", "Remoteness region:",
           choices=c(
             "Major Cities",
             "Regional and Remote",
             "-Inner Regional"="Inner Regional",
             "-Outer Regional"="Outer Regional",
             "-Remote and Very Remote"="Remote and Very Remote"
           ),
           selected=c(
             "Major Cities",
             "Regional and Remote"
           )
        )
      ),
      conditionalPanel(
        condition="input.Plot=='RAPage' & input.jurR!='Australia'",
        checkboxGroupInput("Rra", "Remoteness region:",
            choices=c(
              "Major Cities",
              "Regional and Remote"
            ),
            selected=c(
              "Major Cities",
              "Regional and Remote"
            )
        )
      ),

    # radio button panelS###############
      conditionalPanel(condition="(input.Plot=='RPPage' | 
        input.Plot=='E9Page' | input.Plot=='EPPage' |
        input.Plot=='O5Page' | input.Plot=='O6Page' | input.Plot=='W8Page')
        & input.yax!='sr' & input.yax!='srci'",
          radioButtons("ageR", "Age range:",inline=TRUE,
          choices=c(
            "All ages", "15 to 64"="15-64"
          ),
          selected=c("All ages")
        ),
        conditionalPanel(
          condition="(input.Plot=='RPPage' & input.jurR=='Australia') | input.Plot=='EPPage'",
          radioButtons("sexR", "Sex:",
            choices=c(
              "Male",
              "Female",
              "People"
            ),
            selected=c("People")
          )
        ),
        conditionalPanel(
          condition="input.Plot=='RPPage' | input.Plot=='EPPage'",
          radioButtons("codR", "Intent:",
            c("All", "Accidental"),
            selected="All"
          )
        )
      ),

    # Drug type with jurisdiction panels (DTJPage)#######
      conditionalPanel(condition="input.Plot=='DTJPage'",
        selectInput("DTjur", "Jurisdiction:",
           c("Australia",
              "New South Wales - All ages"="New South Wales",
              "Victoria - All ages"="Victoria",
              "Queensland - All ages"="Queensland",
              "South Australia - All ages"="South Australia",
              "Western Australia - All ages"="Western Australia",
              "Tasmania - All ages"="Tasmania",
              "Northern Territory - All ages"="Northern Territory",
              "Australian Capital Territory - All ages"="Australian Capital Territory"  )
        ),

        conditionalPanel(condition="input.DTjur=='Australia'",
          selectInput("DTage", "Age range:",
           choices=c(
             "All ages","15 to 64"="15-64"
           ),
           selected=c("All ages")
          )
        ),
        radioButtons("DTJdrop", "Variable for dropdown list:",
          choices=c(
              "Sex (only Aus) & Intent"="IntSx",
              "Drug"="Drug"
          ), #inline=T,
          selected=c("IntSx")
        ),

        conditionalPanel(condition="input.DTJdrop=='IntSx'",
          conditionalPanel(condition="input.DTjur=='Australia'",
            selectInput("DTIsex", "Sex:",
              choices=c("People",
                "Female - select All ages as Age range"="Female",
                "Male - select All ages as Age range"="Male",
                "Male & Female - select All ages as Age range"="MF"),
              selected=c("People")
            ),
            conditionalPanel(condition="input.DTIsex=='People'",
              selectInput("DTIcod", "Intent:",
                choices=c("All", "Accidental", "Intentional", "Undetermined"),
                selected=c("All")
              )
            )
          ),
          conditionalPanel(condition="input.DTjur!='Australia' |
            (input.DTjur=='Australia' & input.DTIsex!='People')",
            selectInput("DTIcod2", "Intent:",
              choices=c("All", "Accidental"),
              selected=c("All")
            )
          )
        )
        #drug type selection below
        #intent & age checkboxes later
      ),

    # Drug type with age panels (DTAPage)##################
      conditionalPanel(condition="input.Plot=='DTAPage'",
        radioButtons("DTAdrop", "Variable for dropdown list:",
            choices=c(
              "Age & Intent"="Age_Intent",
              "Drug"="Drug"
            ),inline=T,
            selected=c("Age_Intent")
        ),
        conditionalPanel(condition="input.DTAdrop=='Age_Intent'",
          selectInput("DTAIcod", "Intent:",
            choices=c("All", "Accidental"),
            selected=c("All")
          ),

          selectInput("DTAIage", "Age:",
            choices=c(
              "15 to 24"="15-24",
              "25 to 34"="25-34",
              "35 to 44"="35-44",
              "45 to 54"="45-54",
              "55 to 64"="55-64",
              "65 to 74"="65-74",
              "75 to 84"="75-84",
              "85+",
              "All ages",
              "15 to 64"="15-64"
            ),
            selected=c("All ages")
          )
        )
        #drug type selection below
        #intent & age checkboxes later
      ),
    # Drug type selection (DTJ & DTA Pages)#######
      conditionalPanel(condition="(input.Plot=='DTAPage' & input.DTAdrop=='Drug')
        | (input.Plot=='DTJPage' & input.DTJdrop=='Drug')",
        selectInput("DTdrugD", label=NULL,
          choices=c(
            "OPIOIDS",
            "heroin",
            "natural & semi-synthetic opioids",
            "methadone",
            "synthetic opioids",
            "AMPHETAMINES",
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "barbiturates",
            "benzodiazepines",
            "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
            ="antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "ANTIDEPRESSANTS",
            "tricyclic & tetracyclic antidepressants",
            "other & unspecified antidepressants",
            "ANTIPSYCHOTICS & NEUROLEPTICS",
            "other & unspecified antipsychotics (e.g. quetiapine)"
            ="other & unspecified antipsychotics",
            "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
            "other nonsteroidal anti-inflammatory drugs",
            "ALCOHOL",
            "COCAINE",
            "CANNABINOIDS"
          ),
          selected=c("OPIOIDS")
        )
      ),
    # Drug type checkbox (DTJ & DTA Pages)#######
      conditionalPanel(condition="(input.Plot=='DTAPage' & input.DTAdrop!='Drug')
        | (input.Plot=='DTJPage' & input.DTJdrop!='Drug')",
        checkboxInput("DTdrugA", label=HTML("<b>Drug:</b>"),value=TRUE
        ),
        HTML("<div style='margin-left: 6%;'>"),
        checkboxGroupInput("DTdrug", NULL, #"Drug:",
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
          selected=c("OPIOIDS", "ALCOHOL", "AMPHETAMINES", "ANTIDEPRESSANTS", 
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "ANTIPSYCHOTICS & NEUROLEPTICS", "CANNABINOIDS",
            "COCAINE", "NONOPIOID ANALGESICS"
          )
        ),
        HTML("</div>")
      ),

    # Opioid, sex & intent control panel (O5Page)###############
      conditionalPanel(condition =
        "input.Plot=='O5Page'",
        radioButtons("O5drop", "Variable for dropdown list:",
          choices=c(
            "Opioid",
            "Intent",
            "Sex"
          ),inline=T,
          selected=c("Opioid")
        )
      ),

    # Opioid and age panels (O4, E0 & W7 Pages)###############
      conditionalPanel(condition =
        "input.Plot=='O4Page' | input.Plot=='E0Page' | input.Plot=='W7Page'",
        radioButtons("dropOA", "Variable for dropdown list:",
          choices=c(
            "Drug", "Age"
          ),inline=T,
          selected=c("Drug")
        ),

        conditionalPanel(condition="input.dropOA=='Age' & input.yax!='sr' & input.yax!='srci'",
          selectInput("ageOAA", label=NULL,
            choices=c(
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
            selected=c("All ages")
          )
        )
        # O4 drug panel under Opioid checkbox (O4 & O5 Pages)
        # E0 drug panel under Exclusive opioid checkbox (E0 & E9 Pages)
        # W7 drug panel under Other drugs with opioid checkboxes (W7 & S8 Pages)
      ),
      conditionalPanel(condition="input.dropOA=='Drug'",
        conditionalPanel(condition="input.Plot=='W7Page'",
          selectInput("W7Ddrug", label="Opioid-induced deaths with:",
            choices=c(
              "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
              "Alcohol",#="All opioids with alcohol",
              "Amphetamines",#="All opioids with amphetamines",
              "Antidepressants",
              "Antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
             ="Antiepileptic & sedative-hypnotic drugs,\nunspecified",
              "Antipsychotics & neuroleptics",
              "Benzodiazepines"
            ),
            selected=c("Alcohol")
          )
        ),
        conditionalPanel(condition="input.Plot=='E0Page'",
          selectInput("E0Odrug", label=NULL,
            choices=c(
              "Exclusive illicit opioids",
              "Exclusive pharmaceutical opioids",
              "Illicit & pharmaceutical opioids",
              "Other & unspecified opioids"
            ),
            selected=c(
              "Exclusive illicit opioids" )
          )
        )
        # Age checkbox at the end
      ),
      conditionalPanel(condition =
        "input.Plot=='O4Page' & input.dropOA=='Drug' |
        input.Plot=='O5Page' & input.O5drop=='Opioid'",
        selectInput("OdrugO", label=NULL,
          choices=c(
            "All opioids",
            "Heroin",
            "Opium",
            "Methadone",
            "Natural & semi-synthetic opioids",
            "Synthetic opioids",
            "Other & unspecified opioids"
          ),
          selected=c("All opioids")
        )
      ),

    # intent & sex panels (All, W8 & E9 + O5 Pages)###############
      conditionalPanel(
        condition="input.Plot=='AllPage' | input.Plot=='W8Page' | input.Plot=='E9Page'",
        radioButtons("dropSI", "Variable for dropdown list:",
           choices=c(
             "Intent", "Sex"
           ), inline=T,
           selected=c("Intent")
        )
      ),
      conditionalPanel(condition="(input.dropSI=='Sex' & 
        (input.Plot=='AllPage' | input.Plot=='W8Page' | input.Plot=='E9Page')) |
        (input.O5drop=='Sex' & input.Plot=='O5Page')",
        selectInput("sex4R", label=NULL,
          choices=c("People", "Female", "Male","Male & Female"="MF")
        )
      ),
      conditionalPanel(condition="input.Plot=='AllPage'",
        conditionalPanel(condition= "input.dropSI!='Intent'",
          checkboxGroupInput(
            "AllScod", label="Intent:",
            c("All", "Accidental", "Intentional", "Undetermined","Other"),
            selected=c("All", "Accidental")
          )
        ),
        # Intent checkbox for W8 & E9 later below with other pages
        conditionalPanel(condition="input.dropSI=='Intent'",
          selectInput("AllIcod", label=NULL,
            choices=c("All", "Accidental", "Intentional", "Undetermined","Other")
          )
        )
      ),
      conditionalPanel(condition=
        "(input.dropSI=='Intent' & input.Plot=='W8Page') |
        (input.O5drop=='Intent' & input.Plot=='O5Page')",
        selectInput("cod4R", label=NULL,
          choices=c("All", "Accidental", "Intentional", "Undetermined")
        )
      ),
      conditionalPanel(condition="input.Plot=='E9Page' & input.dropSI=='Intent'",
        selectInput("E9Icod", label=NULL,
          choices=c("All", "Accidental", "Intentional")
        )
      ),
      # Sex checkboxes for later below with other pages

    # Sex checkbox (All, O5, O6, W8 & E9 Pages)###############
      conditionalPanel(condition="input.Plot=='O6Page' |
        ((input.Plot=='AllPage' | input.Plot=='W8Page' | input.Plot=='E9Page')
        & input.dropSI!='Sex') | (input.Plot=='O5Page' & input.O5drop!='Sex')
        | (input.Plot=='DTJPage' & input.DTJdrop=='Drug' & input.DTjur=='Australia')",
        conditionalPanel(condition="input.Plot=='DTJPage' & input.DTJdrop=='Drug' & input.DTjur=='Australia' & input.DTage=='15-64' & input.sexC!='People'",
          HTML("<i>Please select All ages for data by male and/or female.</i>")
        ),
        checkboxGroupInput("sexC", label="Sex:",
           choices=c("People", "Female", "Male"),
           selected=c("People", "Female", "Male")
        ),
      ),

    # Age range checkbox (CPage)###############
      conditionalPanel(
        condition="input.Plot=='CPage' & input.yax!='sr' & input.yax!='srci'",
        checkboxGroupInput("Cage", "Age range:",
          choices=c(
            " All ages"," 15 to 64"=" 15-64"
          ),
          selected=c(" All ages")
        )
      ),
    # Intent(2C) checkbox (RA, O6, C & Am Pages)###############
      conditionalPanel(
        condition="input.Plot=='RAPage' | input.Plot=='O6Page' |
        input.Plot=='CPage' | input.Plot=='AmPage' |
        (input.Plot=='DTAPage' & input.DTAdrop=='Drug') |
        (input.Plot=='DTJPage' & input.DTJdrop=='Drug' & input.DTjur!='Australia')",
        checkboxGroupInput("cod2C", "Intent:",
          c("All", "Accidental"),
          selected="All"
        )
      ),

    # Intent(3C) checkbox (E0 & E9 Pages)###############
      conditionalPanel(
        condition="input.Plot=='E0Page' | (input.Plot=='E9Page' & input.dropSI=='Sex')",
        checkboxGroupInput("cod3C", "Intent:",
          c("All", "Accidental", "Intentional"),
          selected="All"
        )
      ),

    # Intent checkbox (4C) (DTJ, O4, O5, W7 & W8 Pages)###############
      conditionalPanel(condition="input.Plot=='O4Page' | input.Plot=='W7Page' |
        (input.Plot=='W8Page' & input.dropSI!='Intent') | 
        (input.Plot=='O5Page' & input.O5drop!='Intent') |
        (input.Plot=='DTJPage' & input.DTJdrop=='Drug' & input.DTjur=='Australia')",
        checkboxGroupInput(
          "cod4C", label="Intent:",
          c("All", "Accidental", "Intentional", "Undetermined"),
          selected=c("All", "Accidental")
        )
      ),

    # Intent split plot radio button (DTJ, O4, O5, W7, W8, Am, C & DTA Pages)###############
      conditionalPanel(condition="(input.AllScod.length==2 & input.Plot=='AllPage' & input.dropSI=='Sex' & input.sex4R!='MF') |
        (input.cod4C.length==2 & ( input.Plot=='O4Page' | input.Plot=='W7Page' |
        (input.Plot=='DTJPage' & input.DTJdrop=='Drug' & input.DTjur=='Australia') |
        (input.Plot=='O5Page' & input.O5drop!='Intent' & (input.O5drop!='Sex' | input.sex4R!='MF') ) |
        (input.Plot=='W8Page' & input.dropSI!='Intent' & input.sex4R!='MF') )) |
        (input.cod3C.length==2 & ( input.Plot=='E0Page' |
        (input.Plot=='E9Page' & input.dropSI!='Intent' & input.sex4R!='MF') )) |
        (input.cod2C.length==2 & ( input.Plot=='AmPage' | input.Plot=='CPage' |
        (input.Plot=='DTAPage' & input.DTAdrop=='Drug') ))",
        radioButtons("codS",label="Show intent as:",
          choices=c("Single plot"=1,"Split plot"=2),
          inline=T, selected=1
        )
      ),

    # Other drugs with opioid checkboxes (W7 & W8 Pages)###############
      conditionalPanel(condition="input.Plot=='W8Page' |
        (input.Plot=='W7Page' & input.dropOA=='Age')",
        checkboxGroupInput("Wdrug", "All opioids with:",
          choices=c(
            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
            "Alcohol",#="All opioids with alcohol",
            "Amphetamines",#="All opioids with amphetamines",
            "Antidepressants",
            "Antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
           ="Antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "Antipsychotics & neuroleptics",
            "Benzodiazepines"
          ),
          selected=c("Alcohol")
        )
      ),
    # Also show all drug-induced & crude rate checkbox (Am, C, W7 & W8 Pages | sr y-axis)###############
      conditionalPanel(condition="((input.yax=='num' | input.yax=='cr') &
        (input.Plot=='AmPage' | input.Plot=='CPage' | input.Plot=='W7Page'
        | input.Plot=='W8Page')) | input.yax=='sr'", # | input.yax=='srci'
        HTML("<b>Also show:</b>"),
        conditionalPanel(condition="( (input.yax=='num' | input.yax=='cr') &
        (input.Plot=='AmPage' | input.Plot=='CPage' | input.Plot=='W7Page' | input.Plot=='W8Page') )
        | ( input.yax=='sr' & ( input.Plot=='AmPage' | input.Plot=='CPage' |
        (input.Plot=='W7Page' & (input.dropOA=='Drug' | input.cod4C.length==1 | (input.cod4C.length==2 & input.codS==2))) |
        (input.Plot=='W8Page' & (input.Wdrug.length==1 |
          (input.dropSI=='Intent' & input.sexC.length==1) |
          ( input.dropSI=='Sex' & (input.cod4C.length==1 | (input.cod4C.length==2 & input.codS==2 & input.sex4R!='MF')) )
        )) ) )",
          checkboxInput("Ashow", "All drug-induced deaths",value=F)
        ),
        conditionalPanel(condition="input.yax=='sr'", # | input.yax=='srci'
          checkboxInput("crude","Crude rate",value=F)
        )
      ),

    # Opioid checkbox (O4 & O5 Pages)###############
      conditionalPanel(condition="(input.Plot=='O4Page' & input.dropOA!='Drug')
        | (input.Plot=='O5Page' & input.O5drop!='Opioid')",
        checkboxGroupInput("OdrugC", "Opioid:",
          choices=c(
              "All opioids",
              "Heroin",
              "Opium",
              "Methadone",
              "Natural & semi-synthetic opioids",
              "Synthetic opioids",
              "Other & unspecified opioids"
          ),
          selected=c("All opioids")
        )
      ),
    # Exclusive opioid checkbox (E9 & E0 Pages)###############
      conditionalPanel(condition="input.Plot=='E9Page' |
        (input.Plot=='E0Page' & input.dropOA=='Age')",
        checkboxGroupInput("Edrug", "Drug:",
          choices=c(
            "Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids",
            "Illicit & pharmaceutical opioids",
            "Other & unspecified opioids"
          ),
          selected=c(
            "Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids"
          )
        )
      ),

    # Age checkbox (All, Am, O4, E0, W7 & DTA Pages)###############
      conditionalPanel(condition="(input.Plot=='AllPage' | input.Plot=='AmPage'
        | ((input.Plot=='O4Page' | input.Plot=='E0Page' | input.Plot=='W7Page')
        & input.dropOA=='Drug') | (input.Plot=='DTAPage' & input.DTAdrop=='Drug'))
        & input.yax!='sr' & input.yax!='srci'",
        checkboxInput("ageAllA", label=HTML("<b>Age:</b>"),value=F
        ),
        HTML("<div style='margin-left: 6%;'>"),
        checkboxGroupInput("ageAll", NULL, #"Age:",
          c("15 to 24"="15-24",
             "25 to 34"="25-34",
             "35 to 44"="35-44",
             "45 to 54"="45-54",
             "55 to 64"="55-64",
             "65 to 74"="65-74",
             "75 to 84"="75-84",
             "85+"="85+",
             "All ages",
             "15 to 64"="15-64"),
          selected=c("All ages")
        ),
        HTML("</div>")
      )
    )
  ),
  conditionalPanel(condition="input.dimension>767 & 
    (input.Plot=='Method' | input.Plot=='Cites' | input.Tab=='Notes')",
    fluidRow(column(includeHTML("DT-logos.html"),width=3,offset=9))
  ),
#####indeterminate checkbox update not working yet - addEventListener not working???
#https://www.w3schools.com/js/js_htmldom_eventlistener.asp
  tags$script(src="DropChk.js")
  )
    
#DRUG TRENDS COLOUR:
#DT: #475c07
#NIDIP: #6e2a8d
#EDRS: #de761c
#IDRS: #00aeef
#DNet: #c4161c
# bootstrap breakpoints: 480, >767 <768, >991 <992, >1199 <1200
}
