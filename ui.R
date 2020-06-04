#For ABS COD 2018 data received in Sept 2019
#N. Man
library(shiny)
library(shinydashboard)
#library(shinyTree)
#library(shinythemes)
#library(ggplot2)
#library(ggthemes)
library(plotly)
library(shinycustomloader)
library(shinyjs)
#library(shinyBS)
#https://stackoverflow.com/questions/54403211/how-to-modify-the-themes-of-shinythemes
#css <- HTML("td, th { padding: 10; }") # doesn't work

ui <- function(request) {
#test dummy for passing query parameters
#textInput("Bulletin",NULL, "")

  bootstrapPage('',
    tags$head(
      includeScript("google_analytics.js"),
      #https://stackoverflow.com/questions/30096187/favicon-in-shiny
      #https://www.w3.org/2005/10/howto-favicon
      tags$link(rel="icon", type="image/png", href="favicon.png")
      #for reducing size of loader gif image - Now included in DT-theme.css
      #   ,tags$style(HTML("
      #     img.loader-img{
      #       width: 50px;
      #       height: auto;
      #     }"))
      #NB: the css file methods occasionally has ERROR: [uv_write] broken pipe warning
      #https://github.com/rstudio/shiny/issues/2371
      #this works by putting css file in www directory
      #    ,tags$link(rel="stylesheet", type="text/css", href="DT-theme.css")
    ),
    #this works by putting css file in parent directory
    # includeCSS("yeti.mod.css"),
    theme="DT-theme.css",
    #   theme = shinytheme("yeti"),
    #   header = singleton(tags$head(includeScript("google_analytics.js"))),
    useShinyjs(),
# https://stackoverflow.com/questions/47896963/hide-and-show-sidebar-panel-in-shiny
    # extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse"); 
    #           $(window).trigger("resize"); }'),
    # extendShinyjs(text='shinyjs.showSidebar = function(params) { $("body").removeClass("sidebar-collapse"); 
    #               $(window).trigger("resize"); }'),
#    bsButton("showpanel", "Show/Hide sidebar",icon = icon("toggle-off"), type = "toggle",style = "info", value = TRUE),
    
  #might be helpful for reusing repetitive inputs like year???
  #https://rstudio.github.io/shinydashboard/structure.html
  # dashboardPage(skin ="green",
  #   dashboardHeader(disable = TRUE),
  #   # All drugs menu tab ---------------------------------------------------------------
  # dashboardBody(
    mainPanel(width=9,
      tags$script(src="DropChk.js"),
      navbarPage(
        title= "Deaths induced by:",
        id = "Plot",
        # fluidPage(
        #   column(width = 8, "Deaths induced by:")
        #   ,column(width = 4, actionButton("showSidebar")
        # ),
#   shinythemes::themeSelector(),
#  title=div(img(src="DrugTrends-Logo.png", style="height: 25px"), "Deaths induced by:"),

#might be helpful for reusing repetitive inputs like year???
#https://rstudio.github.io/shinydashboard/structure.html
  # All drugs menu tab ---------------------------------------------------------------
    navbarMenu("All drugs",
      
    # All drugs by jurisdiction, intent, age and sex (Tables 1a, 1b & 1c) ---------------------------
      tabPanel(value = "AllPage",
        "Drug-induced deaths by jurisdiction, age and sex",
        h2("Drug-induced deaths by jurisdiction, intent, age and sex"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            # mainPanel(width=9,
              withLoader(plotlyOutput("AllPlot", width = "100%", height = "600px"),
#                 type = "html", loader = "loader4"),
#create animated gif logo in Photoshop; put DT_NIDIP_tween.gif in www directory
#                 type="image", loader="NIDIP_MovingS.gif"),
#                 type="image", loader="DT_NIDIP_onion30.gif"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteAll.html")) #Markdown("notesAllDrugsPlot.md"))
            # ),

#             sidebarPanel(width=3,
#     #         downloadButton("AllDrugHtml", "Generate report") #WIP: html download - only static if html only
#             )
          ),
          tabPanel("Notes", includeHTML("notesAll.html")) #Markdown("notesAllDrugs.md"))
#includeMarkdown("notesAllDrugs.md"), tableOutput('AllOD') )
        )
      ),

    # Remoteness by jurisdiction and intent (Tables R1, R4 & R5) -----------------------------------------
      tabPanel(value = "RAPage",
        "Drug-induced deaths by jurisdiction and remoteness area",
        h2("Drug-induced deaths by jurisdiction, remoteness area and intent"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("RAPlot", width = "100%", height = "600px"),
            #           type = "html", loader = "loader4"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteRem.html")
          ),
          tabPanel("Notes", includeHTML("notesRem.html")) #Markdown("notesRemote.md"))
          #includeMarkdown("notesRemote.md"), tableOutput("RemOD"))
        )
      ),

    # Remoteness percents (Tables R) -----------------------------------------
      tabPanel(value = "RPPage",
        "Percentages of drug-induced deaths by remoteness area",
        h2("Percentages of drug-induced deaths by remoteness area"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            # mainPanel(
              withLoader(plotlyOutput("RPPlot", width = "100%", height = "600px"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteRem.html") #Markdown("notesRemotePlot.md"))
            # ),
            
            # sidebarPanel(
              # selectInput("yaxRP", "Plot:",
              #   c(
              #     "Number of deaths" = "num",
              #     "Percentages of all drug-induced deaths" = "percent"
              #   ),
              #   selected = "percent"
              # ),


              # conditionalPanel(
              #   condition = "input.RPjur != 'Australia' || input.RPsex != 'All' || input.RPyax != 'deaths'",
                # radioButtons("RPage", "Age range:",
                #     c("All ages","15 to 64"="15-64"),
                #     selected = "All ages"
                # )
              # ),

              # conditionalPanel(
              #   condition = "input.RPjur == 'Australia' && input.RPsex == 'All' & input.RPyax == 'deaths'",
                # checkboxGroupInput("RPAage", "Age range:",
                #     c(
                    # "15 to 24" = "15-24",
                    # "25 to 34" = "25-34",
                    # "35 to 44" = "35-44",
                    # "45 to 54" = "45-54",
                    # "55 to 64" = "55-64",
                    # "65 to 74" = "65-74",
                    # "75 to 84" = "75-84",
                #       "All ages",
                #       "15 to 64" = "15-64"
                #     ),
                #     selected = c("All ages")
                # )
              # )
            # )
          ),
          tabPanel("Notes", includeHTML("notesRem.html")) #Markdown("notesRemote.md"))
          #includeMarkdown("notesRemote.md"), tableOutput("Rem_OD"))
        )
      ),

    # All drug-induced deaths by drug, jurisdiction, intent and/or sex (Table 12, 12b & 12c)------------------------------------------------
      tabPanel(value = "DTJPage",
        "Drug-induced deaths by drug, jurisdiction and/or sex",
        h2("Drug-induced deaths by drug, jurisdiction, intent and/or sex"),
      
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("DTJPlot", width = "100%", height = "600px"),
              #   type = "html", loader = "loader4"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteDTJ.html")
           ),
           tabPanel("Notes", includeHTML("notesDT.html")) #Markdown("notesAllByDrug.md"))
  #includeMarkdown("notesDT.md"), tableOutput('DT_OD'))
         )
      ),


    # All drug-induced deaths by drug, age and intent (Table 12 & 12a)------------------------------------------------
      tabPanel(value = "DTAPage",
        "Drug-induced deaths by drug and age",
        h2("Drug-induced deaths by drug, age and intent"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("DTAPlot", width = "100%", height = "600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteDTA.html")
           ),
          tabPanel("Notes", includeHTML("notesDT.html")) #Markdown("notesAllByDrug.md"))
  #includeMarkdown("notesDT.md"), tableOutput('DT_OD'))
        )
      )
    ),


  # Opioids menu tab -------------------------------------------------------------
    navbarMenu("Opioids",

    # Opioids by opioid, age and intent (Table 4) -----------------------------------------
      tabPanel(value = "O4Page",
        "By opioid and age",
        h2("Opioid-induced deaths by opioid, age and intent"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("O4Plot", width = "100%", height = "600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp4.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html")) #Markdown("notesOpioids.md"))
        )
      ),

    # Opioids by intent, opioid and sex (Table 5) -----------------------------------------
      tabPanel(value = "O5Page",
        "By opioid and sex",
        h2("Opioid-induced deaths by opioid, intent and sex"),

        tabsetPanel(type = "tabs",
          tabPanel( "Plot",
              withLoader(plotlyOutput("O5Plot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp5.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html")) #Markdown("notesOpioids.md"))
        )
      ),

    # Opioids by intent, jurisdiction and sex (Table 6) ---------------------------------
      tabPanel(value = "O6Page",
        "By sex and jurisdiction",
        h2("Opioid-induced deaths by intent, jurisdiction and sex"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("O6Plot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp6.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html")) #Markdown("notesOpioids.md"))
        )
      ),
  
    # Exclusive opioids by age and intent (Table 10)-----------------------------------------
      tabPanel(value = "E0Page",
        "By exclusive opioid type and age",
        h2("Opioid-induced deaths by exclusive opioid type, age and intent"),
        
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("E0Plot", width = "100%", height = "600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteEOp.html")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html")) #Markdown("notesOpioidsExclusive.md"))
        )
      ),
      
    # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11)-----------------------------------------
      tabPanel(value = "E9Page",
        "By exclusive opioid type, jurisdiction and sex",
        h2("Opioid-induced deaths by exclusive opioid type, jurisdiction, intent and sex"),
        
        tabsetPanel(type = "tabs",
          tabPanel( "Plot",
              withLoader(plotlyOutput("E9Plot", width = "100%", height = "600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteEOp.html")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html")) #Markdown("notesOpioidsExclusive.md"))
        )
      ),
      
    # Exclusive opioids percents (Tables 10 & 11) -----------------------------------------
      tabPanel(value = "EPPage",
        "Exclusive opioid types as percentages",
        h2("Opioid-induced deaths by exclusive opioid type as percentages of all opioid-induced deaths"),
        
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("EPPlot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html")) #Markdown("notesOpioidsExclusive.md"))
        )
      ),
  
    # Opioids with other drugs by age and intent (Table 7)-----------------------------------------
    tabPanel(value = "W7Page",
      "Opioids with other drugs, by age",
      h2("Opioid-induced deaths by other drugs with opioids, age and intent"),
  
      tabsetPanel(type = "tabs",
        tabPanel("Plot",
            withLoader(plotlyOutput("W7Plot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
            includeHTML("fnoteWOp.html")
        ),
        tabPanel("Notes", includeHTML("notesWOp.html")) #Markdown("notesOpioidsOtherDrugs.md"))
      )
    ),
  
    # Opioids with other drugs by sex and intent (Table 8) -----------------------------------------
    tabPanel(value = "W8Page",
      "Opioids with other drugs, by sex",
      h2("Opioid-induced deaths by other drugs with opioids, sex and intent"),
  
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            # mainPanel(width = 9,
              withLoader(plotlyOutput("W8Plot", width = "100%", height = "600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteWOp.html")
            # ),
            # sidebarPanel(width = 3,
              # selectInput( "W8age", "Age range:",
              #   choices = c(
              #     "All ages", "15 to 64"="15-64"
              #   ),
              #   selected = c("All ages")
              # ),

              # radioButtons("W8drop", "Variable for dropdown list:",
              #   choices = c(
              #     "Sex", "Intent"
              #   ), inline = T,
              #   selected = c("Sex")
              # ),
              # conditionalPanel( condition = "input.W8drop == 'Sex'",
              #   selectInput("W8Ssex", label = NULL,
              #     choices = c("All", "Female", "Male","Male & Female"="MF"),
              #     selected = c("All")
              #   ),
              #   checkboxGroupInput("W8Scod", "Intent:",
              #     c("All", "Accidental", "Intentional", "Undetermined"),
              #     selected = c("All", "Accidental", "Intentional", "Undetermined")
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.W8drop == 'Intent'",
              #   selectInput("W8Icod", label = NULL,
              #     c("All", "Accidental", "Intentional", "Undetermined"),
              #     selected = c("All") ),
              #   checkboxGroupInput("W8Isex", "Sex:",
              #     choices = c("All", "Female", "Male"),
              #     selected = c("All", "Female", "Male")
              #   )
              # ),
  
              # checkboxGroupInput("W8drug", "All opioids with:",
              #   choices = c(
              #     "4-aminophenol derivatives (e.g. paracetamol)" = "4-aminophenol derivatives",
              #     "Alcohol",# = "All opioids with alcohol",
              #     "Amphetamines",# = "All opioids with amphetamines",
              #     "Antidepressants",
              #     "Antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
              #     = "Antiepileptic & sedative-hypnotic drugs,\nunspecified",
              #     "Antipsychotics & neuroleptics",
              #     "Benzodiazepines"
              #   ),
              #   selected = c("Alcohol")
              # ),
              # checkboxGroupInput("W8show", "Also show:",
              #   choices = c("all drug-induced deaths")
              # )
            # )
          ),
          tabPanel("Notes", includeHTML("notesWOp.html")) #Markdown("notesOpioidsOtherDrugs.md"))
        )
      )
    ),


  # Stimulants menu tab ---------------------------------------------------------------
    navbarMenu("Stimulants",
  
    # Amphetamines tab (Table 2) --------------------------------------------------------
      tabPanel(value = "AmPage",
        "Amphetamine-induced deaths",
        h2("Amphetamine-induced deaths"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
              withLoader(plotlyOutput("AmPlot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteAms.html") #Markdown("notesAmphetaminesPlot.md"))
          ),
          tabPanel("Notes", includeHTML("notesAms.html")) #Markdown("notesAmphetamines.md"))
          #includeMarkdown("notesAms.md"), tableOutput('AmphOD'))
        )
      ),

    # Cocaine tab (Table 3) -------------------------------------------------------------
      tabPanel(value = "CPage",
        "Cocaine-induced deaths",
        h2("Cocaine-induced deaths"),
  
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            withLoader(plotlyOutput("CPlot", width = "100%", height = "600px"),
              type="image", loader="DT_NIDIP_tween.gif"),
            includeHTML("fnoteCoke.html")
          ),
          tabPanel("Notes", includeHTML("notesCoke.html")) #Markdown("notesCocaine.md"))
          #includeMarkdown("notesCoke.md"), tableOutput('CocaineOD') )
        )
      )
    ),

  # Explanatory Notes tab ---------------------------------------------------------------
    tabPanel(
      "Explanatory notes", includeHTML("notesExplanatory.html") #Markdown("notesOverall.md")
    ),


  # Citation tab ------------------------------------------------------------
    tabPanel(
      "Citation & acknowledgements",includeHTML("Citation.html")
   )#trying to hide side panel - but messes up on update###############
    # ,tabPanel(
    #   actionButton("showSidebar",label="Plot options")
    # )
#  ),
  )
  ),
  # dashboardSidebar(
# https://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding - not for narrow screens
    HTML("<div class='col-sm-0' style='padding-top: 50px;
      background-image:
        linear-gradient(to bottom,
          rgba(0, 135, 73, 1) 0%,
          rgba(0, 135, 73, 1) 100%),
        linear-gradient(to bottom,
          rgba(0, 135, 73, 1) 0%,
          rgba(0, 135, 73, 1) 100%);
      background-clip: content-box, padding-box'>"),
    sidebarPanel(id="Sidebar",width=3,
# tags$style(type="text/css", "body {padding-top: 70px;}"),
    # year slider###############
      conditionalPanel(
        condition = "input.Plot != 'RAPage' & input.Plot != 'RPPage' &
        input.Plot != 'E9Page' & input.Plot != 'E0Page' & input.Plot != 'EPPage'",
        sliderInput( "yr97", "Period",
          min = 1997, max = 2018,
          value = c(1997, 2018), sep = ""
        )
      ),
      conditionalPanel(
        condition = "input.Plot == 'E9Page' | input.Plot == 'E0Page' | input.Plot == 'EPPage'",
        sliderInput( "yr07", "Period",
          min = 2007, max = 2018,
          value = c(2007, 2018), sep = ""
        )
      ),
      conditionalPanel(
        condition = "input.Plot == 'RAPage' | input.Plot == 'RPPage'",
        sliderInput( "yr11", "Period",
          min = 2011, max = 2018,
          value = c(2011, 2018), sep = ""
        )
      ),
  ####For user-defined year intervals
    # radioButtons("xax", "Interval between years:",
    #   choices = c(1, 2, 5), inline=T, selected = 2
    # ),
###HTML info on width of plot for WIP: <rect class="nsewdrag drag" width="854" height="474" ;"></rect>

    # y-variable list###############
      conditionalPanel(
        condition = "input.Plot != 'RPPage' & input.Plot != 'EPPage'",
        selectInput( "yax", "Plot:",
           c(
             "Number of deaths" = "num",
             "Deaths per 100,000 people" = "r5",
             "Deaths per 100,000 people (95% CI)" = "r5ci",
             "Deaths per 1,000,000 people" = "r6",
             "Deaths per 1,000,000 people (95% CI)" = "r6ci"
           ),
           selected = "r5"
        )
      ),

    # geographical lists###############
      conditionalPanel(
        condition = "input.Plot == 'AllPage' | input.Plot == 'O6Page'
        | input.Plot == 'E9Page' | input.Plot == 'EPPage'",
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
        condition = "input.Plot == 'RAPage' | input.Plot == 'RPPage'",
        selectInput("jurR", "Jurisdiction:",
          c(
            "Australia",
            "New South Wales",
            "Victoria",
            "Queensland",
            "South Australia",
            "Western Australia"
          ),
          selected = "Australia"
        )
      ),

      conditionalPanel(
        condition = "input.Plot == 'RAPage' & input.jurR == 'Australia'",
        checkboxGroupInput("RAra", "Remoteness region:",
           choices = c(
             "Major Cities",
             "Regional and Remote",
             "-Inner Regional"="Inner Regional",
             "-Outer Regional"="Outer Regional",
             "-Remote and Very Remote"="Remote and Very Remote"
           ),
           selected = c(
             "Major Cities",
             "Regional and Remote"
           )
        )
      ),
      conditionalPanel(
        condition = "input.Plot == 'RAPage' & input.jurR != 'Australia'",
        checkboxGroupInput("Rra", "Remoteness region:",
            choices = c(
              "Major Cities",
              "Regional and Remote"
            ),
            selected = c(
              "Major Cities",
              "Regional and Remote"
            )
        )
      ),

    # radio button panelS###############
      conditionalPanel(condition = "input.Plot == 'RPPage' | 
        input.Plot == 'E9Page' | input.Plot == 'EPPage' |
        input.Plot == 'O5Page' | input.Plot == 'O6Page' | input.Plot == 'W8Page'",
          radioButtons( "ageR", "Age range:",inline = TRUE,
          choices = c(
            "All ages", "15 to 64"="15-64"
          ),
          selected = c("All ages")
        ),
        conditionalPanel(
          condition = "(input.Plot == 'RPPage' & input.jurR == 'Australia') | input.Plot == 'EPPage'",
          radioButtons("sexR", "Sex:",
            choices = c(
              "Male",
              "Female",
              "All"
            ),
            selected = c("All")
          )
        ),
        conditionalPanel(
          condition = "input.Plot == 'RPPage' | input.Plot == 'EPPage'",
          radioButtons( "codR", "Intent:",
            c("All", "Accidental"),
            selected = "All"
          )
        )
      ),

    # Drug type with jurisdiction panels (DTJPage)#######
      conditionalPanel( condition = "input.Plot == 'DTJPage'",
        selectInput( "DTjur", "Jurisdiction:",
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

        conditionalPanel( condition = "input.DTjur == 'Australia'",
          selectInput( "DTage", "Age range:",
           choices = c(
             "All ages","15 to 64" = "15-64"
           ),
           selected = c("All ages")
          )
        ),
        radioButtons("DTJdrop", "Variable for dropdown list:",
          choices = c(
              "Sex (only Aus) & Intent"="IntSx",
              "Drug"="Drug"
          ), #inline = T,
          selected = c("IntSx")
        ),

        conditionalPanel( condition = "input.DTJdrop == 'IntSx'",
          conditionalPanel( condition = "input.DTjur == 'Australia'",
            selectInput("DTIsex", "Sex:",
              choices = c("All",
                "Female - select All ages as Age range"="Female",
                "Male - select All ages as Age range"="Male",
                "Male & Female - select All ages as Age range"="MF"),
              selected = c("All")
            ),
            conditionalPanel( condition = "input.DTIsex == 'All'",
              selectInput("DTIcod", "Intent:",
                choices = c("All", "Accidental", "Intentional", "Undetermined"),
                selected = c("All")
              )
            )
          ),
          conditionalPanel( condition = "input.DTjur != 'Australia' |
            (input.DTjur == 'Australia' & input.DTIsex != 'All')",
            selectInput("DTIcod2", "Intent:",
              choices = c("All", "Accidental"),
              selected = c("All")
            )
          )
        )
        # conditionalPanel( condition = "input.DTJdrop == 'Drug'",
        #drug type selection below
        #intent & age checkboxes later
      ),

    # Drug type with age panels (DTAPage)##################
      conditionalPanel( condition = "input.Plot == 'DTAPage'",
        radioButtons("DTAdrop", "Variable for dropdown list:",
            choices = c(
              "Age & Intent"="Age_Intent",
              "Drug"="Drug"
            ),inline = T,
            selected = c("Age_Intent")
        ),
        conditionalPanel(condition = "input.DTAdrop == 'Age_Intent'",
          selectInput("DTAIcod", "Intent:",
            choices = c("All", "Accidental"),
            selected = c("All")
          ),

          selectInput( "DTAIage", "Age:",
            choices = c(
              "15 to 24" = "15-24",
              "25 to 34" = "25-34",
              "35 to 44" = "35-44",
              "45 to 54" = "45-54",
              "55 to 64" = "55-64",
              "65 to 74" = "65-74",
              "75 to 84" = "75-84",
              "All ages",
              "15 to 64" = "15-64"
            ),
            selected = c("All ages")
          )
        )
        # conditionalPanel(condition = "input.DTAdrop == 'Drug'" ...
        #drug type selection below
        #intent & age checkboxes later
      ),
    # Drug type selection (DTJ & DTA Pages)#######
      conditionalPanel(condition = "(input.Plot == 'DTAPage' & input.DTAdrop == 'Drug')
        | (input.Plot == 'DTJPage' & input.DTJdrop == 'Drug')",
        selectInput("DTdrugD", label = NULL,
          choices = c(
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
          selected = c("OPIOIDS")
        )
      ),
    # Drug type checkbox (DTJ & DTA Pages)#######
      conditionalPanel(condition = "(input.Plot == 'DTAPage' & input.DTAdrop != 'Drug')
        | (input.Plot == 'DTJPage' & input.DTJdrop != 'Drug')",
        checkboxInput("DTdrugA", label=HTML("<b>Drug:</b>"),value=TRUE
        ),
        HTML("<div style='margin-left: 6%;'>"),
        checkboxGroupInput("DTdrug", NULL, #"Drug:",
          choices = c(
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
          selected = c("OPIOIDS", "ALCOHOL", "AMPHETAMINES", "ANTIDEPRESSANTS", 
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "ANTIPSYCHOTICS & NEUROLEPTICS", "CANNABINOIDS",
            "COCAINE", "NONOPIOID ANALGESICS"
          )
        ),
        HTML("</div>")
      ),

    # Opioid, sex & intent control panel (O5Page)###############
      conditionalPanel( condition =
        "input.Plot == 'O5Page'",
        radioButtons("O5drop", "Variable for dropdown list:",
          choices = c(
            "Opioid",
            "Intent",
            "Sex"
          ),inline = T,
          selected = c("Opioid")
        )
      ),

    # Opioid and age panels (O4, E0 & W7 Pages)###############
      conditionalPanel( condition =
        "input.Plot == 'O4Page' | input.Plot == 'E0Page' | input.Plot == 'W7Page'",
        radioButtons("dropOA", "Variable for dropdown list:",
          choices = c(
            "Drug", "Age"
          ),inline = T,
          selected = c("Drug")
        ),

        conditionalPanel( condition = "input.dropOA == 'Age'",
          selectInput("ageOAA", label = NULL,
            choices = c(
               "15 to 24" = "15-24",
               "25 to 34" = "25-34",
               "35 to 44" = "35-44",
               "45 to 54" = "45-54",
               "55 to 64" = "55-64",
               "65 to 74" = "65-74",
               "75 to 84" = "75-84",
               "All ages",
               "15 to 64" = "15-64"
            ),
            selected = c("All ages")
          )
        )
        # O4 drug panel under Opioid checkbox (O4 & O5 Pages)
        # E0 drug panel under Exclusive opioid checkbox (E0 & E9 Pages)
        # W7 drug panel under Other drugs with opioid checkboxes (W7 & S8 Pages)
      ),
      conditionalPanel( condition = "input.dropOA == 'Drug'",
        conditionalPanel( condition = "input.Plot == 'W7Page'",
          selectInput( "W7Ddrug", label = "Opioid-induced deaths with:",
            choices = c(
              "4-aminophenol derivatives (e.g. paracetamol)" = "4-aminophenol derivatives",
              "Alcohol",# = "All opioids with alcohol",
              "Amphetamines",# = "All opioids with amphetamines",
              "Antidepressants",
              "Antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
              = "Antiepileptic & sedative-hypnotic drugs,\nunspecified",
              "Antipsychotics & neuroleptics",
              "Benzodiazepines"
            ),
            selected = c("Alcohol")
          )
        ),
        conditionalPanel( condition = "input.Plot == 'E0Page'",
          selectInput("E0Odrug", label = NULL,
            choices = c(
              "Exclusive illicit opioids",
              "Exclusive pharmaceutical opioids",
              "Illicit & pharmaceutical opioids",
              "Other & unspecified opioids"
            ),
            selected = c(
              "Exclusive illicit opioids" )
          )
        )
        # Age checkbox at the end
      ),
      conditionalPanel( condition =
        "input.Plot == 'O4Page' & input.dropOA == 'Drug' |
        input.Plot == 'O5Page' & input.O5drop == 'Opioid'",
        selectInput("OdrugO", label = NULL,
          choices = c(
            "All opioids",
            "Heroin",
            "Opium",
            "Methadone",
            "Natural & semi-synthetic opioids",
            "Synthetic opioids",
            "Other & unspecified opioids"
          ),
          selected = c("All opioids")
        )
      ),

    # intent & sex panels (All, W8 & E9 + O5 Pages)###############
      conditionalPanel(
        condition = "input.Plot == 'AllPage' | input.Plot == 'W8Page' | input.Plot == 'E9Page'",
        radioButtons("dropSI", "Variable for dropdown list:",
           choices = c(
             "Intent", "Sex"
           ), inline = T,
           selected = c("Intent")
        )
      ),
      conditionalPanel(condition = "(input.dropSI == 'Sex' & 
        (input.Plot == 'AllPage' | input.Plot == 'W8Page' | input.Plot == 'E9Page')) |
        (input.O5drop == 'Sex' & input.Plot == 'O5Page')",
        selectInput("sex4R", label = NULL,
          choices = c("All", "Female", "Male","Male & Female"="MF")
        )
      ),
      conditionalPanel( condition = "input.Plot == 'AllPage'",
        conditionalPanel( condition =  "input.dropSI != 'Intent'",
          checkboxGroupInput(
            "AllScod", label = "Intent:",
            c("All", "Accidental", "Intentional", "Undetermined","Other"),
            selected = c("All", "Accidental")
          )
        ),
        # Intent checkbox for W8 & E9 later below with other pages
        conditionalPanel( condition = "input.dropSI == 'Intent'",
          selectInput("AllIcod", label = NULL,
            choices = c("All", "Accidental", "Intentional", "Undetermined","Other")
          )
        )
      ),
      conditionalPanel(condition = 
        "(input.dropSI == 'Intent' & input.Plot == 'W8Page') |
        (input.O5drop == 'Intent' & input.Plot == 'O5Page')",
        selectInput("cod4R", label = NULL,
          choices = c("All", "Accidental", "Intentional", "Undetermined")
        )
      ),
      conditionalPanel( condition = "input.Plot == 'E9Page' & input.dropSI == 'Intent'",
        selectInput("E9Icod", label = NULL,
          choices = c("All", "Accidental", "Intentional")
        )
      ),
      # Sex checkboxes for later below with other pages

    # Sex checkbox (All, O5, O6, W8 & E9 Pages)###############
      conditionalPanel( condition = "input.Plot == 'O6Page' |
        ((input.Plot == 'AllPage' | input.Plot == 'W8Page' | input.Plot == 'E9Page')
        & input.dropSI != 'Sex') | (input.Plot == 'O5Page' & input.O5drop != 'Sex')
        | (input.Plot == 'DTJPage' & input.DTJdrop == 'Drug' & input.DTjur == 'Australia')",
        conditionalPanel( condition = "input.Plot == 'DTJPage' & input.DTJdrop == 'Drug' & input.DTjur == 'Australia' & input.DTage == '15-64' & input.sexC != 'All'",
          HTML("<i>Please select All ages for data by male and/or female.</i>")
        ),
        checkboxGroupInput("sexC", label = "Sex:",
           choices = c("All", "Female", "Male"),
           selected = c("All", "Female", "Male")
        ),
      ),
    # Age range checkbox (CPage)###############
      conditionalPanel(
        condition = "input.Plot == 'CPage'",
        checkboxGroupInput("Cage", "Age range:",
          choices = c(
            "All ages","15 to 64" = "15-64"
          ),
            selected = c("All ages")
        )
      ),
    # Intent(2C) checkbox (RA, O6, C & Am Pages)###############
      # HTML("<p><b>Intent:</b></p>"),
      # shinyTree("Amcod",
      #     checkbox = TRUE, themeIcons = FALSE),
      # HTML("<b>Age:</b>"),
      # shinyTree("Amage",
      #     checkbox = TRUE, theme="proton", themeIcons = FALSE),
      conditionalPanel(
        condition = "input.Plot == 'RAPage' | input.Plot == 'O6Page' |
        input.Plot == 'CPage' | input.Plot == 'AmPage' |
        (input.Plot == 'DTAPage' & input.DTAdrop == 'Drug') |
        (input.Plot == 'DTJPage' & input.DTJdrop == 'Drug' & input.DTjur != 'Australia')",
          checkboxGroupInput("cod2C", "Intent:",
            c("All", "Accidental"),
            selected = "All"
          )
      ),

    # Intent(3C) checkbox (E0 & E9 Pages)###############
      conditionalPanel(
        condition = "input.Plot == 'E0Page' | (input.Plot == 'E9Page' & input.dropSI == 'Sex')",
          checkboxGroupInput("cod3C", "Intent:",
            c("All", "Accidental", "Intentional"),
            selected = "All"
          )
      ),

    # Other drugs with opioid checkboxes (W7 & W8 Pages)###############
      conditionalPanel( condition = "input.Plot == 'W8Page' |
        (input.Plot == 'W7Page' & input.dropOA == 'Age')",
        checkboxGroupInput("Wdrug", "All opioids with:",
          choices = c(
            "4-aminophenol derivatives (e.g. paracetamol)" = "4-aminophenol derivatives",
            "Alcohol",# = "All opioids with alcohol",
            "Amphetamines",# = "All opioids with amphetamines",
            "Antidepressants",
            "Antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
            = "Antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "Antipsychotics & neuroleptics",
            "Benzodiazepines"
          ),
          selected = c("Alcohol")
        )
      ),
      conditionalPanel( condition = 
        "input.Plot == 'W8Page' | input.Plot == 'W7Page'",
        checkboxGroupInput("Wshow", "Also show:",
          choices = c("all drug-induced deaths")
        )
      ),
    # Intent panels(4C) (DTJ, O4, O5, W7 & W8 Pages)###############
      conditionalPanel( condition = "input.Plot == 'O4Page' | input.Plot == 'W7Page' |
        (input.Plot == 'W8Page' & input.dropSI != 'Intent' & input.sex4R != 'MF') | 
        (input.Plot == 'O5Page' & input.O5drop != 'Intent') |
        (input.Plot == 'DTJPage' & input.DTJdrop == 'Drug' & input.DTjur == 'Australia')",
        checkboxGroupInput(
          "cod4C", label = "Intent:",
          c("All", "Accidental", "Intentional", "Undetermined"),
          selected = c("All", "Accidental")
        ),
        conditionalPanel( condition = "input.cod4C.length == 2 &
          (input.Plot == 'O4Page' | input.Plot == 'W7Page' | input.Plot == 'DTJPage' |
          (input.Plot == 'O5Page' & (input.O5drop != 'Sex' | input.sex4R != 'MF') ) |
          (input.Plot == 'W8Page') )",
          radioButtons("cod4C2",label = "Show intent as:",
            choices = c("Single plot"=1,"Side-by-side plot"=2),
            inline = T, selected = 1
          )
        )
      ),

    # Opioid checkbox (O4 & O5 Pages)###############
      conditionalPanel( condition = "(input.Plot == 'O4Page' & input.dropOA != 'Drug')
        | (input.Plot == 'O5Page' & input.O5drop != 'Opioid')",
        checkboxGroupInput("OdrugC", "Opioid:",
          choices = c(
              "All opioids",
              "Heroin",
              "Opium",
              "Methadone",
              "Natural & semi-synthetic opioids",
              "Synthetic opioids",
              "Other & unspecified opioids"
          ),
          selected = c("All opioids")
        )
      ),
    # Exclusive opioid checkbox (E9 & E0 Pages)###############
      conditionalPanel( condition = "input.Plot == 'E9Page' |
        (input.Plot == 'E0Page' & input.dropOA == 'Age')",
        checkboxGroupInput("Edrug", "Drug:",
          choices = c(
            "Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids",
            "Illicit & pharmaceutical opioids",
            "Other & unspecified opioids"
          ),
          selected = c(
            "Exclusive illicit opioids",
            "Exclusive pharmaceutical opioids"
          )
        )
      ),

    # Age checkbox (All, Am, O4, E0, W7 & DTA Pages)###############
      conditionalPanel( condition = "input.Plot == 'AllPage' | input.Plot == 'AmPage'
        | ((input.Plot == 'O4Page' | input.Plot == 'E0Page' | input.Plot == 'W7Page')
        & input.dropOA == 'Drug') | (input.Plot == 'DTAPage' & input.DTAdrop == 'Drug')",
        checkboxInput("ageAllA", label=HTML("<b>Age:</b>"),value=TRUE
        ),
        HTML("<div style='margin-left: 6%;'>"),
        checkboxGroupInput( "ageAll", NULL, #"Age:",
          c( "15 to 24" = "15-24",
             "25 to 34" = "25-34",
             "35 to 44" = "35-44",
             "45 to 54" = "45-54",
             "55 to 64" = "55-64",
             "65 to 74" = "65-74",
             "75 to 84" = "75-84",
             "All ages",
             "15 to 64" = "15-64"),
          selected = c("All ages")
        ),
        HTML("</div>")
      )
    ),
  HTML("</div>")
  # ),
  )
    
#DRUG TRENDS COLOUR:
#DT: #475c07
#NIDIP: #6e2a8d
#EDRS: #de761c
#IDRS: #00aeef
#DNet: #c4161c
}
