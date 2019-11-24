#For ABS COD 2018 data received in Sept 2019
#N. Man
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(shinycustomloader)

#https://stackoverflow.com/questions/54403211/how-to-modify-the-themes-of-shinythemes
#css <- HTML("td, th { padding: 10; }") # doesn't work

ui <- function(req) {
  
bootstrapPage('',

#this one works by putting header.css in parent directory
#NB: the css file methods occasionally has a ERROR: [uv_write] broken pipe warning
#https://github.com/rstudio/shiny/issues/2371
# includeCSS("header.css"),
  #for reducing size of loader gif image
  tags$head(
    tags$style(HTML("
      img.loader-img{
          width: 50px;
          height: auto;
      }
    "))
#        td, th { padding: 10; } # doesn't work
#this one works by putting header.css in www directory
#    tags$link(rel = "stylesheet", type = "text/css", href = "header.css")
  ),
#https://stackoverflow.com/questions/30096187/favicon-in-shiny
#https://www.w3.org/2005/10/howto-favicon
  tags$head(tags$link(rel="icon", type="image/png", href="favicon.png")
  ),

  navbarPage(
  header = singleton(tags$head(includeScript("google_analytics.js"))),
# fluidRow(
#   column(width = 12, "Deaths induced by:"))
#   , column(width = 4, img(src="DrugTrends-Logo.png", style="height: 25px")),

  theme = shinytheme("yeti"),
#   title=div(img(src="DrugTrends-Logo.png", style="height: 25px"), "Deaths induced by:"),
  title= "Deaths induced by:",
  id = "Plot",
  # All drugs menu tab ---------------------------------------------------------------
    navbarMenu(
      "All drugs",
      
    # All drugs by jurisdiction, intent, age and sex (Tables 1a, 1b & 1c) ---------------------------
      tabPanel("Drug-induced deaths by jurisdiction, intent, age and sex",
        value = "PlotAll",
        h1("Drug-induced deaths by jurisdiction, intent, age and sex"),

        tabsetPanel(
          type = "tabs",
          tabPanel("Plot",
            mainPanel(
              withLoader(plotlyOutput("allPlot", width = "100%", height = "600px"),
#                 type = "html", loader = "loader4"),
#create animated gif logo in Photoshop; put DT_NIDIP_tween.gif in www directory
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesAllDrugsPlot.md"))
            ),

            sidebarPanel(
              sliderInput( "yrAll", "Period",
                  min = 1997, max = 2018,
                  value = c(2008, 2018), sep = ""
              ),
              selectInput( "yaxAll", "Plot:",
                  c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "rateht",
                  "Deaths per 100,000 people (95% CI)" = "ratehtci",
                  "Deaths per 1,000,000 people" = "ratem",
                  "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                  ),
                  selected = "rateht"
              ),

              selectInput(
                  "stateAll", "Jurisdiction:",
                  c("Australia",
                   "New South Wales",
                   "Victoria",
                   "Queensland",
                   "South Australia",
                   "Western Australia",
                   "Tasmania",
                   "Northern Territory",
                   "Australian Capital Territory"  )
              ),

#Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("DropAll", "Variable for dropdown list:",
                  choices = c(
                      "Intent",
                      "Sex"
                  ), inline = T,
                  selected = c("Intent")
              ),

#             uiOutput("AllBControl"),
#From: https://shiny.rstudio.com/reference/shiny/1.0.4/conditionalPanel.html
              conditionalPanel(
                  condition = "input.DropAll == 'Intent'",
                      selectInput("codAllI", label = NULL,
                      choices = c("All", "Accidental", "Intentional", "Undetermined","Other")
                  ),
                  checkboxGroupInput("sexAllI", label = "Sex:",
                      choices = c("All", "Female", "Male"),
                      selected = c("All", "Female", "Male")
                  )
              ),
      
              conditionalPanel(
                  condition = "input.DropAll == 'Sex'",
                  selectInput("sexAllS", label = NULL,
                      choices = c("All", "Female", "Male","Male & Female"="MF")
                  ),
                  checkboxGroupInput(
                      "codAllS", label = "Intent:",
                      c("All", "Accidental", "Intentional", "Undetermined","Other"),
                      selected = c("All", "Accidental")
                  )
              ),

              checkboxGroupInput(
                  "ageAll", "Age:",
                  c( "15 to 24" = "15-24",
                     "25 to 34" = "25-34",
                     "35 to 44" = "35-44",
                     "45 to 54" = "45-54",
                     "55 to 64" = "55-64",
                     "65 to 74" = "65-74",
                     "75 to 84" = "75-84",
                     "All ages",
                     "15 to 64" = "15-64"),
                  selected = "15-64"
              )
    #         downloadButton("AllDrugHtml", "Generate report") #WIP: html download - only static if html only
            )
          ),
          tabPanel("Notes", includeMarkdown("notesAllDrugs.md"))
        )
      ),

      tabPanel(value = "PlotR",
  # Remoteness by jurisdiction and intent (Tables R1, R4 & R5) -----------------------------------------
        "Drug-induced deaths by jurisdiction, remoteness area and intent",
        h1("Drug-induced deaths"),
        h3("by jurisdiction, remoteness area and intent"),
        
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            mainPanel(
              withLoader(plotlyOutput("remotePlot", width = "100%", height = "600px"),
            #           type = "html", loader = "loader4"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesRemotePlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("yrR", "Period",
                          min = 2011, max = 2018,
                          value = c(2011, 2018), sep = ""
              ),
              selectInput("yaxR", "Plot:",
                  c(
                    "Number of deaths" = "num",
                    "Deaths per 100,000 people" = "rateht",
                    "Deaths per 100,000 people (95% CI)" = "ratehtci",
                    "Deaths per 1,000,000 people" = "ratem",
                    "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                  ),
                  selected = "rateht"
              ),

              selectInput("stateR", "Jurisdiction:",
                  c(
                    "Australia",
                    "New South Wales",
                    "Victoria",
                    "Queensland",
                    "South Australia",
                    "Western Australia"
                  ),
                  selected = "Australia"
              ),
        
              conditionalPanel(
                condition = "input.stateR == 'Australia'",
                checkboxGroupInput("regionRA", "Remoteness region:",
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
                condition = "input.stateR != 'Australia'",
                checkboxGroupInput("regionR", "Remoteness region:",
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
        
              checkboxGroupInput("codR", label = "Intent:",
                  c("All", "Accidental"),
                  selected = c("All")
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesRemote.md"))
        )
      ),

      tabPanel(
        value = "PlotRP",
  # Remoteness percents (Tables R) -----------------------------------------
        "Percentages drug-induced deaths by remoteness area",
        h1("Drug-induced deaths"),
        h3("Percentages of all drug-induced deaths by remoteness area"),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("remotePlotP", width = "100%", height = "600px"),
                         #           type = "html", loader = "loader4")
                         type="image", loader="DT_NIDIP_tween.gif")
            ),
            
            sidebarPanel(
              sliderInput("yrRP", "Period",
                  min = 2011, max = 2018,
                  value = c(2011, 2018), sep = ""
              ),
              # selectInput("yaxRP", "Plot:",
              #             c(
              #               "Number of deaths" = "num",
              #               "Percentages of all drug-induced deaths" = "percent"
              #             ),
              #             selected = "percent"
              # ),
              
              selectInput(
                  "stateRP", "Jurisdiction:",
                  c(
                    "Australia",
                    "New South Wales",
                    "Victoria",
                    "Queensland",
                    "South Australia",
                    "Western Australia"
                  )
              ),

              radioButtons(
                  "codRP", "Intent:",
                  c("All", "Accidental"),
                  selected = "All"
              ),

              conditionalPanel(
                condition = "input.stateRP == 'Australia'",
                radioButtons("sexRP", "Sex:",
                    choices = c(
                      "Male",
                      "Female",
                      "All"
                    ),
                    selected = c("All")
                )
              ),
              
              # conditionalPanel(
              #   condition = "input.stateRP != 'Australia' || input.sexRP != 'All' || input.yaxRP != 'deaths'",
                radioButtons("ageRP", "Age range:",
                    c("All ages","15 to 64"="15-64"),
                    selected = "All ages"
                )
              # ),

              # conditionalPanel(
              #   condition = "input.stateRP == 'Australia' && input.sexRP == 'All' & input.yaxRP == 'deaths'",
                # checkboxGroupInput("ageRPA", "Age range:",
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
                #     selected = "15-64"
                # )
              # )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesRemote.md"))
        )
      ),

# All drug-induced deaths by drug, jurisdiction, intent and/or sex (Table 12, 12b & 12c)------------------------------------------------
      tabPanel("Drug-induced deaths by drug, jurisdiction, intent and/or sex",
        value = "PlotDT",
        "",
        h1("Drug-induced deaths by drug, jurisdiction, intent and/or sex"),
      
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("drugtypePlot", width = "100%", height = "600px"),
                        #   type = "html", loader = "loader4"),
                        type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesAllByDrugJPlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("yrDT", "Period",
                           min = 1997, max = 2018,
                           value = c(2008, 2018), sep = ""
              ),
              selectInput(
                "yaxDT", "Plot:",
                 c(
                   "Number of deaths" = "num",
                   "Deaths per 100,000 people" = "rateht",
                   "Deaths per 100,000 people (95% CI)" = "ratehtci",
                   "Deaths per 1,000,000 people" = "ratem",
                   "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                 ),
                 selected = "rateht"
              ),
              
              selectInput(
                 "stateDT", "Jurisdiction:",
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

              conditionalPanel(
                condition = "input.stateDT == 'Australia'",
                   selectInput( "ageDT", "Age range:",
                    choices = c(
                      "All ages","15 to 64" = "15-64"
                    ),
                    selected = c("15-64")
                   )
              ),
  #       Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("DropDT", "Variable for dropdown list:",
                  choices = c(
                      "Sex (only Australia) and Intent"="IntSx",
                      "Drug"="Drug"
                  ),inline = T,
                  selected = c("IntSx")
              ),

              conditionalPanel(
                  condition = "input.DropDT == 'IntSx'",
                  conditionalPanel(
                    condition = "input.stateDT == 'Australia'",
                      selectInput("sexDTI", "Sex:",
                          choices = c("All",
                              "Female - select All ages as Age range"="Female",
                              "Male - select All ages as Age range"="Male",
                              "Male & Female - select All ages as Age range"="MF"),
                          selected = c("All")
                      ),
                      conditionalPanel(
                        condition = "input.sexDTI == 'All'",
                        selectInput("codDTI", "Intent:",
                                    choices = c("All", "Accidental", "Intentional", "Undetermined"),
                                    selected = c("All")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.sexDTI != 'All'",
                        selectInput("codDTIS", "Intent:",
                                    choices = c("All", "Accidental"),
                                    selected = c("All")
                        )
                      )
                  ),
                  conditionalPanel(
                    condition = "input.stateDT != 'Australia'",
                    selectInput("codDTIJ", "Intent:",
                                choices = c("All", "Accidental"),
                                selected = c("All")
                    )
                  ),
                  checkboxGroupInput("drugDTI", "Drug:",
                       choices = c(
                          "OPIOIDS",
                          "heroin",
                          "natural and semi-synthetic opioids",
                          "methadone",
                          "synthetic opioids",
                          "AMPHETAMINES",
                          "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
                          "barbiturates",
                          "benzodiazepines",
                          "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
                          ="antiepileptic and sedative-hypnotic drugs, unspecified",
                          "ANTIDEPRESSANTS",
                          "tricyclic and tetracyclic antidepressants",
                          "other and unspecified antidepressants",
                          "ANTIPSYCHOTICS & NEUROLEPTICS",
                          "other and unspecified antipsychotics (e.g. quetiapine)"
                          ="other and unspecified antipsychotics",
                          "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
                          "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
                          "other nonsteroidal anti-inflammatory drugs",
                          "ALCOHOL",
                          "COCAINE",
                          "CANNABIS DERIVATIVES"
                       ),
                      selected = c("AMPHETAMINES", "COCAINE", "OPIOIDS", "ALCOHOL")
                   )
                ),
               
               conditionalPanel(
                  condition = "input.DropDT == 'Drug'",
                      selectInput("drugDTD", label = NULL,
                         choices = c(
                            "OPIOIDS",
                            "heroin",
                            "natural and semi-synthetic opioids",
                            "methadone",
                            "synthetic opioids",
                            "AMPHETAMINES",
                            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
                            "barbiturates",
                            "benzodiazepines",
                            "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
                            ="antiepileptic and sedative-hypnotic drugs, unspecified",
                            "ANTIDEPRESSANTS",
                            "tricyclic and tetracyclic antidepressants",
                            "other and unspecified antidepressants",
                            "ANTIPSYCHOTICS & NEUROLEPTICS",
                            "other and unspecified antipsychotics (e.g. quetiapine)"
                            ="other and unspecified antipsychotics",
                            "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
                            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
                            "other nonsteroidal anti-inflammatory drugs",
                            "ALCOHOL",
                            "COCAINE",
                            "CANNABIS DERIVATIVES"
                         ),
                         selected = c("ALCOHOL") ),
                 
                      checkboxGroupInput("codDTD", label = "Intent:",
                            c("All", "Accidental", "Intentional", "Undetermined"),
                            selected = c("All", "Accidental", "Intentional", "Undetermined")
                      ),
                      conditionalPanel(
                        condition = "input.stateDT == 'Australia'",
                          checkboxGroupInput("sexDTD", "Sex:",
                             choices = c("All",
                                         "Female - please select All ages as Age range"="Female",
                                         "Male - please select All ages as Age range"="Male"),
                             selected = c("All")
                       )
                    )
                 )
               
               # selectInput("ageDT", "Age range:",
               #     choices = c(
               #       "All ages","15 to 64" = "15-64"
               #     ),
               #     selected = "15-64"
               # ),
               # 
               # checkboxGroupInput("drugDT", "Drug:",
               #    choices = c(
               #      "OPIOIDS",
               #      "heroin",
               #      "natural and semi-synthetic opioids",
               #      "methadone",
               #      "synthetic opioids",
               #      "AMPHETAMINES",
               #      "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
               #      "barbiturates",
               #      "benzodiazepines",
               #      "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
               #      "ANTIDEPRESSANTS",
               #      "tricyclic and tetracyclic antidepressants",
               #      "other and unspecified antidepressants",
               #      "ANTIPSYCHOTICS & NEUROLEPTICS",
               #      "other and unspecified antipsychotics (e.g. quetiapine)",
               #      "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
               #      "4-aminophenol derivatives (e.g. paracetamol)",
               #      "other nonsteroidal anti-inflammatory drugs",
               #      "Alcohol",
               #      "CANNABIS DERIVATIVES",
               #      "Cocaine"
               #    ),
               #    selected = c("AMPHETAMINES", "Cocaine", "OPIOIDS", "Alcohol")
               # )
             )
           ),
           tabPanel("Notes", includeMarkdown("notesAllByDrug.md"))
         )
      ),


  # All drug-induced deaths by drug, age and intent (Table 12 & 12a)------------------------------------------------
      tabPanel("Drug-induced deaths by drug, age and intent",
         value = "PlotDTA",
         "",
         h1("Drug-induced deaths by drug, age and intent"),

         tabsetPanel(
           type = "tabs",
           tabPanel(
             "Plot",
             mainPanel(
               withLoader(plotlyOutput("drugtypePlotA", width = "100%", height = "600px"),
                #   type = "html", loader = "loader4"),
                    type="image", loader="DT_NIDIP_tween.gif"),
               fluidRow(includeMarkdown("notesAllByDrugPlot.md"))
             ),

             sidebarPanel(
               sliderInput("yrDTA", "Period",
                  min = 1997, max = 2018,
                  value = c(2008, 2018), sep = ""
               ),
               selectInput(
                   "yaxDTA", "Plot:",
                   c(
                     "Number of deaths" = "num",
                     "Deaths per 100,000 people" = "rateht",
                     "Deaths per 100,000 people (95% CI)" = "ratehtci",
                     "Deaths per 1,000,000 people" = "ratem",
                     "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                   ),
                   selected = "rateht"
               ),

  #       Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
               radioButtons("DropDTA", "Variable for dropdown list:",
                  choices = c(
                    "Age and Intent"="Age_Intent",
                    "Drug"="Drug"
                  ),inline = T,
                  selected = c("Age_Intent")
               ),
  #######seems to make Console really verbose with the inputs########
  #           uiOutput("DTControl")
  #       From: https://shiny.rstudio.com/reference/shiny/1.0.4/conditionalPanel.html
  #             conditionalPanel( condition = "input.DropDTA == 'Age_Intent'",
  #                 uiOutput("DTAge_Intent")
  #              ),
               # conditionalPanel(
               #   condition = "input.DropDTA == 'Drug'",
               #   uiOutput("DTDrug")
               # )

              conditionalPanel(
                condition = "input.DropDTA == 'Age_Intent'",
                  selectInput("codDTAI", "Intent:",
                      choices = c("All", "Accidental"),
                      selected = c("All")
                  ),

                  selectInput( "ageDTAI", "Age:",
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
                      selected = c("15-64")
                  ),
                  checkboxGroupInput("drugDTAI", "Drug:",
                      choices = c(
                         "OPIOIDS",
                         "heroin",
                         "natural and semi-synthetic opioids",
                         "methadone",
                         "synthetic opioids",
                         "AMPHETAMINES",
                         "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
                         "barbiturates",
                         "benzodiazepines",
                         "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
                         ="antiepileptic and sedative-hypnotic drugs, unspecified",
                         "ANTIDEPRESSANTS",
                         "tricyclic and tetracyclic antidepressants",
                         "other and unspecified antidepressants",
                         "ANTIPSYCHOTICS & NEUROLEPTICS",
                         "other and unspecified antipsychotics (e.g. quetiapine)"="other and unspecified antipsychotics",
                         "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
                         "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
                         "other nonsteroidal anti-inflammatory drugs",
                         "ALCOHOL",
                         "COCAINE",
                         "CANNABIS DERIVATIVES"
                      ),
                     selected = c("AMPHETAMINES", "COCAINE", "OPIOIDS", "ALCOHOL")
                  )
               ),

               conditionalPanel(
                 condition = "input.DropDTA == 'Drug'",
                 selectInput("drugDTAD", label = NULL,
                    choices = c(
                      "OPIOIDS",
                      "heroin",
                      "natural and semi-synthetic opioids",
                      "methadone",
                      "synthetic opioids",
                      "AMPHETAMINES",
                      "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
                      "barbiturates",
                      "benzodiazepines",
                      "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
                        ="antiepileptic and sedative-hypnotic drugs, unspecified",
                      "ANTIDEPRESSANTS",
                      "tricyclic and tetracyclic antidepressants",
                      "other and unspecified antidepressants",
                      "ANTIPSYCHOTICS & NEUROLEPTICS",
                      "other and unspecified antipsychotics (e.g. quetiapine)"="other and unspecified antipsychotics",
                      "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
                      "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
                      "other nonsteroidal anti-inflammatory drugs",
                      "ALCOHOL",
                      "COCAINE",
                      "CANNABIS DERIVATIVES"
                      ),
                     selected = c("ALCOHOL") ),

                 checkboxGroupInput("codDTAD", label = "Intent:",
                     c("All", "Accidental"),
                     selected = c("All") ),
                 checkboxGroupInput("ageDTAD", "Age:",
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
                    selected = "15-64"   )
               )
             )
           ),
           tabPanel("Notes", includeMarkdown("notesAllByDrug.md"))
        )
      )
    ),


    navbarMenu(

  # Opioids menu tab -------------------------------------------------------------
      "Opioids",

      tabPanel(
        value = "PlotO4",
    # Opioids by opioid, age and intent (Table 4) -----------------------------------------
        "By opioid, age and intent",
        h1("Opioid-induced deaths"),
        h3("By opioid, age and intent"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("opioidPlot4", width = "100%", height = "600px"), 
     #           type = "html", loader = "loader4"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesOpioidAPlot.md"))
            ),

            sidebarPanel(
              sliderInput("yrOA", "Period",
                min = 1997,
                max = 2018, value = c(2008, 2018), sep = ""
              ),
              selectInput( "yaxOA", "Plot:",
                  c( "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "rateht",
                  "Deaths per 100,000 people (95% CI)" = "ratehtci",
                  "Deaths per 1,000,000 people" = "ratem",
                  "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                  ),
                  selected = "rateht"
              ),

#Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("DropOA", "Variable for dropdown list:",
                choices = c(
                  "Opioid",
                  "Age"
                ),inline = T,
                selected = c("Opioid")
              ),
#              uiOutput("OAControl")

              conditionalPanel( condition = "input.DropOA == 'Opioid'",
                  selectInput("drugOAO", label = NULL,
                      choices = c(
                        "All opioids",
                        "Heroin",
                        "Opium",
                        "Methadone",
                        "Natural and semi-synthetic opioids",
                        "Synthetic opioids",
                        "Other and unspecified opioids"
                      ),
                      selected = c("All opioids") ),
                  checkboxGroupInput("ageOAO", "Age:",
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
                      selected = c("15-64")
                  )
              ),

              conditionalPanel( condition = "input.DropOA == 'Age'",
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
                      selected = c("15-64") ),
                  checkboxGroupInput("drugOAA", "Opioid:",
                      choices = c(
                          "All opioids",
                          "Heroin",
                          "Opium",
                          "Methadone",
                          "Natural and semi-synthetic opioids",
                          "Synthetic opioids",
                          "Other and unspecified opioids"
                      ),
                      selected = c("All opioids")
                  )
              ),
  
              checkboxGroupInput( "codOA", "Intent:",
                  c("All", "Accidental", "Intentional", "Undetermined"),
                  selected = c("All", "Accidental", "Intentional", "Undetermined")
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioids.md"))
        )
      ),

      tabPanel(
        value = "PlotOB",
      # Opioids by intent, opioid and sex (Table 5) -----------------------------------------
        "By opioid, intent and sex",
        h1("Opioid-induced deaths"),
        h3("By opioid, intent and sex"),

        tabsetPanel(
          type = "tabs",
          tabPanel( "Plot",

            mainPanel(
              withLoader(plotlyOutput("opioidPlotB", width = "100%", height = "600px"),
      #           type = "html", loader = "loader4"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesOpioidBPlot.md"))
            ),

            sidebarPanel(
              sliderInput("yrOB", "Period",
                min = 1997, max = 2018,
                value = c(2008, 2018), sep = ""
              ),
              selectInput(
                "yaxOB", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "rateht",
                  "Deaths per 100,000 people (95% CI)" = "ratehtci",
                  "Deaths per 1,000,000 people" = "ratem",
                  "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                ),
                selected = "rateht"
              ),

              selectInput(
                "ageOB", "Age range:",
                c("All ages","15 to 64" = "15-64"),
                c("15-64")
              ),
        #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("DropOB", "Variable for dropdown list:",
                           choices = c(
                             "Opioid",
                             "Intent",
                             "Sex"
                           ),inline = T,
                           selected = c("Opioid")
              ),
              conditionalPanel( condition = "input.DropOB == 'Opioid'",
                  selectInput( "drugOBO", label = NULL,
                       choices = c(
                           "All opioids",
                           "Heroin",
                           "Opium",
                           "Methadone",
                           "Natural and semi-synthetic opioids",
                           "Synthetic opioids",
                           "Other and unspecified opioids"
                       ),
                       selected = c("All opioids")),
                  checkboxGroupInput( "codOBO", "Intent:",
                      c("All", "Accidental", "Intentional", "Undetermined"),
                      selected = c("All")
                  ),
                  checkboxGroupInput("sexOBO", "Sex:",
                       choices = c("All", "Female", "Male"),
                       selected = c("All", "Female", "Male") )
              ),
              conditionalPanel( condition = "input.DropOB == 'Sex'",
                  selectInput("sexOBS", "Sex:",
                       choices = c("All", "Female", "Male","Male & Female"="MF"),
                       selected = c("All") ),
                  checkboxGroupInput("codOBS", "Intent:",
                       c("All", "Accidental", "Intentional", "Undetermined"),
                       selected = c("All", "Accidental", "Intentional", "Undetermined")
                  ),
                  checkboxGroupInput("drugOBS", "Opioid:",
                      choices = c(
                          "All opioids",
                          "Heroin",
                          "Opium",
                          "Methadone",
                          "Natural and semi-synthetic opioids",
                          "Synthetic opioids",
                          "Other and unspecified opioids"
                         ),
                         selected = c("All opioids")
                  )
              ),
              conditionalPanel(
                  condition = "input.DropOB == 'Intent'",
                      selectInput("codOBI", label = NULL,
                           c("All", "Accidental", "Intentional", "Undetermined"),
                                 selected = c("All") ),
                  checkboxGroupInput("sexOBI", "Sex:",
                                     choices = c("All", "Female", "Male"),
                                     selected = c("All", "Female", "Male") ),
                  checkboxGroupInput("drugOBI", "Opioid:",
                       choices = c(
                           "All opioids",
                           "Heroin",
                           "Opium",
                           "Methadone",
                           "Natural and semi-synthetic opioids",
                           "Synthetic opioids",
                           "Other and unspecified opioids"
                       ),
                       selected = c("All opioids")
                  )
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioids.md"))
        )
      ),

      tabPanel(
        value = "PlotOD",
        # Opioids by intent, jurisdiction and sex (Table 6) ---------------------------------
        "By intent, sex and jurisdiction",
        h1("Opioid-induced deaths"),
        h3("By intent, jurisdiction and sex"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("opioidPlotD", width = "100%", height = "600px"),
              #           type = "html", loader = "loader4"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesOpioidDPlot.md"))
            ),
    
            sidebarPanel(
              sliderInput("yrOD", "Period",
                min = 1997,
                max = 2018, value = c(2008, 2018), sep = ""
              ),
              selectInput(
                "yaxOD", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "rateht",
                  "Deaths per 100,000 people (95% CI)" = "ratehtci",
                  "Deaths per 1,000,000 people" = "ratem",
                  "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                ),
                selected = "rateht"
              ),
    
              selectInput(
                "ageOD", "Age range:",
                c("All ages","15 to 64" = "15-64"),
                selected = "15-64"
              ),
    
              selectInput(
                "stateOD", "Jurisdiction:",
                c(
                  "Australia",
                  "New South Wales",
                  "Victoria",
                  "Queensland",
                  "South Australia",
                  "Western Australia",
                  "Tasmania",
                  "Northern Territory",
                  "Australian Capital Territory"
                )
              ),

              checkboxGroupInput(
                "codOD", "Intent:",
                c("All", "Accidental"),
                selected = "All"
              ),
    
              checkboxGroupInput("sexOD", "Sex:",
                choices = c("Male", "Female", "All"),
                selected = c("Male", "Female", "All")
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioids.md"))
        )
      ),
  
      tabPanel(
        value = "Plot10",
        # Exclusive opioids by age and intent (Table 10)-----------------------------------------
        "Exclusive opioids by opioid type, age and intent",
        h1("Opioid-induced deaths"),
        h3("Exclusive opioids by opioid type, age and intent"),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("opioidPlot10", width = "100%", height = "600px"), 
                         #           type = "html", loader = "loader4"),
                         #create animated gif logo in Photoshop
                         type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesOpioidsExclusivePlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("yr10", "Period",
                  min = 2007,max = 2018, 
                  value = c(2008, 2018), sep = ""
              ),
              selectInput( "yax10", "Plot:",
                   c( "Number of deaths" = "num",
                      "Deaths per 100,000 people" = "rateht",
                      "Deaths per 100,000 people (95% CI)" = "ratehtci",
                      "Deaths per 1,000,000 people" = "ratem",
                      "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                   ),
                   selected = "rateht"
              ),
              
              radioButtons("Drop10", "Variable for dropdown list:",
                   choices = c(
                     "Opioid",
                     "Age"
                   ),inline = T,
                   selected = c("Opioid")
              ),
              
              conditionalPanel(
                condition = "input.Drop10 == 'Opioid'",
                  selectInput("drug10O", label = NULL,
                      choices = c(
                        "Exclusive illicit opioids",
                        "Exclusive pharmaceutical opioids",
                        "Illicit and pharmaceutical opioids",
                        "Other and unspecified opioids"
                      ),
                      selected = c(
                        "Exclusive illicit opioids" )
                  ),
                  checkboxGroupInput("age10O", "Age:",
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
                      selected = c("15-64")
                  )
              ),
              
              conditionalPanel(
                condition = "input.Drop10 == 'Age'",
                  selectInput("age10A", label = NULL,
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
                  selected = c("15-64") ),

                  checkboxGroupInput("drug10A", "Opioid:",
                      choices = c(
                        "Exclusive illicit opioids",
                        "Exclusive pharmaceutical opioids",
                        "Illicit and pharmaceutical opioids",
                        "Other and unspecified opioids"
                      ),
                      selected = c(
                        "Exclusive illicit opioids",
                        "Exclusive pharmaceutical opioids"
                      )
                  )
              ),
              
              checkboxGroupInput( "cod10", "Intent:",
                  c("All", "Accidental", "Intentional"),
                  selected = c("All", "Accidental", "Intentional")
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioidsExclusive.md"))
        )
      ),
      
      tabPanel(
        value = "PlotOG",
        # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11)-----------------------------------------
        "Exclusive opioids by opioid type, jurisdiction, intent and sex",
        h1("Opioid-induced deaths"),
        h3("Exclusive opioids by opioid type, jurisdiction, intent and sex"),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("PlotOG", width = "100%", height = "600px"),
                         #           type = "html", loader = "loader4"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesOpioidsExclusivePlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("yrOG", "Period",
                  min = 2007, max = 2018,
                  value = c(2008, 2018), sep = ""
              ),
              selectInput(
                "yaxOG", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "rateht",
                  "Deaths per 100,000 people (95% CI)" = "ratehtci",
                  "Deaths per 1,000,000 people" = "ratem",
                  "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                ),
                selected = "rateht"
              ),
              
              selectInput(
                "stateOG", "Jurisdiction:",
                c(
                  "Australia",
                  "New South Wales",
                  "Victoria",
                  "Queensland",
                  "South Australia",
                  "Western Australia",
                  "Tasmania",
                  "Northern Territory",
                  "Australian Capital Territory"
                )
              ),
              
              selectInput(
                "ageOG", "Age range:",
                c("All ages","15 to 64"="15-64"
                ),
                selected = "15-64"
              ),
              
              #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("Drop9", "Variable for dropdown list:",
                   choices = c(
                     "Intent",
                     "Sex"
                   ), inline = T,
                   selected = c("Intent")
              ),
              
              #             uiOutput("Control9"),
              #From: https://shiny.rstudio.com/reference/shiny/1.0.4/conditionalPanel.html
              conditionalPanel(
                condition = "input.Drop9 == 'Intent'",
                selectInput("cod9I", label = NULL,
                    choices = c("All", "Accidental", "Intentional")
                ),
                checkboxGroupInput("sex9I", label = "Sex:",
                     choices = c("All", "Female", "Male"),
                     selected = c("All")
                )
              ),
              
              conditionalPanel(
                condition = "input.Drop9 == 'Sex'",
                selectInput("sex9S", label = NULL,
                    choices = c("All", "Female", "Male","Male & Female"="MF"),
                    selected = c("All") ) ,
                checkboxGroupInput(
                  "cod9S", label = "Intent:",
                  c("All", "Accidental", "Intentional"),
                  selected = c("All") )
              ),
              
              checkboxGroupInput("drugOG", "Drug:",
                   choices = c(
                     "Exclusive illicit opioids",
                     "Exclusive pharmaceutical opioids",
                     "Illicit and pharmaceutical opioids",
                     "Other and unspecified opioids"
                   ),
                   selected = c(
                     "Exclusive illicit opioids",
                     "Exclusive pharmaceutical opioids"
                   )
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioidsExclusive.md"))
        )
      ),
      
      tabPanel(
        value = "PlotOH",
        # Exclusive opioids percents (Tables 10 & 11) -----------------------------------------
        "Exclusive opioids as percentages",
        h1("Opioid-induced deaths"),
        h3("Exclusive opioids as percentages of all opioid-induced deaths"),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("PlotOH", width = "100%", height = "600px"),
      #           type = "html", loader = "loader4")
                   type="image", loader="DT_NIDIP_tween.gif")
            ),
            
            sidebarPanel(
              sliderInput("yrOH", "Period",
                    min = 2007, max = 2018,
                    value = c(2008, 2018), sep = ""
              ),
              
              selectInput(
                "stateOH", "Jurisdiction:",
                c(
                  "Australia",
                  "New South Wales",
                  "Victoria",
                  "Queensland",
                  "South Australia",
                  "Western Australia",
                  "Tasmania",
                  "Northern Territory",
                  "Australian Capital Territory"
                )
              ),
              
              radioButtons(
                "codOH", "Intent:",
                c("All", "Accidental"),
                selected = "All"
              ),
              
              radioButtons(
                "ageOH", "Age range:",
                c("All ages","15 to 64"="15-64"
                ),
                selected = "15-64"
              ),
              
              radioButtons("sexOH", "Sex:",
                   choices = c(
                     "Male",
                     "Female",
                     "All"
                   ),
                   selected = c("All")
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioidsExclusive.md"))
        )
      ),
  
    tabPanel(
      value = "PlotOE",
      # Opioids with other drugs by age and intent (Table 7)-----------------------------------------
      "Opioids with other drugs, by age and intent",
      h1("Opioid-induced deaths"),
      h3("Opioids with other drugs"),
  
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(width = 9,
            withLoader(plotlyOutput("PlotOE", width = "100%", height = "600px"),
            #           type = "html", loader = "loader4"),
                type="image", loader="DT_NIDIP_tween.gif"),
            fluidRow(includeMarkdown("notesOpioidsOtherDrugsPlot.md"))
          ),
          sidebarPanel(width = 3,
            sliderInput( "yrOE", "Period",
              min = 1997,
              max = 2018, value = c(2008, 2018), sep = ""
            ),
            selectInput( "yaxOE", "Plot:",
              c(
                "Number of deaths" = "num",
                "Deaths per 100,000 people" = "rateht",
                "Deaths per 100,000 people (95% CI)" = "ratehtci",
                "Deaths per 1,000,000 people" = "ratem",
                "Deaths per 1,000,000 people (95% CI)" = "ratemci"
              ),
              selected = "rateht"
            ),
  
            #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
            radioButtons("DropOE", "Variable for dropdown list:",
                 choices = c(
                   "Drug with opioid"="Drug",
                   "Age"
                 ), inline = T,
                 selected = c("Drug")
            ),
            conditionalPanel( condition = "input.DropOE == 'Drug'",
                selectInput( "drugOED", label = NULL,
                   choices = c(
                     "Alcohol",# = "All opioids with alcohol",
                     "Amphetamines",# = "All opioids with amphetamines",
                     "Antidepressants",# = "All opioids with antidepressants",
                     "Antipsychotics",# = "All opioids with antipsychotics",
                     "Benzodiazepines",# = "All opioids with benzodiazepines",
                     "Paracetamol",# = "All opioids with paracetamol",
                     "Pregabalin" # = "All opioids with pregabalin"
                   ),
                   selected = c("Alcohol")
                ),
                checkboxGroupInput("ageOED", "Age:",
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
                    selected = c("15-64")
                )
            ),
                
            conditionalPanel( condition = "input.DropOE == 'Age'",
                selectInput("ageOEA", "Age:",
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
                    selected = c("15-64")
                ),
                checkboxGroupInput("drugOEA", "All opioids with:",
                    choices = c(
                      "Alcohol",# = "All opioids with alcohol",
                      "Amphetamines",# = "All opioids with amphetamines",
                      "Antidepressants",# = "All opioids with antidepressants",
                      "Antipsychotics",# = "All opioids with antipsychotics",
                      "Benzodiazepines",# = "All opioids with benzodiazepines",
                      "Paracetamol",# = "All opioids with paracetamol",
                      "Pregabalin"# = "All opioids with pregabalin"
                    ),
                    selected = c("Alcohol")
                )
            ),
            checkboxGroupInput("codOE", "Intent:",
                c("All", "Accidental", "Intentional", "Undetermined"),
                selected = "All"
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesOpioidsOtherDrugs.md"))
      )
    ),
  
    tabPanel(
      value = "PlotOF",
  # Opioids with other drugs by sex (Table 8) -----------------------------------------
      "Opioids with other drugs, by sex and intent",
      h1("Opioid-induced deaths"),
      h3("Opioids with other drugs"),
  
  #    tabPanel(
  #      "Opioids and other drugs by sex",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width = 9,
              withLoader(plotlyOutput("PlotOF", width = "100%", height = "600px"),
              #   type = "html", loader = "loader4"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeMarkdown("notesOpioidsOtherDrugsPlot.md"))
            ),
            sidebarPanel(width = 3,
              sliderInput("yrOF", "Period",
                min = 1997,
                max = 2018, value = c(2008, 2018), sep = ""
              ),
              selectInput(
                "yaxOF", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "rateht",
                  "Deaths per 100,000 people (95% CI)" = "ratehtci",
                  "Deaths per 1,000,000 people" = "ratem",
                  "Deaths per 1,000,000 people (95% CI)" = "ratemci"
                ),
                selected = "rateht"
              ),
              selectInput( "ageO8", "Age range:",
                  c("All ages","15 to 64"="15-64"
                  ),
                  selected = "15-64"
              ),
  
              radioButtons("DropO8", "Variable for dropdown list:",
                           choices = c(
                             "Sex",
                             "Intent"
                           ),inline = T,
                           selected = c("Sex")
              ),
              conditionalPanel( condition = "input.DropO8 == 'Sex'",
                  selectInput("sexO8S", label = NULL,
                      choices = c("All", "Female", "Male","Male & Female"="MF"),
                      selected = c("All")
                  ),
                  checkboxGroupInput("codO8S", "Intent:",
                       c("All", "Accidental", "Intentional", "Undetermined"),
                       selected = c("All", "Accidental", "Intentional", "Undetermined")
                  )
              ),
              conditionalPanel(
                condition = "input.DropO8 == 'Intent'",
                selectInput("codO8I", label = NULL,
                      c("All", "Accidental", "Intentional", "Undetermined"),
                      selected = c("All") ),
                checkboxGroupInput("sexO8I", "Sex:",
                     choices = c("All", "Female", "Male"),
                     selected = c("All", "Female", "Male")
                )
              ),
  
              checkboxGroupInput("drugOF", "All opioids with:",
                choices = c(
                  "Alcohol",# = "All opioids with alcohol",
                  "Amphetamines",# = "All opioids with amphetamines",
                  "Antidepressants",# = "All opioids with antidepressants",
                  "Antipsychotics",# = "All opioids with antipsychotics",
                  "Benzodiazepines",# = "All opioids with benzodiazepines",
                  "Paracetamol",# = "All opioids with paracetamol",
                  "Pregabalin"# = "All opioids with pregabalin"
                ),
                selected = c("Alcohol")
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioidsOtherDrugs.md"))
        )
  #    )
        )
    ),


# Stimulants menu tab ---------------------------------------------------------------
    navbarMenu(
    "Stimulants",
  
  # Amphetamines tab (Table 2) --------------------------------------------------------
      tabPanel(
        value = "PlotA",
        "Amphetamines",
        h1("Amphetamine-induced deaths"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("amphetaminePlot", width = "100%", height = "600px"),
    #           type = "html", loader = "loader4"),
                type="image", loader="NIDIP_MovingS.gif"),
              fluidRow(includeMarkdown("notesAmphetaminesPlot.md"))
            ),

            sidebarPanel(
            sliderInput("yrA", "Period",
              min = 1997, max = 2018,
              value = c(2008, 2018), sep = ""
            ),
            selectInput(
              "yaxA", "Plot:",
              c(
                "Number of deaths" = "num",
                "Deaths per 100,000 people" = "rateht",
                "Deaths per 100,000 people (95% CI)" = "ratehtci",
                "Deaths per 1,000,000 people" = "ratem",
                "Deaths per 1,000,000 people (95% CI)" = "ratemci"
              ),
              selected = "rateht"
            ),

            checkboxGroupInput("codA", "Intent:",
              c("All", "Accidental"),
              selected = "All"
            ),

            checkboxGroupInput("ageA", "Age:",
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
              selected = c("15-64")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesAmphetamines.md"))
      )
    ),

    # Cocaine tab (Table 3) -------------------------------------------------------------
    tabPanel(
      value = "PlotC",
      "Cocaine",
      h1("Cocaine-induced deaths"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(
            withLoader(plotlyOutput("cocainePlot", width = "100%", height = "600px"),
            #          type = "html", loader = "loader4"),
            #create animated gif logo in Photoshop
                type="image", loader="DT_NIDIP_onion30.gif"),
            fluidRow(includeMarkdown("notesCocainePlot.md"))
          ),

          sidebarPanel(
            sliderInput("yrC", "Period",
              min = 1997,
              max = 2018, value = c(2008, 2018), sep = ""
            ),
            selectInput(
              "yaxC", "Plot:",
              c(
                "Number of deaths" = "num",
                "Deaths per 100,000 people" = "rateht",
                "Deaths per 100,000 people (95% CI)" = "ratehtci",
                "Deaths per 1,000,000 people" = "ratem",
                "Deaths per 1,000,000 people (95% CI)" = "ratemci"
              ),
              selected = "rateht"
            ),

            checkboxGroupInput(
              "codC", "Intent:",
              c(
                "All" = "All",
                "Accidental" = "Accidental"
              ),
              selected = "All"
            ),

            checkboxGroupInput("ageC", "Age range:",
              choices = c(
                "All ages","15 to 64" = "15-64"
              ),
                selected = c("15-64")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesCocaine.md"))
      )
    )
  ),

    # Notes tab ---------------------------------------------------------------
    tabPanel(
      "Explanatory notes", includeMarkdown("notesOverall.md")
    ),


    # Citation tab ------------------------------------------------------------
    tabPanel(
      "Citation and acknowledgements",
      fluidRow(
        column(width = 8, includeMarkdown("notesCitation.md"))
       , column(width = 4, includeHTML("DTLogo.html"))
      )
    )
  ),

#seem to be ignored
#  renderCSS(type="image", loader="DT_NIDIP_onion30.gif"),
  tags$style(type = 'text/css', '.navbar { background-color: #2E823F;}'
      , '.navbar-default .dropdown-menu {background-color: #2E823F;}' #this one worked
    #  , '.navbar-default .dropdown-toggle {background-color: #475c07;}' # works but only where it is a dropdown
      , '.irs-from, .irs-to, .irs-single {background: #338822;}' #color for year sliders
      , '.irs-bar { border-top: 1px solid #338822;
        border-bottom: 1px solid #338822;
        background: #338822; }'
    #  , '.navbar-default .navbar-nav>.active>a:focus {background-color: #6a7d14;}'
    #  , '.navbar-default .navbar-brand {color: black;}'
    #  , '.navbar-default .navbar-nav>.open>a, 
    #  .navbar-default .navbar-nav>.open>a:hover, 
    #  .navbar-default .navbar-nav>.open>a:focus {background-color: transparent;}'
    #  , 'td, th { padding: 10; }' #doesn't work
  )
)
    
#DRUG TRENDS COLOUR:
#DT: #475c07
#NIDIP: #6e2a8d
#EDRS: #de761c
#IDRS: #00aeef
#DNet: #c4161c
}
