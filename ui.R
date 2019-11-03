#For ABS COD 2018 data received in Sept 2019
#N. Man
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(shinycustomloader)

ui <- function(req) {
  
bootstrapPage('',

  navbarPage(
    header = singleton(tags$head(includeScript("google_analytics.js"))),
  # fluidRow(
  #   column(width = 12, "Deaths induced by:"))
  #   , column(width = 4, img(src="DrugTrends-Logo-stacked.png", style="height: 25px")),

    theme = shinytheme("yeti"),
#   title=div(img(src="DrugTrends-Logo-stacked.png", style="height: 25px"), "Deaths induced by:"),
  title= "Deaths induced by:",
  id = "Plot",
    # All drugs tab ---------------------------------------------------------------
    navbarMenu(
      "All drugs",
      
    # All drugs by age and sex (Not needed - basic plot of the next)------------------------------------------------
#       tabPanel("Drug Induced Deaths by age and sex",
#                value = "PlotAll",
#                h1("Drug Induced Deaths by age and sex"),
# 
#                tabsetPanel(
#                  type = "tabs",
#                  tabPanel(
#                    "Plot",
#                    mainPanel(
#                      withLoader(plotlyOutput("allPlot", width = "100%", height = "600px"), type = "html", loader = "loader4"),
#                      fluidRow(includeMarkdown("notesAllDrugsPlotPage.md"))
#                    ),
#                    
#                    sidebarPanel(
#                      sliderInput("yearsAll", "Period",
#                                  min = 1997,
#                                  max = 2018, value = c(2007, 2018), sep = ""
#                      ),
#                      selectInput(
#                        "plotAll", "Plot:",
#                        c(
#                          "Number of deaths" = "deaths",
#                          "Deaths per 100,000 people" = "deathrateht",
#                          "Deaths per 100,000 people (95% CI)" = "deathratehtci",
#                          "Deaths per 1,000,000 people" = "deathratem",
#                          "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
#                        ),
#                        selected = "deathrateht"
#                      ),
#                      
#                      checkboxGroupInput(
#                        "sexAll", "Sex:",
#                        c("All", "Female", "Male"),
#                        selected = c("Male", "Female", "All")
#                      ),
#                      
#                      checkboxGroupInput("ageAll", "Age group:",
#                                         choices = c(
#                                           "15 to 24" = "15-24",
#                                           "25 to 34" = "25-34",
#                                           "35 to 44" = "35-44",
#                                           "45 to 54" = "45-54",
#                                           "55 to 64" = "55-64",
#                                           "65 to 74" = "65-74",
#                                           "75 to 84" = "75-84",
# #                                          "15 to 54" = "15-54",
#                                           "15 to 64" = "15-64",
#                                           "All ages" = "All ages"
#                                         ),
#                                         selected = c("15-64")
#                      )
#                   # downloadButton("AllDrugHtml", "Generate report") #WIP: html download - only static if html only
#                    )),
#                  
#                  tabPanel("Notes", includeMarkdown("notesAllDrugs.md")))
#       ),
#       
      # All drugs by jurisdiction, intent, age and sex ------------------------------------------------
      tabPanel("Drug Induced Deaths by jurisdiction, intent, age and sex",
           value = "PlotAllB",
           h1("Drug Induced Deaths by jurisdiction, intent, age and sex"),
           
           tabsetPanel(
               type = "tabs",
               tabPanel(
                 "Plot",
                 mainPanel(
                   withLoader(plotlyOutput("allPlotB", width = "100%", height = "600px"), type = "html", loader = "loader4"),
                   fluidRow(includeMarkdown("notesAllDrugsIntentPlot.md"))
                 ),
                 
                 sidebarPanel(
                     sliderInput("yearsAllB", "Period",
                                 min = 1997,
                                 max = 2018, value = c(2007, 2018), sep = ""
                     ),
                     selectInput(
                       "plotAllB", "Plot:",
                       c(
                         "Number of deaths" = "deaths",
                         "Deaths per 100,000 people" = "deathrateht",
                         "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                         "Deaths per 1,000,000 people" = "deathratem",
                         "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                       ),
                       selected = "deathrateht"
                     ),
                     
                     selectInput(
                       "stateAllB", "Jurisdiction:",
                       c(
                         "Australia"="AUS",
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
                     
                     #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
                     radioButtons("DropAllB", "Variable for dropdown list:",
                                  choices = c(
                                    "Intent",
                                    "Sex"
                                  ),inline = T,
                                  selected = c("Intent")
                     ),

#                     uiOutput("AllBControl"),
    #From: https://shiny.rstudio.com/reference/shiny/1.0.4/conditionalPanel.html can't get dropdown to update dataset
             conditionalPanel(
                condition = "input.DropAllB == 'Intent'",
                  selectInput("codAllI", label = NULL,
                          choices = c("All", "Accidental", "Intentional", "Undetermined","Other")
                  ),
                  checkboxGroupInput("sexAllBI", label = "Sex:",
                          choices = c("All", "Female", "Male"),
                          selected = c("All")
                  )
             ),

             conditionalPanel(
               condition = "input.DropAllB == 'Sex'",
               selectInput("sexAllBS", label = NULL,
                                choices = c("All", "Female", "Male"),
                                selected = c("All") ) ,
               checkboxGroupInput(
                 "codAllS", label = "Intent:",
                 c("All", "Accidental", "Intentional", "Undetermined","Other"),
                 selected = c("All") )
              ),


         # selectInput(
         #   "codAllB", "Intent:",
         #   c("All", "Accidental", "Intentional", "Undetermined")
         # ),
         # 
         # checkboxGroupInput("sexAllB", "Sex:",
         #                    choices = c("Male", "Female", "All"),
         #                    selected = c("Male", "Female", "All")
         # ),
                     
                     checkboxGroupInput(
                       "ageAllB", "Age:",
                       c(
                         "15 to 24" = "15-24",
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
                   )
                 ),
                 tabPanel("Notes", includeMarkdown("notesAllDrugsByIntent.md"))
               )),
      
      # All drugs by drug type  ------------------------------------------------
      tabPanel("Drug Induced Deaths by drug type, age and intent",
               value = "PlotDT",
               "",
               h1("Drug induced deaths by drug type, age and intent"),
               
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Plot",
                   mainPanel(
                     withLoader(plotlyOutput("drugtypePlot", width = "100%", height = "600px"), type = "html", loader = "loader4"),
                     fluidRow(includeMarkdown("notesAllDrugsByDrug.md"))
                   ),
                   
                   sidebarPanel(
                     sliderInput("yearsDT", "Period",
                                 min = 1997,
                                 max = 2018, value = c(2007, 2018), sep = ""
                     ),
                     selectInput(
                       "plotDT", "Plot:",
                       c(
                         "Number of deaths" = "deaths",
                         "Deaths per 100,000 people" = "deathrateht",
                         "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                         "Deaths per 1,000,000 people" = "deathratem",
                         "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                       ),
                       selected = "deathrateht"
                     ),
                     
                     #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
                     radioButtons("DropDT", "Variable for dropdown list:",
                                  choices = c(
                                    "Age and Intent"="Age_Intent",
                                    "Drug type"="Drug"
                                  ),inline = T,
                                  selected = c("Age_Intent")
                     ),
                     uiOutput("DTControl")
                     # conditionalPanel(
                     #    condition = "input.DropDT == 'Age_Intent'",
                     #    uiOutput("DTAge_Intent")
                     # ),
                     # conditionalPanel(
                     #   condition = "input.DropDT == 'Drug'",
                     #   uiOutput("DTDrug")
                     # )
                     
#                     From: https://shiny.rstudio.com/reference/shiny/1.0.4/conditionalPanel.html can't get dropdown to update dataset
                     # conditionalPanel(
                     #    condition = "input.DropDT == 'Age_Intent'",
                     #      selectInput("codDT", "Intent:",
                     #              choices = c("All", "Accidental")
                     #      ),
                     #      selectInput("AgeDT", label = "Age:",
                     #              choices = c(
                     #                "15 to 24" = "15-24",
                     #                "25 to 34" = "25-34",
                     #                "35 to 44" = "35-44",
                     #                "45 to 54" = "45-54",
                     #                "55 to 64" = "55-64",
                     #                "65 to 74" = "65-74",
                     #                "75 to 84" = "75-84",
                     #                "All ages",
                     #                "15 to 64" = "15-64"),
                     #              selected = "15-64"
                     #      ),
                     #      checkboxGroupInput("drugDT", "Drug:",
                     #             choices = c(
                     #                 "OPIOIDS",
                     #                 "heroin",
                     #                 "natural and semi-synthetic opioids",
                     #                 "methadone",
                     #                 "synthetic opioids",
                     #                 "AMPHETAMINES",
                     #                 "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
                     #                 "barbiturates",
                     #                 "benzodiazepines",
                     #                 "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
                     #                 "ANTIDEPRESSANTS",
                     #                 "tricyclic and tetracyclic antidepressants",
                     #                 "other and unspecified antidepressants",
                     #                 "ANTIPSYCHOTICS & NEUROLEPTICS",
                     #                 "other and unspecified antipsychotics (e.g. quetiapine)",
                     #                 "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
                     #                 "4-aminophenol derivatives (e.g. paracetamol)",
                     #                 "other nonsteroidal anti-inflammatory drugs",
                     #                 "Alcohol",
                     #                 "Cocaine",
                     #                 "CANNABIS DERIVATIVES"
                     #             ),
                     #             selected = c("AMPHETAMINES", "Cocaine", "OPIOIDS", "Alcohol")
                     #    )
                     # ),
                     # 
                     # conditionalPanel(
                     #   condition = "input.DropDT == 'Drug'",
                     #   selectInput("drugDT", label = NULL,
                     #      choices = c(
                     #        "OPIOIDS",
                     #        "heroin",
                     #        "natural and semi-synthetic opioids",
                     #        "methadone",
                     #        "synthetic opioids",
                     #        "AMPHETAMINES",
                     #        "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
                     #        "barbiturates",
                     #        "benzodiazepines",
                     #        "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
                     #        "ANTIDEPRESSANTS",
                     #        "tricyclic and tetracyclic antidepressants",
                     #        "other and unspecified antidepressants",
                     #        "ANTIPSYCHOTICS & NEUROLEPTICS",
                     #        "other and unspecified antipsychotics (e.g. quetiapine)",
                     #        "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
                     #        "4-aminophenol derivatives (e.g. paracetamol)",
                     #        "other nonsteroidal anti-inflammatory drugs",
                     #        "Alcohol",
                     #        "Cocaine",
                     #        "CANNABIS DERIVATIVES"
                     #        ),
                     #       selected = c("Alcohol") ),
                     # 
                     #   checkboxGroupInput(
                     #     "codDT", label = "Intent:",
                     #     c("All", "Accidental"),
                     #     selected = c("All") ),
                     #   checkboxGroupInput("ageDT", "Age group:",
                     #        choices = c(
                     #          "15 to 24" = "15-24",
                     #          "25 to 34" = "25-34",
                     #          "35 to 44" = "35-44",
                     #          "45 to 54" = "45-54",
                     #          "55 to 64" = "55-64",
                     #          "65 to 74" = "65-74",
                     #          "75 to 84" = "75-84",
                     #          "All ages",
                     #          "15 to 64" = "15-64"
                     #        ),
                     #        selected = "15-64"   )
                     # )

                     # selectInput("ageDT", "Age group:",
                     #             choices = c(
                     #               "15 to 24" = "15-24",
                     #               "25 to 34" = "25-34",
                     #               "35 to 44" = "35-44",
                     #               "45 to 54" = "45-54",
                     #               "55 to 64" = "55-64",
                     #               "65 to 74" = "65-74",
                     #               "75 to 84" = "75-84",
                     #               "All ages",
                     #               "15 to 64" = "15-64"
                     #             ),
                     #             selected = "15-64"
                     # ),
                     # 
                     # checkboxGroupInput("drugDT", "Drug:",
                     #                    choices = c(
                     #                      "OPIOIDS",
                     #                      "heroin",
                     #                      "natural and semi-synthetic opioids",
                     #                      "methadone",
                     #                      "synthetic opioids",
                     #                      "AMPHETAMINES",
                     #                      "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
                     #                      "barbiturates",
                     #                      "benzodiazepines",
                     #                      "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
                     #                      "ANTIDEPRESSANTS",
                     #                      "tricyclic and tetracyclic antidepressants",
                     #                      "other and unspecified antidepressants",
                     #                      "ANTIPSYCHOTICS & NEUROLEPTICS",
                     #                      "other and unspecified antipsychotics (e.g. quetiapine)",
                     #                      "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
                     #                      "4-aminophenol derivatives (e.g. paracetamol)",
                     #                      "other nonsteroidal anti-inflammatory drugs",
                     #                      "Alcohol",
                     #                      "CANNABIS DERIVATIVES",
                     #                      "Cocaine"
                     #                    ),
                     #                    selected = c("AMPHETAMINES", "Cocaine", "OPIOIDS", "Alcohol")
                     # )
                   )
                 ),
                 tabPanel("Notes", includeMarkdown("notesAllDrugsByDrugType.md")))
      )),
    
    
    navbarMenu(

      # Opioids tab -------------------------------------------------------------
      "Opioids",

      tabPanel(
        value = "PlotOA",
        # Opioids by opioid, age and intent -----------------------------------------
        "By opioid, age and intent",
        h1("Opioid induced deaths"),
        h3("By opioid, age and intent"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("opioidPlotA", width = "100%", height = "600px"), type = "html", loader = "loader4"),
              fluidRow(includeMarkdown("notesOpioidAPlot.md"))
            ),

            sidebarPanel(
              sliderInput("yearsOA", "Period",
                min = 1997,
                max = 2018, value = c(2007, 2018), sep = ""
              ),
              selectInput(
                "plotOA", "Plot:",
                c(
                  "Number of deaths" = "deaths",
                  "Deaths per 100,000 people" = "deathrateht",
                  "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                  "Deaths per 1,000,000 people" = "deathratem",
                  "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                ),
                selected = "deathrateht"
              ),

#Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("DropOA", "Variable for dropdown list:",
                choices = c(
                  "Opioid",
                  "Age"
                ),inline = T,
                selected = c("Opioid")
              ),
              uiOutput("OAControl")

#OLD CODE BELOW
#               selectInput("drugOA", "Opioid:",
#                 choices = c(
#                   "All opioids",
#                   "Heroin",
#                   "Methadone",
#                   "Opium",
#                   "Natural and semi-synthetic opioids",
#                   "Synthetic opioids",
#                   "Other and unspecified opioids"
#                 ),
#                 selected = c("All opioids")
#               ),
# 
#               checkboxGroupInput("ageOA", "Age group:",
#                 choices = c(
#                   "15 to 24" = "15-24",
#                   "25 to 34" = "25-34",
#                   "35 to 44" = "35-44",
#                   "45 to 54" = "45-54",
#                   "55 to 64" = "55-64",
#                   "65 to 74" = "65-74",
#                   "75 to 84" = "75-84",
# #                  "15 to 54" = "15-54",
#                   "15 to 64" = "15-64",
#                   "All ages" = "All ages"
#                 ),
#                 selected = c("15-64")
#               ),
# 
#               checkboxGroupInput(
#                 "codOA", "Intent:",
#                 c("All", "Accidental", "Intentional", "Undetermined"),
#                 selected = c("All", "Accidental", "Intentional", "Undetermined")
#               )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioids.md"))
        )
      ),
      tabPanel(
        value = "PlotOB",
        # Opioids by intent, opioid and sex -----------------------------------------
        "By opioid, intent and sex",
        h1("Opioid induced deaths"),
        h3("By opioid, intent and sex"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",

            mainPanel(
              withLoader(plotlyOutput("opioidPlotB", width = "100%", height = "600px"), type = "html", loader = "loader4"),
              fluidRow(includeMarkdown("notesOpioidBPlot.md"))
            ),


            sidebarPanel(
              sliderInput("yearsOB", "Period",
                min = 1997,
                max = 2018, value = c(2007, 2018), sep = ""
              ),
              selectInput(
                "plotOB", "Plot:",
                c(
                  "Number of deaths" = "deaths",
                  "Deaths per 100,000 people" = "deathrateht",
                  "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                  "Deaths per 1,000,000 people" = "deathratem",
                  "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                ),
                selected = "deathrateht"
              ),

              selectInput(
                "ageOB", "Age group:",
                c("All ages", "15 to 64" = "15-64")
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
              uiOutput("OBControl")

              # selectInput(
              #   "ageOB", "Age range:",
              #   c("All ages", "15 to 64" = "15-64")
              # ),
              # 
              # selectInput(
              #   "codOB", "Intent:",
              #   c("All", "Accidental", "Intentional", "Undetermined")
              # ),
              # 
              # checkboxGroupInput("drugOB", "Opioid:",
              #   choices = c(
              #     "All opioids",
              #     "Heroin",
              #     "Methadone",
              #     "Opium",
              #     "Natural and semi-synthetic opioids",
              #     "Synthetic opioids",
              #     "Other and unspecified opioids"
              #   ),
              #   selected = c("All opioids")
              # ),
              # 
              # checkboxGroupInput("sexOB", "Sex:",
              #   choices = c("Male", "Female", "All"),
              #   selected = c("Male", "Female", "All")
              # )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioids.md"))
        )
      ),

      # tabPanel(
      #   value = "PlotOC",
      #   # Opioids by sex, intent and opioid -----------------------------------------
      #   "By sex, intent and opioid",
      #   h1("Opioid induced deaths"),
      #   h3("By sex, intent and opioid"),
      # 
      #   tabsetPanel(
      #     type = "tabs",
      #     tabPanel(
      #       "Plot",
      # 
      #       mainPanel(
      #         withLoader(plotlyOutput("opioidPlotC", width = "100%", height = "600px"), type = "html", loader = "loader4"),
      #         fluidRow(includeMarkdown("notesOpioidsPlot.md"))
      #       ),
      # 
      #       sidebarPanel(
      #         sliderInput("yearsOC", "Period",
      #           min = 1997,
      #           max = 2018, value = c(2007, 2018), sep = ""
      #         ),
      #         selectInput(
      #           "plotOC", "Plot:",
      #           c(
      #             "Number of deaths" = "deaths",
      #             "Deaths per 100,000 people" = "deathrateht",
      #             "Deaths per 100,000 people (95% CI)" = "deathratehtci",
      #             "Deaths per 1,000,000 people" = "deathratem",
      #             "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
      #           ),
      #           selected = "deathrateht"
      #         ),
      # 
      #         selectInput(
      #           "ageOC", "Age range:",
      #           c("All ages", "15 to 64" = "15-64")
      #         ),
      # 
      #         selectInput("sexOC", "Sex:",
      #           choices = c("Male", "Female", "All"),
      #           selected = c("All")
      #         ),
      # 
      # 
      #         checkboxGroupInput("drugOC", "Opioid:",
      #           choices = c(
      #             "All opioids",
      #             "Heroin",
      #             "Methadone",
      #             "Opium",
      #             "Natural and semi-synthetic opioids",
      #             "Synthetic opioids",
      #             "Other and unspecified opioids"
      #           ),
      #           selected = c("All opioids")
      #         ),
      # 
      #         checkboxGroupInput(
      #           "codOC", "Intent:",
      #           c("All", "Accidental", "Intentional", "Undetermined"),
      #           selected = c("All")
      #         )
      #       )
      #     ),
      #     tabPanel("Notes", includeMarkdown("notesOpioids.md"))
      #   )
      # ),


      tabPanel(
        value = "PlotOD",

        # Opioids by intent, jurisdiction and sex ---------------------------------
        "By intent, sex and jurisdiction",

        h1("Opioid induced deaths"),
        h3("By intent, jurisdiction and sex"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("opioidPlotD", width = "100%", height = "600px"), type = "html", loader = "loader4"),
              fluidRow(includeMarkdown("notesOpioidDPlot.md"))
            ),

            sidebarPanel(
              sliderInput("yearsOD", "Period",
                min = 1997,
                max = 2018, value = c(2007, 2018), sep = ""
              ),
              selectInput(
                "plotOD", "Plot:",
                c(
                  "Number of deaths" = "deaths",
                  "Deaths per 100,000 people" = "deathrateht",
                  "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                  "Deaths per 1,000,000 people" = "deathratem",
                  "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                ),
                selected = "deathrateht"
              ),

              selectInput(
                "ageOD", "Age group:",
                c("All ages", "15 to 64" = "15-64")
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
        value = "PlotOE",
        # Opioids with other drugs by age and sex-----------------------------------------
        "Opioids with other drugs, by age group and sex",
        h1("Opioid induced deaths"),
        h3("Opioids with other drugs"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("PlotOE", width = "100%", height = "600px"), type = "html", loader = "loader4"),
              fluidRow(includeMarkdown("notesOpioidsOtherDrugsPlot.md"))
            ),
            sidebarPanel(
              sliderInput("yearsOE", "Period",
                min = 1997,
                max = 2018, value = c(2007, 2018), sep = ""
              ),
              selectInput(
                "plotOE", "Plot:",
                c(
                  "Number of deaths" = "deaths",
                  "Deaths per 100,000 people" = "deathrateht",
                  "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                  "Deaths per 1,000,000 people" = "deathratem",
                  "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                ),
                selected = "deathrateht"
              ),

              selectInput(
                "intentOE", "Intent:",
                c("All", "Accidental", "Intentional", "Undetermined"),
                selected = "All"
              ),


              selectInput("sexOE", "Sex:",
                choices = c(
                  "Male",
                  "Female",
                  "All"
                ),
                selected = c("All")
              ),


              checkboxGroupInput("ageOE", "Age group:",
                choices = c(
                  "All ages" = "All ages",
                  "15 to 64" = "15-64"
                ),
                selected = c("15-64")
              ),

              checkboxGroupInput("drugOE", "All opioids with:",
                choices = c(
                  "Alcohol" = "All opioids with alcohol",
                  "Amphetamines" = "All opioids with amphetamines",
                  "Antidepressants" = "All opioids with antidepressants",
                  "Antipsychotics" = "All opioids with antipsychotics",
                  "Benzodiazepines" = "All opioids with benzodiazepines",
                  "Paracetamol" = "All opioids with paracetamol",
                  "Pregabalin" = "All opioids with pregabalin"
                ),
                selected = c("All opioids with alcohol")
              )
            )
          ),
          tabPanel("Notes", includeMarkdown("notesOpioidsOtherDrugs.md"))
        )
      ),

      tabPanel(
        value = "PlotOF",
        # Opioids with other drugs by sex -----------------------------------------
        "Opioids with other drugs, by sex",
        h1("Opioid induced deaths"),
        h3("Opioids with other drugs"),

        tabPanel(
          "Opioids and other drugs by sex",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              mainPanel(
                withLoader(plotlyOutput("PlotOF", width = "100%", height = "600px"), type = "html", loader = "loader4"),
                fluidRow(includeMarkdown("notesOpioidsOtherDrugsPlot.md"))
              ),
              sidebarPanel(
                sliderInput("yearsOF", "Period",
                  min = 1997,
                  max = 2018, value = c(2007, 2018), sep = ""
                ),
                selectInput(
                  "plotOF", "Plot:",
                  c(
                    "Number of deaths" = "deaths",
                    "Deaths per 100,000 people" = "deathrateht",
                    "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                    "Deaths per 1,000,000 people" = "deathratem",
                    "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                  ),
                  selected = "deathrateht"
                ),

                selectInput(
                  "intentOF", "Intent:",
                  c("All", "Accidental", "Intentional", "Undetermined"),
                  selected = "All"
                ),

                selectInput(
                  "ageOF", "Age group:",
                  c("15-64",
                    "All ages" = "All ages"
                  ),
                  selected = "15-64"
                ),

                checkboxGroupInput("sexOF", "Sex:",
                  choices = c(
                    "Male",
                    "Female",
                    "All"
                  ),
                  selected = c("All")
                ),
                checkboxGroupInput("drugOF", "All opioids with:",
                  choices = c(
                    "Alcohol" = "All opioids with alcohol",
                    "Amphetamines" = "All opioids with amphetamines",
                    "Antidepressants" = "All opioids with antidepressants",
                    "Antipsychotics" = "All opioids with antipsychotics",
                    "Benzodiazepines" = "All opioids with benzodiazepines",
                    "Paracetamol" = "All opioids with paracetamol",
                    "Pregabalin" = "All opioids with pregabalin"),
                  selected = c("All opioids with alcohol")
                )
              )
            ),
            tabPanel("Notes", includeMarkdown("notesOpioidsOtherDrugs.md"))
          )
        )
      ),

      tabPanel(
        value = "PlotOG",
        # Exclusive opioids -----------------------------------------
        "Exclusive opioids",
        h1("Opioid induced deaths"),
        h3("Exclusive opioids"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("PlotOG", width = "100%", height = "600px"), type = "html", loader = "loader4"),
              fluidRow(includeMarkdown("notesOpioidsExclusivePlot.md"))
            ),

            sidebarPanel(
              sliderInput("yearsOG", "Period",
                min = 2007,
                max = 2018, value = c(2007, 2018), sep = ""
              ),
              selectInput(
                "plotOG", "Plot:",
                c(
                  "Number of deaths" = "deaths",
                  "Deaths per 100,000 people" = "deathrateht",
                  "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                  "Deaths per 1,000,000 people" = "deathratem",
                  "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                ),
                selected = "deathrateht"
              ),

              selectInput(
                "intentOG", "Intent:",
                c("All", "Accidental", "Intentional", "Undetermined"),
                selected = "All"
              ),

              selectInput(
                "ageOG", "Age group:",
                c("15 to 64"="15-64",
                  "All ages"
                ),
                selected = "15-64"
              ),

              checkboxGroupInput("sexOG", "Sex:",
                choices = c(
                  "Male",
                  "Female",
                  "All"
                ),
                selected = c("All")
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
        # Exclusive opioids percents -----------------------------------------
        "Exclusive opioids as percentages",
        h1("Opioid induced deaths"),
        h3("Exclusive opioids as percentages of all opioid induced deaths"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("PlotOH", width = "100%", height = "600px"), type = "html", loader = "loader4")
            ),

            sidebarPanel(
              sliderInput("yearsOH", "Period",
                min = 2007,
                max = 2018, value = c(2007, 2018), sep = ""
              ),

              radioButtons(
                "intentOH", "Intent:",
                c("All", "Accidental"),
                selected = "All"
              ),

              radioButtons(
                "ageOH", "Age group:",
                c("15 to 64"="15-64",
                  "All ages"
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
      )
    ),


    # Amphetamines tab --------------------------------------------------------
    tabPanel(
      value = "PlotA",
      "Amphetamines",
      h1("Amphetamine induced deaths"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(
            withLoader(plotlyOutput("amphetaminePlot", width = "100%", height = "600px"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notesAmphetaminesPlot.md"))
          ),

          sidebarPanel(
            sliderInput("yearsA", "Period",
              min = 1997,
              max = 2018, value = c(2007, 2018), sep = ""
            ),
            selectInput(
              "plotA", "Plot:",
              c(
                "Number of deaths" = "deaths",
                "Deaths per 100,000 people" = "deathrateht",
                "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                "Deaths per 1,000,000 people" = "deathratem",
                "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
              ),
              selected = "deathrateht"
            ),

            radioButtons(
              "codA", "Intent:",
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
#                "15 to 54" = "15-54",
                "All ages" = "All ages",
                "15 to 64" = "15-64"
              ),
              selected = c("15-64")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesAmphetamines.md"))
      )
    ),

    # Cocaine tab -------------------------------------------------------------
    tabPanel(
      value = "PlotC",
      "Cocaine",
      h1("Cocaine induced deaths"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(
            withLoader(plotlyOutput("cocainePlot", width = "100%", height = "600px"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notesCocainePlot.md"))
          ),

          sidebarPanel(
            sliderInput("yearsC", "Period",
              min = 1997,
              max = 2018, value = c(2007, 2018), sep = ""
            ),
            selectInput(
              "plotC", "Plot:",
              c(
                "Number of deaths" = "deaths",
                "Deaths per 100,000 people" = "deathrateht",
                "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                "Deaths per 1,000,000 people" = "deathratem",
                "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
              ),
              selected = "deathrateht"
            ),

            radioButtons(
              "codC", "Intent:",
              c(
                "All" = "All",
                "Accidental" = "Accidental"
              ),
              selected = "All"
            ),

            radioButtons("ageC", "Age group:",
              choices = c(
                "All ages" = "All ages",
#                "15 to 54" = "15-54",
                "15 to 64" = "15-64"
              ),
                selected = c("15-64")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesCocaine.md"))
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
       , column(width = 4, includeHTML("DNetLogo.html"))
      )
    )
  ) ,
  tags$style(type = 'text/css', '.navbar { background-color: #6d2888;}'
            , '.navbar-default .dropdown-menu {background-color: #6d2888;}' #this one worked
            # , '.navbar-default .dropdown-toggle {background-color: #465d02;}' # works but only where it is a dropdown
            , '.irs-from, .irs-to, .irs-single {background: #465d02;}' #color for year sliders
            , '.irs-bar { border-top: 1px solid #465d02;
              border-bottom: 1px solid #465d02;
              background: #465d02; }'
            , '.navbar-default .navbar-nav>.active>a:focus {background-color: 465d02;}'
        #     , '.navbar-default .navbar-brand {color: black;}'
        #     , '.navbar-default .navbar-nav>.open>a, 
        #     .navbar-default .navbar-nav>.open>a:hover, 
        #     .navbar-default .navbar-nav>.open>a:focus {background-color: transparent;}'
  ))
    
}
