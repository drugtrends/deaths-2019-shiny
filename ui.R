#For ABS COD 2018 data received in Sept 2019
#N. Man
library(shiny)
#library(shinyTree)
#library(shinythemes)
#library(ggplot2)
#library(ggthemes)
library(plotly)
library(shinycustomloader)

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

    navbarPage(
# fluidRow(
#   column(width = 12, "Deaths induced by:"))
#   , column(width = 4, img(src="DrugTrends-Logo.png", style="height: 25px")),
#    shinythemes::themeSelector(),
#  title=div(img(src="DrugTrends-Logo.png", style="height: 25px"), "Deaths induced by:"),
    title= "Deaths induced by:",
    id = "Plot",

#might be helpful for reusing repetitive inputs like year???
#https://rstudio.github.io/shinydashboard/structure.html
  # All drugs menu tab ---------------------------------------------------------------
    navbarMenu("All drugs",
      
    # All drugs by jurisdiction, intent, age and sex (Tables 1a, 1b & 1c) ---------------------------
      tabPanel("Drug-induced deaths by jurisdiction, intent, age and sex",
        value = "allPage",
        h1("Drug-induced deaths by jurisdiction, intent, age and sex"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            mainPanel(width=9,
              withLoader(plotlyOutput("allPlot", width = "100%", height = "600px"),
#                 type = "html", loader = "loader4"),
#create animated gif logo in Photoshop; put DT_NIDIP_tween.gif in www directory
#                 type="image", loader="NIDIP_MovingS.gif"),
#                 type="image", loader="DT_NIDIP_onion30.gif"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteAll.html")) #Markdown("notesAllDrugsPlot.md"))
            ),

            sidebarPanel(width=3,
              sliderInput( "Allyr", "Period",
                  min = 1997, max = 2018,
                  value = c(1997, 2018), sep = ""
              ),
              selectInput( "Allyax", "Plot:",
                  c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                  ),
                  selected = "r5"
              ),

              selectInput(
                  "Alljur", "Jurisdiction:",
                  c("Australia",
                    "New South Wales",
                    "Victoria",
                    "Queensland",
                    "South Australia",
                    "Western Australia",
                    "Tasmania",
                    "Northern Territory",
                    "Australian Capital Territory" )
              ),

              radioButtons("AllDrop", "Variable for dropdown list:",
                  choices = c(
                    "Intent", "Sex"
                  ), inline = T,
                  selected = c("Intent")
              ),
#Alternative to conditionalPanel: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
#             uiOutput("AllBControl"),
#From: https://shiny.rstudio.com/reference/shiny/1.0.4/conditionalPanel.html
              conditionalPanel(
                  condition = "input.AllDrop == 'Intent'",
                      selectInput("AllIcod", label = NULL,
                      choices = c("All", "Accidental", "Intentional", "Undetermined","Other")
                  ),
                  checkboxGroupInput("AllIsex", label = "Sex:",
                      choices = c("All", "Female", "Male"),
                      selected = c("All", "Female", "Male")
                  )
              ),
      
              conditionalPanel(
                  condition = "input.AllDrop == 'Sex'",
                  selectInput("AllSsex", label = NULL,
                      choices = c("All", "Female", "Male","Male & Female"="MF")
                  ),
                  checkboxGroupInput(
                      "AllScod", label = "Intent:",
                      c("All", "Accidental", "Intentional", "Undetermined","Other"),
                      selected = c("All", "Accidental")
                  )
              ),

              checkboxGroupInput(
                  "Allage", "Age:",
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
              )
    #         downloadButton("AllDrugHtml", "Generate report") #WIP: html download - only static if html only
            )
          ),
          tabPanel("Notes", includeHTML("notesAll.html")) #Markdown("notesAllDrugs.md"))
#includeMarkdown("notesAllDrugs.md"), tableOutput('AllOD') )
        )
      ),

      tabPanel(value = "PlotR",
  # Remoteness by jurisdiction and intent (Tables R1, R4 & R5) -----------------------------------------
        "Drug-induced deaths by jurisdiction, remoteness area and intent",
        h1("Drug-induced deaths by jurisdiction, remoteness area and intent"),

        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            mainPanel(
              withLoader(plotlyOutput("remotePlot", width = "100%", height = "600px"),
            #           type = "html", loader = "loader4"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteRem.html")) #Markdown("notesRemotePlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("Ryr", "Period",
                          min = 2011, max = 2018,
                          value = c(2011, 2018), sep = ""
              ),
              selectInput("Ryax", "Plot:",
                  c(
                    "Number of deaths" = "num",
                    "Deaths per 100,000 people" = "r5",
                    "Deaths per 100,000 people (95% CI)" = "r5ci",
                    "Deaths per 1,000,000 people" = "r6",
                    "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                  ),
                  selected = "r5"
              ),

              selectInput("Rjur", "Jurisdiction:",
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
                condition = "input.Rjur == 'Australia'",
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
                condition = "input.Rjur != 'Australia'",
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
        
              checkboxGroupInput("Rcod", label = "Intent:",
                  c("All", "Accidental"),
                  selected = c("All")
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesRem.html")) #Markdown("notesRemote.md"))
          #includeMarkdown("notesRemote.md"), tableOutput("RemOD"))
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
                         type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteRem.html")) #Markdown("notesRemotePlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("RPyr", "Period",
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
                  "RPjur", "Jurisdiction:",
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
                  "RPcod", "Intent:",
                  c("All", "Accidental"),
                  selected = "All"
              ),

              conditionalPanel(
                condition = "input.RPjur == 'Australia'",
                radioButtons("RPsex", "Sex:",
                    choices = c(
                      "Male",
                      "Female",
                      "All"
                    ),
                    selected = c("All")
                )
              ),
              
              # conditionalPanel(
              #   condition = "input.RPjur != 'Australia' || input.RPsex != 'All' || input.RPyax != 'deaths'",
                radioButtons("RPage", "Age range:",
                    c("All ages","15 to 64"="15-64"),
                    selected = "All ages"
                )
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
            )
          ),
          tabPanel("Notes", includeHTML("notesRem.html")) #Markdown("notesRemote.md"))
          #includeMarkdown("notesRemote.md"), tableOutput("Rem_OD"))
        )
      ),

# All drug-induced deaths by drug, jurisdiction, intent and/or sex (Table 12, 12b & 12c)------------------------------------------------
      tabPanel("Drug-induced deaths by drug, jurisdiction, intent and/or sex",
        value = "DTPage",
        h1("Drug-induced deaths by drug, jurisdiction, intent and/or sex"),
      
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width = 9,
              withLoader(plotlyOutput("DTPlot", width = "100%", height = "600px"),
                        #   type = "html", loader = "loader4"),
                        type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteDTJ.html")) #Markdown("notesAllByDrugJPlot.md"))
            ),
            
            sidebarPanel(width = 3,
              sliderInput("DTyr", "Period",
                min = 1997, max = 2018,
                value = c(1997, 2018), sep = ""
              ),
              selectInput(
                "DTyax", "Plot:",
                 c(
                   "Number of deaths" = "num",
                   "Deaths per 100,000 people" = "r5",
                   "Deaths per 100,000 people (95% CI)" = "r5ci",
                   "Deaths per 1,000,000 people" = "r6",
                   "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                 ),
                 selected = "r5"
              ),
              
              selectInput(
                 "DTjur", "Jurisdiction:",
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
                condition = "input.DTjur == 'Australia'",
                   selectInput( "DTage", "Age range:",
                    choices = c(
                      "All ages","15 to 64" = "15-64"
                    ),
                    selected = c("All ages")
                   )
              ),
  #       Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("DTDrop", "Variable for dropdown list:",
                  choices = c(
                      "Sex (only Aus) & Intent"="IntSx",
                      "Drug"="Drug"
                  ), #inline = T,
                  selected = c("IntSx")
              ),

              conditionalPanel(
                  condition = "input.DTDrop == 'IntSx'",
                  conditionalPanel(
                    condition = "input.DTjur == 'Australia'",
                      selectInput("DTIsex", "Sex:",
                          choices = c("All",
                              "Female - select All ages as Age range"="Female",
                              "Male - select All ages as Age range"="Male",
                              "Male & Female - select All ages as Age range"="MF"),
                          selected = c("All")
                      ),
                      conditionalPanel(
                        condition = "input.DTIsex == 'All'",
                        selectInput("DTIcod", "Intent:",
                                    choices = c("All", "Accidental", "Intentional", "Undetermined"),
                                    selected = c("All")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.DTIsex != 'All'",
                        selectInput("DTIScod", "Intent:",
                                    choices = c("All", "Accidental"),
                                    selected = c("All")
                        )
                      )
                  ),
                  conditionalPanel(
                    condition = "input.DTjur != 'Australia'",
                    selectInput("DTIJcod", "Intent:",
                                choices = c("All", "Accidental"),
                                selected = c("All")
                    )
                  ),
                  checkboxGroupInput("DTIdrug", "Drug:",
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
                   )
                ),
               
               conditionalPanel(
                  condition = "input.DTDrop == 'Drug'",
                      selectInput("DTDdrug", label = NULL,
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
                         selected = c("OPIOIDS") ),
                 
                      checkboxGroupInput("DTDcod", label = "Intent:",
                            c("All", "Accidental", "Intentional", "Undetermined"),
                            selected = c("All", "Accidental", "Intentional", "Undetermined")
                      ),
                      conditionalPanel(
                        condition = "input.DTjur == 'Australia'",
                          checkboxGroupInput("DTDsex", "Sex:",
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
               #     selected = c("All ages")
               # ),
               # 
               # checkboxGroupInput("drugDT", "Drug:",
               #    choices = c(
               #      "OPIOIDS",
               #      "heroin",
               #      "natural & semi-synthetic opioids",
               #      "methadone",
               #      "synthetic opioids",
               #      "AMPHETAMINES",
               #      "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
               #      "barbiturates",
               #      "benzodiazepines",
               #      "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
               #      "ANTIDEPRESSANTS",
               #      "tricyclic & tetracyclic antidepressants",
               #      "other & unspecified antidepressants",
               #      "ANTIPSYCHOTICS & NEUROLEPTICS",
               #      "other & unspecified antipsychotics (e.g. quetiapine)",
               #      "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
               #      "4-aminophenol derivatives (e.g. paracetamol)",
               #      "other nonsteroidal anti-inflammatory drugs",
               #      "Alcohol",
               #      "CANNABINOIDS",
               #      "Cocaine"
               #    ),
               #    selected = c("AMPHETAMINES", "Cocaine", "OPIOIDS", "Alcohol")
               # )
             )
           ),
           tabPanel("Notes", includeHTML("notesDT.html")) #Markdown("notesAllByDrug.md"))
  #includeMarkdown("notesDT.md"), tableOutput('DT_OD'))
         )
      ),


  # All drug-induced deaths by drug, age and intent (Table 12 & 12a)------------------------------------------------
      tabPanel("Drug-induced deaths by drug, age and intent",
        value = "DTAPage",
        h1("Drug-induced deaths by drug, age and intent"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width = 9,
              withLoader(plotlyOutput("DTAPlot", width = "100%", height = "600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteDTA.html")) #Markdown("notesAllByDrugPlot.md"))
            ),

            sidebarPanel(width = 3,
              sliderInput("DTAyr", "Period",
                  min = 1997, max = 2018,
                  value = c(1997, 2018), sep = ""
              ),
              selectInput(
                  "DTAyax", "Plot:",
                  c(
                    "Number of deaths" = "num",
                    "Deaths per 100,000 people" = "r5",
                    "Deaths per 100,000 people (95% CI)" = "r5ci",
                    "Deaths per 1,000,000 people" = "r6",
                    "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                  ),
                  selected = "r5"
              ),

  #       Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("DTADrop", "Variable for dropdown list:",
                  choices = c(
                    "Age & Intent"="Age_Intent",
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

              conditionalPanel(condition = "input.DTADrop == 'Age_Intent'",
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
                ),
                checkboxGroupInput("DTAIdrug", "Drug:",
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
                    "other & unspecified antipsychotics (e.g. quetiapine)"="other & unspecified antipsychotics",
                    "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
                    "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
                    "other nonsteroidal anti-inflammatory drugs",
                    "ALCOHOL",
                    "COCAINE",
                    "CANNABINOIDS"
                  ),
                  selected = c("OPIOIDS", "ALCOHOL", "AMPHETAMINES", "ANTIDEPRESSANTS", 
                    "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
                    "ANTIPSYCHOTICS & NEUROLEPTICS", "CANNABINOIDS", 
                    "COCAINE", "NONOPIOID ANALGESICS")
                )
              ),

              conditionalPanel(condition = "input.DTADrop == 'Drug'",
                selectInput("DTADdrug", label = NULL,
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
                    "other & unspecified antipsychotics (e.g. quetiapine)"="other & unspecified antipsychotics",
                    "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
                    "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
                    "other nonsteroidal anti-inflammatory drugs",
                    "ALCOHOL",
                    "COCAINE",
                    "CANNABINOIDS"
                    ),
                  selected = c("ALCOHOL")
                ),

                checkboxGroupInput("DTADcod", label = "Intent:",
                  c("All", "Accidental"),
                  selected = c("All")
                ),
                checkboxGroupInput("DTADage", "Age:",
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
            )
          ),
          tabPanel("Notes", includeHTML("notesDT.html")) #Markdown("notesAllByDrug.md"))
  #includeMarkdown("notesDT.md"), tableOutput('DT_OD'))
        )
      )
    ),


    navbarMenu(
  # Opioids menu tab -------------------------------------------------------------
      "Opioids",

      tabPanel(
        value = "O4Page",
    # Opioids by opioid, age and intent (Table 4) -----------------------------------------
        "By opioid, age and intent",
        h1("Opioid-induced deaths"),
        h3("By opioid, age and intent"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width=9,
              withLoader(plotlyOutput("O4Plot", width = "100%", height = "600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteOp4.html")) #Markdown("notesOpioidAPlot.md"))
            ),

            sidebarPanel(width=3,
              sliderInput("O4yr", "Period",
                min = 1997, max = 2018,
                value = c(1997, 2018), sep = ""
              ),
          ####For user-defined year intervals
            # radioButtons("xaxO4", "Interval between years:",
            #   choices = c(1, 2, 5), inline=T, selected = 2
            # ),
###HTML info on width of plot for WIP: <rect class="nsewdrag drag" width="854" height="474" ;"></rect>
              selectInput( "O4yax", "Plot:",
                  c( "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                  ),
                  selected = "r5"
              ),

#Below based on link: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("O4Drop", "Variable for dropdown list:",
                choices = c(
                  "Opioid", "Age"
                ),inline = T,
                selected = c("Opioid")
              ),

              conditionalPanel( condition = "input.O4Drop == 'Opioid'",
                selectInput("O4Odrug", label = NULL,
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
                ),
                checkboxGroupInput("O4Oage", "Age:",
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
              ),

              conditionalPanel( condition = "input.O4Drop == 'Age'",
                selectInput("O4Aage", label = NULL,
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
                ),
                checkboxGroupInput("O4Adrug", "Opioid:",
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
  
              checkboxGroupInput( "O4cod", "Intent:",
                c("All", "Accidental", "Intentional", "Undetermined"),
                selected = c("All", "Accidental", "Intentional", "Undetermined")
              ),
              conditionalPanel( condition = "input.O4cod.length == 2",
                radioButtons("O4cod2",label = "Show intent as:",
                  choices = c("Single plot"=1,"Side-by-side plot"=2),
                  inline = T, selected = 1
                )
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesOp.html")) #Markdown("notesOpioids.md"))
        )
      ),

      tabPanel(
        value = "O5Page",
      # Opioids by intent, opioid and sex (Table 5) -----------------------------------------
        "By opioid, intent and sex",
        h1("Opioid-induced deaths"),
        h3("By opioid, intent and sex"),

        tabsetPanel(
          type = "tabs",
          tabPanel( "Plot",

            mainPanel(width=9,
              withLoader(plotlyOutput("O5Plot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteOp5.html")) #Markdown("notesOpioidBPlot.md"))
            ),

            sidebarPanel(width=3,
              sliderInput("O5yr", "Period",
                min = 1997, max = 2018,
                value = c(1997, 2018), sep = ""
              ),
              selectInput( "O5yax", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                ),
                selected = "r5"
              ),

              selectInput(
                "O5age", "Age range:",
                c("All ages","15 to 64" = "15-64"),
                selected = c("All ages")
              ),
        #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons("O5Drop", "Variable for dropdown list:",
                choices = c(
                  "Opioid",
                  "Intent",
                  "Sex"
                ),inline = T,
                selected = c("Opioid")
              ),
              conditionalPanel( condition = "input.O5Drop == 'Opioid'",
                selectInput( "O5Odrug", label = NULL,
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
                ),
                checkboxGroupInput( "O5Ocod", "Intent:",
                  c("All", "Accidental", "Intentional", "Undetermined"),
                  selected = c("All")
                ),
                checkboxGroupInput("O5Osex", "Sex:",
                  choices = c("All", "Female", "Male"),
                  selected = c("All", "Female", "Male")
                )
              ),
              conditionalPanel( condition = "input.O5Drop == 'Sex'",
                selectInput("O5Ssex", "Sex:",
                  choices = c("All", "Female", "Male","Male & Female"="MF"),
                  selected = c("All")
                ),
                checkboxGroupInput("O5Scod", "Intent:",
                  c("All", "Accidental", "Intentional", "Undetermined"),
                  selected = c("All", "Accidental", "Intentional", "Undetermined")
                ),
                checkboxGroupInput("O5Sdrug", "Opioid:",
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
              conditionalPanel( condition = "input.O5Drop == 'Intent'",
                selectInput("O5Icod", label = NULL,
                  c("All", "Accidental", "Intentional", "Undetermined"),
                  selected = c("All")
                ),
                checkboxGroupInput("O5Isex", "Sex:",
                  choices = c("All", "Female", "Male"),
                  selected = c("All", "Female", "Male") ),
                checkboxGroupInput("O5Idrug", "Opioid:",
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
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesOp.html")) #Markdown("notesOpioids.md"))
        )
      ),

      tabPanel(
        value = "O6Page",
  # Opioids by intent, jurisdiction and sex (Table 6) ---------------------------------
        "By intent, sex and jurisdiction",
        h1("Opioid-induced deaths"),
        h3("By intent, jurisdiction and sex"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width=9,
              withLoader(plotlyOutput("O6Plot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteOp6.html")) #Markdown("notesOpioidDPlot.md"))
            ),

            sidebarPanel(width=3,
              sliderInput("O6yr", "Period",
                min = 1997, max = 2018,
                value = c(1997, 2018), sep = ""
              ),
              selectInput("O6yax", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                ),
                selected = "r5"
              ),
    
              selectInput(
                "O6age", "Age range:",
                c("All ages","15 to 64" = "15-64"),
                selected = c("All ages")
              ),
    
              selectInput(
                "O6jur", "Jurisdiction:",
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
                "O6cod", "Intent:",
                c("All", "Accidental"),
                selected = "All"
              ),
    
              checkboxGroupInput("O6sex", "Sex:",
                choices = c("Male", "Female", "All"),
                selected = c("Male", "Female", "All")
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesOp.html")) #Markdown("notesOpioids.md"))
        )
      ),
  
      tabPanel(
        value = "E0Page",
      # Exclusive opioids by age and intent (Table 10)-----------------------------------------
        "Exclusive opioids by opioid type, age and intent",
        h1("Opioid-induced deaths"),
        h3("Exclusive opioids by opioid type, age and intent"),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("E0Plot", width = "100%", height = "600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteEOp.html")) #Markdown("notesOpioidsExclusivePlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("E0yr", "Period",
                  min = 2007,max = 2018, 
                  value = c(1997, 2018), sep = ""
              ),
              selectInput( "E0yax", "Plot:",
                   c( "Number of deaths" = "num",
                      "Deaths per 100,000 people" = "r5",
                      "Deaths per 100,000 people (95% CI)" = "r5ci",
                      "Deaths per 1,000,000 people" = "r6",
                      "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                   ),
                   selected = "r5"
              ),
              
              radioButtons( "E0Drop", "Variable for dropdown list:",
                   choices = c(
                     "Opioid",
                     "Age"
                   ),inline = T,
                   selected = c("Opioid")
              ),
              
              conditionalPanel( condition = "input.E0Drop == 'Opioid'",
                selectInput("E0Odrug", label = NULL,
                  choices = c(
                    "Exclusive illicit opioids",
                    "Exclusive pharmaceutical opioids",
                    "Illicit & pharmaceutical opioids",
                    "Other & unspecified opioids"
                  ),
                  selected = c(
                    "Exclusive illicit opioids" )
                ),
                checkboxGroupInput("E0Oage", "Age:",
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
              ),
              
              conditionalPanel( condition = "input.E0Drop == 'Age'",
                selectInput("E0Aage", label = NULL,
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
                  selected = c("All ages") ),

                checkboxGroupInput("E0Adrug", "Opioid:",
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
              
              checkboxGroupInput( "E0cod", "Intent:",
                c("All", "Accidental", "Intentional"),
                selected = c("All", "Accidental", "Intentional")
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesEOp.html")) #Markdown("notesOpioidsExclusive.md"))
        )
      ),
      
      tabPanel(
        value = "E9Page",
        # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11)-----------------------------------------
        "Exclusive opioids by opioid type, jurisdiction, intent and sex",
        h1("Opioid-induced deaths"),
        h3("Exclusive opioids by opioid type, jurisdiction, intent and sex"),
        
        tabsetPanel(
          type = "tabs",
          tabPanel( "Plot",
            mainPanel(
              withLoader(plotlyOutput("E9Plot", width = "100%", height = "600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteEOp.html")) #Markdown("notesOpioidsExclusivePlot.md"))
            ),
            
            sidebarPanel(
              sliderInput("E9yr", "Period",
                  min = 2007, max = 2018,
                  value = c(1997, 2018), sep = ""
              ),
              selectInput( "E9yax", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                ),
                selected = "r5"
              ),
              selectInput( "E9jur", "Jurisdiction:",
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
              
              selectInput( "E9age", "Age range:",
                c("All ages","15 to 64"="15-64"
                ),
                selected = c("All ages")
              ),
              
              #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
              radioButtons( "E9Drop", "Variable for dropdown list:",
                choices = c(
                  "Intent", "Sex"
                ), inline = T,
                selected = c("Intent")
              ),

            #https://shiny.rstudio.com/gallery/dynamic-ui.html
            # uiOutput("Control9"),
#From: https://shiny.rstudio.com/reference/shiny/1.0.4/conditionalPanel.html
              conditionalPanel(
                condition = "input.E9Drop == 'Intent'",
                selectInput("E9Icod", label = NULL,
                  choices = c("All", "Accidental", "Intentional")
                ),
                checkboxGroupInput("E9Isex", label = "Sex:",
                  choices = c("All", "Female", "Male"),
                  selected = c("All")
                )
              ),
              
              conditionalPanel(
                condition = "input.E9Drop == 'Sex'",
                selectInput("E9Ssex", label = NULL,
                  choices = c("All", "Female", "Male","Male & Female"="MF"),
                  selected = c("All")
                ) ,
                checkboxGroupInput(
                  "E9Scod", label = "Intent:",
                  c("All", "Accidental", "Intentional"),
                  selected = c("All")
                )
              ),
              
              checkboxGroupInput("E9drug", "Drug:",
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
            )
          ),
          tabPanel("Notes", includeHTML("notesEOp.html")) #Markdown("notesOpioidsExclusive.md"))
        )
      ),
      
      tabPanel(
        value = "EPPage",
        # Exclusive opioids percents (Tables 10 & 11) -----------------------------------------
        "Exclusive opioids as percentages",
        h1("Opioid-induced deaths"),
        h3("Exclusive opioids as percentages of all opioid-induced deaths"),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(
              withLoader(plotlyOutput("EPPlot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif")
            ),
            
            sidebarPanel(
              sliderInput("EPyr", "Period",
                  min = 2007, max = 2018,
                  value = c(1997, 2018), sep = ""
              ),
              
              selectInput( "EPjur", "Jurisdiction:",
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
              
              radioButtons( "EPcod", "Intent:",
                c("All", "Accidental"),
                selected = "All"
              ),

              radioButtons( "EPage", "Age range:",
                c("All ages","15 to 64"="15-64"
                ),
                selected = c("All ages")
              ),
              
              radioButtons("EPsex", "Sex:",
                choices = c(
                  "Male",
                  "Female",
                  "All"
                ),
                selected = c("All")
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesEOp.html")) #Markdown("notesOpioidsExclusive.md"))
        )
      ),
  
    tabPanel(
      value = "W7Page",
      # Opioids with other drugs by age and intent (Table 7)-----------------------------------------
      "Opioids with other drugs, by age and intent",
      h1("Opioid-induced deaths"),
      h3("Other drugs with opioids by age and intent"),
  
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(width = 9,
            withLoader(plotlyOutput("W7Plot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
            fluidRow(includeHTML("fnoteWOp.html")) #Markdown("notesOpioidsOtherDrugsPlot.md"))
          ),
          sidebarPanel(width = 3,
            sliderInput( "W7yr", "Period",
              min = 1997, max = 2018,
              value = c(1997, 2018), sep = ""
            ),
            selectInput( "W7yax", "Plot:",
              c("Number of deaths" = "num",
                "Deaths per 100,000 people" = "r5",
                "Deaths per 100,000 people (95% CI)" = "r5ci",
                "Deaths per 1,000,000 people" = "r6",
                "Deaths per 1,000,000 people (95% CI)" = "r6ci"
              ),
              selected = "r5"
            ),
  
            #Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
            radioButtons("W7Drop", "Variable for dropdown list:",
              choices = c(
                "Drug with opioid"="Drug",
                "Age"
              ), inline = T,
              selected = c("Drug")
            ),
            conditionalPanel( condition = "input.W7Drop == 'Drug'",
              selectInput( "W7Ddrug", label = NULL,
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
              ),
              checkboxGroupInput("W7Dage", "Age:",
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
            ),
                
            conditionalPanel( condition = "input.W7Drop == 'Age'",
              selectInput("W7Aage", "Age:",
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
              ),
              checkboxGroupInput("W7Adrug", "All opioids with:",
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
              ),
              checkboxGroupInput("W7show", "Also show:",
                choices = c("all drug-induced deaths")
              )
            ),

            checkboxGroupInput("W7cod", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined"),
              selected = "All"
            )
          )
        ),
        tabPanel("Notes", includeHTML("notesWOp.html")) #Markdown("notesOpioidsOtherDrugs.md"))
      )
    ),
  
    tabPanel(
      value = "W8Page",
  # Opioids with other drugs by sex (Table 8) -----------------------------------------
      "Opioids with other drugs, by sex and intent",
      h1("Opioid-induced deaths"),
      h3("Other drugs with opioids by sex and intent"),
  
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width = 9,
              withLoader(plotlyOutput("W8Plot", width = "100%", height = "600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteWOp.html")) #Markdown("notesOpioidsOtherDrugsPlot.md"))
            ),
            sidebarPanel(width = 3,
              sliderInput("W8yr", "Period",
                min = 1997, max = 2018,
                value = c(1997, 2018), sep = ""
              ),
              selectInput( "W8yax", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                ),
                selected = "r5"
              ),
              selectInput( "W8age", "Age range:",
                choices = c(
                  "All ages", "15 to 64"="15-64"
                ),
                selected = c("All ages")
              ),
  
              radioButtons("W8Drop", "Variable for dropdown list:",
                choices = c(
                  "Sex", "Intent"
                ), inline = T,
                selected = c("Sex")
              ),
              conditionalPanel( condition = "input.W8Drop == 'Sex'",
                selectInput("W8Ssex", label = NULL,
                  choices = c("All", "Female", "Male","Male & Female"="MF"),
                  selected = c("All")
                ),
                checkboxGroupInput("W8Scod", "Intent:",
                  c("All", "Accidental", "Intentional", "Undetermined"),
                  selected = c("All", "Accidental", "Intentional", "Undetermined")
                )
              ),
              conditionalPanel(
                condition = "input.W8Drop == 'Intent'",
                selectInput("W8Icod", label = NULL,
                  c("All", "Accidental", "Intentional", "Undetermined"),
                  selected = c("All") ),
                checkboxGroupInput("W8Isex", "Sex:",
                  choices = c("All", "Female", "Male"),
                  selected = c("All", "Female", "Male")
                )
              ),
  
              checkboxGroupInput("W8drug", "All opioids with:",
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
              ),
              checkboxGroupInput("W8show", "Also show:",
                choices = c("all drug-induced deaths")
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesWOp.html")) #Markdown("notesOpioidsOtherDrugs.md"))
        )
      )
    ),


# Stimulants menu tab ---------------------------------------------------------------
    navbarMenu(
    "Stimulants",
  
  # Amphetamines tab (Table 2) --------------------------------------------------------
      tabPanel(
        value = "AmPage",
        "Amphetamines",
        h1("Amphetamine-induced deaths"),

        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width = 9,
              withLoader(plotlyOutput("AmPlot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteAms.html")) #Markdown("notesAmphetaminesPlot.md"))
            ),

            sidebarPanel(width = 3,
              sliderInput("Amyr", "Period",
                min = 1997, max = 2018,
                value = c(1997, 2018), sep = ""
              ),
#             uiOutput("year"),
              selectInput(
                "Amyax", "Plot:",
                c(
                  "Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                ),
                selected = "r5"
              ),
  
            # HTML("<p><b>Intent:</b></p>"),
            # shinyTree("Amcod",
            #     checkbox = TRUE, themeIcons = FALSE),
            # HTML("<b>Age:</b>"),
            # shinyTree("Amage",
            #     checkbox = TRUE, theme="proton", themeIcons = FALSE)
              
              checkboxGroupInput("Amcod", "Intent:",
                c("All", "Accidental"),
                selected = "All"
              ),
              checkboxGroupInput("Amage", "Age:",
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
          ),
          tabPanel("Notes", includeHTML("notesAms.html")) #Markdown("notesAmphetamines.md"))
          #includeMarkdown("notesAms.md"), tableOutput('AmphOD'))
        )
      ),

    # Cocaine tab (Table 3) -------------------------------------------------------------
      tabPanel(
        value = "CPage",
        "Cocaine",
        h1("Cocaine-induced deaths"),
  
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Plot",
            mainPanel(width = 9,
              withLoader(plotlyOutput("CPlot", width = "100%", height = "600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(includeHTML("fnoteCoke.html")) #Markdown("notesCocainePlot.md"))
            ),
  
            sidebarPanel(width = 3,
#https://stackoverflow.com/questions/43866046/reuse-input-in-rshiny-app - for linking common parameters
              sliderInput("Cyr", "Period",
                min = 1997, max = 2018,
                value = c(1997, 2018), sep = ""
              ),
#             uiOutput("year"),
              selectInput("Cyax", "Plot:",
                c("Number of deaths" = "num",
                  "Deaths per 100,000 people" = "r5",
                  "Deaths per 100,000 people (95% CI)" = "r5ci",
                  "Deaths per 1,000,000 people" = "r6",
                  "Deaths per 1,000,000 people (95% CI)" = "r6ci"
                ),
                selected = "r5"
              ),
  
              checkboxGroupInput("Ccod", "Intent:",
                c(
                  "All" = "All",
                  "Accidental" = "Accidental"
                ),
                selected = "All"
              ),
  
              checkboxGroupInput("Cage", "Age range:",
                choices = c(
                  "All ages","15 to 64" = "15-64"
                ),
                  selected = c("All ages")
              )
            )
          ),
          tabPanel("Notes", includeHTML("notesCoke.html")) #Markdown("notesCocaine.md"))
          #includeMarkdown("notesCoke.md"), tableOutput('CocaineOD') )
        )
      )
    ),

    # Notes tab ---------------------------------------------------------------
    tabPanel(
      "Explanatory notes", includeHTML("notesExplanatory.html") #Markdown("notesOverall.md")
    ),


    # Citation tab ------------------------------------------------------------
    tabPanel(
      "Citation and acknowledgements",includeHTML("Citation.html")
      # fluidRow(
      #   column(width = 8, includeMarkdown("notesCitation.md"))
      #  , column(width = 4, includeHTML("DTLogo.html"))
      # )
    )#  ),

#seem to be ignored
#  renderCSS(type="image", loader="DT_NIDIP_onion30.gif"),
#  tags$style(type = 'text/css'
#      , '.navbar { background-color: #24813f;}'
#      , '.navbar-default .dropdown-menu {background-color: #24813f;}' this one worked
    #  , '.navbar-default .dropdown-toggle {background-color: #475c07;}' # works but only where it is a dropdown
#      , '.irs-from, .irs-to, .irs-single, .irs-grid-pol {background: #6a7d14;}' #color for year sliders
#      , '.irs-bar { border-top: 1px solid #6a7d14;
#        border-bottom: 1px solid #6a7d14;
#        background: #6a7d14; }'
    #  , '.navbar-default .navbar-nav>.active>a:focus {background-color: #6a7d14;}'
    #  , '.navbar-default .navbar-brand {color: black;}'
    #  , '.navbar-default .navbar-nav>.open>a, 
    #  .navbar-default .navbar-nav>.open>a:hover, 
    #  .navbar-default .navbar-nav>.open>a:focus {background-color: transparent;}'
#doesn't work
#      , '.table > thead > tr > th { border-top: 2px solid #ddd;}'
#      , '.table>caption+thead>tr:first-child>th,.table>colgroup+thead>tr:first-child>th,.table>thead:first-child>tr:first-child>th,
#        .table>caption+thead>tr:first-child>td,.table>colgroup+thead>tr:first-child>td,.table>thead:first-child>tr:first-child>td {border-top: 2px }'
#      , '.table>thead {border-top: 2px; }'
  )
)
    
#DRUG TRENDS COLOUR:
#DT: #475c07
#NIDIP: #6e2a8d
#EDRS: #de761c
#IDRS: #00aeef
#DNet: #c4161c
}
