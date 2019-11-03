#For ABS COD 2018 data received in Sept 2019
#N. Man
#TO FIX: # # # # # # # # # # # # # # # # # # # # # # # # # #
#For renderUI !!!!Warning: Length of logical index must be 1 or ..., not 0!!!!
#Still runs with the error
#- drug type: table 23, 24 & 25 - still needs updating
#- shorten series label for "All opioids with..."?
#- title graph with the selected group(s) (e.g. from dropdown list)
#so data in title don't need to be labelled)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(shinycustomloader)

#reading RDS file format is apparently more efficient
#https://appsilon.com/fast-data-loading-from-files-to-r/
#drug type data now loaded only when needed
#load other data only when needed as later app development
df <- readRDS("ABS_COD2018_All.rds")

#needs to be sorted [order(...)]
#weird proportions plot from 2015 onwards because of duplicates by AUS
#- need to filter out by table!="10" & table!="18"
df_EOp <- readRDS("ABS_COD2018_EOp.rds")
df_Stim <- readRDS("ABS_COD2018_Stim.rds")

agecols <- c(
  "15-24" = "#c09840",
  "25-34" = "#657d39",
  "35-44" = "#76b74b",
  "45-54" = "#4db598",
  "55-64" = "#6b8bcd",
  "65-74" = "#8d62ca",
  "75-84" = "#c75fa1",
#  "15-54" = "#fdcc8a",
  "15-64" = "#fc8d59",
  "All ages" = "#e34a33"
)

agetype <- c(
#  "15-54" = 2,
  "15-64" = 1,
  "All ages" = 2
)

agesexcols <- c(
"15-24,All" = "#c09840",
"25-34,All" = "#657d39",
"35-44,All" = "#76b74b",
"45-54,All" = "#4db598",
"55-64,All" = "#6b8bcd",
"65-74,All" = "#8d62ca",
"75-84,All" = "#c75fa1",
"15-64,All" = "#fc8d59",
"All ages,All" = "#e34a33",

"15-24,Male" = "#c09840",
"25-34,Male" = "#657d39",
"35-44,Male" = "#76b74b",
"45-54,Male" = "#4db598",
"55-64,Male" = "#6b8bcd",
"65-74,Male" = "#8d62ca",
"75-84,Male" = "#c75fa1",
"15-64,Male" = "#fc8d59",
"All ages,Male" = "#e34a33",

"15-24,Female" = "#c09840",
"25-34,Female" = "#657d39",
"35-44,Female" = "#76b74b",
"45-54,Female" = "#4db598",
"55-64,Female" = "#6b8bcd",
"65-74,Female" = "#8d62ca",
"75-84,Female" = "#c75fa1",
"15-64,Female" = "#fc8d59",
"All ages,Female" = "#e34a33"
)

agesextype <- c(
  "15-24,All" = 1,
  "25-34,All" = 1,
  "35-44,All" = 1,
  "45-54,All" = 1,
  "55-64,All" = 1,
  "65-74,All" = 1,
  "75-84,All" = 1,
  "15-64,All" = 1,
  "All ages,All" = 1,

  "15-24,Male" = 2,
  "25-34,Male" = 2,
  "35-44,Male" = 2,
  "45-54,Male" = 2,
  "55-64,Male" = 2,
  "65-74,Male" = 2,
  "75-84,Male" = 2,
  "15-64,Male" = 2,
  "All ages,Male" = 2,
  
  "15-24,Female" = 3,
  "25-34,Female" = 3,
  "35-44,Female" = 3,
  "45-54,Female" = 3,
  "55-64,Female" = 3,
  "65-74,Female" = 3,
  "75-84,Female" = 3,
  "15-64,Female" = 3,
  "All ages,Female" = 3
)

agecodtype <- c(
  "15-24,All" = 1,
  "25-34,All" = 1,
  "35-44,All" = 1,
  "45-54,All" = 1,
  "55-64,All" = 1,
  "65-74,All" = 1,
  "75-84,All" = 1,
  "15-64,All" = 1,
  "All ages,All" = 1,

  "15-24,Accidental" = 2,
  "25-34,Accidental" = 2,
  "35-44,Accidental" = 2,
  "45-54,Accidental" = 2,
  "55-64,Accidental" = 2,
  "65-74,Accidental" = 2,
  "75-84,Accidental" = 2,
  "15-64,Accidental" = 2,
  "All ages,Accidental" = 2,

  "15-24,Intentional" = 3,
  "25-34,Intentional" = 3,
  "35-44,Intentional" = 3,
  "45-54,Intentional" = 3,
  "55-64,Intentional" = 3,
  "65-74,Intentional" = 3,
  "75-84,Intentional" = 3,
  "15-64,Intentional" = 3,
  "All ages,Intentional" = 3,

  "15-24,Undetermined" = 4,
  "25-34,Undetermined" = 4,
  "35-44,Undetermined" = 4,
  "45-54,Undetermined" = 4,
  "55-64,Undetermined" = 4,
  "65-74,Undetermined" = 4,
  "75-84,Undetermined" = 4,
  "15-64,Undetermined" = 4,
  "All ages,Undetermined" = 4,
  
  "15-24,Other" = 5,
  "25-34,Other" = 5,
  "35-44,Other" = 5,
  "45-54,Other" = 5,
  "55-64,Other" = 5,
  "65-74,Other" = 5,
  "75-84,Other" = 5,
  "15-64,Other" = 5,
  "All ages,Other" = 5
)

agecodcols <- c(
  "15-24,All" = "#c09840",
  "25-34,All" = "#657d39",
  "35-44,All" = "#76b74b",
  "45-54,All" = "#4db598",
  "55-64,All" = "#6b8bcd",
  "65-74,All" = "#8d62ca",
  "75-84,All" = "#c75fa1",
  "15-64,All" = "#fc8d59",
  "All ages,All" = "#e34a33",
  
  "15-24,Accidental" = "#c09840",
  "25-34,Accidental" = "#657d39",
  "35-44,Accidental" = "#76b74b",
  "45-54,Accidental" = "#4db598",
  "55-64,Accidental" = "#6b8bcd",
  "65-74,Accidental" = "#8d62ca",
  "75-84,Accidental" = "#c75fa1",
  "15-64,Accidental" = "#fc8d59",
  "All ages,Accidental" = "#e34a33",
  
  "15-24,Intentional" = "#c09840",
  "25-34,Intentional" = "#657d39",
  "35-44,Intentional" = "#76b74b",
  "45-54,Intentional" = "#4db598",
  "55-64,Intentional" = "#6b8bcd",
  "65-74,Intentional" = "#8d62ca",
  "75-84,Intentional" = "#c75fa1",
  "15-64,Intentional" = "#fc8d59",
  "All ages,Intentional" = "#e34a33",
  
  "15-24,Undetermined" = "#c09840",
  "25-34,Undetermined" = "#657d39",
  "35-44,Undetermined" = "#76b74b",
  "45-54,Undetermined" = "#4db598",
  "55-64,Undetermined" = "#6b8bcd",
  "65-74,Undetermined" = "#8d62ca",
  "75-84,Undetermined" = "#c75fa1",
  "15-64,Undetermined" = "#fc8d59",
  "All ages,Undetermined" = "#e34a33",
  
  "15-24,Other" = "#c09840",
  "25-34,Other" = "#657d39",
  "35-44,Other" = "#76b74b",
  "45-54,Other" = "#4db598",
  "55-64,Other" = "#6b8bcd",
  "65-74,Other" = "#8d62ca",
  "75-84,Other" = "#c75fa1",
  "15-64,Other" = "#fc8d59",
  "All ages,Other" = "#e34a33"
)

codtype <- c(
  "All" = 1,
  "Accidental" = 2,
  "Intentional" = 3,
  "Undetermined" = 4,
  "Other" = 5
)

drugcols <- c("All opioids" = "#000000",
              "Heroin" = "#88CCEE",
              "Methadone" = "#44AA99",
              "Synthetic opioids" = "#117733",
              "Opium" = "#999933",
              "Natural and semi-synthetic opioids" = "#332288",
              "Other and unspecified opioids" = "#CC6677",
              "Cocaine" = "#DDCC77",
              "Amphetamines" = "#AA4499",
              
              "Alcohol"="#88CCEE",
              "Amphetamines"="#AA4499",
              "Antidepressants"="#117733",
              "Antipsychotics"="#999933",
              "Benzodiazepines"="#332288",
              "Paracetamol"="#CC6677",
              "Pregabalin"="#DDCC77",

              "All opioids with alcohol"="#88CCEE",
              "All opioids with amphetamines"="#AA4499",
              "All opioids with antidepressants"="#117733",
              "All opioids with antipsychotics"="#999933",
              "All opioids with benzodiazepines"="#332288",
              "All opioids with paracetamol"="#CC6677",
              "All opioids with pregabalin"="#DDCC77" )

drugltype <- c(
  "Exclusive illicit opioids"=1,
  "Exclusive pharmaceutical opioids"=2,
  "Illicit and pharmaceutical opioids"=3,
  "Other and unspecified opioids"=4)

sextype <- c(
  "All" = 1,
  "Male" = 2,
  "Female" = 3
)

sexcodtype <- c(
  "All,All" = 1,
  "All,Accidental" = 2,
  "All,Intentional" = 3,
  "All,Undetermined" = 4,
  "Male,All" = 1,
  "Male,Accidental" = 2,
  "Male,Intentional" = 3,
  "Male,Undetermined" = 4,
  "Female,All" = 1,
  "Female,Accidental" = 2,
  "Female,Intentional" = 3,
  "Female,Undetermined" = 4
)

sexcols <- c(
  "All" = "#808080",
  "All,All" = "#808080",
  "All,Accidental" = "#808080",
  "All,Intentional" = "#808080",
  "All,Undetermined" = "#808080",
  "Male" = "#5B7EBB",
  "Male,All" = "#5B7EBB",
  "Male,Accidental" = "#5B7EBB",
  "Male,Intentional" = "#5B7EBB",
  "Male,Undetermined" = "#5B7EBB",
  "Female" = "#B3564D",
  "Female,All" = "#B3564D",
  "Female,Accidental" = "#B3564D",
  "Female,Intentional" = "#B3564D",
  "Female,Undetermined" = "#B3564D"
)

# statecols <- c(
#   "NSW" = "#72CDF4",
#   "Vic" = "purple",
#   "Qld" = "#7C0040",
#   "WA" = "#FFD200",
#   "SA" = "#E31837",
#   "Tas" = "#00583D",
#   "ACT" = "#0079C1",
#   "NT" = "#F58426",
#   "Aus" = "#666666"
# )

# https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
#e6194B, #3cb44b, #ffe119, #4363d8, #f58231, #911eb4, #42d4f4, #f032e6, #bfef45, #fabebe, 
#469990, #e6beff, #9A6324, #fffac8, #800000, #aaffc3, #808000, #ffd8b1, #000075, #a9a9a9
alldrugcols <- c(
  "Alcohol"="#3cb44b",
  "OPIOIDS"="#000000",
  "heroin"="#ffe119",
  "natural and semi-synthetic opioids"="#4363d8",
  "methadone"="#f58231",
  "synthetic opioids"="#911eb4",
  "ANTIDEPRESSANTS"="#42d4f4",
  "tricyclic and tetracyclic antidepressants"="#e6194B",
  "other and unspecified antidepressants"="#a9a9a9",
  "CANNABIS DERIVATIVES"="#f032e6",
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS"="#bfef45",
  "barbiturates"="#ffd8b1",
  "benzodiazepines"="#fabebe",
  "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)"="#469990",
  "ANTIPSYCHOTICS & NEUROLEPTICS"="#e6beff",
  "other and unspecified antipsychotics (e.g. quetiapine)"="#9A6324",
  "Cocaine"="#DDCC77",
  "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="#800000",
  "4-aminophenol derivatives (e.g. paracetamol)"="#aaffc3",
  "other nonsteroidal anti-inflammatory drugs"="#000075",
  "AMPHETAMINES"="#808000"
)

Alldrugcols <- c(
  "Alcohol"="#469990",
  "OPIOIDS"="#000000",
  "heroin"="#000000",
  "natural and semi-synthetic opioids"="#000000",
  "methadone"="#000000",
  "synthetic opioids"="#000000",
  "ANTIDEPRESSANTS"="#CC6677",
  "tricyclic and tetracyclic antidepressants"="#CC6677",
  "other and unspecified antidepressants"="#CC6677",
  "CANNABIS DERIVATIVES"="#999933",
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS"="#117733",
  "barbiturates"="#117733",
  "benzodiazepines"="#117733",
  "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)"="#117733",
  "ANTIPSYCHOTICS & NEUROLEPTICS"="#88CCEE",
  "other and unspecified antipsychotics (e.g. quetiapine)"="#88CCEE",
  "Cocaine"="#DDCC77",
  "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="#332288",
  "4-aminophenol derivatives (e.g. paracetamol)"="#332288",
  "other nonsteroidal anti-inflammatory drugs"="#332288",
  "AMPHETAMINES"="#AA4499"
)

Alldrugtype <- c(
  "Alcohol"=1,
  "OPIOIDS"=1,
  "heroin"=2,
  "natural and semi-synthetic opioids"=3,
  "methadone"=4,
  "synthetic opioids"=5,
  "ANTIDEPRESSANTS"=1,
  "tricyclic and tetracyclic antidepressants"=2,
  "other and unspecified antidepressants"=3,
  "CANNABIS DERIVATIVES"=1,
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS"=1,
  "barbiturates"=2,
  "benzodiazepines"=3,
  "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)"=4,
  "ANTIPSYCHOTICS & NEUROLEPTICS"=1,
  "other and unspecified antipsychotics (e.g. quetiapine)"=2,
  "Cocaine"=1,
  "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"=1,
  "4-aminophenol derivatives (e.g. paracetamol)"=2,
  "other nonsteroidal anti-inflammatory drugs"=3,
  "AMPHETAMINES"=1
)

# Allow for site's state to be bookmarked via the url
# See https://shiny.rstudio.com/articles/bookmarking-state.html for details
enableBookmarking("url")

server <- function(input, output, session) {

  # Allow direct linking to specific tabs (with default configs)  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    print(query1)
    if(query1 == "tab=PlotOA"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOA")
    }
    if(query1 == "tab=PlotOB"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOB")
    }
    if(query1 == "tab=PlotOC"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOC")
    }
    if(query1 == "tab=PlotOD"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOD")
    }
    if(query1 == "tab=PlotOE"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOE")
    }
    if(query1 == "tab=PlotOF"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOF")
    }
    if(query1 == "tab=PlotOG"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOG")
    }
    if(query1 == "tab=PlotOH"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOH")
    }
    if(query1 == "tab=PlotA"){ #Amphetamines
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotA")
    }
    if(query1 == "tab=PlotC"){ #Cocaine
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotC")
    }
    # if(query1 == "tab=PlotD"){
    #   updateTabsetPanel(session, inputId = "Plot", selected = "PlotD")
    # }
  # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })


# Radio buttons for dropdown list-----------------------------------------------------------------------------------------
#Below based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
#!!!ERROR!!! Warning: Length of logical index must be 1 or 8580, not 0
#maybe because it is experimental?
#observe function still doesn't work but might have got rid of potential error with using *if statement* in tagList
#Might need this???: https://stackoverflow.com/questions/42169380/shiny-renderui-with-multiple-inputs
  # observe({
  #   if (input$DropAllB == "Intent")
  #     x <- selectInput("codAll", label = NULL,
  #                      c("All", "Accidental", "Intentional", "Undetermined", "Other") )
  #   if (input$DropAllB == "Sex")
  #     x <- selectInput("sexAllB", label = NULL,
  #                      choices = c("All", "Female", "Male") )
  # 
  #   if (input$DropAllB == "Sex")
  #     y <- checkboxGroupInput(
  #       "codAll", "Intent:",
  #       c("All", "Accidental", "Intentional", "Undetermined", "Other"),
  #       selected = c("All", "Accidental", "Intentional", "Undetermined", "Other")  )
  #   if (input$DropAllB == "Intent")
  #     y <- checkboxGroupInput("sexAllB", "Sex:",
  #                             choices = c("Male", "Female", "All"),
  #                             selected = c("Male", "Female", "All"))
  # 
  #   output$AllBControl <- renderUI({
  #     tagList(x,y)
  #   })
  # })
  
  observe({
      output$DTAge_Intent <- renderUI({
          tagList(
              selectInput("codDT", "Intent:",
                  choices = c("All", "Accidental"),
                  selected = c("All") ),
              selectInput("ageDT", label = "Age:",
                  choices = c(
                      "15 to 24" = "15-24",
                      "25 to 34" = "25-34",
                      "35 to 44" = "35-44",
                      "45 to 54" = "45-54",
                      "55 to 64" = "55-64",
                      "65 to 74" = "65-74",
                      "75 to 84" = "75-84",
                      "All ages",
                      "15 to 64" = "15-64"),
                  selected = "15-64" ),
              checkboxGroupInput("drugDT", "Drug:",
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
                      "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
                      "ANTIDEPRESSANTS",
                      "tricyclic and tetracyclic antidepressants",
                      "other and unspecified antidepressants",
                      "ANTIPSYCHOTICS & NEUROLEPTICS",
                      "other and unspecified antipsychotics (e.g. quetiapine)",
                      "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
                      "4-aminophenol derivatives (e.g. paracetamol)",
                      "other nonsteroidal anti-inflammatory drugs",
                      "Alcohol",
                      "Cocaine",
                      "CANNABIS DERIVATIVES"
                  ),
                  selected = c("AMPHETAMINES", "Cocaine", "OPIOIDS", "Alcohol")
              )
          )
      })
  #   if (input$DropDT == "Age_Intent")
  #   xDT <- list(selectInput("codDT", "Intent:",
  #                 choices = c("All", "Accidental"),
  #                 selected = c("All") ),
  #             selectInput("ageDT", label = "Age:",
  #                 choices = c(
  #                     "15 to 24" = "15-24",
  #                     "25 to 34" = "25-34",
  #                     "35 to 44" = "35-44",
  #                     "45 to 54" = "45-54",
  #                     "55 to 64" = "55-64",
  #                     "65 to 74" = "65-74",
  #                     "75 to 84" = "75-84",
  #                     "All ages",
  #                     "15 to 64" = "15-64"),
  #                 selected = "15-64" ) )
  # 
  # if (input$DropDT == "Drug")
  #   xDT <- selectInput("drugDT", label = NULL,
  #            choices = c(
  #                "OPIOIDS",
  #                "heroin",
  #                "natural and semi-synthetic opioids",
  #                "methadone",
  #                "synthetic opioids",
  #                "AMPHETAMINES",
  #                "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
  #                "barbiturates",
  #                "benzodiazepines",
  #                "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
  #                "ANTIDEPRESSANTS",
  #                "tricyclic and tetracyclic antidepressants",
  #                "other and unspecified antidepressants",
  #                "ANTIPSYCHOTICS & NEUROLEPTICS",
  #                "other and unspecified antipsychotics (e.g. quetiapine)",
  #                "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
  #                "4-aminophenol derivatives (e.g. paracetamol)",
  #                "other nonsteroidal anti-inflammatory drugs",
  #                "Alcohol",
  #                "Cocaine",
  #                "CANNABIS DERIVATIVES"
  #            ),
  #            selected = c("Alcohol") )
  # 
  # if (input$DropDT == "Age_Intent")
  #   yDT <- checkboxGroupInput("drugDT", "Drug:",
  #           choices = c(
  #               "OPIOIDS",
  #               "heroin",
  #               "natural and semi-synthetic opioids",
  #               "methadone",
  #               "synthetic opioids",
  #               "AMPHETAMINES",
  #               "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
  #               "barbiturates",
  #               "benzodiazepines",
  #               "antiepileptic and sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
  #               "ANTIDEPRESSANTS",
  #               "tricyclic and tetracyclic antidepressants",
  #               "other and unspecified antidepressants",
  #               "ANTIPSYCHOTICS & NEUROLEPTICS",
  #               "other and unspecified antipsychotics (e.g. quetiapine)",
  #               "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
  #               "4-aminophenol derivatives (e.g. paracetamol)",
  #               "other nonsteroidal anti-inflammatory drugs",
  #               "Alcohol",
  #               "Cocaine",
  #               "CANNABIS DERIVATIVES"
  #           ),
  #           selected = c("AMPHETAMINES", "Cocaine", "OPIOIDS", "Alcohol")
  #   )
  # 
  # if (input$DropDT == "Drug")
  #   yDT <- list(checkboxGroupInput(
  #                 "codDT", label = "Intent:",
  #                 c("All", "Accidental"),
  #                 selected = c("All") ),
  #           checkboxGroupInput("ageDT", "Age group:",
  #                choices = c(
  #                    "15 to 24" = "15-24",
  #                    "25 to 34" = "25-34",
  #                    "35 to 44" = "35-44",
  #                    "45 to 54" = "45-54",
  #                    "55 to 64" = "55-64",
  #                    "65 to 74" = "65-74",
  #                    "75 to 84" = "75-84",
  #                    "All ages",
  #                    "15 to 64" = "15-64"
  #                ),
  #                selected = "15-64"   ) )
  # 
  # output$DTControl <- renderUI({
  #   tagList(xDT,yDT)
  # })
  })

  observe({
    if (input$DropOA == "Opioid")
      x <- selectInput("drugOAO", label = NULL,
                choices = c(
                  "All opioids",
                  "Heroin",
                  "Methadone",
                  "Opium",
                  "Natural and semi-synthetic opioids",
                  "Synthetic opioids",
                  "Other and unspecified opioids"
                ),
                selected = c("All opioids"))

    if (input$DropOA == "Age")
      x <- selectInput("ageOAA", label = NULL,
                  choices = c(
                    "15 to 24" = "15-24",
                    "25 to 34" = "25-34",
                    "35 to 44" = "35-44",
                    "45 to 54" = "45-54",
                    "55 to 64" = "55-64",
                    "65 to 74" = "65-74",
                    "75 to 84" = "75-84",
                    "15 to 64" = "15-64",
                    "All ages" = "All ages"
                  ),
                  selected = c("15-64")
      )
    if (input$DropOA != "Opioid") {
      y <- checkboxGroupInput("drugOAA", "Opioid:",
                         choices = c(
                           "All opioids",
                           "Heroin",
                           "Methadone",
                           "Opium",
                           "Natural and semi-synthetic opioids",
                           "Synthetic opioids",
                           "Other and unspecified opioids"
                         ),
                         selected = c("All opioids")
      ) }
    else {
      if (input$DropOA != "Age" & input$DropOA != "Intent")
      y <- checkboxGroupInput("ageOAO", "Age:",
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
    }
    output$OAControl <- renderUI({
      tagList(x,y)
    })
#    reactiveValuesToList(input) #this resets to default even as the user selects a different value!
  })

  # observe({
  #   if (input$DropOB == "Opioid")
  #     x <- selectInput("drugOB", label = NULL,
  #                      choices = c(
  #                        "All opioids",
  #                        "Heroin",
  #                        "Methadone",
  #                        "Opium",
  #                        "Natural and semi-synthetic opioids",
  #                        "Synthetic opioids",
  #                        "Other and unspecified opioids"
  #                      ),
  #                      selected = c("All opioids"))
  #   if (input$DropOB == "Sex")
  #     x <- selectInput("sexOB", "Sex:",
  #         choices = c("All", "Female", "Male"),
  #         selected = c("All"))
  #   if (input$DropOB == "Intent")
  #     x <- selectInput("codOB", label = NULL,
  #       c("All", "Accidental", "Intentional", "Undetermined"),
  #       selected = c("All") )
  # 
  #   if (input$DropOB != "Opioid") {
  #     y <- checkboxGroupInput("drugOB", "Opioid:",
  #                             choices = c(
  #                               "All opioids",
  #                               "Heroin",
  #                               "Methadone",
  #                               "Opium",
  #                               "Natural and semi-synthetic opioids",
  #                               "Synthetic opioids",
  #                               "Other and unspecified opioids"
  #                             ),
  #                             selected = c("All opioids")
  #     ) }
  #   else {
  #       y <- checkboxGroupInput(
  #         "codOB", "Intent:",
  #         c("All", "Accidental", "Intentional", "Undetermined"),
  #         selected = c("All")
  #       )
  #   }
  #   
  #   if (input$DropOB == "Sex") {
  #     z <- checkboxGroupInput(
  #       "codOB", "Intent:",
  #       c("All", "Accidental", "Intentional", "Undetermined"),
  #       selected = c("All", "Accidental", "Intentional", "Undetermined")
  #     )
  #   }
  #   else if (input$DropOB != "Sex") {
  #     z <- checkboxGroupInput("sexOB", "Sex:",
  #         choices = c("All", "Female", "Male"),
  #         selected = c("All", "Female", "Male")
  #     ) }
  #   output$OBControl <- renderUI({
  #     tagList(x,y,z)
  #   })
  # })

  # Plot OA -----------------------------------------------------------------
  output$opioidPlotA <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
#Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html  
    if (input$DropOA == "Opioid") {
      sub <- subset(df_Op, subset = (sex == "All" & location == "Aus" & nature == "Underlying" & drug == input$drugOAO &
        intent %in% input$codOA & age_group %in% input$ageOAO &
        (year >= input$yearsOA[[1]] & year <= input$yearsOA[[2]])))
      sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")

      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year", title=input$drugOAO) +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    Legend <- "Age by intent"
    }

    else if (input$DropOA == "Age") {
      sub <- subset(df_Op, subset = (sex == "All" & location == "Aus" & nature == "Underlying" & drug %in% input$drugOAA &
                               intent %in% input$codOA & age_group == input$ageOAA &
                               (year >= input$yearsOA[[1]] & year <= input$yearsOA[[2]])))

      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = intent, group = 1) +
        geom_line() + labs(x = "Year", title=paste0("Age: ",input$ageOAA)) +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by intent"
    }

    if (input$plotOA == "deaths") {
        p <- p + aes(y = n,
                     text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Opioid: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Age group: ", age_group)
        ) +  scale_y_continuous(limits = c(0, max(sub$n, 500))) +
          labs(y = "Number of deaths")
      }
      
      else if (input$plotOA == "deathrateht" | input$plotOA == "deathratehtci") {
        p <- p + aes(y = rate_ht, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Opioid: ", str_to_title(drug),
          "<br>Intent: ", str_to_title(intent),
          "<br>Age group: ", age_group)
        ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
          labs(y = "Deaths per 100,000")
        if (input$plotOA == "deathratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
      }
      
      else if (input$plotOA == "deathratem" | input$plotOA == "deathratemci") {
        p <- p + aes(y = rate_m, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Opioid: ", str_to_title(drug),
          "<br>Intent: ", str_to_title(intent),
          "<br>Age group: ", age_group)
        ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
          labs(y = "Deaths per 1,000,000")
        if (input$plotOA == "deathratemci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
      }
      
      validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                        "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                        "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  # Plot OB -----------------------------------------------------------------
  output$opioidPlotB <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    # sub <- subset(df_Op, subset = (age_group == input$ageOB & location == "Aus" & nature == "Underlying" &
    #    drug %in% input$drugOB & intent == input$codOB & sex %in% input$sexOB  &
    #     (year >= input$yearsOB[[1]] & year <= input$yearsOB[[2]])))

    if (input$DropOB == "Opioid") {
      sub <- subset(df_Op, subset = (age_group == input$ageOB & location == "Aus" & nature == "Underlying" &
                                   drug == input$drugOBO & intent %in% input$codOBO & sex %in% input$sexOBO  &
                                   (year >= input$yearsOB[[1]] & year <= input$yearsOB[[2]])))
      sub$sex_intent <- paste(sub$sex,sub$intent,sep=",")

      p <- ggplot(sub) + aes(x = year, colour = sex_intent, linetype = sex_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = sexcols) +
        scale_linetype_manual(values = sexcodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Sex by intent"
    }
    else if (input$DropOB == "Intent") {
      sub <- subset(df_Op, subset = (age_group == input$ageOB & location == "Aus" & nature == "Underlying" &
                                     drug %in% input$drugOBI & sex %in% input$sexOBI & intent ==input$codOBI & 
                                     (year >= input$yearsOB[[1]] & year <= input$yearsOB[[2]])))

      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = sex, group = 1) +
        geom_line() +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by sex"
    }
    else if (input$DropOB == "Sex") {
      sub <- subset(df_Op, subset = (age_group == input$ageOB & location == "Aus" & nature == "Underlying" & 
                                    drug %in% input$drugOBS & intent %in% input$codOBS & sex == input$sexOBS &
                                     (year >= input$yearsOB[[1]] & year <= input$yearsOB[[2]])))
      
      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by intent"
    }

    if (input$plotOB == "deaths") {
      p <- p + aes(y = n, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Age group: ", age_group,
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 500))) +
        labs(x = "Year", y = "Number of deaths")
    }

    else if (input$plotOB == "deathrateht" | input$plotOB == "deathratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Age group: ", age_group,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x = "Year", y = "Deaths per 100,000") +
        if (input$plotOB == "deathratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }

    else if (input$plotOB == "deathratem" | input$plotOB == "deathratemci") {
      p <- p + aes(y = rate_m, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Age group: ", age_group,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) +
        scale_y_continuous(limits = c(0, max(sub$rate_mt_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        if (input$plotOB == "deathratemci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                     "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                     "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
 })

# Plot OC (NOT NEEDED - in Plot OB)

  # Plot OD -----------------------------------------------------------------
  output$opioidPlotD <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    sub <- subset(df_Op, subset = (age_group == input$ageOD & drug == "All opioids" & jurisdiction == input$stateOD &
                                  intent %in% input$codOD & sex %in% input$sexOD &
                                  (year >= input$yearsOD[[1]] & year <= input$yearsOD[[2]])))
    sub$sex_intent <- paste(sub$sex,sub$intent,sep=",")
  
      p <- ggplot(sub) + aes(x = year, 
        colour = sex_intent, linetype = sex_intent, group = 1) + #colour = location, linetype = sex,
        geom_line() + labs(x = "Year", title=paste0(input$stateOD,", ",input$ageOD) ) +
        scale_y_continuous(limits = c(0, NA)) +
        scale_colour_manual(values = sexcols) + #statecols
        scale_linetype_manual(values = sexcodtype) + #sextype
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))

    if (input$plotOD == "deaths") {
      p <- p + aes(y = n, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", location,
          "<br>Sex: ", sex
        )
      ) + labs(y = "Number of deaths") +
      scale_y_continuous(limits = c(0, max(sub$n, 500)))
    }
    
    else if (input$plotOD == "deathrateht" | input$plotOD == "deathratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", location,
          "<br>Sex: ", sex
        )
      ) + labs(y = "Deaths per 100,000") +
      scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5)))
      if (input$plotOD == "deathratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }

    else if (input$plotOD == "deathratem" | input$plotOD == "deathratemci") {
      p <- p + aes(y = rate_m, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", location,
          "<br>Sex: ", sex
        )
      ) + labs(y = "Deaths per 1,000,000") +
      scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25)))
      if (input$plotOD == "deathratemci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Sex by intent", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                        "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                        "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })


  # Amphetamine plot --------------------------------------------------------
  output$amphetaminePlot <- renderPlotly({
    sub <- subset(df_Stim, subset = (drug == "Amphetamines" & intent == input$codA & nature == "Underlying" &
      age_group %in% input$ageA & sex == "All" & jurisdiction == "Australia" &
        (year >= input$yearsA[[1]] & year <= input$yearsA[[2]])))

    if (input$plotA == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, max(sub$n, 500)))+
        labs(x = "Year", y = "Number of deaths") +
        scale_colour_manual(values = agecols) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    }

    else if (input$plotA == "deathrateht" | input$plotA == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x = "Year", y = "Deaths per 100,000") +
        scale_colour_manual(values = agecols) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        if (input$plotA == "deathratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }

    else if (input$plotA == "deathratem" | input$plotA == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        scale_colour_manual(values = agecols) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        if (input$plotA == "deathratemci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Age", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                         "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                         "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))  })

# Cocaine plot ------------------------------------------------------------
  output$cocainePlot <- renderPlotly({
    sub <- subset(df_Stim, subset = (drug == "Cocaine" & intent == input$codC & nature == "Underlying" &
      age_group == input$ageC & sex == "All" & jurisdiction == "Australia" &
        (year >= input$yearsC[[1]] & year <= input$yearsC[[2]])))

    if (input$plotC == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, max(sub$n, 500))) +
        labs(x = "Year", y = "Number of deaths") +
        scale_colour_manual(values = agecols) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    }

    else if (input$plotC == "deathrateht" | input$plotC == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x = "Year", y = "Deaths per 100,000") +
        scale_colour_manual(values = agecols) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        if (input$plotC == "deathratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }

    else if (input$plotC == "deathratem" | input$plotC == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        scale_colour_manual(values = agecols) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        if (input$plotC == "deathratemci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      layout(margin = list(b = 100, l = 100), showlegend=FALSE) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  ##WIP: html download - only static if html only
  #output$AllDrugHtml <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "AllDrug.html",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(n = input$slider)
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  # All drug deaths by intent, jurisdiction and sex (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$allPlot <- renderPlotly({
    #Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html  
    if (input$DropAll == "Intent") {
      sub <- filter(df, age_group %in% input$ageAll & drug == "Drug induced deaths" & jurisdiction == input$stateAll &
                                  intent == input$codAllI & sex %in% input$sexAllI &
                                  (year >= input$yearsAll[[1]] & year <= input$yearsAll[[2]]) )
      sub$age_sex <- paste(sub$age_group,sub$sex,sep=",")

      p <- ggplot(sub) + aes(x = year, colour = age_sex, linetype = age_sex, group = 1) +
        geom_line() + theme_light() + 
        scale_colour_manual(values = agesexcols) +
        scale_linetype_manual(values = agesextype) + theme(legend.title = element_blank()) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x)))) +
        labs(title=input$stateAllB)
      Legend <- "Age & sex"
    }
    else if (input$DropAll == "Sex") {
      sub <- subset(df, subset = (age_group %in% input$ageAll & drug == "Drug induced deaths" & jurisdiction == input$stateAll &
                                    intent %in% input$codAllS & sex == input$sexAllS &
                                    (year >= input$yearsAll[[1]] & year <= input$yearsAll[[2]]) ) )
      sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x)))) +
        labs(title=input$stateAllB)
      Legend <- "Age & intent"
    }
    
    if (input$plotAll == "deaths") {
      p <- p + aes(y = n, text = paste0(
              "Year: ", year,
              "<br>Deaths: ", n,
              "<br>Intent: ", str_to_title(intent),
  #           "<br>Jurisdiction: ", location,
              "<br>Age: ", age_group,
              "<br>Sex: ", sex
            )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 500))) +
        labs(x = "Year", y = "Number of deaths")
    }
    
    else if (input$plotAll == "deathrateht" | input$plotAll == "deathratehtci") {
      p <- p + aes(y = rate_ht,
             text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Intent: ", str_to_title(intent), 
  #            "<br>Jurisdiction: ", location,
               "<br>Age: ", age_group,
               "<br>Sex: ", sex
             )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x = "Year", y = "Deaths per 100,000")
      if (input$plotAll == "deathratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$plotAll == "deathratem" | input$plotAll == "deathratemci") {
      p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Intent: ", str_to_title(intent), 
    #          "<br>Jurisdiction: ", location,
               "<br>Age: ", age_group,
               "<br>Sex: ", sex
             )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000")
      if (input$plotAll == "deathratemci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # All drugs by type plot ----------------------------------------------------------
  output$drugtypePlot <- renderPlotly({
    df_DT <- readRDS("ABS_COD2018_DT.rds")

#Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html  
    if (input$DropDT == "Age_Intent") {
      sub <- subset(df_DT, subset = (intent==input$codDT & nature=="Underlying" & age_group == input$ageDT
                    & sex == "All" & jurisdiction == "Australia" & drug %in% input$drugDT
                    & (year >= input$yearsDT[[1]] & year <= input$yearsDT[[2]] ) ) )
    p <- ggplot(sub) + aes(x = year, colour = drug, linetype = drug, group = 1) +
      geom_line() + labs(x = "Year") +
      scale_colour_manual(values = Alldrugcols) +
      scale_linetype_manual(values = Alldrugtype) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    LO <- "h"
    Legend <- ""
    LY <- -0.15
    }
    if (input$DropDT == "Drug") {
      sub <- subset(df_DT, subset = (intent %in% input$codDTD & nature=="Underlying" & drug == input$drugDTD & 
                       age_group %in% input$ageDTD & sex == "All" & jurisdiction == "Australia" &
                       (year >= input$yearsDT[[1]] & year <= input$yearsDT[[2]]) ) )
      sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      LO <- "v"
      Legend <- "Age & intent"
      LY <- 0.95
    }
    
    if (input$plotDT == "deaths") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", str_to_title(drug),
        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 500))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$plotDT == "deathrateht" | input$plotDT == "deathratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Drug: ", str_to_title(drug),
        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$plotDT == "deathratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$plotDT == "deathratem" | input$plotDT == "deathratemci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Drug: ", drug,
        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$plotDT == "deathratemci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = .95, yanchor = "bottom",
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
#      layout(legend = list(orientation = "h", y = -0.15, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      layout(legend = list(orientation = LO, y = LY, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                  "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                  "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # Opioids with other drugs  -----------------------------------------------------------------
  output$PlotOE <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds") %>%
      mutate( drug = case_when(
        drug == "All opioids with alcohol" ~ "Alcohol",
        drug == "All opioids with amphetamines" ~ "Amphetamines",
        drug == "All opioids with antidepressants" ~ "Antidepressants",
        drug == "All opioids with antipsychotics" ~ "Antipsychotics",
        drug == "All opioids with benzodiazepines" ~ "Benzodiazepines",
        drug == "All opioids with paracetamol" ~ "Paracetamol",
        drug == "All opioids with pregabalin" ~ "Pregabalin",
        TRUE ~ drug )
      )

    if (input$DropOE == "Drug") {
    sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                    drug == input$drugOED & intent %in% input$codOE & age_group %in% input$ageOED & 
                    (year >= input$yearsOE[[1]] & year <= input$yearsOE[[2]]) & sex=="All")
    sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
    
    p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        labs(x = "Year", title=paste0("All opioids with ",input$drugOED) ) + geom_line() +
        scale_colour_manual(values = agecodcols) + scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    Legend <- "Age by intent"
    }
    if (input$DropOE == "Age") {
      sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                    drug %in% input$drugOEA & intent %in% input$codOE & age_group == input$ageOEA & 
                      (year >= input$yearsOE[[1]] & year <= input$yearsOE[[2]]) & sex=="All")
      
      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = intent, group = 1) +
        labs(x = "Year") + geom_line() +
        scale_colour_manual(values = drugcols) + scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug with opioid by intent"
    }
    
    if (input$plotOE == "deaths") {
        p <- p + aes(y = n, text = paste0(
                     "Year: ", year,
                     "<br>Deaths: ", n,
                     "<br>Drug with opioid: ", str_to_title(drug),
                     "<br>Intent: ", str_to_title(intent),
                     "<br>Sex: ", sex,
                     "<br>Age group: ", age_group ) ) + 
                scale_y_continuous(limits = c(0, max(sub$n, 500) ) ) +
                labs(y = "Number of deaths")
    }

    if (input$plotOE == "deathrateht" | input$plotOE == "deathratehtci") {
        p <- p + aes(y = rate_ht, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Drug with opioid: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group
            )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
            labs(y = "Deaths per 100,000")

        if (input$plotOE == "deathratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$plotOE == "deathratem" | input$plotOE == "deathratemci") {
        p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Drug with opioid: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group) ) + 
               scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) + labs(y = "Deaths per 1,000,000")
        if (input$plotOE == "deathratemci") {
            p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                      "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                      "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Opioids and other drugs by sex ------------------------------------------
  output$PlotOF <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds") %>%
      mutate( drug = case_when(
        drug == "All opioids with alcohol" ~ "Alcohol",
        drug == "All opioids with amphetamines" ~ "Amphetamines",
        drug == "All opioids with antidepressants" ~ "Antidepressants",
        drug == "All opioids with antipsychotics" ~ "Antipsychotics",
        drug == "All opioids with benzodiazepines" ~ "Benzodiazepines",
        drug == "All opioids with paracetamol" ~ "Paracetamol",
        drug == "All opioids with pregabalin" ~ "Pregabalin",
        TRUE ~ drug )
      )

    if (input$DropO8 == "Sex") {
      sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                    drug %in% input$drugOF & intent %in% input$codO8S & 
                    sex == input$sexO8S & age_group == input$ageO8 & 
                    (year >= input$yearsOF[[1]] & year <= input$yearsOF[[2]] ) )
      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = intent, group = 1) + 
            geom_line() + 
            labs(x = "Year", title = paste0("Age group:",input$ageO8,"  Sex: ",input$sexO8S) ) +
            scale_colour_manual(values = drugcols) +
            scale_linetype_manual(values = codtype) +
            scale_x_continuous(breaks = function(x) unique(floor(pretty(x))) )
      Legend = "Drug by intent"
    }
    if (input$DropO8 == "Intent") {
        sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                      drug %in% input$drugOF & intent == input$codO8I & 
                      sex %in% input$sexO8I & age_group == input$ageO8 & 
                      (year >= input$yearsOF[[1]] & year <= input$yearsOF[[2]] ) )
        print(input$DropO8)
        p <- ggplot(sub) + aes(x = year, colour = drug, linetype = sex, group = 1) + 
              geom_line() + 
              labs(x = "Year", title = paste0("Age group:",input$ageO8,"  Intent: ",input$codO8S) ) +
              scale_colour_manual(values = drugcols) +
              scale_linetype_manual(values = sextype) +
              scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        Legend = "Drug by sex"
    }
    
    if (input$plotOF == "deaths") {
      p <- p + aes(y = n, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Drug with opioid: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
            )) +
            scale_y_continuous(limits = c(0, NA)) + labs(y = "Number of deaths")
    }
    
    if (input$plotOF == "deathrateht" | input$plotOF == "deathratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Drug with opioid: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
           )) + geom_line() +
          scale_y_continuous(limits = c(0, NA)) +
          labs(y = "Deaths per 100,000")
        if (input$plotOF == "deathratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$plotOF == "deathratem" | input$plotOF == "deathratemci") {
      p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex
            )) + geom_line() +
            scale_y_continuous(limits = c(0, NA)) +
            labs(y = "Deaths per 1,000,000")
        if (input$plotOF == "deathratemci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
          theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = Legend, xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                    "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                    "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })
  
  # Exclusive opioids ------------------------------------------
  output$PlotOG <- renderPlotly({
    sub <- filter(df_EOp, jurisdiction == "Australia" &
                    drug %in% input$drugOG & intent == input$intentOG & age_group == input$ageOG & 
                    (year >= input$yearsOG[[1]] & year <= input$yearsOG[[2]]) & sex %in% input$sexOG)
    
    
    if (input$plotOG == "deaths") {
      p <- ggplot(sub) + aes(x = year, y = n, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank()) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    }
    
    if (input$plotOG == "deathrateht" | input$plotOG == "deathratehtci") {
      p <- ggplot(sub) + aes(x = year, y = rate_ht, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank()) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        if (input$plotOG == "deathratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$plotOG == "deathratem" | input$plotOG == "deathratemci") {
      p <- ggplot(sub) + aes(x = year, y = rate_m, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) + scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank()) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        if (input$plotOG == "deathratemci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Sex and drug", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                             "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                             "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$PlotOH <- renderPlotly({
    sub <- filter(df_EOp, drug %in% c( "Exclusive illicit opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Illicit and pharmaceutical opioids",
                                   "Other and unspecified opioids") &
                    intent == input$intentOH & 
                    age_group == input$ageOH & 
                    (year >= input$yearsOH[[1]] & year <= input$yearsOH[[2]]) & 
                    sex == input$sexOH &
                    jurisdiction == "Australia" & table!="10" & table!="18") %>% 
      group_by(year, intent, nature, sex, jurisdiction, age_group) %>% 
      distinct() %>%
      mutate(alldeaths = sum(n),
             percent = round(n/sum(n)*100, 2),
             drug = factor(drug, levels = c( "Other and unspecified opioids",
                                             "Illicit and pharmaceutical opioids",
                                             "Exclusive pharmaceutical opioids",
                                             "Exclusive illicit opioids"
                                             )))

    p <- ggplot(sub, aes(x=year, y=percent, fill=drug, group=1, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Percent: ", percent, "%",
        "<br>Drug: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex
    ))) +
      geom_area() +
      labs(x = "Year", y = "Percent of opioid induced deaths") +
      theme_light() + 
      theme(legend.title = element_blank()) + 
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x)))) +
      scale_fill_brewer(palette = "PuBu")
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())

    ggplotly(p,  tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      layout(
        images = list(
          source = "DrugTrends-Logo.png",
          x = 0.01, xanchor = "left", y = .99, yanchor = "top",
          sizex = 0.07, sizey = 0.2,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ))  %>%
      add_annotations(
        text = "Drug", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                        "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                        "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })
  
}
