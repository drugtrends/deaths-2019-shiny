#For ABS COD 2018 data received in Sept 2019
#N. Man
#TO FIX: # # # # # # # # # # # # # # # # # # # # # # # # # #
#For renderUI !!!!Warning: Length of logical index must be 1 or ..., not 0!!!!
#Still runs with the error
#- drug type: table 23, 24 & 25 - still needs updating
#- title graph with the selected group(s)??? (e.g. from dropdown list)
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
df <- readRDS("ABS_COD2018_All.rds")
df_Stim <- readRDS("ABS_COD2018_Stim.rds")

# agecols <- c(
#   "15-24" = "#c09840",
#   "25-34" = "#657d39",
#   "35-44" = "#76b74b",
#   "45-54" = "#4db598",
#   "55-64" = "#6b8bcd",
#   "65-74" = "#8d62ca",
#   "75-84" = "#c75fa1",
# #  "15-54" = "#fdcc8a",
#   "15-64" = "#fc8d59",
#   "All ages" = "#e34a33"
# )

agesexcols <- c(
"15-24 All" = "#ff0000",
"25-34 All" = "#aa0055",
"35-44 All" = "#5500aa",
"45-54 All" = "#0000ff",
"55-64 All" = "#00aa80",
"65-74 All" = "#00ff00",
"75-84 All" = "#ffcc00",
"All ages All" = "#000000",
"15-64 All" = "gray50",

"15-24 Male" = "#ff0000",
"25-34 Male" = "#aa0055",
"35-44 Male" = "#5500aa",
"45-54 Male" = "#0000ff",
"55-64 Male" = "#00aa80",
"65-74 Male" = "#00cc00",
"75-84 Male" = "#ffcc00",
"All ages Male" = "#000000",
"15-64 Male" = "gray50",

"15-24 Female" = "#ff0000",
"25-34 Female" = "#aa0055",
"35-44 Female" = "#5500aa",
"45-54 Female" = "#0000ff",
"55-64 Female" = "#00aa80",
"65-74 Female" = "#00cc00",
"75-84 Female" = "#ffcc00",
"All ages Female" = "#000000",
"15-64 Female" = "gray50"
)

agecodcols <- c(
  "15-24 All" = "#c09840",
  "25-34 All" = "#76b74b",
  "35-44 All" = "#6b8bcd",
  "45-54 All" = "#8d62ca",
  "55-64 All" = "#c75fa1",
  "65-74 All" = "#fc8d59",
  "75-84 All" = "#e34a33",
  "All ages All" = "#000000",
  "15-64 All" = "#808080",
  
  "15-24 Accidental" = "#c09840",
  "25-34 Accidental" = "#76b74b",
  "35-44 Accidental" = "#6b8bcd",
  "45-54 Accidental" = "#8d62ca",
  "55-64 Accidental" = "#c75fa1",
  "65-74 Accidental" = "#fc8d59",
  "75-84 Accidental" = "#e34a33",
  "All ages Accidental" = "#000000",
  "15-64 Accidental" = "#808080",
  
  "15-24 Intentional" = "#c09840",
  "25-34 Intentional" = "#76b74b",
  "35-44 Intentional" = "#6b8bcd",
  "45-54 Intentional" = "#8d62ca",
  "55-64 Intentional" = "#c75fa1",
  "65-74 Intentional" = "#fc8d59",
  "75-84 Intentional" = "#e34a33",
  "All ages Intentional" = "#000000",
  "15-64 Intentional" = "#808080",
  
  "15-24 Undetermined" = "#c09840",
  "25-34 Undetermined" = "#76b74b",
  "35-44 Undetermined" = "#6b8bcd",
  "45-54 Undetermined" = "#8d62ca",
  "55-64 Undetermined" = "#c75fa1",
  "65-74 Undetermined" = "#fc8d59",
  "75-84 Undetermined" = "#e34a33",
  "All ages Undetermined" = "#000000",
  "15-64 Undetermined" = "#808080",
  
  "15-24 Other" = "#c09840",
  "25-34 Other" = "#76b74b",
  "35-44 Other" = "#6b8bcd",
  "45-54 Other" = "#8d62ca",
  "55-64 Other" = "#c75fa1",
  "65-74 Other" = "#fc8d59",
  "75-84 Other" = "#e34a33",
  "All ages Other" = "#000000",
  "15-64 Other" = "#808080"
)

agesextype <- c(
  "15-24 All" = 1,
  "25-34 All" = 1,
  "35-44 All" = 1,
  "45-54 All" = 1,
  "55-64 All" = 1,
  "65-74 All" = 1,
  "75-84 All" = 1,
  "All ages All" = 1,
  "15-64 All" = 1,
  
  "15-24 Male" = 2,
  "25-34 Male" = 2,
  "35-44 Male" = 2,
  "45-54 Male" = 2,
  "55-64 Male" = 2,
  "65-74 Male" = 2,
  "75-84 Male" = 2,
  "All ages Male" = 2,
  "15-64 Male" = 2,
  
  "15-24 Female" = 3,
  "25-34 Female" = 3,
  "35-44 Female" = 3,
  "45-54 Female" = 3,
  "55-64 Female" = 3,
  "65-74 Female" = 3,
  "75-84 Female" = 3,
  "All ages Female" = 3,
  "15-64 Female" = 3
)

agecodtype <- c(
  "15-24 All" = 1,
  "25-34 All" = 1,
  "35-44 All" = 1,
  "45-54 All" = 1,
  "55-64 All" = 1,
  "65-74 All" = 1,
  "75-84 All" = 1,
  "All ages All" = 1,
  "15-64 All" = 1,

  "15-24 Accidental" = 2,
  "25-34 Accidental" = 2,
  "35-44 Accidental" = 2,
  "45-54 Accidental" = 2,
  "55-64 Accidental" = 2,
  "65-74 Accidental" = 2,
  "75-84 Accidental" = 2,
  "All ages Accidental" = 2,
  "15-64 Accidental" = 2,

  "15-24 Intentional" = 3,
  "25-34 Intentional" = 3,
  "35-44 Intentional" = 3,
  "45-54 Intentional" = 3,
  "55-64 Intentional" = 3,
  "65-74 Intentional" = 3,
  "75-84 Intentional" = 3,
  "All ages Intentional" = 3,
  "15-64 Intentional" = 3,

  "15-24 Undetermined" = 4,
  "25-34 Undetermined" = 4,
  "35-44 Undetermined" = 4,
  "45-54 Undetermined" = 4,
  "55-64 Undetermined" = 4,
  "65-74 Undetermined" = 4,
  "75-84 Undetermined" = 4,
  "All ages Undetermined" = 4,
  "15-64 Undetermined" = 4,
  
  "15-24 Other" = 5,
  "25-34 Other" = 5,
  "35-44 Other" = 5,
  "45-54 Other" = 5,
  "55-64 Other" = 5,
  "65-74 Other" = 5,
  "75-84 Other" = 5,
  "All ages Other" = 5,
  "15-64 Other" = 5
)

codtype <- c(
  "All" = 1,
  "Accidental" = 2,
  "Intentional" = 3,
  "Undetermined" = 4,
  "Other" = 5
)

drugcols <- c("All opioids" = "#000000",
              "Heroin" = "orange",
              "Opium" = "brown",
              "Methadone" = "red",
              "Synthetic opioids" = "blue",
              "Natural and semi-synthetic opioids" = "purple",
#              "Other and unspecified opioids" = "#CC6677",
              "Cocaine" = "brown",

              "Alcohol"="purple",
              "Amphetamines"="hotpink",
              "Antidepressants"="orange",
              "Antipsychotics"="blue",
              "Benzodiazepines"="chartreuse",
              "Paracetamol"="cyan",
              "Pregabalin"="forestgreen",

              "Exclusive illicit opioids"="orange",
              "Exclusive pharmaceutical opioids"="#e34a33", #red
              "Illicit and pharmaceutical opioids"="#3300dd", #blue
              "Other and unspecified opioids"="#00bb33") #green
              # "All opioids with alcohol"="#88CCEE",
              # "All opioids with amphetamines"="#AA4499",
              # "All opioids with antidepressants"="#117733",
              # "All opioids with antipsychotics"="#999933",
              # "All opioids with benzodiazepines"="#332288",
              # "All opioids with paracetamol"="#CC6677",
              # "All opioids with pregabalin"="#DDCC77" )

drugltype <- c(
  "Exclusive illicit opioids"=1,
  "Exclusive pharmaceutical opioids"=2,
  "Illicit and pharmaceutical opioids"=3,
  "Other and unspecified opioids"=4)

regcols <- c(
  "Major Cities" = "red",
  "Inner Regional" = "purple",
  "Outer Regional" = "blue",
  "Remote and Very Remote" = "forestgreen",
  "Regional and Remote" = "orange"
)

regtype <- c(
  "Major Cities" = 1,
  "Inner Regional" = 2,
  "Outer Regional" = 3,
  "Remote and Very Remote" = 4,
  "Regional and Remote" = 2
)

sextype <- c(
  "All" = 1,
  "Male" = 2,
  "Female" = 3
)

sexcodtype <- c(
  "All All" = 1,
  "All Accidental" = 2,
  "All Intentional" = 3,
  "All Undetermined" = 4,
  "Male All" = 1,
  "Male Accidental" = 2,
  "Male Intentional" = 3,
  "Male Undetermined" = 4,
  "Female All" = 1,
  "Female Accidental" = 2,
  "Female Intentional" = 3,
  "Female Undetermined" = 4
)

sexcols <- c(
  "All" = "#000000",
  "All All" = "#000000",
  "All Accidental" = "#000000",
  "All Intentional" = "#000000",
  "All Undetermined" = "#000000",
  "Male" = "#0000ff",
  "Male All" = "#0000ff",
  "Male Accidental" = "#0000ff",
  "Male Intentional" = "#0000ff",
  "Male Undetermined" = "#0000ff",
  "Female" = "#ff0000",
  "Female All" = "#ff0000",
  "Female Accidental" = "#ff0000",
  "Female Intentional" = "#ff0000",
  "Female Undetermined" = "#ff0000"
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
# Alldrugcols <- c(
#   "ALCOHOL"="#469990",
#   "OPIOIDS"="#000000",
#   "heroin"="#000000",
#   "natural and semi-synthetic opioids"="#000000",
#   "methadone"="#000000",
#   "synthetic opioids"="#000000",
#   "ANTIDEPRESSANTS"="#CC6677",
#   "tricyclic and tetracyclic antidepressants"="#CC6677",
#   "other and unspecified antidepressants"="#CC6677",
#   "CANNABIS DERIVATIVES"="#999933",
#   "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS"="#117733",
#   "barbiturates"="#117733",
#   "benzodiazepines"="#117733",
#   "antiepileptic and sedative-hypnotic drugs, unspecified"="#117733",
#   "ANTIPSYCHOTICS & NEUROLEPTICS"="#88CCEE",
#   "other and unspecified antipsychotics"="#88CCEE",
#   "COCAINE"="#DDCC77",
#   "NONOPIOID ANALGESICS"="#332288",
#   "4-aminophenol derivatives"="#332288",
#   "other nonsteroidal anti-inflammatory drugs"="#332288",
#   "AMPHETAMINES"="#AA4499"
# )

Alldrugcols <- c(
  "ALCOHOL"="purple",
  "OPIOIDS"="#000000",
  "heroin"="#000000",
  "natural and semi-synthetic opioids"="#000000",
  "methadone"="#000000",
  "synthetic opioids"="#000000",
  "ANTIDEPRESSANTS"="orange",
  "tricyclic and tetracyclic antidepressants"="orange",
  "other and unspecified antidepressants"="orange",
  "CANNABIS DERIVATIVES"="forestgreen",
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS"="chartreuse",
  "barbiturates"="chartreuse",
  "benzodiazepines"="chartreuse",
  "antiepileptic and sedative-hypnotic drugs, unspecified"="chartreuse",
  "ANTIPSYCHOTICS & NEUROLEPTICS"="blue",
  "other and unspecified antipsychotics"="blue",
  "COCAINE"="brown",
  "NONOPIOID ANALGESICS"="cyan",
  "4-aminophenol derivatives"="cyan",
  "other nonsteroidal anti-inflammatory drugs"="cyan",
  "AMPHETAMINES"="hotpink"
)

Alldrugtype <- c(
  "ALCOHOL"=1,
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
  "antiepileptic and sedative-hypnotic drugs, unspecified"=4,
  "ANTIPSYCHOTICS & NEUROLEPTICS"=1,
  "other and unspecified antipsychotics"=2,
  "COCAINE"=1,
  "NONOPIOID ANALGESICS"=1,
  "4-aminophenol derivatives"=2,
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
    if(query1 == "tab=PlotAll"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotAll")
    }
    if(query1 == "tab=PlotDT"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotDT")
    }
    if(query1 == "tab=PlotO4"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotO4")
    }
    if(query1 == "tab=PlotO5"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotO5")
    }
    if(query1 == "tab=PlotO6"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotO6")
    }
    if(query1 == "tab=Plot10"){
      updateTabsetPanel(session, inputId = "Plot", selected = "Plot10")
    }
    if(query1 == "tab=PlotE9"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotE9")
    }
    if(query1 == "tab=PlotEP"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotEP")
    }
    if(query1 == "tab=PlotW7"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotW7")
    }
    if(query1 == "tab=PlotW8"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotW8")
    }
    if(query1 == "tab=PlotA"){ #Amphetamines
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotA")
    }
    if(query1 == "tab=PlotC"){ #Cocaine
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotC")
    }
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
  #   if (input$DropE9 == "Intent")
  #     x <- selectInput("codE9I", label = NULL,
  #                      c("All", "Accidental", "Intentional", "Undetermined", "Other") )
  # 
  #   if (input$DropE9 == "Sex")
  #     y <- checkboxGroupInput(
  #       "codE9S", "Intent:",
  #       c("All", "Accidental", "Intentional", "Undetermined", "Other"),
  #       selected = c("All", "Accidental", "Intentional", "Undetermined", "Other")  )
  #   if (input$DropE9 == "Intent")
  #     y <- checkboxGroupInput("sexE9S", "Sex:",
  #                             choices = c("Male", "Female", "All"),
  #                             selected = c("Male", "Female", "All"))
  # 
  #   output$Control9 <- renderUI({
  #     tagList(x,y)
  #   })
# })
  
#   observe({
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
# })

  # Amphetamine plot (Table 2) --------------------------------------------------------
  output$amphetaminePlot <- renderPlotly({
    sub <- subset(df_Stim, subset = (jurisdiction == "Australia" & sex == "All" & 
           drug == "Amphetamines" & nature == "Underlying" & intent %in% input$codA & 
           age_group %in% input$ageA & (year >= input$yrA[[1]] & year <= input$yrA[[2]])))
   p <- ggplot(sub) + geom_line() + labs(x = "Year") +
        aes(x = year, linetype = age_intent, colour = age_intent, group = 1) +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))

    if (input$yaxA == "num") {
      p <- p + aes(y = n, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250)))+
        labs(y = "Number of deaths")
    }
    
    else if (input$yaxA == "rateht" | input$yaxA == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxA == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxA == "ratem" | input$yaxA == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxA == "ratemci") {
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
  
  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$cocainePlot <- renderPlotly({
    sub <- subset(df_Stim, subset = (drug == "Cocaine" & intent %in% input$codC & nature == "Underlying" &
                                       age_group %in% input$ageC & sex == "All" & jurisdiction == "Australia" &
                                       (year >= input$yrC[[1]] & year <= input$yrC[[2]])))
    
    p <- ggplot(sub) + aes(x = year, linetype=age_intent, colour=age_intent, group=1) +
      geom_line() + labs(x = "Year") +
      scale_linetype_manual(values = agecodtype) +
      scale_colour_manual(values = agecodcols) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    
    if (input$yaxC == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Intent: ", str_to_title(intent),
        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      )
      ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$yaxC == "rateht" | input$yaxC == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxC == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxC == "ratem" | input$yaxC == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxC == "ratemci") {
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
        text = "Age group by intent", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
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
      sub <- filter(df, age_group %in% input$ageAll & jurisdiction == input$jurAll &
                                  intent == input$codAllI & sex %in% input$sexAllI &
                                  (year >= input$yrAll[[1]] & year <= input$yrAll[[2]]) )

      p <- ggplot(sub) + aes(x = year, colour = age_sex, linetype = age_sex, group = 1) + 
        geom_line() + scale_colour_manual(values = agesexcols) +
        scale_linetype_manual(values = agesextype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x)))) +
        labs(title=paste0(input$jurAll,", Intent: ",input$codAllI) )
      Legend <- "Age & sex"
    }
    else if (input$DropAll == "Sex") {
      if (input$sexAllS != "MF") {
      sub <- subset(df, subset = (age_group %in% input$ageAll & jurisdiction == input$jurAll &
                                    intent %in% input$codAllS & sex == input$sexAllS &
                                    (year >= input$yrAll[[1]] & year <= input$yrAll[[2]]) ) )
      p <- ggplot(sub) + labs(title=paste0(input$jurAll,", Sex: ",input$sexAllS) )
      }
      else {
        sub <- subset(df, subset = (age_group %in% input$ageAll & jurisdiction == input$jurAll &
                                      intent %in% input$codAllS & sex != "All" &
                                      (year >= input$yrAll[[1]] & year <= input$yrAll[[2]]) ) )
        p <- ggplot(sub) + labs(title=input$jurAll) +
          facet_grid(cols = vars(sex)) +
          theme(strip.background = element_rect(fill="#6a7d14")) #doesn't work https://github.com/tidyverse/ggplot2/issues/2096
      }
      p <- p + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      
      Legend <- "Age & intent"
    }
    
    if (input$yaxAll == "num") {
      p <- p + aes(y = n, text = paste0(
              "Year: ", year,
              "<br>Deaths: ", n,
              "<br>Intent: ", str_to_title(intent),
#             "<br>Jurisdiction: ", location,
              "<br>Age: ", age_group,
              "<br>Sex: ", sex
            )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(x = "Year", y = "Number of deaths")
    }
    
    else if (input$yaxAll == "rateht" | input$yaxAll == "ratehtci") {
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
      if (input$yaxAll == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxAll == "ratem" | input$yaxAll == "ratemci") {
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
      if (input$yaxAll == "ratemci") {
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
      layout(legend = list(y = 0.95, yanchor = "top", traceorder = "normal"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Remoteness by jurisdiction, intent and sex (Table R) ------------------------------------------
  output$remotePlot <- renderPlotly({
    df_R <- readRDS("ABS_COD2018_remoteness.rds")
    if (input$jurR=="Australia") {
      regR <- input$raRA
    }
    else {
      regR <- input$raR
    }
    sub <- filter(df_R, jurisdiction == input$jurR & age_group == "All ages" &
                    sex == "All" & intent %in% input$codR & region %in% regR &
                    (year >= input$yrR[[1]] & year <= input$yrR[[2]]) )

    p <- ggplot(sub) + aes(x = year, colour = region, linetype = intent , group = 1) +
        geom_line() + labs(x = "Year", title=paste0(input$jurR,", All ages") ) +
        scale_colour_manual(values = regcols) +
        scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Region by intent"

    if (input$yaxR == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    if (input$yaxR == "rateht" | input$yaxR == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxR == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    if (input$yaxR == "ratem" | input$yaxR == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), "% (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$rate_m,.1))) +
        labs(y = "Percentage of drug-induced deaths among all deaths")
      if (input$yaxR == "ratemci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
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

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$remotePlotP <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    df_R <- readRDS("ABS_COD2018_remoteness.rds")
#    sub <- filter(df_R,DID>=50)
    if (input$jurRP == "Australia") {
      sub <- filter(df_R, intent == input$codRP & 
            age_group == input$ageRP &
            (year >= input$yrRP[[1]] & year <= input$yrRP[[2]]) & 
            sex == input$sexRP & jurisdiction == input$jurRP)
    }
    else {
      sub <- filter(df_R, intent == input$codRP & 
            age_group == input$ageRP & 
            (year >= input$yrRP[[1]] & year <= input$yrRP[[2]]) & 
            sex == "All" &
            jurisdiction == input$jurRP)
    }

#    if (input$yaxRP != "num") {
      if (input$jurRP == "Australia" & input$sexRP == "All" & input$ageRP == "All ages" ) {
        sub <- filter(sub, region!="Regional and Remote") %>%
          group_by(year, intent, sex, jurisdiction, age_group) %>% 
          distinct() %>%
          mutate(alldeaths = sum(n),
                 percent = round(n/sum(n)*100, 2),
                 region = factor(region, levels = c( "Remote and Very Remote",
                                                     "Outer Regional",
                                                     "Inner Regional",
                                                     "Major Cities"
                 )))
      }
      else {
        sub <- filter(sub, region=="Regional and Remote" | region=="Major Cities" ) %>%
          group_by(year, intent, sex, jurisdiction, age_group) %>% 
          distinct() %>%
          mutate(alldeaths = sum(n),
                 percent = round(n/sum(n)*100, 2),
                 region = factor(region, levels = c( "Regional and Remote",
                                                     "Major Cities"
                 )))
      }
      
      p <- ggplot(sub, aes(x=year, y=percent, fill=region, group=1, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Percent: ", percent, "%",
        "<br>Area: ", str_to_title(region),
        "<br>Jurisdiction: ", jurisdiction,
        "<br>Age: ", age_group,
        "<br>Sex: ", sex,
        "<br>Intent: ", str_to_title(intent)
      ))) + geom_area() + labs(y = "Percent of drug-induced deaths") +
      scale_fill_manual(values = regcols) #c("#d3d798", "#b3bd50", "#95a327", "#6a7d14"))
#     scale_fill_manual(values = c("#c1c870", "#748a34", "#465d02", "#1f3300"))
    # }
    # else {
    #   p <- ggplot(sub) + aes(x = year, colour = age_group, linetype = region , group = 1) +
    #     geom_line() + labs(x = "Year", title=paste0(input$jurRP,", intent: ") ) +
    #     scale_colour_manual(values = agecols) +
    #     scale_linetype_manual(values = regtype) +
    #     scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    #   Legend <- "Region by age"
    #   
    #   if (input$yaxR == "num") {
    #     p <- p + aes(y = n, text = paste0(
    #       "Year: ", year,
    #       "<br>Deaths: ", n,
    #       "<br>Region: ", str_to_title(region),
    #       "<br>Intent: ", str_to_title(intent)
    #       #        ,"<br>Sex: ", sex
    #     )) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
    #       labs(y = "Number of deaths")
    #   }
    #   
    # }
    p <- p + labs(x = "Year") +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title = element_blank(),
            panel.grid.minor.x = element_blank(),
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
        text = "Remoteness area", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })

  # All drugs by type plot (Table 12, 12b & 12c) ----------------------------------------------------------
  output$drugtypePlot <- renderPlotly({
    df_DT <- readRDS("ABS_COD2018_DT.rds")
  
    if (input$DropDT == "IntSx") {
      if (input$jurDT == "Australia") {
          if (input$sexDTI=="All") {
            codDTI=input$codDTI
          }
          else {
            codDTI=input$codDTIS
          }
          if (input$sexDTI!="MF") {
            sub <- subset(df_DT, subset = (intent==codDTI & drug %in% input$drugDTI
                          & age_group == input$ageDT & sex == input$sexDTI & jurisdiction == input$jurDT
                          & (year >= input$yrDT[[1]] & year <= input$yrDT[[2]] ) ) )
            p <- ggplot(sub) + labs(title=paste0(
            input$jurDT,", Age: ",input$ageDT,", Sex: ",input$sexDTI,", Intent: ",codDTI) )
          }
          else {
            sub <- subset(df_DT, subset = (intent==codDTI & drug %in% input$drugDTI
                          & age_group == input$ageDT & sex != "All" & jurisdiction == input$jurDT
                          & (year >= input$yrDT[[1]] & year <= input$yrDT[[2]] ) ) )
            p <- ggplot(sub) + facet_grid(cols = vars(sex) ) + labs(title=paste0(
              input$jurDT,", Age: ",input$ageDT,", Intent: ",codDTI) )
          }
      }
      if (input$jurDT != "Australia") {
          sub <- subset(df_DT, subset = (intent==input$codDTIJ & drug %in% input$drugDTI
                             & age_group == "All ages" & sex == "All" & jurisdiction == input$jurDT
                             & (year >= input$yrDT[[1]] & year <= input$yrDT[[2]] ) ) )
          p <- ggplot(sub) + labs(title=paste0(
            input$jurDT,", Age: All ages, Sex: All persons, Intent: ",input$codDTIJ) )
      }
        p <- p + aes(x = year, colour = drug, linetype = drug, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = Alldrugcols) +
        scale_linetype_manual(values = Alldrugtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      LO <- "h"
      Legend <- ""
      LY <- -0.15
    }
    
    if (input$DropDT == "Drug") {
      if (input$jurDT == "Australia") {
        sub <- subset(df_DT, subset = (intent %in% input$codDTD & drug == input$drugDTD & 
                    age_group == input$ageDT & sex %in% input$sexDTD & jurisdiction == input$jurDT &
                    (year >= input$yrDT[[1]] & year <= input$yrDT[[2]]) ) )
        p <- ggplot(sub) + labs(title=paste0(
          input$jurDT,", Age: ",input$ageDT,", Drug: ",input$drugDTD) ) +
          aes(x = year, colour = sex_intent, linetype = sex_intent, group = 1) +
          scale_colour_manual(values = sexcols) +
          scale_linetype_manual(values = sexcodtype)
        Legend <- "Sex & intent"
      }
      if (input$jurDT != "Australia") {
        sub <- subset(df_DT, subset = (intent %in% input$codDTD & drug == input$drugDTD & 
                     age_group == "All ages" & sex == "All" & jurisdiction == input$jurDT &
                     (year >= input$yrDT[[1]] & year <= input$yrDT[[2]]) ) )
        p <- ggplot(sub) + labs(title=paste0(
          input$jurDT,", Age: All ages, Sex: All persons, Drug: ",input$drugDTD) ) +
          aes(x = year, linetype = intent, group = 1) +
          scale_linetype_manual(values = codtype)
        Legend <- "Intent"
      }
      p <- p + geom_line() + labs(x = "Year") +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      LO <- "v"
      LY <- 0.95
    }
    
    if (input$yaxDT == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$yaxDT == "rateht" | input$yaxDT == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Drug: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxDT == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxDT == "ratem" | input$yaxDT == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Drug: ", drug,
        "<br>Intent: ", str_to_title(intent),
        "<br>Sex: ", sex,
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxDT == "ratemci") {
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

  # All drugs by type plot (Tables 12 & 12a) ----------------------------------------------------------
  output$drugtypePlotA <- renderPlotly({
    df_DT <- readRDS("ABS_COD2018_DT.rds")

#Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
    if (input$DropDTA == "Age_Intent") {
      sub <- subset(df_DT, subset = (intent==input$codDTAI & nature=="Underlying" & age_group == input$ageDTAI
                    & sex == "All" & jurisdiction == "Australia" & drug %in% input$drugDTAI
                    & (year >= input$yrDTA[[1]] & year <= input$yrDTA[[2]] ) ) )
    p <- ggplot(sub) + aes(x = year, colour = drug, linetype = drug, group = 1) +
      geom_line() + labs(x = "Year") +
      scale_colour_manual(values = Alldrugcols) +
      scale_linetype_manual(values = Alldrugtype) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    LO <- "h"
    Legend <- ""
    LY <- -0.15
    }
    if (input$DropDTA == "Drug") {
      sub <- subset(df_DT, subset = (intent %in% input$codDTAD & nature=="Underlying" & drug == input$drugDTAD &
                       age_group %in% input$ageDTAD & sex == "All" & jurisdiction == "Australia" &
                       (year >= input$yrDTA[[1]] & year <= input$yrDTA[[2]]) ) )
#      sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      LO <- "v"
      Legend <- "Age & intent"
      LY <- 0.95
    }

    if (input$yaxDTA == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", str_to_title(drug),
        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }

    else if (input$yaxDTA == "rateht" | input$yaxDTA == "ratehtci") {
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
      if (input$yaxDTA == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }

    else if (input$yaxDTA == "ratem" | input$yaxDTA == "ratemci") {
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
      if (input$yaxDTA == "ratemci") {
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

  # Plot O4 (Table 4) -----------------------------------------------------------------
  output$opPlot4 <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    #Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html  
    if (input$DropO4 == "Opioid") {
      sub <- subset(df_Op, subset = (sex == "All" & location == "Aus" & drug == input$drugO4O &
                                       intent %in% input$codO4 & age_group %in% input$ageO4O &
                                       (year >= input$yrO4[[1]] & year <= input$yrO4[[2]])))
      #      sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year", title=input$drugO4O) +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Age by intent"
    }
    
    else if (input$DropO4 == "Age") {
      sub <- subset(df_Op, subset = (sex == "All" & location == "Aus" & drug %in% input$drugO4A &
                                       intent %in% input$codO4 & age_group == input$ageO4A &
                                       (year >= input$yrO4[[1]] & year <= input$yrO4[[2]])))
      
      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = intent, group = 1) +
        geom_line() + labs(x = "Year", title=paste0("Age: ",input$ageO4A)) +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by intent"
    }
    
    if (input$yaxO4 == "num") {
      p <- p + aes(y = n,
                   text = paste0(
                     "Year: ", year,
                     "<br>Deaths: ", n,
                     "<br>Opioid: ", str_to_title(drug),
                     "<br>Intent: ", str_to_title(intent),
                     "<br>Age group: ", age_group)
      ) +  scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$yaxO4 == "rateht" | input$yaxO4 == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxO4 == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxO4 == "ratem" | input$yaxO4 == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxO4 == "ratemci") {
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
  # Plot O5 (Table 5)-----------------------------------------------------------------
  output$opPlot5 <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    # sub <- subset(df_Op, subset = (age_group == input$ageO5 & location == "Aus" &
    #    drug %in% input$drugO5 & intent == input$codO5 & sex %in% input$sexO5  &
    #     (year >= input$yrO5[[1]] & year <= input$yrO5[[2]])))
    
    if (input$DropO5 == "Opioid") {
      sub <- subset(df_Op, subset = (age_group == input$ageO5 & location == "Aus" &
                                       drug == input$drugO5O & intent %in% input$codO5O & sex %in% input$sexO5O  &
                                       (year >= input$yrO5[[1]] & year <= input$yrO5[[2]])))
      #      sub$sex_intent <- paste(sub$sex,sub$intent,sep=",")
      
      p <- ggplot(sub) + aes(x = year, colour = sex_intent, linetype = sex_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = sexcols) +
        scale_linetype_manual(values = sexcodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Sex by intent"
    }
    else if (input$DropO5 == "Intent") {
      sub <- subset(df_Op, subset = (age_group == input$ageO5 & location == "Aus" &
                                       drug %in% input$drugO5I & sex %in% input$sexO5I & intent ==input$codO5I & 
                                       (year >= input$yrO5[[1]] & year <= input$yrO5[[2]])))
      
      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = sex, group = 1) +
        geom_line() +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by sex"
    }
    else if (input$DropO5 == "Sex") {
      if (input$sexO5S != "MF") {
        sub <- subset(df_Op, subset = (age_group == input$ageO5 & location == "Aus" &
                                         drug %in% input$drugO5S & intent %in% input$codO5S & sex == input$sexO5S &
                                         (year >= input$yrO5[[1]] & year <= input$yrO5[[2]])))
        
        p <- ggplot(sub)
      }
      else {
        sub <- subset(df_Op, subset = (age_group == input$ageO5 & location == "Aus" &
                                         drug %in% input$drugO5S & intent %in% input$codO5S & sex != "All" &
                                         (year >= input$yrO5[[1]] & year <= input$yrO5[[2]])))
        
        p <- ggplot(sub) + facet_grid(cols = vars(sex) )
      }
      p <- p + aes(x = year, colour = drug, linetype = intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by intent"
    }
    
    if (input$yaxO5 == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Age group: ", age_group,
        "<br>Intent: ", str_to_title(intent),
        "<br>Opioid: ", drug,
        "<br>Sex: ", sex
      )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(x = "Year", y = "Number of deaths")
    }
    
    else if (input$yaxO5 == "rateht" | input$yaxO5 == "ratehtci") {
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
        labs(x = "Year", y = "Deaths per 100,000")
      if (input$yaxO5 == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxO5 == "ratem" | input$yaxO5 == "ratemci") {
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
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000")
      if (input$yaxO5 == "ratemci") {
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
  
  # Plot O6 (Table 6) -----------------------------------------------------------------
  output$opPlot6 <- renderPlotly({
    df_Op <- readRDS("ABS_COD2018_Op.rds")
    sub <- subset(df_Op, subset = (age_group == input$ageOD & drug == "All opioids" & jurisdiction == input$jurOD &
                                     intent %in% input$codOD & sex %in% input$sexOD &
                                     (year >= input$yrOD[[1]] & year <= input$yrOD[[2]])))
    #    sub$sex_intent <- paste(sub$sex,sub$intent,sep=",")
    
    p <- ggplot(sub) + aes(x = year, 
                           colour = sex_intent, linetype = sex_intent, group = 1) + #colour = location, linetype = sex,
      geom_line() + labs(x = "Year", title=paste0(input$jurOD,", ",input$ageOD) ) +
      scale_colour_manual(values = sexcols) + #statecols
      scale_linetype_manual(values = sexcodtype) + #sextype
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    
    if (input$yaxOD == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Intent: ", str_to_title(intent), 
        "<br>Jurisdiction: ", location,
        "<br>Sex: ", sex
      )
      ) + labs(y = "Number of deaths") +
        scale_y_continuous(limits = c(0, max(sub$n, 250)))
    }
    
    else if (input$yaxOD == "rateht" | input$yaxOD == "ratehtci") {
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
      if (input$yaxOD == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxOD == "ratem" | input$yaxOD == "ratemci") {
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
      if (input$yaxOD == "ratemci") {
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
  
# Opioids with other drugs (Table 7) -----------------------------------------------------------------
  output$WopPlot7 <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    if (input$DropW7 == "Drug") {
    sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                    drug == input$drugW7D & intent %in% input$codW7 & age_group %in% input$ageW7D & 
                    (year >= input$yrW7[[1]] & year <= input$yrW7[[2]]) & sex=="All")
#    sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
    
    p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        labs(x = "Year", title=paste0("All opioids with ",input$drugW7D) ) + geom_line() +
        scale_colour_manual(values = agecodcols) + scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
    Legend <- "Age by intent"
    }
    if (input$DropW7 == "Age") {
      sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                    drug %in% input$drugW7A & intent %in% input$codW7 & age_group == input$ageW7A & 
                      (year >= input$yrW7[[1]] & year <= input$yrW7[[2]]) & sex=="All")
      
      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = intent, group = 1) +
        labs(x = "Year") + geom_line() +
        scale_colour_manual(values = drugcols) + scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug with opioid <br>by intent"
    }
    
    if (input$yaxW7 == "num") {
        p <- p + aes(y = n, text = paste0(
                     "Year: ", year,
                     "<br>Deaths: ", n,
                     "<br>Drug with opioid: ", str_to_title(drug),
                     "<br>Intent: ", str_to_title(intent),
                     "<br>Sex: ", sex,
                     "<br>Age group: ", age_group ) ) + 
                scale_y_continuous(limits = c(0, max(sub$n, 250) ) ) +
                labs(y = "Number of deaths")
    }

    if (input$yaxW7 == "rateht" | input$yaxW7 == "ratehtci") {
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

        if (input$yaxW7 == "ratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$yaxW7 == "ratem" | input$yaxW7 == "ratemci") {
        p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Drug with opioid: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group) ) + 
               scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) + labs(y = "Deaths per 1,000,000")
        if (input$yaxW7 == "ratemci") {
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
  
  # Opioids and other drugs by sex (Table 8) ------------------------------------------
  output$WopPlot8 <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    if (input$DropW8 == "Sex") {
      if (input$sexW8S != "MF") {
        sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                        drug %in% input$drugW8 & intent %in% input$codW8S & 
                        sex == input$sexW8S & age_group == input$ageW8 & 
                        (year >= input$yrW8[[1]] & year <= input$yrW8[[2]] ) )
        p <- ggplot(sub)
      }
      else {
        sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
                    drug %in% input$drugW8 & intent %in% input$codW8S & 
                    sex != "All" & age_group == input$ageW8 & 
                    (year >= input$yrW8[[1]] & year <= input$yrW8[[2]] ) )
        p <- ggplot(sub) + facet_grid(cols = vars(sex))
      }
      p <- p + aes(x = year, colour = drug, linetype = intent, group = 1) + 
            geom_line() + 
            labs(x = "Year", title = paste0("Age group:",input$ageW8,"  Sex: ",input$sexW8S) ) +
            scale_colour_manual(values = drugcols) +
            scale_linetype_manual(values = codtype) +
            scale_x_continuous(breaks = function(x) unique(floor(pretty(x))) )
      Legend = "Drug with opioid<br>by intent"
    }
    if (input$DropW8 == "Intent") {
        sub <- filter(df_OpW, drug %in% input$drugW8 & intent == input$codW8I &
                      sex %in% input$sexW8I & age_group == input$ageW8 & 
                      (year >= input$yrW8[[1]] & year <= input$yrW8[[2]] ) )
        print(input$DropW8)
        p <- ggplot(sub) + aes(x = year, colour = drug, linetype = sex, group = 1) + 
              geom_line() + 
              labs(x = "Year", title = paste0("Age group:",input$ageW8,"  Intent: ",input$codW8I) ) +
              scale_colour_manual(values = drugcols) +
              scale_linetype_manual(values = sextype) +
              scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
        Legend = "Drug with opioid<br>by sex"
    }
    
    if (input$yaxW8 == "num") {
      p <- p + aes(y = n, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Drug with opioid: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
            )) +
            scale_y_continuous(limits = c(0, max(sub$n, 250))) + labs(y = "Number of deaths")
    }
    
    if (input$yaxW8 == "rateht" | input$yaxW8 == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Drug with opioid: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
           )) + geom_line() +
          scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
          labs(y = "Deaths per 100,000")
        if (input$yaxW8 == "ratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$yaxW8 == "ratem" | input$yaxW8 == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex
            )) + geom_line() +
            scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
            labs(y = "Deaths per 1,000,000")
        if (input$yaxW8 == "ratemci") {
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

  # Exclusive Opioids by age and intent (Table 10) -----------------------------------------------------------------
  output$EopPlot10 <- renderPlotly({
    df_EOp <- readRDS("ABS_COD2018_EOp.rds")
      if (input$Drop10 == "Opioid") {
      sub <- filter(df_EOp, sex == "All" & location == "Aus" & # nature == "Underlying" &
                  drug == input$drug10O & intent %in% input$cod10 & age_group %in% input$age10O &
                  (year >= input$yr10[[1]] & year <= input$yr10[[2]])) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
#        sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year", title=input$drug10O) +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Age by intent"
    }
    
    else if (input$Drop10 == "Age") {
      sub <- subset(df_EOp, subset = (sex == "All" & location == "Aus" & # nature == "Underlying" &
                  drug %in% input$drug10A & intent %in% input$cod10 & age_group == input$age10A &
                  (year >= input$yr10[[1]] & year <= input$yr10[[2]]))) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
      
      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = intent, group = 1) +
        geom_line() + labs(x = "Year", title=paste0("Age: ",input$age10A)) +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by intent"
    }
    
    if (input$yax10 == "num") {
      p <- p + aes(y = n,
                   text = paste0(
                     "Year: ", year,
                     "<br>Deaths: ", n,
                     "<br>Opioid: ", str_to_title(drug),
                     "<br>Intent: ", str_to_title(intent),
                     "<br>Age group: ", age_group)
      ) +  scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$yax10 == "rateht" | input$yax10 == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yax10 == "ratehtci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yax10 == "ratem" | input$yax10 == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yax10 == "ratemci") {
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

  # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11) ------------------------------------------
  output$EopPlot9 <- renderPlotly({
    df_EOp <- readRDS("ABS_COD2018_EOp.rds")
    if (input$DropE9 == "Intent") {
      sub <- filter(df_EOp, jurisdiction == input$jurE9 & age_group == input$ageE9 &
                    sex %in% input$sexE9I & intent == input$codE9I & drug %in% input$drugE9 & 
                    (year >= input$yrE9[[1]] & year <= input$yrE9[[2]]) ) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)

      p <- ggplot(sub) + aes(x = year, colour = drug, linetype = sex, group = 1) +
        geom_line() + labs(x = "Year", title=paste0(input$jurE9,", Age: ",input$ageE9," Intent: ",input$codE9I) ) +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by sex"
    }
    if (input$DropE9 == "Sex") {
      if (input$sexE9S != "MF") {
        sub <- filter(df_EOp, jurisdiction == input$jurE9 & age_group == input$ageE9 &
                        sex == input$sexE9S & intent %in% input$codE9S & drug %in% input$drugE9 & 
                        (year >= input$yrE9[[1]] & year <= input$yrE9[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
        p <- ggplot(sub) + labs(x = "Year", title=paste0(input$jurE9,", Age: ",input$ageE9," Sex: ",input$sexE9S) )
      }
      else {
        sub <- filter(df_EOp, jurisdiction == input$jurE9 & age_group == input$ageE9 &
                        sex != "All" & intent %in% input$codE9S & drug %in% input$drugE9 & 
                        (year >= input$yrE9[[1]] & year <= input$yrE9[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
        p <- ggplot(sub) + facet_grid(cols = vars(sex)) +
          labs(x = "Year", title=paste0(input$jurE9,", Age: ",input$ageE9) )
      }
      p <- p + aes(x = year, colour = drug, linetype = intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) +
        scale_x_continuous(breaks = function(x) unique(floor(pretty(x))))
      Legend <- "Drug by intent"
    }


    if (input$yaxE9 == "num") {
      p <- p + aes(y = n, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
          )) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
          labs(y = "Number of deaths")
    }
    
    if (input$yaxE9 == "rateht" | input$yaxE9 == "ratehtci") {
      p <- p + aes(y = rate_ht, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
            )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
            labs(y = "Deaths per 100,000")
        if (input$yaxE9 == "ratehtci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }

    if (input$yaxE9 == "ratem" | input$yaxE9 == "ratemci") {
      p <- p + aes(y = rate_m, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
            )) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
            labs(y = "Deaths per 1,000,000")
        if (input$yaxE9 == "ratemci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
  # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
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
  
  # Exclusive opioids as percents ------------------------------------------
  output$EopPlotP <- renderPlotly({
    #needs to be sorted [order(...)]
    #weird proportions plot from 2015 onwards because of duplicates by AUS
    #- need to make distinct
    df_EOp <- readRDS("ABS_COD2018_EOp.rds")
    sub <- filter(df_EOp, drug %in% c( "Exclusive illicit opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Illicit and pharmaceutical opioids",
                                   "Other and unspecified opioids") &
                    intent == input$codEP & 
                    age_group == input$ageEP & 
                    (year >= input$yrEP[[1]] & year <= input$yrEP[[2]]) & 
                    sex == input$sexEP &
                    jurisdiction == input$jurEP) %>% # & table!="10" & table!="18"
      group_by(year, intent, sex, jurisdiction, age_group) %>% 
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
      scale_fill_manual(values = drugcols) #c("#d3d798", "#b3bd50", "#95a327", "#6a7d14"))
#      scale_fill_manual(values = c("#c1c870", "#748a34", "#465d02", "#1f3300"))
    
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
