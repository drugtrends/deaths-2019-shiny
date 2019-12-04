#For ABS COD 2018 data received in Sept 2019
#N. Man
library(shiny)
library(shinyTree)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(shinycustomloader)
#for drug type plots
#https://stackoverflow.com/questions/47062532/multiple-lines-for-text-per-legend-label-in-ggplot2
library(stringr)

#reading RDS file format is apparently more efficient
#https://appsilon.com/fast-data-loading-from-files-to-r/
df <- readRDS("ABS_COD2018_All.rds")

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

# "15-24" = "#c09840",
# "25-34" = "#76b74b",
# "35-44" = "#6b8bcd",
# "45-54" = "#8d62ca",
# "55-64" = "#c75fa1",
# "65-74" = "#fc8d59",
# "75-84" = "#e34a33",
# "All ages" = "#000000",
# "15-64" = "#808080"
# )

agecodcols <- c(
  "15-24" = "#ff0000",
  "25-34" = "#aa0055",
  "35-44" = "#5500aa",
  "45-54" = "#0000ff",
  "55-64" = "#00aa80",
  "65-74" = "#00ff00",
  "75-84" = "#ffcc00",
  "All ages" = "#000000",
  "15-64" = "gray50",

  "All" = "#000000",
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
  "65-74 Male" = "#00ff00",
  "75-84 Male" = "#ffcc00",
  "All ages Male" = "#000000",
  "15-64 Male" = "808080",
  
  "15-24 Female" = "#ff0000",
  "25-34 Female" = "#aa0055",
  "35-44 Female" = "#5500aa",
  "45-54 Female" = "#0000ff",
  "55-64 Female" = "#00aa80",
  "65-74 Female" = "#00ff00",
  "75-84 Female" = "#ffcc00",
  "All ages Female" = "#000000",
  "15-64 Female" = "808080",
  
  "Accidental" = "#000000",
  "15-24 Accidental" = "#ff0000",
  "25-34 Accidental" = "#aa0055",
  "35-44 Accidental" = "#5500aa",
  "45-54 Accidental" = "#0000ff",
  "55-64 Accidental" = "#00aa80",
  "65-74 Accidental" = "#00ff00",
  "75-84 Accidental" = "#ffcc00",
  "All ages Accidental" = "#000000",
  "15-64 Accidental" = "#808080",
  
  "Intentional" = "#000000",
  "15-24 Intentional" = "#ff0000",
  "25-34 Intentional" = "#aa0055",
  "35-44 Intentional" = "#5500aa",
  "45-54 Intentional" = "#0000ff",
  "55-64 Intentional" = "#00aa80",
  "65-74 Intentional" = "#00ff00",
  "75-84 Intentional" = "#ffcc00",
  "All ages Intentional" = "#000000",
  "15-64 Intentional" = "#808080",
  
  "Undetermined" = "#000000",
  "15-24 Undetermined" = "#ff0000",
  "25-34 Undetermined" = "#aa0055",
  "35-44 Undetermined" = "#5500aa",
  "45-54 Undetermined" = "#0000ff",
  "55-64 Undetermined" = "#00aa80",
  "65-74 Undetermined" = "#00ff00",
  "75-84 Undetermined" = "#ffcc00",
  "All ages Undetermined" = "#000000",
  "15-64 Undetermined" = "#808080",
  
  "Other" = "#000000",
  "15-24 Other" = "#ff0000",
  "25-34 Other" = "#aa0055",
  "35-44 Other" = "#5500aa",
  "45-54 Other" = "#0000ff",
  "55-64 Other" = "#00aa80",
  "65-74 Other" = "#00ff00",
  "75-84 Other" = "#ffcc00",
  "All ages Other" = "#000000",
  "15-64 Other" = "#808080"
)

agecodtype <- c(
  "All" = 1,
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
  "15-64 Female" = 3,
  
  "Accidental" = 2,
  "15-24 Accidental" = 2,
  "25-34 Accidental" = 2,
  "35-44 Accidental" = 2,
  "45-54 Accidental" = 2,
  "55-64 Accidental" = 2,
  "65-74 Accidental" = 2,
  "75-84 Accidental" = 2,
  "All ages Accidental" = 2,
  "15-64 Accidental" = 2,

  "Intentional" = 3,
  "15-24 Intentional" = 3,
  "25-34 Intentional" = 3,
  "35-44 Intentional" = 3,
  "45-54 Intentional" = 3,
  "55-64 Intentional" = 3,
  "65-74 Intentional" = 3,
  "75-84 Intentional" = 3,
  "All ages Intentional" = 3,
  "15-64 Intentional" = 3,

  "Undetermined" = 4,
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

regcols <- c(
  "Major Cities" = "red",
  "Inner Regional" = "purple",
  "Outer Regional" = "blue",
  "Remote and Very Remote" = "forestgreen",
  "Regional and Remote" = "orange"
)

regcodcols <- c(
  "Major Cities,All" = "red",
  "Inner Regional,All" = "purple",
  "Outer Regional,All" = "blue",
  "Remote and Very Remote,All" = "forestgreen",
  "Regional and Remote,All" = "orange",

  "Major Cities,Accidental" = "red",
  "Inner Regional,Accidental" = "purple",
  "Outer Regional,Accidental" = "blue",
  "Remote and Very Remote,Accidental" = "forestgreen",
  "Regional and Remote,Accidental" = "orange"
)

regcodtype <- c(
  "Major Cities,All" = 1,
  "Inner Regional,All" = 1,
  "Outer Regional,All" = 1,
  "Remote and Very Remote,All" = 1,
  "Regional and Remote,All" = 1,
  
  "Major Cities,Accidental" = 2,
  "Inner Regional,Accidental" = 2,
  "Outer Regional,Accidental" = 2,
  "Remote and Very Remote,Accidental" = 2,
  "Regional and Remote,Accidental" = 2
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

opcols <- c(
  # "Alcohol"="purple",
  # "Amphetamines"="hotpink",
  # "Antidepressants"="orange",
  # "Antipsychotics & neuroleptics"="blue",
  # "Benzodiazepines"="chartreuse",
  # "4-aminophenol derivatives"="cyan",
  # "Antiepileptic & sedative-hypnotic drugs,\nunspecified"="forestgreen",
  # 
  # "All opioids" = "#000000",
  # "Heroin" = "orange",
  # "Opium" = "brown",
  # "Methadone" = "red",
  # "Synthetic opioids" = "blue",
  # "Natural & semi-synthetic opioids" = "purple",
# "Other & unspecified opioids" = "#CC6677",
# "Cocaine" = "brown",
  
  "Exclusive illicit opioids"="orange",
  "Exclusive pharmaceutical opioids"="#e34a33", #red
  "Illicit & pharmaceutical opioids"="#3300dd", #blue
  "Other & unspecified opioids"="#00bb33") #green
# "All opioids with alcohol"="#88CCEE",
# "All opioids with amphetamines"="#AA4499",
# "All opioids with antidepressants"="#117733",
# "All opioids with antipsychotics"="#999933",
# "All opioids with benzodiazepines"="#332288",
# "All opioids with paracetamol"="#CC6677",
# "All opioids with pregabalin"="#DDCC77" )

opwcodcols <- c(
  "Alcohol"="purple",
  "Amphetamines"="hotpink",
  "Antidepressants"="orange",
  "Antipsychotics & neuroleptics"="blue",
  "Benzodiazepines"="chartreuse",
  "4-aminophenol derivatives"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified"="forestgreen",

  "Alcohol,All"="purple",
  "Amphetamines,All"="hotpink",
  "Antidepressants,All"="orange",
  "Antipsychotics & neuroleptics,All"="blue",
  "Benzodiazepines,All"="chartreuse",
  "4-aminophenol derivatives,All"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,All"="forestgreen",
  
  "Alcohol,Female"="purple",
  "Amphetamines,Female"="hotpink",
  "Antidepressants,Female"="orange",
  "Antipsychotics & neuroleptics,Female"="blue",
  "Benzodiazepines,Female"="chartreuse",
  "4-aminophenol derivatives,Female"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Female"="forestgreen",

  "Alcohol,Male"="purple",
  "Amphetamines,Male"="hotpink",
  "Antidepressants,Male"="orange",
  "Antipsychotics & neuroleptics,Male"="blue",
  "Benzodiazepines,Male"="chartreuse",
  "4-aminophenol derivatives,Male"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Male"="forestgreen",
  
  "Alcohol,Accidental"="purple",
  "Amphetamines,Accidental"="hotpink",
  "Antidepressants,Accidental"="orange",
  "Antipsychotics & neuroleptics,Accidental"="blue",
  "Benzodiazepines,Accidental"="chartreuse",
  "4-aminophenol derivatives,Accidental"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Accidental"="forestgreen",

  "Alcohol,Intentional"="purple",
  "Amphetamines,Intentional"="hotpink",
  "Antidepressants,Intentional"="orange",
  "Antipsychotics & neuroleptics,Intentional"="blue",
  "Benzodiazepines,Intentional"="chartreuse",
  "4-aminophenol derivatives,Intentional"="cyan",
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Intentional"="forestgreen",
  
  "Alcohol,Undetermined"="purple",
  "Antidepressants,Undetermined"="orange",
  "Antipsychotics & neuroleptics,Undetermined"="blue",
  "Benzodiazepines,Undetermined"="chartreuse",
  "4-aminophenol derivatives,Undetermined"="cyan")

opcodcols <- c("All opioids" = "#000000",
  "Heroin" = "orange",
  "Opium" = "brown",
  "Methadone" = "red",
  "Synthetic opioids" = "blue",
  "Natural & semi-synthetic opioids" = "purple",
  "Other & unspecified opioids"="#00bb33",
  
  "All opioids,All" = "#000000",
  "Heroin,All" = "orange",
  "Opium,All" = "brown",
  "Methadone,All" = "red",
  "Synthetic opioids,All" = "blue",
  "Natural & semi-synthetic opioids,All" = "purple",
  "Other & unspecified opioids,All"="#00bb33",
  
  "All opioids,Female" = "#000000",
  "Heroin,Female" = "orange",
  "Opium,Female" = "brown",
  "Methadone,Female" = "red",
  "Synthetic opioids,Female" = "blue",
  "Natural & semi-synthetic opioids,Female" = "purple",
  "Other & unspecified opioids,Female"="#00bb33",
  
  "All opioids,Male" = "#000000",
  "Heroin,Male" = "orange",
  "Opium,Male" = "brown",
  "Methadone,Male" = "red",
  "Synthetic opioids,Male" = "blue",
  "Natural & semi-synthetic opioids,Male" = "purple",
  "Other & unspecified opioids,Male"="#00bb33",
  
  "All opioids,Accidental" = "#000000",
  "Heroin,Accidental" = "orange",
  "Methadone,Accidental" = "red",
  "Synthetic opioids,Accidental" = "blue",
  "Natural & semi-synthetic opioids,Accidental" = "purple",
  "Other & unspecified opioids,Accidental"="#00bb33",
  
  "All opioids,Intentional" = "#000000",
  "Heroin,Intentional" = "orange",
  "Methadone,Intentional" = "red",
  "Synthetic opioids,Intentional" = "blue",
  "Natural & semi-synthetic opioids,Intentional" = "purple",

  "All opioids,Undetermined" = "#000000",
  "Heroin,Undetermined" = "orange",
  "Methadone,Undetermined" = "red",
  "Synthetic opioids,Undetermined" = "blue",
  "Natural & semi-synthetic opioids,Undetermined" = "purple",
  
  "Exclusive illicit opioids,All"="orange",
  "Exclusive pharmaceutical opioids,All"="#e34a33", #red
  "Illicit & pharmaceutical opioids,All"="#3300dd", #blue
  
  "Exclusive illicit opioids,Female"="orange",
  "Exclusive pharmaceutical opioids,Female"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Female"="#3300dd", #blue
  
  "Exclusive illicit opioids,Male"="orange",
  "Exclusive pharmaceutical opioids,Male"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Male"="#3300dd", #blue
  
  "Exclusive illicit opioids,Accidental"="orange",
  "Exclusive pharmaceutical opioids,Accidental"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Accidental"="#3300dd", #blue
  
  "Exclusive illicit opioids,Intentional"="orange",
  "Exclusive pharmaceutical opioids,Intentional"="#e34a33", #red
  "Illicit & pharmaceutical opioids,Intentional"="#3300dd" #blue
#,"Other & unspecified opioids"="#00bb33" #green
  )

opwcodtype <- c("Alcohol"=1,
  "Amphetamines"=1,
  "Antidepressants"=1,
  "Antipsychotics & neuroleptics"=1,
  "Benzodiazepines"=1,
  "4-aminophenol derivatives"=1,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified"=1,
  
  "Alcohol,All"=1,
  "Amphetamines,All"=1,
  "Antidepressants,All"=1,
  "Antipsychotics & neuroleptics,All"=1,
  "Benzodiazepines,All"=1,
  "4-aminophenol derivatives,All"=1,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,All"=1,
  
  "Alcohol,Female"=2,
  "Amphetamines,Female"=2,
  "Antidepressants,Female"=2,
  "Antipsychotics & neuroleptics,Female"=2,
  "Benzodiazepines,Female"=2,
  "4-aminophenol derivatives,Female"=2,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Female"=2,
  
  "Alcohol,Male"=3,
  "Amphetamines,Male"=3,
  "Antidepressants,Male"=3,
  "Antipsychotics & neuroleptics,Male"=3,
  "Benzodiazepines,Male"=3,
  "4-aminophenol derivatives,Male"=3,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Male"=3,

  "Alcohol,Accidental"=2,
  "Amphetamines,Accidental"=2,
  "Antidepressants,Accidental"=2,
  "Antipsychotics & neuroleptics,Accidental"=2,
  "Benzodiazepines,Accidental"=2,
  "4-aminophenol derivatives,Accidental"=2,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Accidental"=2,
  
  "Alcohol,Intentional"=3,
  "Amphetamines,Intentional"=3,
  "Antidepressants,Intentional"=3,
  "Antipsychotics & neuroleptics,Intentional"=3,
  "Benzodiazepines,Intentional"=3,
  "4-aminophenol derivatives,Intentional"=3,
  "Antiepileptic & sedative-hypnotic drugs,\nunspecified,Intentional"=3,
  
  "Alcohol,Undetermined"=4,
  "Antidepressants,Undetermined"=4,
  "Antipsychotics & neuroleptics,Undetermined"=4,
  "Benzodiazepines,Undetermined"=4,
  "4-aminophenol derivatives,Undetermined"=4)

opcodtype <- c("All opioids,All" = 1,
  "Heroin,All" = 1,
  "Opium,All" = 1,
  "Methadone,All" = 1,
  "Synthetic opioids,All" = 1,
  "Natural & semi-synthetic opioids,All" = 1,
  "Other & unspecified opioids,All" = 1,
  
  "All opioids,Female" = 2,
  "Heroin,Female" = 2,
  "Opium,Female" = 2,
  "Methadone,Female" = 2,
  "Synthetic opioids,Female" = 2,
  "Natural & semi-synthetic opioids,Female" = 2,
  "Other & unspecified opioids,Female" = 2,
  
  "All opioids,Male" = 3,
  "Heroin,Male" = 3,
  "Opium,Male" = 3,
  "Methadone,Male" = 3,
  "Synthetic opioids,Male" = 3,
  "Natural & semi-synthetic opioids,Male" = 3,
  "Other & unspecified opioids,Male" = 3,
  
  "All opioids,Accidental" = 2,
  "Heroin,Accidental" = 2,
  "Methadone,Accidental" = 2,
  "Synthetic opioids,Accidental" = 2,
  "Natural & semi-synthetic opioids,Accidental" = 2,
  "Other & unspecified opioids,Accidental" = 2,
  
  "All opioids,Intentional" = 3,
  "Heroin,Intentional" = 3,
  "Methadone,Intentional" = 3,
  "Synthetic opioids,Intentional" = 3,
  "Natural & semi-synthetic opioids,Intentional" = 3,

  "All opioids,Undetermined" = 4,
  "Heroin,Undetermined" = 4,
  "Methadone,Undetermined" = 4,
  "Synthetic opioids,Undetermined" = 4,
  "Natural & semi-synthetic opioids,Undetermined" = 4,

  "Exclusive illicit opioids,All"=1,
  "Exclusive pharmaceutical opioids,All"=1,
  "Illicit & pharmaceutical opioids,All"=1,
  
  "Exclusive illicit opioids,Female"=2,
  "Exclusive pharmaceutical opioids,Female"=2,
  "Illicit & pharmaceutical opioids,Female"=2,
  
  "Exclusive illicit opioids,Male"=3,
  "Exclusive pharmaceutical opioids,Male"=3,
  "Illicit & pharmaceutical opioids,Male"=3,
  
  "Exclusive illicit opioids,Accidental"=2,
  "Exclusive pharmaceutical opioids,Accidental"=2,
  "Illicit & pharmaceutical opioids,Accidental"=2,
  
  "Exclusive illicit opioids,Intentional"=3,
  "Exclusive pharmaceutical opioids,Intentional"=3,
  "Illicit & pharmaceutical opioids,Intentional"=3
)

# https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
#e6194B, #3cb44b, #ffe119, #4363d8, #f58231, #911eb4, #42d4f4, #f032e6, #bfef45, #fabebe, 
#469990, #e6beff, #9A6324, #fffac8, #800000, #aaffc3, #808000, #ffd8b1, #000075, #a9a9a9
# dtcols <- c(
#   "ALCOHOL"="#469990",
#   "OPIOIDS"="#000000",
#   "heroin"="#000000",
#   "natural & semi-synthetic opioids"="#000000",
#   "methadone"="#000000",
#   "synthetic opioids"="#000000",
#   "ANTIDEPRESSANTS"="#CC6677",
#   "tricyclic & tetracyclic antidepressants"="#CC6677",
#   "other & unspecified antidepressants"="#CC6677",
#   "CANNABINOIDS"="#999933",
#   "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS"="#117733",
#   "barbiturates"="#117733",
#   "benzodiazepines"="#117733",
#   "antiepileptic & sedative-hypnotic drugs, unspecified"="#117733",
#   "ANTIPSYCHOTICS & NEUROLEPTICS"="#88CCEE",
#   "other & unspecified antipsychotics"="#88CCEE",
#   "COCAINE"="#DDCC77",
#   "NONOPIOID ANALGESICS"="#332288",
#   "4-aminophenol derivatives"="#332288",
#   "other nonsteroidal anti-inflammatory drugs"="#332288",
#   "AMPHETAMINES"="#AA4499"
# )

dtcols <- c(
  "ALCOHOL"="purple",
  "OPIOIDS"="#000000",
  "heroin"="#000000",
  "natural & semi-synthetic opioids"="#000000",
  "methadone"="#000000",
  "synthetic opioids"="#000000",
  "ANTIDEPRESSANTS"="orange",
  "tricyclic & tetracyclic antidepressants"="orange",
  "other & unspecified antidepressants"="orange",
  "CANNABINOIDS"="forestgreen",
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS"="chartreuse",
  "barbiturates"="chartreuse",
  "benzodiazepines"="chartreuse",
  "antiepileptic & sedative-hypnotic drugs,\nunspecified"="chartreuse",
  "ANTIPSYCHOTICS & NEUROLEPTICS"="blue",
  "other & unspecified antipsychotics"="blue",
  "COCAINE"="brown",
  "NONOPIOID ANALGESICS"="cyan",
  "4-aminophenol derivatives"="cyan",
  "other nonsteroidal anti-inflammatory drugs"="cyan",
  "AMPHETAMINES"="hotpink"
)

dttype <- c(
  "ALCOHOL"=1,
  "OPIOIDS"=1,
  "heroin"=2,
  "natural & semi-synthetic opioids"=3,
  "methadone"=4,
  "synthetic opioids"=5,
  "ANTIDEPRESSANTS"=1,
  "tricyclic & tetracyclic antidepressants"=2,
  "other & unspecified antidepressants"=3,
  "CANNABINOIDS"=1,
  "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS"=1,
  "barbiturates"=2,
  "benzodiazepines"=3,
  "antiepileptic & sedative-hypnotic drugs,\nunspecified"=4,
  "ANTIPSYCHOTICS & NEUROLEPTICS"=1,
  "other & unspecified antipsychotics"=2,
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
    if(query1 == "tab=allPage"){
      updateTabsetPanel(session, inputId = "Plot", selected = "allPage")
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
  #                "natural & semi-synthetic opioids",
  #                "methadone",
  #                "synthetic opioids",
  #                "AMPHETAMINES",
  #                "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
  #                "barbiturates",
  #                "benzodiazepines",
  #                "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
  #                "ANTIDEPRESSANTS",
  #                "tricyclic & tetracyclic antidepressants",
  #                "other & unspecified antidepressants",
  #                "ANTIPSYCHOTICS & NEUROLEPTICS",
  #                "other & unspecified antipsychotics (e.g. quetiapine)",
  #                "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
  #                "4-aminophenol derivatives (e.g. paracetamol)",
  #                "other nonsteroidal anti-inflammatory drugs",
  #                "Alcohol",
  #                "Cocaine",
  #                "CANNABINOIDS"
  #            ),
  #            selected = c("Alcohol") )
  # 
  # if (input$DropDT == "Age_Intent")
  #   yDT <- checkboxGroupInput("drugDT", "Drug:",
  #           choices = c(
  #               "OPIOIDS",
  #               "heroin",
  #               "natural & semi-synthetic opioids",
  #               "methadone",
  #               "synthetic opioids",
  #               "AMPHETAMINES",
  #               "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",
  #               "barbiturates",
  #               "benzodiazepines",
  #               "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)",
  #               "ANTIDEPRESSANTS",
  #               "tricyclic & tetracyclic antidepressants",
  #               "other & unspecified antidepressants",
  #               "ANTIPSYCHOTICS & NEUROLEPTICS",
  #               "other & unspecified antipsychotics (e.g. quetiapine)",
  #               "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS",
  #               "4-aminophenol derivatives (e.g. paracetamol)",
  #               "other nonsteroidal anti-inflammatory drugs",
  #               "Alcohol",
  #               "Cocaine",
  #               "CANNABINOIDS"
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

####For shinyTree - need to work out how to set default values
#   output$ageA <- renderTree({
#     list(
#       "10-year age groups" = list("15-24"="",
#         "25-34"="", "35-44"="", "45-54"="",
#         "55-64"="", "65-74"="", "75-84"=""),
#       "15-64" = "",
#       "All ages" = "")
#   })
#   output$codA <- renderTree({
#     # intA=list("Intent:"=list(
#     #   'All' = '1',
#     #   'Accidental' = '2') )
#     intA=list(
#       'All' = '1',
#       'Accidental' = '2')
# #Trying: https://stackoverflow.com/questions/55258868/shinytree-with-default-selected-value
#      attr(intA[[1]]['All'],"stselected")=TRUE #doesn't work for selecting default but doesn't break it either
#      attr(intA[[1]],"stopened")=TRUE
#      intA
#   })

  # Amphetamine plot (Table 2) --------------------------------------------------------
  output$amphetaminePlot <- renderPlotly({
    df_Stim <- readRDS("ABS_COD2018_Stim.rds")
####For shinyTree
#    print( dim.data.frame(get_selected(input$ageA,c("classid"))) )
#    print( get_selected(input$codA) )
  # ageA <- get_selected(input$ageA,c("classid"))
  # codA <- get_selected(input$codA,c("classid"))
    sub <- subset(df_Stim, subset = (jurisdiction == "Australia" & sex == "All" & 
           drug == "Amphetamines" & nature == "Underlying" & intent %in% input$codA & 
           age_group %in% input$ageA & (year >= input$yrA[[1]] & year <= input$yrA[[2]])))
    p <- ggplot(sub) + geom_line() + labs(x = "Year") +
      aes(x = year, group = 1) +
      scale_colour_manual(values = agecodcols) +
      scale_x_continuous(breaks = seq(input$yrA[[1]],input$yrA[[2]],2) )
    if (dim.data.frame(input$codA)[2]==1) {
      p <- p + aes(colour = age_group) +
        labs(title = paste0("Intent: ",input$codA) )
      Legend <- "Age"
    }
    else {
      p <- p + aes(colour = age_intent, linetype = age_intent) +
        scale_linetype_manual(values = agecodtype)
      Legend <- "Age by intent"
    }

    if (input$yaxA == "num") {
      p <- p + aes(y = n, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250)))+
        labs(y = "Number of deaths")
    }
    
    else if (input$yaxA == "r5" | input$yaxA == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxA == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxA == "r6" | input$yaxA == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
#          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxA == "r6ci") {
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))  })
  
  # Cocaine plot (Table 3) ------------------------------------------------------------
  output$cocainePlot <- renderPlotly({
    df_Stim <- readRDS("ABS_COD2018_Stim.rds")
    sub <- subset(df_Stim, subset = (drug == "Cocaine" & intent %in% input$codC & nature == "Underlying" &
                                       age_group %in% input$ageC & sex == "All" & jurisdiction == "Australia" &
                                       (year >= input$yrC[[1]] & year <= input$yrC[[2]])))
    
    p <- ggplot(sub) + aes(x = year, linetype=age_intent, colour=age_intent, group=1) +
      geom_line() + labs(x = "Year") +
      scale_linetype_manual(values = agecodtype) +
      scale_colour_manual(values = agecodcols) +
      scale_x_continuous(breaks = seq(input$yrC[[1]],input$yrC[[2]],2) )
    
    if (input$yaxC == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Intent: ", str_to_title(intent),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      )
      ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }
    
    else if (input$yaxC == "r5" | input$yaxC == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxC == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxC == "r6" | input$yaxC == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Age group: ", age_group
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxC == "r6ci") {
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
        text = "Age by intent", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
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
      p <- ggplot(sub) + aes(x = year) + geom_line() +
        scale_colour_manual(values = agecodcols) +
        scale_x_continuous(breaks = seq(input$yrAll[[1]],input$yrAll[[2]],2) ) #function(x) unique(floor(pretty(x,high.u.bias = 0,u5.bias = .1))))
      if (dim.data.frame(input$sexAllI)[2]==1) {
        p <- p + aes(colour = age_group, group = 1) + 
          labs(title=paste0(input$jurAll,", Intent: ",input$codAllI,", Sex: ",input$sexAllI) )
        Legend <- "Age"
      }
      else {
        p <- p + aes(colour = age_sex, linetype = age_sex, group = 1) +
          scale_linetype_manual(values = agecodtype) +
          labs(title=paste0(input$jurAll,", Intent: ",input$codAllI) )
        Legend <- "Age & sex"
      }
    }
    else if (input$DropAll == "Sex") {
      if (input$sexAllS != "MF") {
        sub <- subset(df, subset = (age_group %in% input$ageAll & jurisdiction == input$jurAll &
                                      intent %in% input$codAllS & sex == input$sexAllS &
                                      (year >= input$yrAll[[1]] & year <= input$yrAll[[2]]) ) )
        p <- ggplot(sub)
        Title <- paste0(input$jurAll,", Sex: ",input$sexAllS)
      }
      else {
        sub <- subset(df, subset = (age_group %in% input$ageAll & jurisdiction == input$jurAll &
                                      intent %in% input$codAllS & sex != "All" &
                                      (year >= input$yrAll[[1]] & year <= input$yrAll[[2]]) ) )
        p <- ggplot(sub) + labs(title=input$jurAll) +
          facet_grid(cols = vars(sex))
        Title <- input$jurAll
      }

      if (dim.data.frame(input$codAllS)[2]==1) {
        p <- p + aes(colour = age_group, group = 1) + 
          scale_colour_manual(values = agecodcols) +
          labs(title=paste0(Title,", Intent: ",input$codAllS) )
        Legend <- "Age"
      }
      else if (dim.data.frame(input$ageAll)[2]==1) {
        p <- p + aes(linetype = intent, group = 1) + 
          scale_linetype_manual(values = agecodtype) +
          labs(title=paste0(Title,", Age: ",input$ageAll) )
        Legend <- "Intent"
      }
      else {
        p <- p + aes(colour = age_intent, linetype = age_intent, group = 1) + 
          scale_colour_manual(values = agecodcols) +
          scale_linetype_manual(values = agecodtype) +
          labs(title= Title )
        Legend <- "Age by intent"
      }
      p <- p + aes(x = year) + geom_line() +
        scale_x_continuous(breaks = seq(input$yrAll[[1]],input$yrAll[[2]],2))
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
    
    else if (input$yaxAll == "r5" | input$yaxAll == "r5ci") {
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
      if (input$yaxAll == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxAll == "r6" | input$yaxAll == "r6ci") {
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
      if (input$yaxAll == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill="#a9e7bb"),
      strip.text = element_text(color="#000000", face = "bold") )
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-drug-induced-deaths-australia-1997-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "#000000")
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top", traceorder = "normal"), margin = list(b = 80)) %>% 
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

    p <- ggplot(sub) + aes(x = year, colour = reg_intent, linetype = reg_intent , group = 1) +
        geom_line() + labs(x = "Year", title=paste0(input$jurR,", All ages") ) +
        scale_colour_manual(values = regcodcols) +
        scale_linetype_manual(values = regcodtype) +
        scale_x_continuous(breaks = seq(input$yrR[[1]],input$yrR[[2]],2) )
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
    
    if (input$yaxR == "r5" | input$yaxR == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxR == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    if (input$yaxR == "r6" | input$yaxR == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), "% (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Region: ", str_to_title(region),
        "<br>Intent: ", str_to_title(intent)
#        ,"<br>Sex: ", sex
      )) + scale_y_continuous(limits = c(0, max(sub$rate_m,.1))) +
        labs(y = "Percentage of drug-induced deaths among all deaths")
      if (input$yaxR == "r6ci") {
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
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
    #     scale_colour_manual(values = agecodcols) +
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
      scale_x_continuous(breaks = seq(input$yrRP[[1]],input$yrRP[[2]],2) )
    
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })

  # All drugs by type plot (Table 12, 12b & 12c) ----------------------------------------------------------
  output$DTPlot <- renderPlotly({
    df_DT <- readRDS("ABS_COD2018_DT.rds")
#  print(str_wrap(dtcols,50))
#  dtcols <- str_wrap(dtcols,50)
#  print(str_wrap("ANTIEPILEPTIC, SEDATIVE-HYPNOTIC & ANTIPARKINSONISM DRUGS",50))
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
        scale_colour_manual(values = dtcols) +
        scale_linetype_manual(values = dttype) +
        scale_x_continuous(breaks = seq(input$yrDT[[1]],input$yrDT[[2]],2) )
      Legend <- "" #"Related drug"
      LO <- "v"
      LY <- 0.99
#      LO <- "h"
#      LY <- -0.15
      if (input$sexDTI!="All") {
        validate(need(nrow(sub) > 0, "Please select All ages for age range."))
      }
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
        Legend <- "Sex by intent"
#Warning about length > 1 but still works for the purpose because first element is used
        if (input$sexDTD!="All") {
          validate(need(nrow(sub) > 0, "Please select All ages for age range."))
        }
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
        scale_x_continuous(breaks = seq(input$yrDT[[1]],input$yrDT[[2]],2) )
      LO <- "v"
      LY <- 0.99
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
    
    else if (input$yaxDT == "r5" | input$yaxDT == "r5ci") {
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
      if (input$yaxDT == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxDT == "r6" | input$yaxDT == "r6ci") {
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
      if (input$yaxDT == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
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
      layout(legend = list(orientation = LO, y = LY, yanchor = "top"), margin = list(b = 80) ) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # All drugs by type plot (Tables 12 & 12a) ----------------------------------------------------------
  output$DTPlotA <- renderPlotly({
    df_DT <- readRDS("ABS_COD2018_DT.rds")

#Based on: https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html
    if (input$DropDTA == "Age_Intent") {
      sub <- subset(df_DT, subset = (intent==input$codDTAI & nature=="Underlying" & age_group == input$ageDTAI
                    & sex == "All" & jurisdiction == "Australia" & drug %in% input$drugDTAI
                    & (year >= input$yrDTA[[1]] & year <= input$yrDTA[[2]] ) ) )
    p <- ggplot(sub) + aes(x = year, colour = str_wrap(drug,50), linetype = str_wrap(drug,50), group = 1) +
      geom_line() + labs(x = "Year") +
      scale_colour_manual(values = dtcols) +
      scale_linetype_manual(values = dttype) +
      scale_x_continuous(breaks = seq(input$yrDTA[[1]],input$yrDTA[[2]],2) )
    Legend <- "Related drug"
    LO <- "v"
    LY <- 0.99
#    LO <- "h"
#    LY <- -0.15
    }
    if (input$DropDTA == "Drug") {
      sub <- subset(df_DT, subset = (intent %in% input$codDTAD & nature=="Underlying" & drug == input$drugDTAD &
                       age_group %in% input$ageDTAD & sex == "All" & jurisdiction == "Australia" &
                       (year >= input$yrDTA[[1]] & year <= input$yrDTA[[2]]) ) )
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = seq(input$yrDTA[[1]],input$yrDTA[[2]],2) )
      Legend <- "Age by intent"
      LO <- "v"
      LY <- 0.99
    }

    if (input$yaxDTA == "num") {
      p <- p + aes(y = n, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Drug: ", str_to_title(drug),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(y = "Number of deaths")
    }

    else if (input$yaxDTA == "r5" | input$yaxDTA == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Drug: ", str_to_title(drug),
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxDTA == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }

    else if (input$yaxDTA == "r6" | input$yaxDTA == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Drug: ", drug,
#        "<br>Nature: ", str_to_title(nature),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age: ", age_group
      )
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxDTA == "r6ci") {
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
        y = 0.99 , yanchor = "bottom",
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
#      layout(legend = list(orientation = "h", y = -0.15, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      layout(legend = list(orientation = LO, y = LY, yanchor = "top"), margin = list(b = 80) ) %>%
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
  ####For user-defined year intervals
  #     yr <- as.numeric(input$xaxO4)
  #     yr <- (input$yrO4[[2]]-input$yrO4[[1]])/yr
      p <- ggplot(sub) + aes(x = year, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = agecodcols) +
        scale_x_continuous(breaks = seq(input$yrO4[[1]],input$yrO4[[2]],2) )
  ####For user-defined year intervals
  #     function(x) unique(floor( pretty(x,n=yr) ) )
      Title <- input$drugO4O

      if (dim.data.frame(input$codO4)[2]==1 | (dim.data.frame(input$codO4)[2]==2 & input$cod2O4==2) ) {
        p <- p + aes(colour = age_group)
        Legend <- "Age"
      }
      else {
        p <- p + aes(colour = age_intent, linetype = age_intent) +
          scale_linetype_manual(values = agecodtype)
        Legend <- "Age by intent"
      }
    }
    
    else if (input$DropO4 == "Age") {
      sub <- subset(df_Op, subset = (sex == "All" & location == "Aus" & drug %in% input$drugO4A &
                 intent %in% input$codO4 & age_group == input$ageO4A &
                 (year >= input$yrO4[[1]] & year <= input$yrO4[[2]])))
      p <- ggplot(sub) + aes(x = year, group = 1) +
        geom_line() + labs(x = "Year") +
        scale_colour_manual(values = opcodcols) +
        scale_x_continuous(breaks = seq(input$yrO4[[1]],input$yrO4[[2]],2) )
  #     function(x) unique(floor( pretty(x,n=yr) ) )
      Title <- paste0("Age: ",input$ageO4A)

      if (dim.data.frame(input$codO4)[2]==1 | (dim.data.frame(input$codO4)[2]==2 & input$cod2O4==2) ) {
        p <- p + aes(colour = drug)
        Legend <- "Drug"
      }
      else {
        p <- p + aes(colour = op_intent, linetype = op_intent) +
          scale_linetype_manual(values = opcodtype)
        Legend <- "Drug by intent"
      }
    }
    
    if (dim.data.frame(input$codO4)[2]==1) {
      Title <- paste0(Title,"; Intent: ",input$codO4)
    }
    if (dim.data.frame(input$codO4)[2]==2 & input$cod2O4==2) {
      p <- p + facet_grid(cols = vars(intent) )
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
    
    else if (input$yaxO4 == "r5" | input$yaxO4 == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yaxO4 == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxO4 == "r6" | input$yaxO4 == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yaxO4 == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + labs(title = Title) + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
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

      p <- ggplot(sub) + aes(x = year, colour = sex_intent, linetype = sex_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = sexcols) +
        scale_linetype_manual(values = sexcodtype) +
        scale_x_continuous(breaks = seq(input$yrO5[[1]],input$yrO5[[2]],2) )
      Legend <- "Sex by intent"
    }
    else if (input$DropO5 == "Intent") {
      sub <- subset(df_Op, subset = (age_group == input$ageO5 & location == "Aus" &
                                       drug %in% input$drugO5I & sex %in% input$sexO5I & intent ==input$codO5I & 
                                       (year >= input$yrO5[[1]] & year <= input$yrO5[[2]])))
      
      p <- ggplot(sub) + aes(x = year, colour = op_sex, linetype = op_sex, group = 1) +
        geom_line() +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$yrO5[[1]],input$yrO5[[2]],2) )
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
      p <- p + aes(x = year, colour = op_intent, linetype = op_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$yrO5[[1]],input$yrO5[[2]],2) )
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
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$n, 250))) +
        labs(x = "Year", y = "Number of deaths")
    }
    
    else if (input$yaxO5 == "r5" | input$yaxO5 == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Age group: ", age_group,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
        "<br>Opioid: ", drug,
        "<br>Sex: ", sex
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(x = "Year", y = "Deaths per 100,000")
      if (input$yaxO5 == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxO5 == "r6" | input$yaxO5 == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Age group: ", age_group,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Intent: ", str_to_title(intent),
        "<br>Opioid: ", drug,
        "<br>Sex: ", sex
      ) ) +
        scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(x = "Year", y = "Deaths per 1,000,000")
      if (input$yaxO5 == "r6ci") {
        p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
      }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
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
      scale_x_continuous(breaks = seq(input$yrOD[[1]],input$yrOD[[2]],2) )
    
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
    
    else if (input$yaxOD == "r5" | input$yaxOD == "r5ci") {
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
      if (input$yaxOD == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yaxOD == "r6" | input$yaxOD == "r6ci") {
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
      if (input$yaxOD == "r6ci") {
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
# Opioids with other drugs (Table 7) -----------------------------------------------------------------
  output$OpWPlot7 <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    sub <- filter(df_OpW, #jurisdiction == "Australia" & word(drug, start = 1, end = 3) == "All opioids with" &
            intent %in% input$codW7 & (year >= input$yrW7[[1]] & year <= input$yrW7[[2]]) & sex=="All")
    if ( is.null(input$showW7) ) {
      sub <- filter(sub, set == "OpioidW")
    }

    if (input$DropW7 == "Drug") {
    sub <- filter(sub, drug == input$drugW7D & age_group %in% input$ageW7D)

    p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        labs(x = "Year", title=paste0("All opioids with ",input$drugW7D) ) + geom_line() +
        scale_colour_manual(values = agecodcols) + scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = seq(input$yrW7[[1]],input$yrW7[[2]],2) )
    Legend <- "Age by intent"
    }
    if (input$DropW7 == "Age") {
      sub <- filter(sub, drug %in% input$drugW7A & age_group == input$ageW7A)
      
      p <- ggplot(sub) + aes(x = year, colour = str_wrap(op_intent,50), linetype = str_wrap(op_intent,50), group = 1) +
        labs(x = "Year") + geom_line() +
        scale_colour_manual(values = opwcodcols) + scale_linetype_manual(values = opwcodtype) +
        scale_x_continuous(breaks = seq(input$yrW7[[1]],input$yrW7[[2]],2) )
      Legend <- "Drug by intent"
    }
    
    if ( is.character(input$showW7) ) {
      p <- p + aes(alpha=primary) +
        scale_alpha_manual(values = c(0.3 , 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    if (input$yaxW7 == "num") {
        p <- p + aes(y = n, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group ) ) + 
              scale_y_continuous(limits = c(0, max(sub$n, 250) ) ) +
              labs(y = "Number of deaths")
    }
    if (input$yaxW7 == "r5" | input$yaxW7 == "r5ci") {
        p <- p + aes(y = rate_ht, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group
            )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
            labs(y = "Deaths per 100,000")

        if (input$yaxW7 == "r5ci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$yaxW7 == "r6" | input$yaxW7 == "r6ci") {
        p <- p + aes(y = rate_m, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Sex: ", sex,
               "<br>Age group: ", age_group) ) + 
               scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) + labs(y = "Deaths per 1,000,000")
        if (input$yaxW7 == "r6ci") {
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
        y = 0.99 , yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                      "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                      "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Opioids and other drugs by sex (Table 8) ------------------------------------------
  output$OpWPlot8 <- renderPlotly({
    df_OpW <- readRDS("ABS_COD2018_OpW.rds")
    sub <- filter(df_OpW, drug %in% input$drugW8 & age_group == input$ageW8 & 
          (year >= input$yrW8[[1]] & year <= input$yrW8[[2]]) )
    if ( is.null(input$showW8) ) {
      sub <- filter(sub, set == "OpioidW")
    }

    if (input$DropW8 == "Sex") {
      if (input$sexW8S != "MF") {
        sub <- filter(sub, intent %in% input$codW8S & sex == input$sexW8S )
        p <- ggplot(sub)
      }
      else {
        sub <- filter(sub, intent %in% input$codW8S & sex != "All" )
        p <- ggplot(sub) + facet_grid(cols = vars(sex))
      }
      p <- p + aes(x = year, colour = str_wrap(op_intent,50), 
            linetype = str_wrap(op_intent,50), group = 1) + geom_line() + 
            labs(x = "Year", title = paste0("Age group:",input$ageW8,"  Sex: ",input$sexW8S) ) +
            scale_colour_manual(values = opwcodcols) +
            scale_linetype_manual(values = opwcodtype) +
            scale_x_continuous(breaks = seq(input$yrW8[[1]],input$yrW8[[2]],2)  )
      Legend <- "Drug by intent"
    }
    if (input$DropW8 == "Intent") {
        sub <- filter(sub, intent == input$codW8I & sex %in% input$sexW8I )
        p <- ggplot(sub) + aes(x = year, colour = op_sex, linetype = op_sex, group = 1) + 
              geom_line() + 
              labs(x = "Year", title = paste0("Age group:",input$ageW8,"  Intent: ",input$codW8I) ) +
              scale_colour_manual(values = opwcodcols) +
              scale_linetype_manual(values = opwcodtype) +
              scale_x_continuous(breaks = seq(input$yrW8[[1]],input$yrW8[[2]],2) )
        Legend <- "Drug by sex"
    }

    if ( is.character(input$showW8) ) {
      p <- p + aes(alpha=primary) +
        scale_alpha_manual(values = c(0.3 , 1) )
      Legend <- paste0(Legend,"<br> by death data type")
    }

    if (input$yaxW8 == "num") {
      p <- p + aes(y = n, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
            )) +
            scale_y_continuous(limits = c(0, max(sub$n, 250))) + labs(y = "Number of deaths")
    }
    
    if (input$yaxW8 == "r5" | input$yaxW8 == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
               "Year: ", year,
               "<br>Deaths: ", n,
               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
               "<br>Drug: ", str_to_title(drug),
               "<br>Intent: ", str_to_title(intent),
               "<br>Age group: ", age_group,
               "<br>Sex: ", sex
           )) + geom_line() +
          scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
          labs(y = "Deaths per 100,000")
        if (input$yaxW8 == "r5ci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }
    
    if (input$yaxW8 == "r6" | input$yaxW8 == "r6ci") {
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
        if (input$yaxW8 == "r6ci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Set theme, remove default legend title and vertical gridlines
    p <- p + theme_light() + theme(legend.title = element_blank()) +
          theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background = element_rect(fill="#a9e7bb"),
          strip.text = element_text(color="#000000", face = "bold") )
    
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                    "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                    "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })

  # Exclusive Opioids by age and intent (Table 10) -----------------------------------------------------------------
  output$OpEPlot10 <- renderPlotly({
    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
      if (input$Drop10 == "Opioid") {
      sub <- filter(df_OpE, sex == "All" & location == "Aus" &
                  drug == input$drug10O & intent %in% input$cod10 & age_group %in% input$age10O &
                  (year >= input$yr10[[1]] & year <= input$yr10[[2]])) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
#        sub$age_intent <- paste(sub$age_group,sub$intent,sep=",")
      
      p <- ggplot(sub) + aes(x = year, colour = age_intent, linetype = age_intent, group = 1) +
        geom_line() + labs(x = "Year", title=input$drug10O) +
        scale_colour_manual(values = agecodcols) +
        scale_linetype_manual(values = agecodtype) +
        scale_x_continuous(breaks = seq(input$yr10[[1]],input$yr10[[2]],2) )
      Legend <- "Age by intent"
    }
    
    else if (input$Drop10 == "Age") {
      sub <- subset(df_OpE, subset = (sex == "All" & location == "Aus" &
                  drug %in% input$drug10A & intent %in% input$cod10 & age_group == input$age10A &
                  (year >= input$yr10[[1]] & year <= input$yr10[[2]]))) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
      
      p <- ggplot(sub) + aes(x = year, colour = op_intent, linetype = op_intent, group = 1) +
        geom_line() + labs(x = "Year", title=paste0("Age: ",input$age10A)) +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$yr10[[1]],input$yr10[[2]],2) )
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
    
    else if (input$yax10 == "r5" | input$yax10 == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
        labs(y = "Deaths per 100,000")
      if (input$yax10 == "r5ci") {
        p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
      }
    }
    
    else if (input$yax10 == "r6" | input$yax10 == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
        "Year: ", year,
        "<br>Deaths: ", n,
        "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
        "<br>Opioid: ", str_to_title(drug),
        "<br>Intent: ", str_to_title(intent),
        "<br>Age group: ", age_group)
      ) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
        labs(y = "Deaths per 1,000,000")
      if (input$yax10 == "r6ci") {
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11) ------------------------------------------
  output$OpEPlot9 <- renderPlotly({
    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
    if (input$DropE9 == "Intent") {
      sub <- filter(df_OpE, jurisdiction == input$jurE9 & age_group == input$ageE9 &
                    sex %in% input$sexE9I & intent == input$codE9I & drug %in% input$drugE9 & 
                    (year >= input$yrE9[[1]] & year <= input$yrE9[[2]]) ) %>%
        distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)

      p <- ggplot(sub) + aes(x = year, colour = op_sex, linetype = op_sex, group = 1) +
        geom_line() + labs(x = "Year", title=paste0(input$jurE9,", Age: ",input$ageE9," Intent: ",input$codE9I) ) +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$yrE9[[1]],input$yrE9[[2]],2) )
      Legend <- "Drug by sex"
    }
    if (input$DropE9 == "Sex") {
      if (input$sexE9S != "MF") {
        sub <- filter(df_OpE, jurisdiction == input$jurE9 & age_group == input$ageE9 &
                        sex == input$sexE9S & intent %in% input$codE9S & drug %in% input$drugE9 & 
                        (year >= input$yrE9[[1]] & year <= input$yrE9[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
        p <- ggplot(sub) + labs(x = "Year", title=paste0(
          input$jurE9,", Age: ",input$ageE9," Sex: ",input$sexE9S) )
      }
      else {
        sub <- filter(df_OpE, jurisdiction == input$jurE9 & age_group == input$ageE9 &
                        sex != "All" & intent %in% input$codE9S & drug %in% input$drugE9 & 
                        (year >= input$yrE9[[1]] & year <= input$yrE9[[2]]) ) %>%
          distinct(drug, year, intent, sex, location, age_group, .keep_all = TRUE)
        p <- ggplot(sub) + facet_grid(cols = vars(sex)) +
          labs(x = "Year", title=paste0(input$jurE9,", Age: ",input$ageE9) )
      }
      p <- p + aes(x = year, colour = op_intent, linetype = op_intent, group = 1) +
        geom_line() +
        scale_colour_manual(values = opcodcols) +
        scale_linetype_manual(values = opcodtype) +
        scale_x_continuous(breaks = seq(input$yrE9[[1]],input$yrE9[[2]],2) )
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
    
    if (input$yaxE9 == "r5" | input$yaxE9 == "r5ci") {
      p <- p + aes(y = rate_ht, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
            )) + scale_y_continuous(limits = c(0, max(sub$rate_ht_ucl, 2.5))) +
            labs(y = "Deaths per 100,000")
        if (input$yaxE9 == "r5ci") {
          p <- p + geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0)
        }
    }

    if (input$yaxE9 == "r6" | input$yaxE9 == "r6ci") {
      p <- p + aes(y = rate_m, text = paste0(
                       "Year: ", year,
                       "<br>Deaths: ", n,
                       "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
                       "<br>Drug: ", str_to_title(drug),
                       "<br>Intent: ", str_to_title(intent),
                       "<br>Sex: ", sex
            )) + scale_y_continuous(limits = c(0, max(sub$rate_m_ucl, 25))) +
            labs(y = "Deaths per 1,000,000")
        if (input$yaxE9 == "r6ci") {
          p <- p + geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0)
        }
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
  # Set theme, remove default legend title and vertical gridlines (NB: need to have grid codes last)
    p <- p + theme_light() + theme(legend.title = element_blank()) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(fill="#a9e7bb"),
            strip.text = element_text(color="#000000", face = "bold") )
    
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                             "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                             "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Exclusive opioids as percents ------------------------------------------
  output$OpEPlotP <- renderPlotly({
    #needs to be sorted [order(...)]
    #weird proportions plot from 2015 onwards because of duplicates by AUS
    #- need to make distinct
    df_OpE <- readRDS("ABS_COD2018_OpE.rds")
    sub <- filter(df_OpE, drug %in% c( "Exclusive illicit opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Illicit & pharmaceutical opioids",
                                   "Other & unspecified opioids") &
                    intent == input$codEP & 
                    age_group == input$ageEP & 
                    (year >= input$yrEP[[1]] & year <= input$yrEP[[2]]) & 
                    sex == input$sexEP &
                    jurisdiction == input$jurEP) %>% # & table!="10" & table!="18"
      group_by(year, intent, sex, jurisdiction, age_group) %>% 
      distinct() %>%
      mutate(alldeaths = sum(n),
             percent = round(n/sum(n)*100, 2),
             drug = factor(drug, levels = c( "Other & unspecified opioids",
                                             "Illicit & pharmaceutical opioids",
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
      scale_x_continuous(breaks = seq(input$yrEP[[1]],input$yrEP[[2]],2) ) +
      scale_fill_manual(values = opcols) #c("#d3d798", "#b3bd50", "#95a327", "#6a7d14"))
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
        y = 0.99, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.99, yanchor = "top"), margin = list(b = 80)) %>%
      config(displaylogo=F, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                        "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                        "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })
  
}
