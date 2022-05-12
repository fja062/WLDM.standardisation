#build for R version 4.0.4

# instal Rtools40
# activate path to the Rtools40
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

options(java.parameters = "-Xmx8g")

library("golem")
library("shinyjs")
library("shiny")
library("shinydashboard")
library("DT")
library("readxl")
library("tidyr")
library("dplyr")
library("stringr")
library("reshape2")
library("xlsx")
library("uuid")
library("janitor")
library("uuid")
library("DiagrammeR")
library("DiagrammeRsvg")
library("rsvg")
library("htmltools")
library("lubridate")
library("anytime")
library("testthat")
library("sf")



run_app()


#If package installation fails
#for (file in list.files("./R", pattern="*.R")) {
#source(paste0("./R/", file)) }
#run_app()
