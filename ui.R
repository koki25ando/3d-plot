library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(data.table)

fluidPage(
  titlePanel("Size Shooting Ability relationship of NBA Plyears"),
  plotlyOutput(outputId = "scatter")
  )