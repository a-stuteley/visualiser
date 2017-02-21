#Mortality Data Visualisation - User Interface Core
#xx/xx/16
#Written by Alexander Stuteley

library(shiny)

#Constructing title
shinyUI(fluidPage(
  #code to split age group columns for each web browser, purely aesthetic
    tags$head(
      tags$style(HTML(".multicol {
                      -webkit-column-count: 3; /* For use on Chrome, Safari, and Opera */
                      -moz-column-count: 3; /* For use on Firefox */
                      column-count: 3;
                      }")
      )
    ),
    
    #running title output
    titlePanel(title),
    
    #constructing top row of ui, calling output methods in server.R
    fluidRow(
      column(3, uiOutput('issues')),
      column(3, uiOutput('geog_and_agegroup')),
      column(3, uiOutput('populations')),
      column(3, uiOutput('standard'))
    ),
    
    #constructing second row of ui, again calling output methods in server.R
    fluidRow(
      column(3, uiOutput('pop1')),
      column(3,
             #the following panels condition on the input from the top row
             #they query the input to the numpop selector in first panel
             conditionalPanel(condition = "input.numpop > 1",
                    uiOutput('pop2')
             )
      ),
      column(3,
             conditionalPanel(condition = "input.numpop > 2",
                    uiOutput('pop3')
             )
      ),
      column(3,
             conditionalPanel(condition = "input.numpop > 3",
                    uiOutput('pop4')
             )
      )
    ),
  
    #constructing area for results to generate
    mainPanel(
      checkboxInput('proj', 'Project trends?',value = FALSE),
      #text warning if your selected populations are not mutually exclusive
      textOutput("intra"),
      #the following sets tabs that can be pressed to change the visualisation
      uiOutput("conditions")
    )
  )
)