#Exploring Mortality Data Beta
#V1.71 - presentation 12/11/16
#Written by Alexander Stuteley
#options(warn = -1)
#loading needed packages
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(reshape)

shinyServer(function(input, output, session) {
  
  output$conditions <- renderUI({
    if (input$proj == FALSE){   
      #the three implemented plots are the next three tabs
      tabsetPanel(
        #table output is the default
        tabPanel("Table",       
                 #button to generate output
                 
                 actionButton('updater',"Update graphs",width = "850px",color = "blue"),
                 
                 tableOutput("table")),
        tabPanel("Graph - Rate per 100,000",  plotOutput("plot1"), textOutput("msg1")),
        tabPanel("Graph - Attributable Risk", plotOutput("plot3"), textOutput("msg3")),
        tabPanel("Graph - Relative Risk",     plotOutput("plot2"), textOutput("msg2")),
        
        #the summary tab provides an easy to download summary of the rates
        tabPanel("Summary",                   selectInput('format',"Select a format",
                                                          c("by pop","by statistic"), 
                                                          selected = "by pop"),
                 tableOutput("summary")
        ),
        tabPanel("B.Y.O. input help", textOutput("help1"),
                 uiOutput('csvemp'),
                 img(src='tab1.PNG', align = "left"), renderText({cat("\n")}),
                 img(src='tab2.PNG', align = "left")
                 
        ),
        #buttons to download outputs, first the table, then graphs, and lastly the summary
        uiOutput('csv'),
        uiOutput('png1'),
        uiOutput('png3'),
        uiOutput('png2'),
        uiOutput('txt')
      )
    } else {
      tabsetPanel(
        #table output is the default
        tabPanel("Table",       
                 #button to generate output
                 
                 actionButton('updater',"Update graphs",width = "850px",color = "blue"),
                 
                 tableOutput("table")),
        tabPanel("Projected mortality", plotOutput("plotp")),
        #  #buttons to download outputs, first the table, then graphs, and lastly the summary
        uiOutput('csv')
      )
    }
  })
  
  #each panel set up in ui.R is rendered in this file
  output$issues <- renderUI({
    #This panel gives selection for country, issue, sub-issue if applicable and 
    #capability to import data through file upload
    wellPanel(
      #core dropdown menu code, repeated many times
      selectInput('country','Select a country:',countrytext,selected=countrytext[1]),
      #core ui conditioning code, repeated many times
      conditionalPanel(
        #condition below is how dropdown field is filled, repeated use of this
        condition = "input.country == 'New Zealand' "
        #,        selectInput('issue1','Select an issue:',issuetext,selected=issuetext[1])
      ),
      conditionalPanel(
        condition = "input.country == 'Cook Islands' ",
        #,selectInput('issuec1','Select an issue:',cookissuetext,selected=cookissuetext[1]),
        p("As population data is only available for 2006 and 2011, they have been extended as an estimate 2 years either side of the Census years")
      ),
      radioButtons('numpop','Select a number of populations:',poptext,selected=poptext[1],inline=TRUE)#,
      
      #br(),
      #br(),
      #p("Space to add projection options")
    )
  })
  
  #observe({
  #  if (input$proj) {
      
  #  }
  #})
  
  output$geog_and_agegroup <- renderUI({
    wellPanel(
      #This panel gives geographic selection using same drop down and conditioning
      selectInput('geog','Select a geographic type:',geotext,selected="National"),
      conditionalPanel(
        condition = "input.geog == 'National' "
      ),
      conditionalPanel(
        condition = "input.geog == 'Sub-National' ",
        selectInput('subgeog','Select a sub-national type:',subgeotext,selected="Regional Council"),
        conditionalPanel(
          condition = "input.subgeog == 'Regional Council' ",
          selectInput('reg','Select a Regional Council:',regtext,selected="Northland")
        ),
        conditionalPanel(
          condition = "input.subgeog == 'TLA' ",
          selectInput('tla','Select a TLA:',tlatext,selected="Far North")
        ),
        #placeholders
        conditionalPanel(
          condition = "input.subgeog == 'DHB' ",
          selectInput('dhb','Select a District Health Board:',dhbtext,selected="Northland")
        ),
        conditionalPanel(
          condition = "input.subgeog == 'Rohe' ",
          p("In development")
        )
      )
      
    )
  })
  
  output$populations <- renderUI({
    #selects age grouping, core checkbox code
    wellPanel(
      selectInput('agegroup','Select an age grouping:',grouptext,selected="Total"),
      conditionalPanel(
        condition = "input.agegroup == '5-year bands' ",
        tags$div(class = "multicol", checkboxGroupInput('selectgroups','Select which bands to include:',selectgrouptext,
                                                        selected=selectgrouptext))
      )
    )
    
  })
  bands = function(){
    if(is.null(input$agegroup) || input$agegroup != '5-year bands'){
      bandle <- 18
    }else{
      bandle <- 0
      for(i in 1:length(input$selectgroups)){
        if(is.element(input$selectgroups[i], selectgrouptext)){
          bandle <- bandle + 1
        }
      }
    }
    bandle
  }
  
  #function to get Lower year of the issue/ethnicity's range
  getLower <- function(group, issue){
    #mortality options for year here
    #if(input$proj == TRUE) {return(getUpper(group, issue) - 15)}
    #if(input$proj == TRUE) return(proj_lower)
    if(is.element(issue,mort)){
      if(is.null(group) || is.na(group)){
      }else if(group == "Maori"){
        if(input$geog == "National"){
          maori_nat_range_lower
        }else if(input$subgeog == "Regional Council"){
          maori_reg_range_lower
        }else if(input$subgeog == "TLA"){
          maori_tla_range_lower
        }else if(input$subgeog == "DHB"){
          maori_dhb_range_lower
        } 
      }else if(group == "Non-Maori"){
        if(input$geog == "National"){
          maori_nat_range_lower
        }else if(input$subgeog == "Regional Council"){
          maori_reg_range_lower
        }else if(input$subgeog == "TLA"){
          maori_tla_range_lower
        }else if(input$subgeog == "DHB"){
          maori_dhb_range_lower
        } 
      }else if(group == "Pacific"){
        if(input$geog == "National"){
          pacific_nat_range_lower
        }else if(input$subgeog == "Regional Council"){
          pacific_reg_range_lower
        }else if(input$subgeog == "TLA"){
          pacific_tla_range_lower
        }else if(input$subgeog == "DHB"){
          pacific_dhb_range_lower
        } 
      }else if(group == "Asian"){
        if(input$geog == "National"){
          asian_nat_range_lower
        }else if(input$subgeog == "Regional Council"){
          asian_reg_range_lower
        }else if(input$subgeog == "TLA"){
          asian_tla_range_lower
        }else if(input$subgeog == "DHB"){
          asian_dhb_range_lower
        } 
      }else if(group == "Total"){
        if(input$geog == "National"){
          total_nat_range_lower
        }else if(input$subgeog == "Regional Council"){
          total_reg_range_lower
        }else if(input$subgeog == "TLA"){
          total_tla_range_lower
        }else if(input$subgeog == "DHB"){
          total_dhb_range_lower
        } 
      }
      #other issues ranges are here, B.Y.O. is not yet dynamic
    }else if(issue == "Cancer - Gastric Death"){
      gastric_death_range_lower
    }else if(issue == "Cancer - Gastric Incidence"){
      gastric_inc_range_lower
    }else if(issue == "Cancer - Priority"){
      priority_range_lower
    }else if(issue == "B.Y.O."){
      byo_lower
    }else if(input$country == "Cook Islands"){
      cook_range_lower
    }
    
  }
  #function to get Upper year of the issue/ethnicity's range
  getUpper <- function(group, issue){
    #if(input$proj == TRUE) return(proj_upper)
    if(is.element(issue,mort)){
      if(is.null(group) || is.na(group)){
      }else if(group == "Maori"){
        if(input$geog == "National"){
          maori_nat_range_upper
        }else if(input$subgeog == "Regional Council"){
          maori_reg_range_upper
        }else if(input$subgeog == "TLA"){
          maori_tla_range_upper
        }else if(input$subgeog == "DHB"){
          maori_dhb_range_upper
        } 
      }else if(group == "Non-Maori"){
        if(input$geog == "National"){
          maori_nat_range_upper
        }else if(input$subgeog == "Regional Council"){
          maori_reg_range_upper
        }else if(input$subgeog == "TLA"){
          maori_tla_range_upper
        }else if(input$subgeog == "DHB"){
          maori_dhb_range_upper
        } 
      }else if(group == "Pacific"){
        if(input$geog == "National"){
          pacific_nat_range_upper
        }else if(input$subgeog == "Regional Council"){
          pacific_reg_range_upper
        }else if(input$subgeog == "TLA"){
          pacific_tla_range_upper
        }else if(input$subgeog == "DHB"){
          pacific_dhb_range_upper
        } 
      }else if(group == "Asian"){
        if(input$geog == "National"){
          asian_nat_range_upper
        }else if(input$subgeog == "Regional Council"){
          asian_reg_range_upper
        }else if(input$subgeog == "TLA"){
          asian_tla_range_upper
        }else if(input$subgeog == "DHB"){
          asian_dhb_range_upper
        } 
      }else{
        if(input$geog == "National"){
          total_nat_range_upper
        }else if(input$subgeog == "Regional Council"){
          total_reg_range_upper
        }else if(input$subgeog == "TLA"){
          total_tla_range_upper
        }else if(input$subgeog == "DHB"){
          total_dhb_range_upper
        } 
      }
    }else if(issue == "Cancer - Gastric Death"){
      gastric_death_range_upper
    }else if(issue == "Cancer - Gastric Incidence"){
      gastric_inc_range_upper
    }else if(issue == "Cancer - Priority"){
      priority_range_upper
    }else if(issue == "B.Y.O."){
      byo_upper
    }else if(input$country == "Cook Islands"){
      cook_range_upper
    }
  }
  
  #below are panels that render depending on how many populations are selected
  output$pop1 <- renderUI({
    wellPanel(
      #each gives ethnicity, gender, year and ability to reduce variance/errors
      h3("Population 1"),
      conditionalPanel(
        #issue selection same as for pop 1 but is included in these panels
        condition = "input.country == 'New Zealand' ",
        selectInput("issue1","Select issue",issuetext),#, selected = "Cancer - Gastric Death"),
        conditionalPanel(
          condition = "input.issue1 == 'Cancer - Priority' ",
          selectInput('priority1','Select a priority cancer:',prioritytext,selected="Colorectal")
        ),
        #p("priority data will be used")
        conditionalPanel(
          condition = "input.issue1 == 'B.Y.O.' ",
          fileInput('file1', 'Choose CSV File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv'))
        )
      ),
      conditionalPanel(
        condition = "input.country == 'Cook Islands' ",
        selectInput("issuec1","Re-select issue",cookissuetext)
      ),
      selectInput("ethnic1","Ethnicity",eth),
      selectInput("gender1","Gender",gen),
      sliderInput("year1","Year range (start and end)",
                                min = range_lower,
                                max = range_upper,
                                step = 1,
                                sep = "",
                                value=c(range_lower,range_upper)),
      #p("The ethnicity selection in this population will allow further population standards based on Census counts of the selected ethnicity"),
      selectInput('extend1','Select an option to increase accuracy:',extext,selected="None"),
      conditionalPanel(
        condition = "input.extend1 == 'Census year +/- 1' "#,
        #p("If starting year selected is a Census year then Census year + 2 is used, if ending year selected is a Census year then Census year - 2 is used")
      ),
      conditionalPanel(
        condition = "input.issue1 == 'Mortality - Total' || input.issue1 == 'Mortality - Pearce Amenable' || input.issue1 == 'Mortality - Pearce Non-Amenable' || input.issue1 == 'Mortality - Ravymont Non-Avoidable' || input.issue1 == 'Mortality - Raymont Avoidable' || input.issue1 == 'Mortality - Ravymont Non-Amenable' || input.issue1 == 'Mortality - Raymont Amenable' || input.issue1 == 'Mortality - Tobias Non-Amenable' || input.issue1 == 'Mortality - Tobias Avoidable' || input.issue1 == 'Mortality - MoH Non-Amenable' || input.issue1 == 'Mortality - MoH Amenable'",
        selectInput('dep1','Select a NZdep stratum if desired:',deptext,selected="None")
        #p("If starting year selected is a Census year then Census year + 2 is used, if ending year selected is a Census year then Census year - 2 is used")
      )
    )
  })
  #this section reacts dynamically to the above selections for when you change
  #ethnicity or gender it reacts appropriately
  observe({
    if(is.null(input$ethnic1) || is.na(input$ethnic1) || is.null(input$issue1) || is.na(input$issue1) || is.null(input$extend1) || is.na(input$extend1)){
    }else{
      updateSliderInput(session=session,inputId="year1",
                                "Year range (start and end)",
                                min = getLower(input$ethnic1,input$issue1),
                                max = getUpper(input$ethnic1,input$issue1),
                                step = 1,
                                value = c(getLower(input$ethnic1,input$issue1),getUpper(input$ethnic1,input$issue1)))
      lowerlim <<- getLower(input$ethnic1,input$issue1)
      upperlim <<- getUpper(input$ethnic1,input$issue1)
   }
  })
  #rendering for pop 2
  output$pop2 <- renderUI({
    wellPanel(
      h3("Population 2"),
      conditionalPanel(
        #issue selection same as for pop 1 but is included in these panels
        condition = "input.country == 'New Zealand' ",
        selectInput("issue2","Select issue",issuetext),#, selected = "Cancer - Gastric Death"),
        conditionalPanel(
          condition = "input.issue2 == 'Cancer - Priority' ",
          selectInput('priority2','Select a priority cancer:',prioritytext,selected="Colorectal")
        ),
        #p("priority data will be used")
        conditionalPanel(
          condition = "input.issue2 == 'B.Y.O.' ",
          fileInput('file2', 'Choose CSV File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv'))
        )
      ),
      conditionalPanel(
        condition = "input.country == 'Cook Islands' ",
        selectInput("issuec2","Re-select issue",cookissuetext)
      ),
      selectInput("ethnic2","Ethnicity",eth),#,selected = "Maori"),
      selectInput("gender2","Gender",gen),
      sliderInput("year2","Year range (start and end)",
                                min = range_lower,
                                max = range_upper,
                                step = 1,
                                sep = "",
                                value=c(range_lower,range_upper)),
      
      
      selectInput('extend2','Select an option to increase accuracy:',extext,selected="None"),
      conditionalPanel(
        condition = "input.extend == '3-year Rolling Average' "
        
      ),
      conditionalPanel(
        condition = "input.issue2 == 'Mortality - Total' || input.issue2 == 'Mortality - Pearce Amenable' || input.issue2 == 'Mortality - Pearce Non-Amenable' || input.issue2 == 'Mortality - Ravymont Non-Avoidable' || input.issue2 == 'Mortality - Raymont Avoidable' || input.issue2 == 'Mortality - Ravymont Non-Amenable' || input.issue2 == 'Mortality - Raymont Amenable' || input.issue2 == 'Mortality - Tobias Non-Amenable' || input.issue2 == 'Mortality - Tobias Avoidable' || input.issue2 == 'Mortality - MoH Non-Amenable' || input.issue2 == 'Mortality - MoH Amenable'",
        selectInput('dep2','Select a NZdep stratum if desired:',deptext,selected="None")
        #p("If starting year selected is a Census year then Census year + 2 is used, if ending year selected is a Census year then Census year - 2 is used")
      )
    )
  })
  observe({
    if(is.null(input$ethnic2) || is.na(input$ethnic2) || is.null(input$issue2) || is.na(input$issue2) || is.null(input$extend2) || is.na(input$extend2)){
    }else{
      updateSliderInput(session=session,inputId="year2",
                        "Year range (start and end)",
                        min = getLower(input$ethnic2,input$issue2),
                        max = getUpper(input$ethnic2,input$issue2),
                        step = 1,
                        value = c(getLower(input$ethnic2,input$issue2),getUpper(input$ethnic2,input$issue2)))
      lowerlim <<- getLower(input$ethnic2,input$issue2)
      upperlim <<- getUpper(input$ethnic2,input$issue2)
    }
  })
  output$pop3 <- renderUI({
    wellPanel(
      h3("Population 3"),
      conditionalPanel(
        #issue selection same as for pop 1 but is included in these panels
        condition = "input.country == 'New Zealand' ",
        selectInput("issue3","Select issue",issuetext),#, selected = "Cancer - Gastric Death"),
        conditionalPanel(
          condition = "input.issue3 == 'Cancer - Priority' ",
          selectInput('priority3','Select a priority cancer:',prioritytext,selected="Colorectal")
        ),
        #p("priority data will be used")
        conditionalPanel(
          condition = "input.issue3 == 'B.Y.O.' ",
          fileInput('file3', 'Choose CSV File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv'))
        )
      ),
      conditionalPanel(
        condition = "input.country == 'Cook Islands' ",
        selectInput("issuec3","Re-select issue",cookissuetext)
      ),
      selectInput("ethnic3","Ethnicity",eth),#,selected = "Non-Maori"),
      selectInput("gender3","Gender",gen),
      sliderInput("year3","Year range (start and end)",
                                min = range_lower,
                                max = range_upper,
                                step = 1,
                                sep = "",
                                value=c(range_lower,range_upper)),
      
      selectInput('extend3','Select an option to increase accuracy:',extext,selected="None"),
      conditionalPanel(
        condition = "input.extend == '3-year Rolling Average' "
        
        #p("set variable to National")
      ),
      conditionalPanel(
        condition = "input.issue3 == 'Mortality - Total' || input.issue3 == 'Mortality - Pearce Amenable' || input.issue3 == 'Mortality - Pearce Non-Amenable' || input.issue3 == 'Mortality - Ravymont Non-Avoidable' || input.issue3 == 'Mortality - Raymont Avoidable' || input.issue3 == 'Mortality - Raymont Non-Amenable' || input.issue3 == 'Mortality - Raymont Amenable' || input.issue3 == 'Mortality - Tobias Non-Amenable' || input.issue3 == 'Mortality - Tobias Avoidable' || input.issue3 == 'Mortality - MoH Non-Amenable' || input.issue3 == 'Mortality - MoH Amenable'",
        selectInput('dep3','Select a NZdep stratum if desired:',deptext,selected="None")
        #p("If starting year selected is a Census year then Census year + 2 is used, if ending year selected is a Census year then Census year - 2 is used")
      )
    )
  })
  observe({
    if(is.null(input$ethnic3) || is.na(input$ethnic3) || is.null(input$issue3) || is.na(input$issue3) || is.null(input$extend3) || is.na(input$extend3)){
    }else{
      updateSliderInput(session=session,inputId="year3",
                        "Year range (start and end)",
                        min = getLower(input$ethnic3,input$issue3),
                        max = getUpper(input$ethnic3,input$issue3),
                        step = 1,
                        value = c(getLower(input$ethnic3,input$issue3),getUpper(input$ethnic3,input$issue3)))
      lowerlim <<- getLower(input$ethnic1,input$issue1)
      upperlim <<- getUpper(input$ethnic1,input$issue1)
    }
  })
  output$pop4 <- renderUI({
    wellPanel(
      h3("Population 4"),
      conditionalPanel(
        #issue selection same as for pop 1 but is included in these panels
        condition = "input.country == 'New Zealand' ",
        selectInput("issue4","Select issue",issuetext),
        conditionalPanel(
          condition = "input.issue4 == 'Cancer - Priority' ",
          selectInput('priority4','Select a priority cancer:',prioritytext,selected="Colorectal")
        ),
        #p("priority data will be used")
        conditionalPanel(
          condition = "input.issue4 == 'B.Y.O.' ",
          fileInput('file4', 'Choose CSV File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv'))
        )
      ),
      conditionalPanel(
        condition = "input.country == 'Cook Islands' ",
        selectInput("issuec4","Re-select issue",cookissuetext)
      ),
      selectInput("ethnic4","Ethnicity",eth),
      selectInput("gender4","Gender",gen),
      sliderInput("year4","Year range (start and end)",
                                min = range_lower,
                                max = range_upper,
                                step = 1,
                                sep = "",
                                value=c(range_lower,range_upper)),
      
      selectInput('extend4','Select an option to increase accuracy:',extext,selected="None"),
      conditionalPanel(
        condition = "input.extend == '3-year Rolling Average' "
        
      ),
      conditionalPanel(
        condition = "input.issue4 == 'Mortality - Total' || input.issue4 == 'Mortality - Pearce Amenable' || input.issue4 == 'Mortality - Pearce Non-Amenable' || input.issue4 == 'Mortality - Raymont Non-Avoidable' || input.issue4 == 'Mortality - Raymont Avoidable' || input.issue4 == 'Mortality - Raymont Non-Amenable' || input.issue4 == 'Mortality - Raymont Amenable' || input.issue4 == 'Mortality - Tobias Non-Amenable' || input.issue4 == 'Mortality - Tobias Avoidable' || input.issue4 == 'Mortality - MoH Non-Amenable' || input.issue4 == 'Mortality - MoH Amenable'",
        selectInput('dep4','Select a NZdep stratum if desired:',deptext,selected="None")
        #p("If starting year selected is a Census year then Census year + 2 is used, if ending year selected is a Census year then Census year - 2 is used")
      )
    )
  })
  observe({
    if(is.null(input$ethnic4) || is.na(input$ethnic4) || is.null(input$issue4) || is.na(input$issue4) || is.null(input$extend4) || is.na(input$extend4)){
    }else{
      updateSliderInput(session=session,inputId="year4",
                        "Year range (start and end)",
                        min = getLower(input$ethnic4,input$issue4),
                        max = getUpper(input$ethnic4,input$issue4),
                        step = 1,
                        value = c(getLower(input$ethnic4,input$issue4),getUpper(input$ethnic4,input$issue4)))
      lowerlim <<- getLower(input$ethnic1,input$issue1)
      upperlim <<- getUpper(input$ethnic1,input$issue1)
    }
  })
  
  #render the standard panel that allows a numpop input and baselines selected
  output$standard <- renderUI({
    wellPanel(
      
      #br(),
      #line break and rr baseline selector dynamic on number of populations
      
      #standard type selector
      selectInput('stdtype','Select a type of standard:',stdtypetext,selected="External"),
      #most of the conditions below are unnecessary and blank but shows its inputs
      conditionalPanel(
        condition = "input.stdtype == 'External' ",
        selectInput('extstd','Select a standard:',extstdtext,selected="WHO")
      ),
      #internal standardisation dynamic on number of pops
      conditionalPanel(
        condition = "input.stdtype == 'Internal' ",
        conditionalPanel(
          condition = "input.numpop == 1",
          selectInput('int1','Select a standard:',intstd1)),
        conditionalPanel(
          condition = "input.numpop == 2",
          selectInput('int2','Select a standard:',intstd2a)),
        conditionalPanel(
          condition = "input.numpop == 3",
          selectInput('int3','Select a standard:',intstd3)),
        conditionalPanel(
          condition = "input.numpop == 4",
          selectInput('int4','Select a standard:',intstd4))
      ),
      conditionalPanel(
        condition = "input.numpop == 1"
      ),#,        selectInput('baseline1','Select baseline for relative risk:',intstd1,selected="Population 1")),
      conditionalPanel(
        condition = "input.numpop == 2",
        selectInput('baseline2a','Select baseline for relative and attributable risk:',intstd2a,selected="Population 1 (default)")),
      conditionalPanel(
        condition = "input.numpop == 3",
        selectInput('baseline3','Select baseline for relative and attributable risk:',intstd3,selected="Population 1 (default)")),
      conditionalPanel(
        condition = "input.numpop == 4",
        selectInput('baseline4','Select baseline for relative and attributable risk:',intstd4,selected="Population 1 (default)"))
      
    )
  })
  #updates internal choices by ethnicity of population 1, adds the asian/pacific options
  observe({
    if(is.null(input$ethnic1) || is.na(input$ethnic1)){
    }else if(input$ethnic1 == "Pacific"){
      updateSelectInput(session=session,inputId="extstd",
                        "Select a standard:",
                        censusP)
    }else if(input$ethnic1 == "Asian"){
      updateSelectInput(session=session,inputId="extstd",
                        "Select a standard:",
                        censusA)
    }else{
      updateSelectInput(session=session,inputId="extstd",
                        "Select a standard:",
                        extstdtext)
    }
  })
  
  #bulk of code starts here where output table gets rendered
  #code is generally blocked into methods in the initial stages
  output$table <- renderTable({
    #simple method identifies how many pops are selected and what the baseline is
    #it then leaves that information for the other methods
    if(is.null(input$numpop) || is.na(input$numpop)){
    }else{
      if(input$numpop == 1){
        getRates(1,1)
      }else if(input$numpop == 2){
        if(input$baseline2a == "Population 1 (default)"){
          getRates(2,1)
        }else{
          getRates(2,2)
        }
      }else if(input$numpop == 3){
        if(input$baseline3 == "Population 1 (default)"){
          getRates(3,1)
        }else if(input$baseline3 == "Population 2"){
          getRates(3,2)
        }else{
          getRates(3,3)
        }
      }else if(input$numpop == 4){
        if(input$baseline4 == "Population 1 (default)"){
          getRates(4,1)
        }else if(input$baseline4 == "Population 2"){
          getRates(4,2)
        }else if(input$baseline4 == "Population 3"){
          getRates(4,3)
        }else{
          getRates(4,4)
        }
      }
    }
  })
    #this method pulls the issue from the select fields based on which populations being looked at
    getIssue <- function(popn){
      if(input$country == "New Zealand"){
        outputissues <- input$issue1
        if(is.null(input$issue2) || is.na(input$issue2) || popn < 2){
        }else{
          outputissues <- input$issue2
          if(is.null(input$issue3) || is.na(input$issue3) || popn < 3){
          }else{
            outputissues <- input$issue3
            if(is.null(input$issue4) || is.na(input$issue4) || popn < 4){
            }else{
              outputissues <- input$issue4
            }
          }
          
        }
      }else{
        outputissues <- input$issuec1
        if(is.null(input$issuec2) || is.na(input$issuec2) || popn < 2){
        }else{
          outputissues <- input$issuec2
          if(is.null(input$issuec3) || is.na(input$issuec3) || popn < 3){
          }else{
            outputissues <- input$issuec3
            if(is.null(input$issuec4) || is.na(input$issuec4) || popn < 4){
            }else{
              outputissues <- input$issuec4
            }
          }
        }
          
      }
      outputissues
    }
  
  #this method pulls the type of priority cancer if necessary, based on pop number
  getPriority <- function(popn){
    if(is.null(input$priority1) || is.na(input$priority1)){
    }else{
      outputpriority <- input$priority1
      if(is.null(input$priority2) || is.na(input$priority2) || popn < 2){
      }else{
        outputpriority <-input$priority2
        if(is.null(input$priority3) || is.na(input$priority3) || popn < 3){
        }else{
          outputpriority <-input$priority3
          if(is.null(input$priority4) || is.na(input$priority4) || popn < 4){
          }else{
            outputpriority <- input$priority4
          }
        }
      }
      
    }
    outputpriority
  }
  
  #this method pulls the type of deprivation index if necessary, based on pop number
  getDep <- function(popn){
    if(is.null(input$dep1) || is.na(input$dep1)){
    }else{
      outputdep <- input$dep1
      if(is.null(input$dep2) || is.na(input$dep2) || popn < 2){
      }else{
        outputdep <- input$dep2
        if(is.null(input$dep3) || is.na(input$dep3) || popn < 3){
        }else{
          outputdep <- input$dep3
          if(is.null(input$dep4) || is.na(input$dep4) || popn < 4){
          }else{
            outputdep <- input$dep4
          }
        }
      }
    }
    if(outputdep == "Decile 1"){
      1
    }else if(outputdep == "Decile 2"){
      2
    }else if(outputdep == "Decile 3"){
      3
    }else if(outputdep == "Decile 4"){
      4
    }else if(outputdep == "Decile 5"){
      5
    }else if(outputdep == "Decile 6"){
      6
    }else if(outputdep == "Decile 7"){
      7
    }else if(outputdep == "Decile 8"){
      8
    }else if(outputdep == "Decile 9"){
      9
    }else if(outputdep == "Decile 10"){
      10
    }else if(outputdep == "Quintile 1"){
      12
    }else if(outputdep == "Quintile 2"){
      34
    }else if(outputdep == "Quintile 3"){
      56
    }else if(outputdep == "Quintile 4"){
      78
    }else if(outputdep == "Quintile 5"){
      910
    }else{
      0
    }
  }
  
  #this method pulls ethnicity by its pop number
  getEth <- function(popn){
    outputethnic <- input$ethnic1
    if(is.null(input$ethnic2) || is.na(input$ethnic2) || popn < 2){
    }else{
      outputethnic <-input$ethnic2
      if(is.null(input$ethnic3) || is.na(input$ethnic3) || popn < 3){
      }else{
        outputethnic <-input$ethnic3
        if(is.null(input$ethnic4) || is.na(input$ethnic4) || popn < 4){
        }else{
          outputethnic <- input$ethnic4
        }
      }
      
    }
    outputethnic
  }
  
  #this method pulls the lower and upper of the year slider
  getYears <- function(popn){
    outputyears <- c(input$year1[1],input$year1[2])
    if(is.null(input$year2[1]) || is.na(input$year2[1]) || is.null(input$year2[2]) || is.na(input$year2[2]) || popn < 2){
    }else{
      outputyears <- c(input$year2[1],input$year2[2])
      if(is.null(input$year3[1]) || is.na(input$year3[1]) || is.null(input$year3[2]) || is.na(input$year3[2]) || popn < 3){
      }else{
        outputyears <- c(input$year3[1],input$year3[2])
        if(is.null(input$year4[1]) || is.na(input$year4[1]) || is.null(input$year4[2]) || is.na(input$year4[2]) || popn < 4){
        }else{
          outputyears <- c(input$year4[1],input$year4[2])
        }
      }
      
    }
    outputyears
  }
  
  #this method pulls the gender based off of pop number
  getGender <- function(popn){
    outputgender <- input$gender1
    if(is.null(input$gender2) || is.na(input$gender2) || popn < 2){
    }else{
      outputgender <-input$gender2
      if(is.null(input$gender3) || is.na(input$gender3) || popn < 3){
      }else{
        outputgender <-input$gender3
        if(is.null(input$gender4) || is.na(input$gender4) || popn < 4){
        }else{
          outputgender <- input$gender4
        }
      }
      
    }
    outputgender
  }
  
  #this method gets the numerator data, it is based on which population, ethnicity
  #and issue as that splits all data files (priority additionally for that issue)
  #many repeats in this one as it selects so specifically
  getData <- function(popn,ethn,issue,prior,dep, regi){
    if(issue == "B.Y.O."){
        outputdata <- read(popn)
        byo_lower <<- outputdata[1,1]
        byo_upper <<- outputdata[length(outputdata[,1])-2,1]
        if(length(outputdata) == 20){
          
        }else{
          byo_eth <- outputdata[1,21]
          if(is.null(byo_eth)){
            byo_eth <<- "Total"
          }
          updateSelector(popn)
          outputdata <- outputdata[,-21]
        }
        updateSliders(popn)
        outputdata[,1] <- rep(outputdata[1,1]:outputdata[length(outputdata[[1]]) - 2,1], each = 3)
    }else{
        if(dep != 0 & regi == "National"){
          outputdata <- read.csv(toString(csvs[which(csvs$ethn == ethn & csvs$issue == issue & csvs$dep == dep & csvs$geog == "National" & csvs$country == "New Zealand"), 4]),sep = as.character(csvs[which(csvs$ethn == ethn & csvs$issue == issue & csvs$dep == dep & csvs$geog == "National" & csvs$country == "New Zealand"), 3]), header = TRUE)
        }else if(regi == "National"){
          outputdata <- read.csv(toString(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "National" & csvs$country == "New Zealand"), 4]),sep = as.character(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "National" & csvs$country == "New Zealand"), 3]), header = TRUE)
        }else if(regi == "Regional Council"){
          #print(toString(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "Regional Council" & csvs$country == "New Zealand"), 4]))
          outputdata <- read.csv(toString(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "Regional Council" & csvs$country == "New Zealand"), 4]),sep = as.character(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "Regional Council" & csvs$country == "New Zealand"), 3]), header = TRUE)
          outputdata <- outputdata[which(outputdata$reg == input$reg), ]
        }else if(regi == "TLA"){
          outputdata <- read.csv(toString(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "TLA" & csvs$country == "New Zealand"), 4]),sep = as.character(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "TLA" & csvs$country == "New Zealand"), 3]), header = TRUE)
          outputdata <- outputdata[which(outputdata$tla == input$tla), ]
        }else if(regi == "DHB"){
          outputdata <- read.csv(toString(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "DHB" & csvs$country == "New Zealand"), 4]),sep = as.character(csvs[which(csvs$ethn == ethn & csvs$issue == issue & is.na(csvs$dep) & csvs$geog == "DHB" & csvs$country == "New Zealand"), 3]), header = TRUE)
          outputdata <- outputdata[which(outputdata$dhb == input$dhb), ]
        }
        if(issue == "Cancer - Priority"){
          outputdata[,2] <- rep(outputdata[(0:(length(outputdata[,1]) / 3 - 1) * 3 + 1), 2], each = 3)
          outputdata[,1] <- rep(outputdata[(0:(length(outputdata[,1]) / 3 - 1) * 3 + 1), 1], each = 3)
          outputdata <- data.frame(outputdata[outputdata$Cancer==prior,])
          outputdata <- outputdata[,-2]
        }else{
          #print(outputdata)
          outputdata[,1] <- rep(outputdata[1,1]:outputdata[length(outputdata[[1]]) - 2,1], each = 3)
        }
    }
    colnames(outputdata)[3] <- "X00.04"
    colnames(outputdata)[4] <- "X05.09"
    outputdata
  }
  
  #function to update sliders for byo selections
  updateSliders <- function(popn){
    if(popn<2){
      updateSliderInput(session=session,inputId="year1",
                        "Year range (start and end)",
                        min = byo_lower,
                        max = byo_upper)
    }else if(popn<3){
      updateSliderInput(session=session,inputId="year2",
                        "Year range (start and end)",
                        min = byo_lower,
                        max = byo_upper)
    }else if(popn<4){
      updateSliderInput(session=session,inputId="year3",
                        "Year range (start and end)",
                        min = byo_lower,
                        max = byo_upper)
    }else if(popn<5){
      updateSliderInput(session=session,inputId="year4",
                        "Year range (start and end)",
                        min = byo_lower,
                        max = byo_upper)
    }
  }
  
  #function to update sliders for projections
  updateSlidersProj <- function(popn, eth, iss){
    if(popn<2){
      updateSliderInput(session=session,inputId="year1",
                        "Year range (start and end)",
                        min = getLower(eth, iss),
                        max = getUpper(eth, iss),
                        value = c(getLower(eth, iss), getUpper(eth, iss)))
    }else if(popn<3){
      updateSliderInput(session=session,inputId="year2",
                        "Year range (start and end)",
                        min = getLower(eth, iss),
                        max = getUpper(eth, iss),
                        value = c(getLower(eth, iss), getUpper(eth, iss)))
    }else if(popn<4){
      updateSliderInput(session=session,inputId="year3",
                        "Year range (start and end)",
                        min = getLower(eth, iss),
                        max = getUpper(eth, iss),
                        value = c(getLower(eth, iss), getUpper(eth, iss)))
    }else if(popn<5){
      updateSliderInput(session=session,inputId="year4",
                        "Year range (start and end)",
                        min = getLower(eth, iss),
                        max = getUpper(eth, iss),
                        value = c(getLower(eth, iss), getUpper(eth, iss)))
    }
  }
  
  #function to update ethnicity for byo selections
  updateSelector <- function(popn){
    if(popn<2){
      updateSelectInput(session=session,inputId="ethnic1",choices = eth, selected = byo_eth)
    }else if(popn<3){
      updateSelectInput(session=session,inputId="ethnic2",choices = eth, selected = byo_eth)
    }else if(popn<4){
      updateSelectInput(session=session,inputId="ethnic3",choices = eth, selected = byo_eth)
    }else if(popn<5){
      updateSelectInput(session=session,inputId="ethnic4",choices = eth, selected = byo_eth)
    }
  }
  #function that returns the denominator to be used in rate calculation
  #very similar to getData but some needed range adjustments
  getDenom <- function(popn,ethn,issue){
    if(input$country == "New Zealand" || is.null(input$country)){
      if(is.element(issue,mort)){
        if(input$geog == "National" || is.null(input$geog)){
          if(is.null(ethn) || ethn == "Total"){
            outputdenom <- data_popest91_total_nat[-c(1:15),]
            outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
          }else if(ethn == "Maori"){
            outputdenom <- data_popest91_maori_nat[-c(1:15),]
            outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
          }else if(ethn == "Non-Maori"){
            outputdenom <- data_popest91_nonmaori_nat[-c(1:15),]
            outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
          }else if(ethn == "Pacific"){
            outputdenom <- data_popcen_pacific_nat
            outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
          }else if(ethn == "Asian"){
            outputdenom <- data_popcen_asian_nat
            outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
          }
         
        }else if(input$subgeog == "Regional Council"){
          outputdenom <- data_popest_total_reg[which(data_popest_total_reg$Region == input$reg),][-c(25:30),][,-2]
        }else if(input$subgeog == "TLA"){
          outputdenom <- data_popest_total_tla[which(data_popest_total_tla$Region == input$tla),][-c(25:30),][,-2]
        }else if(input$subgeog == "DHB"){
          outputdenom <- data_popest_total_dhb[which(data_popest_total_dhb$Region == input$dhb),][-c(25:30),][,-2]
        }
      }else if(issue == "Cancer - Gastric Death"){
        if(input$geog == "National" || is.null(input$geog)){
          if(ethn == "Total" || is.null(ethn)){
            outputdenom <- rbind(data_popestdisc95_total_nat[-c(1:33),],(data_popest91_total_nat[-c(1:15,67:69),]))
          }else if(ethn == "Maori"){
            outputdenom <- rbind(data_popestdisc90_maori_nat[-c(1:33),],(data_popest91_maori_nat[-c(67:69),]))
          }else if(ethn == "Non-Maori"){
            outputdenom <- rbind(data_popestdisc90_nonmaori_nat[-c(1:33),],(data_popest91_nonmaori_nat[-c(67:69),]))
          }
          outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
        }
      }else if(issue == "Cancer - Gastric Incidence"){
        if(input$geog == "National" || is.null(input$geog)){
          if(ethn == "Total" || is.null(ethn)){
            outputdenom <- rbind(data_popestdisc95_total_nat[-c(1:78),],(data_popest91_total_nat[-c(1:15),]))
          }else if(ethn == "Maori"){
            outputdenom <- rbind(data_popestdisc90_maori_nat[-c(1:78),],data_popest91_maori_nat)
          }else if(ethn == "Non-Maori"){
            outputdenom <- rbind(data_popestdisc90_nonmaori_nat[-c(1:78),],data_popest91_nonmaori_nat)
          }
          outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
        }
      }else if(issue == "Cancer - Priority"){
        if(input$geog == "National" || is.null(input$geog)){
          if(ethn == "Total" || is.null(ethn)){
            outputdenom <- data_popest91_total_nat[58:66,]
          }else if(ethn == "Maori"){
            outputdenom <- data_popest91_maori_nat[58:66,]
          }else if(ethn == "Non-Maori"){
            outputdenom <- data_popest91_nonmaori_nat[58:66,]
          }
          outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
        }
      }else if(issue == "B.Y.O."){
        if(input$geog == "National" || is.null(input$geog)){
          if(byo_lower < 1991){
            if(is.null(ethn) || ethn == "Total"){
              data_popest_comb_total <- rbind(data_popestdisc95_total_nat[1:162,], data_popest91_total_nat)
              outputdenom <- data_popest_comb_total[which(data_popest_comb_total$Year==byo_lower)[1]:(which(data_popest_comb_total$Year==byo_upper)[1]+2),]
            }else if(ethn == "Maori"){
              data_popest_comb_maori <- rbind(data_popestdisc90_maori_nat, data_popest91_maori_nat)
              outputdenom <- data_popest_comb_maori[which(data_popest_comb_maori$Year==byo_lower)[1]:(which(data_popest_comb_maori$Year==byo_upper)[1]+2),]
            }else if(ethn == "Non-Maori"){
              data_popest_comb_nonmaori <- rbind(data_popestdisc90_nonmaori_nat, data_popest91_nonmaori_nat)
              outputdenom <- data_popest_comb_nonmaori[which(data_popest_comb_nonmaori$Year==byo_lower)[1]:(which(data_popest_comb_nonmaori$Year==byo_upper)[1]+2),]
            }
          }else{
            if(is.null(ethn) || ethn == "Total"){
              outputdenom <- data_popest91_total_nat[which(data_popest91_total_nat$Year==byo_lower)[1]:(which(data_popest91_total_nat$Year==byo_upper)[1]+2),]
            }else if(ethn == "Maori"){
              outputdenom <- data_popest91_maori_nat[which(data_popest91_total_nat$Year==byo_lower)[1]:(which(data_popest91_total_nat$Year==byo_upper)[1]+2),]
            }else if(ethn == "Non-Maori"){
              outputdenom <- data_popest91_nonmaori_nat[which(data_popest91_total_nat$Year==byo_lower)[1]:(which(data_popest91_total_nat$Year==byo_upper)[1]+2),]
            }
          }
          outputdenom[,1] <- rep(outputdenom[1,1]:outputdenom[length(outputdenom[[1]]) - 2,1], each = 3)
        }
      }
      colnames(outputdenom)[3] <- "X00.04"
      colnames(outputdenom)[4] <- "X05.09"
      outputdenom
    }else{
      outputdenom <- data_popcen_cook
      colnames(outputdenom)[3] <- "X00.04"
      colnames(outputdenom)[4] <- "X05.09"
      outputdenom
    }
  }
  
  #returns default range of selected data in vector of lower,upper
  getRange <- function(popn,ethn,issue){
    if(input$proj == TRUE) return(c(getLower(ethn, issue), getUpper(ethn, issue)))
    else if(input$country == "New Zealand" || is.null(input$country)){
      if(is.element(issue,mort)){
        if(input$geog == "National" || is.null(input$geog)){
          if(is.null(ethn) || ethn == "Total"){
            range_l <- total_nat_range_lower
            range_u <- total_nat_range_upper
          }else if(ethn == "Maori" || ethn == "Non-Maori"){
            range_l <- maori_nat_range_lower
            range_u <- maori_nat_range_upper
          }else if(ethn == "Pacific"){
            range_l <- pacific_nat_range_lower
            range_u <- pacific_nat_range_upper
          }else if(ethn == "Asian"){
            range_l <- asian_nat_range_lower
            range_u <- asian_nat_range_upper
          }
        }else if(input$subgeog == "Regional Council"){
          range_l <- total_reg_range_lower
          range_u <- total_reg_range_upper
        }else if(input$subgeog == "TLA"){
          range_l <- total_tla_range_lower
          range_u <- total_tla_range_upper
        }else if(input$subgeog == "DHB"){
          range_l <- total_dhb_range_lower
          range_u <- total_dhb_range_upper
        }
        
      }else if(issue == "Cancer - Gastric Death" || is.null(issue)){
        if(input$geog == "National" || is.null(input$geog)){
          range_l <- gastric_death_range_lower
          range_u <- gastric_death_range_upper
        }
      }else if(issue == "Cancer - Gastric Incidence"){
        if(input$geog == "National" || is.null(input$geog)){
          range_l <- gastric_inc_range_lower
          range_u <- gastric_inc_range_upper
        }
      }else if(issue == "Cancer - Priority"){
        if(input$geog == "National" || is.null(input$geog)){
          range_l <- priority_range_lower
          range_u <- priority_range_upper
        }
      }else if(issue == "B.Y.O."){
        range_l <- byo_lower
        range_u <- byo_upper
      }
    }else if(input$country == "Cook Islands"){
      range_l <- cook_range_lower
      range_u <- cook_range_upper
    }
    c(range_l,range_u)
  }
  #core method which pulls everything needed together before starting calculations
  dataset <- function(popn){
    
    if (input$geog == "National") {
      regi <- input$geog
    } else {
      regi <- input$subgeog
    }
    issue <- getIssue(popn)
    ethn <- getEth(popn)
    prior <- getPriority(popn)
    dep <- getDep(popn)
    gend <- getGender(popn)
    if(input$proj == TRUE) {updateSlidersProj(popn, ethn, issue)}
    years <- getYears(popn)
    
    data <- getData(popn,ethn,issue,prior,dep, regi)
    denom <- getDenom(popn,ethn,issue)
    if(input$proj == TRUE) {
      #data <- data[-which(data$Year < 1998), ]#2012) ,]
      #denom <- denom[-which(denom$Year < 1998), ]#2012) ,]
    }
    
    #create data frame based on gender
    df <- data.frame(data[data$Factor==gend,])
    #create denominator frame based on gender
    denomdf <- data.frame(denom[denom$Factor==gend,])
    range <- getRange(popn,ethn,issue)
    range_lower <- range[1]
    range_upper <- range[2]
    #This cleans up the labels and unnecessary columns etc in data by looking at agegroup selection
    if(input$agegroup == "5-year bands" || input$agegroup == "Total" || is.null(input$agegroup)){
      df <- df[-21]
      ##removing DEP
      colnames(df) = gsub("X","",colnames(df))
      colnames(df)[19] = gsub("80.and.over","80+",colnames(df)[19])
      colnames(df) = gsub("[.]","-",colnames(df))
      df[2] <- df[20]
      colnames(df)[2] = "Total"
      df <- df[-20]
      colnames(denomdf) = gsub("X","",colnames(denomdf))
      colnames(denomdf)[19] = gsub("80.and.over","80+",colnames(denomdf)[19])
      colnames(denomdf) = gsub("[.]","-",colnames(denomdf))
      denomdf[2] <- denomdf[20]
      colnames(denomdf)[2] = "Total"
      denomdf <- denomdf[-20]
      if(input$agegroup == "5-year bands"){
        df <- cbind(df[1],df[input$selectgroups])
        denomdf <- cbind(denomdf[1],denomdf[input$selectgroups])
        df[,2:length(df)] <- sapply(df[,2:length(df)],as.numeric)
        denomdf[,2:length(denomdf)] <- sapply(denomdf[,2:length(denomdf)],as.numeric)
      }
    }else{  
    }
    #binds data and denominator into one dataset to be accessed
    if(years[1] == range_lower && years[2] == range_upper){
      rbind(df,denomdf)
    }else{
      rbind(df[-c(which(df$Year<years[1]),which(df$Year>years[2])),],denomdf[-c(which(denomdf$Year<years[1]),which(denomdf$Year>years[2])),])
    }
  }
  
  #method that is called from the rendering that pulls the datasets and splits them accordingly
  #before binding numerators and denominators separately and num pops and denoms sent on
  getRates <- function(popn,denompopn){
    da1 <- dataset(1)
    den1 <- da1[(0.5*length(da1[,1])+1):length(da1[,1]),]
    den1$pop = 1
    df1 <- da1[1:(0.5*length(da1[,1])),]
    df1$pop = 1
    dfrr <- df1
    denrr <- den1
    if(popn > 1){
      da2 <- dataset(2)
      den2 <- da2[(0.5*length(da2[,1])+1):length(da2[,1]),]
      den2$pop = 2
      df2 <- da2[1:(0.5*length(da2[,1])),]
      df2$pop = 2
      dfrr <- rbind(df1,df2)
      denrr <- rbind(den1,den2)
    }
    if(popn > 2){
      da3 <- dataset(3)
      den3 <- da3[(0.5*length(da3[,1])+1):length(da3[,1]),]
      den3$pop = 3
      df3 <- da3[1:(0.5*length(da3[,1])),]
      df3$pop = 3
      dfrr <- rbind(df1,df2,df3)
      denrr <- rbind(den1,den2,den3)
    }
    if(popn > 3){
      da4 <- dataset(4)
      den4 <- da4[(0.5*length(da4[,1])+1):length(da4[,1]),]
      den4$pop = 4
      df4 <- da4[1:(0.5*length(da4[,1])),]
      df4$pop = 4
      dfrr <- rbind(df1,df2,df3,df4)
      denrr <- rbind(den1,den2,den3,den4)
    }
    colnames(dfrr)[length(dfrr)] <- "pop"
    colnames(denrr)[length(denrr)] <- "pop"
    #relativerisk contains almost all serious calculation and it returns the output table
    #which is output and also placed into the csv output slot for download to work
    outputdf <- relativeRisk(dfrr,denrr,popn,denompopn)
    tocsv <<- outputdf
    outputdf
  }
  #function which calculates all rates, rr, ar, standardisation and table output
  relativeRisk <- function(counts,pops,num,base){
    if(is.na(base)){
      base <- 1
    }
    #creating data frame to eventually return
    plotdata <- NULL
    plotdata <- data.frame(rep(colnames(counts)[2:(length(counts)-1)],nrow(counts)))
    colnames(plotdata)[1] <- "ageband"
    
    loopingyear <- rep(counts$Year[1:length(counts[,1])], each = ncol(counts) - 2)
    loopingpop <- rep(counts$pop[1:length(counts[,1])], each = ncol(counts) - 2)
    plotdata$year <- as.factor(loopingyear)
    plotdata$pop <- as.factor(loopingpop)
    
    loopingdeaths <- t(as.matrix(counts[1:length(counts[,1]), 2:(length(counts[1,]) - 1)]))
    loopingpeople <- t(as.matrix(pops[1:length(counts[,1]), 2:(length(counts[1,]) - 1)]))

    deaths <- as.numeric(loopingdeaths)
    people <- as.numeric(loopingpeople)

    plotdata$deaths <- deaths
    plotdata$people <- people

    #checks what number of populations is largest so correct divisions are used
    if(plotdata$pop[length(plotdata$pop)] == '1'){
      upto <- 1
    }else if(plotdata$pop[length(plotdata$pop)] == '2'){
      upto <- 2
    }else if(plotdata$pop[length(plotdata$pop)] == '3'){
      upto <- 3
    }else{
      upto <- 4
    }
    #largely repeated section of code identifying age group and one of 3 accuracy
    #improving techniques and completes the necessary calculations for that pop
    if(input$agegroup == "Total"){
      if(input$extend1 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          #checking if invalid input
        }else{
          #splits off all but interested pop
          exdf1 <- plotdata[which(plotdata$pop == '1'),]
          expost1 <- plotdata[which(plotdata$pop != '1'),]
          exdf1$rolldeath <- NA
          exdf1$rollpeople <- NA
          #calculates rolling average
          exdf1$rolldeath[19:(length(people)/upto-18)] <- (exdf1$deaths[19:(length(people)/upto-18)-18]+exdf1$deaths[19:(length(people)/upto-18)]+exdf1$deaths[19:(length(people)/upto-18)+18])/3
          exdf1$rollpeople[19:(length(people)/upto-18)] <- exdf1$people[19:(length(people)/upto-18)]
          #constructs vectors based on newly calculated data
          plotdata$deaths <- c(exdf1$rolldeath,expost1$deaths)
          plotdata$people <- c(exdf1$rollpeople,expost1$people)
        }
      }else if(input$extend1 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          exdf1 <- plotdata[which(plotdata$pop == '1'),]
          expost1 <- plotdata[which(plotdata$pop != '1'),]
          exdf1$rolldeath <- NA
          exdf1$rollpeople <- NA
          
          #same as above except is total division over sum of 3 years
          exdf1$rolldeath[19:(length(people)/upto-18)] <- exdf1$deaths[19:(length(people)/upto-18)-18]+exdf1$deaths[19:(length(people)/upto-18)]+exdf1$deaths[19:(length(people)/upto-18)+18]
          exdf1$rollpeople[19:(length(people)/upto-18)] <- exdf1$people[19:(length(people)/upto-18)-18]+exdf1$people[19:(length(people)/upto-18)]+exdf1$people[19:(length(people)/upto-18)+18]

          for(i in 19:(length(people)/upto-18)){
            exdf1$rolldeath[i] <- exdf1$deaths[i-18]+exdf1$deaths[i]+exdf1$deaths[i+18]
            exdf1$rollpeople[i] <- exdf1$people[i-18]+exdf1$people[i]+exdf1$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf1$rolldeath[i] <- NA
              exdf1$rollpeople[i] <- NA
            }else{
            }
          }         
#           ifelse((19:(length(people)/upto-18)+17) %% 54 < 36,{
#             exdf1$rolldeath <- NA
#             exdf1$rollpeople <- NA
#           },)
#         
          plotdata$deaths <- c(exdf1$rolldeath,expost1$deaths)
          plotdata$people <- c(exdf1$rollpeople,expost1$people)
        }
      }else if(input$extend1 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          exdf1 <- plotdata[which(plotdata$pop == '1'),]
          expost1 <- plotdata[which(plotdata$pop != '1'),]
          exdf1$rolldeath <- NA
          exdf1$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            
            if(is.element(exdf1$year[i],cyears) | (input$country == "Cook Islands" & exdf1$year[i] == 2011)){
              exdf1$rolldeath[i] <- exdf1$deaths[i-18]+exdf1$deaths[i]+exdf1$deaths[i+18]
              exdf1$rollpeople[i] <- exdf1$people[i]*3
            }else{
            }
          }
          for(i in 1:18){
            if(is.element(exdf1$year[1],cyears)){
              exdf1$rolldeath[18+i] <- exdf1$deaths[i]+exdf1$deaths[18+i]+exdf1$deaths[36+i]
              exdf1$rollpeople[18+i] <- exdf1$people[i]*3
            }
            if(is.element(exdf1$year[length(exdf1$year)],cyears)){
              exdf1$rolldeath[length(exdf1$year)-36+i] <- exdf1$deaths[length(exdf1$year)-18+i]+exdf1$deaths[length(exdf1$year)-36+i]+exdf1$deaths[length(exdf1$year)-54+i]
              exdf1$rollpeople[length(exdf1$year)-36+i] <- exdf1$people[length(exdf1$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(exdf1$rolldeath,expost1$deaths)
          plotdata$people <- c(exdf1$rollpeople,expost1$people)
        }
      }
      #same as above repeated for pop 2
      if(input$extend2 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre2 <- plotdata[which(plotdata$pop == '1'),]
          exdf2 <- plotdata[which(plotdata$pop == '2'),]
          expost2a <- plotdata[which(plotdata$pop == '3'),]
          expost2b <- plotdata[which(plotdata$pop == '4'),]
          exdf2$rolldeath <- NA
          exdf2$rollpeople <- NA
          exdf2$rolldeath[19:(length(people)/upto-18)] <- (exdf2$deaths[19:(length(people)/upto-18)-18]+exdf2$deaths[19:(length(people)/upto-18)]+exdf2$deaths[19:(length(people)/upto-18)+18])/3
          exdf2$rollpeople[19:(length(people)/upto-18)] <- exdf2$people[19:(length(people)/upto-18)]
          plotdata$deaths <- c(expre2$deaths,exdf2$rolldeath,expost2a$deaths,expost2b$deaths)
          plotdata$people <- c(expre2$people,exdf2$rollpeople,expost2a$people,expost2b$people)
        }
      }else if(input$extend2 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre2 <- plotdata[which(plotdata$pop == '1'),]
          exdf2 <- plotdata[which(plotdata$pop == '2'),]
          expost2a <- plotdata[which(plotdata$pop == '3'),]
          expost2b <- plotdata[which(plotdata$pop == '4'),]
          exdf2$rolldeath <- NA
          exdf2$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            exdf2$rolldeath[i] <- exdf2$deaths[i-18]+exdf2$deaths[i]+exdf2$deaths[i+18]
            exdf2$rollpeople[i] <- exdf2$people[i-18]+exdf2$people[i]+exdf2$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf2$rolldeath[i] <- NA
              exdf2$rollpeople[i] <- NA
            }else{
            }
          }
          plotdata$deaths <- c(expre2$deaths,exdf2$rolldeath,expost2a$deaths,expost2b$deaths)
          plotdata$people <- c(expre2$people,exdf2$rollpeople,expost2a$people,expost2b$people)
        }
      }else if(input$extend2 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre2 <- plotdata[which(plotdata$pop == '1'),]
          exdf2 <- plotdata[which(plotdata$pop == '2'),]
          expost2a <- plotdata[which(plotdata$pop == '3'),]
          expost2b <- plotdata[which(plotdata$pop == '4'),]
          exdf2$rolldeath <- NA
          exdf2$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){

            if(is.element(exdf2$year[i],cyears) | (input$country == "Cook Islands" & exdf2$year[i] == 2011)){
              exdf2$rolldeath[i] <- exdf2$deaths[i-18]+exdf2$deaths[i]+exdf2$deaths[i+18]
              exdf2$rollpeople[i] <- exdf2$people[i]*3
            }else{
            }
          }
          for(i in 1:18){
            if(is.element(exdf2$year[1],cyears)){
              exdf2$rolldeath[18+i] <- exdf2$deaths[i]+exdf2$deaths[18+i]+exdf2$deaths[36+i]
              exdf2$rollpeople[18+i] <- exdf2$people[i]*3
            }
            if(is.element(exdf2$year[length(exdf2$year)],cyears)){
              exdf2$rolldeath[length(exdf2$year)-36+i] <- exdf2$deaths[length(exdf2$year)-18+i]+exdf2$deaths[length(exdf2$year)-36+i]+exdf2$deaths[length(exdf2$year)-54+i]
              exdf2$rollpeople[length(exdf2$year)-36+i] <- exdf2$people[length(exdf2$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(expre2$deaths,exdf2$rolldeath,expost2a$deaths,expost2b$deaths)
          plotdata$people <- c(expre2$people,exdf2$rollpeople,expost2a$people,expost2b$people)
        }
      }
      if(input$extend3 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre3a <- plotdata[which(plotdata$pop == '1'),]
          expre3b <- plotdata[which(plotdata$pop == '2'),]
          exdf3 <- plotdata[which(plotdata$pop == '3'),]
          expost3 <- plotdata[which(plotdata$pop == '4'),]
          exdf3$rolldeath <- NA
          exdf3$rollpeople <- NA
          exdf3$rolldeath[19:(length(people)/upto-18)] <- (exdf3$deaths[19:(length(people)/upto-18)-18]+exdf3$deaths[19:(length(people)/upto-18)]+exdf3$deaths[19:(length(people)/upto-18)+18])/3
          exdf3$rollpeople[19:(length(people)/upto-18)] <- exdf3$people[19:(length(people)/upto-18)]
          plotdata$deaths <- c(expre3a$deaths,expre3b$deaths,exdf3$rolldeath,expost3$deaths)
          plotdata$people <- c(expre3a$people,expre3b$people,exdf3$rollpeople,expost3$people)
        }
      }else if(input$extend3 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre3a <- plotdata[which(plotdata$pop == '1'),]
          expre3b <- plotdata[which(plotdata$pop == '2'),]
          exdf3 <- plotdata[which(plotdata$pop == '3'),]
          expost3 <- plotdata[which(plotdata$pop == '4'),]
          exdf3$rolldeath <- NA
          exdf3$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            exdf3$rolldeath[i] <- exdf3$deaths[i-18]+exdf3$deaths[i]+exdf3$deaths[i+18]
            exdf3$rollpeople[i] <- exdf3$people[i-18]+exdf3$people[i]+exdf3$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf3$rolldeath[i] <- NA
              exdf3$rollpeople[i] <- NA
            }else{
            }
          }
          plotdata$deaths <- c(expre3a$deaths,expre3b$deaths,exdf3$rolldeath,expost3$deaths)
          plotdata$people <- c(expre3a$people,expre3b$people,exdf3$rollpeople,expost3$people)
        }
      }else if(input$extend3 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre3a <- plotdata[which(plotdata$pop == '1'),]
          expre3b <- plotdata[which(plotdata$pop == '2'),]
          exdf3 <- plotdata[which(plotdata$pop == '3'),]
          expost3 <- plotdata[which(plotdata$pop == '4'),]
          exdf3$rolldeath <- NA
          exdf3$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            
            if(is.element(exdf3$year[i],cyears) | (input$country == "Cook Islands" & exdf3$year[i] == 2011)){
              exdf3$rolldeath[i] <- exdf3$deaths[i-18]+exdf3$deaths[i]+exdf3$deaths[i+18]
              exdf3$rollpeople[i] <- exdf3$people[i]*3
            }else{
            }
          }
          for(i in 1:18){
            if(is.element(exdf3$year[1],cyears)){
              exdf3$rolldeath[18+i] <- exdf3$deaths[i]+exdf3$deaths[18+i]+exdf3$deaths[36+i]
              exdf3$rollpeople[18+i] <- exdf3$people[i]*3
            }
            if(is.element(exdf3$year[length(exdf3$year)],cyears)){
              exdf3$rolldeath[length(exdf3$year)-36+i] <- exdf3$deaths[length(exdf3$year)-18+i]+exdf3$deaths[length(exdf3$year)-36+i]+exdf3$deaths[length(exdf3$year)-54+i]
              exdf3$rollpeople[length(exdf3$year)-36+i] <- exdf3$people[length(exdf3$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(expre3a$deaths,expre3b$deaths,exdf3$rolldeath,expost3$deaths)
          plotdata$people <- c(expre3a$people,expre3b$people,exdf3$rollpeople,expost3$people)
        }
      }
      if(input$extend4 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre4 <- plotdata[which(plotdata$pop != '4'),]
          exdf4 <- plotdata[which(plotdata$pop == '4'),]
          exdf4$rolldeath <- NA
          exdf4$rollpeople <- NA
          exdf4$rolldeath[19:(length(people)/upto-18)] <- (exdf4$deaths[19:(length(people)/upto-18)-18]+exdf4$deaths[19:(length(people)/upto-18)]+exdf4$deaths[19:(length(people)/upto-18)+18])/3
          exdf4$rollpeople[19:(length(people)/upto-18)] <- exdf4$people[19:(length(people)/upto-18)]
          plotdata$deaths <- c(expre4$deaths,exdf4$rolldeath)
          plotdata$people <- c(expre4$people,exdf4$rollpeople)
        }
      }else if(input$extend4 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre4 <- plotdata[which(plotdata$pop != '4'),]
          exdf4 <- plotdata[which(plotdata$pop == '4'),]
          exdf4$rolldeath <- NA
          exdf4$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            exdf4$rolldeath[i] <- exdf4$deaths[i-18]+exdf4$deaths[i]+exdf4$deaths[i+18]
            exdf4$rollpeople[i] <- exdf4$people[i-18]+exdf4$people[i]+exdf4$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf4$rolldeath[i] <- NA
              exdf4$rollpeople[i] <- NA
            }else{
            }
          }
          plotdata$deaths <- c(expre4$deaths,exdf4$rolldeath)
          plotdata$people <- c(expre4$people,exdf4$rollpeople)
        }
      }else if(input$extend4 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre4 <- plotdata[which(plotdata$pop != '4'),]
          exdf4 <- plotdata[which(plotdata$pop == '4'),]
          exdf4$rolldeath <- NA
          exdf4$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            if(is.element(exdf4$year[i],cyears) | (input$country == "Cook Islands" & exdf4$year[i] == 2011)){
              exdf4$rolldeath[i] <- exdf4$deaths[i-18]+exdf4$deaths[i]+exdf4$deaths[i+18]
              exdf4$rollpeople[i] <- exdf4$people[i]*3
            }else{
            }
          }
          for(i in 1:18){
            if(is.element(exdf4$year[1],cyears)){
              exdf4$rolldeath[18+i] <- exdf4$deaths[i]+exdf4$deaths[18+i]+exdf4$deaths[36+i]
              exdf4$rollpeople[18+i] <- exdf4$people[i]*3
            }
            if(is.element(exdf4$year[length(exdf4$year)],cyears)){
              exdf4$rolldeath[length(exdf4$year)-36+i] <- exdf4$deaths[length(exdf4$year)-18+i]+exdf4$deaths[length(exdf4$year)-36+i]+exdf4$deaths[length(exdf4$year)-54+i]
              exdf4$rollpeople[length(exdf4$year)-36+i] <- exdf4$people[length(exdf4$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(expre4$deaths,exdf4$rolldeath)
          plotdata$people <- c(expre4$people,exdf4$rollpeople)
        }
      }
    }else{
      if(input$extend1 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          
        }else{
          exdf1 <- plotdata[which(plotdata$pop == '1'),]
          expost1 <- plotdata[which(plotdata$pop != '1'),]
          exdf1$rolldeath <- NA
          exdf1$rollpeople <- NA
          exdf1$rolldeath[19:(length(people)/upto-18)] <- (exdf1$deaths[19:(length(people)/upto-18)-18]+exdf1$deaths[19:(length(people)/upto-18)]+exdf1$deaths[19:(length(people)/upto-18)+18])/3
          exdf1$rollpeople[19:(length(people)/upto-18)] <- exdf1$people[19:(length(people)/upto-18)]
          plotdata$deaths <- c(exdf1$rolldeath,expost1$deaths)
          plotdata$people <- c(exdf1$rollpeople,expost1$people)
        }
      }else if(input$extend1 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          exdf1 <- plotdata[which(plotdata$pop == '1'),]
          expost1 <- plotdata[which(plotdata$pop != '1'),]
          exdf1$rolldeath <- NA
          exdf1$rollpeople <- NA
        
          for(i in 19:(length(people)/upto-18)){
            exdf1$rolldeath[i] <- exdf1$deaths[i-18]+exdf1$deaths[i]+exdf1$deaths[i+18]
            exdf1$rollpeople[i] <- exdf1$people[i-18]+exdf1$people[i]+exdf1$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf1$rolldeath[i] <- NA
              exdf1$rollpeople[i] <- NA
            }else{
            }
          }
          plotdata$deaths <- c(exdf1$rolldeath,expost1$deaths)
          plotdata$people <- c(exdf1$rollpeople,expost1$people)
        }
      }else if(input$extend1 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          exdf1 <- plotdata[which(plotdata$pop == '1'),]
          expost1 <- plotdata[which(plotdata$pop != '1'),]
          exdf1$rolldeath <- NA
          exdf1$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            if(is.element(exdf1$year[i],cyears) | (input$country == "Cook Islands" & exdf1$year[i] == 2011)){
              exdf1$rolldeath[i] <- exdf1$deaths[i-18]+exdf1$deaths[i]+exdf1$deaths[i+18]
              exdf1$rollpeople[i] <- exdf1$people[i]*3
            }else{

            }
          }
          for(i in 1:18){
            if(is.element(exdf1$year[1],cyears)){
              exdf1$rolldeath[18+i] <- exdf1$deaths[i]+exdf1$deaths[18+i]+exdf1$deaths[36+i]
              exdf1$rollpeople[18+i] <- exdf1$people[i]*3
            }
            if(is.element(exdf1$year[length(exdf1$year)],cyears)){
              exdf1$rolldeath[length(exdf1$year)-36+i] <- exdf1$deaths[length(exdf1$year)-18+i]+exdf1$deaths[length(exdf1$year)-36+i]+exdf1$deaths[length(exdf1$year)-54+i]
              exdf1$rollpeople[length(exdf1$year)-36+i] <- exdf1$people[length(exdf1$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(exdf1$rolldeath,expost1$deaths)
          plotdata$people <- c(exdf1$rollpeople,expost1$people)
        }
      }
      if(input$extend2 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre2 <- plotdata[which(plotdata$pop == '1'),]
          exdf2 <- plotdata[which(plotdata$pop == '2'),]
          expost2a <- plotdata[which(plotdata$pop == '3'),]
          expost2b <- plotdata[which(plotdata$pop == '4'),]
          exdf2$rolldeath <- NA
          exdf2$rollpeople <- NA
          exdf2$rolldeath[19:(length(people)/upto-18)] <- (exdf2$deaths[19:(length(people)/upto-18)-18]+exdf2$deaths[19:(length(people)/upto-18)]+exdf2$deaths[19:(length(people)/upto-18)+18])/3
          exdf2$rollpeople[19:(length(people)/upto-18)] <- exdf2$people[19:(length(people)/upto-18)]
          plotdata$deaths <- c(expre2$deaths,exdf2$rolldeath,expost2a$deaths,expost2b$deaths)
          plotdata$people <- c(expre2$people,exdf2$rollpeople,expost2a$people,expost2b$people)
        }
      }else if(input$extend2 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre2 <- plotdata[which(plotdata$pop == '1'),]
          exdf2 <- plotdata[which(plotdata$pop == '2'),]
          expost2a <- plotdata[which(plotdata$pop == '3'),]
          expost2b <- plotdata[which(plotdata$pop == '4'),]
          exdf2$rolldeath <- NA
          exdf2$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            exdf2$rolldeath[i] <- exdf2$deaths[i-18]+exdf2$deaths[i]+exdf2$deaths[i+18]
            exdf2$rollpeople[i] <- exdf2$people[i-18]+exdf2$people[i]+exdf2$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf2$rolldeath[i] <- NA
              exdf2$rollpeople[i] <- NA
            }else{
            }
          }
          plotdata$deaths <- c(expre2$deaths,exdf2$rolldeath,expost2a$deaths,expost2b$deaths)
          plotdata$people <- c(expre2$people,exdf2$rollpeople,expost2a$people,expost2b$people)
        }
      }else if(input$extend2 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre2 <- plotdata[which(plotdata$pop == '1'),]
          exdf2 <- plotdata[which(plotdata$pop == '2'),]
          expost2a <- plotdata[which(plotdata$pop == '3'),]
          expost2b <- plotdata[which(plotdata$pop == '4'),]
          exdf2$rolldeath <- NA
          exdf2$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            
            if(is.element(exdf2$year[i],cyears) | (input$country == "Cook Islands" & exdf2$year[i] == 2011)){
              exdf2$rolldeath[i] <- exdf2$deaths[i-18]+exdf2$deaths[i]+exdf2$deaths[i+18]
              exdf2$rollpeople[i] <- exdf2$people[i]*3
            }else{

            }
          }
          for(i in 1:18){
            if(is.element(exdf2$year[1],cyears)){
              exdf2$rolldeath[18+i] <- exdf2$deaths[i]+exdf2$deaths[18+i]+exdf2$deaths[36+i]
              exdf2$rollpeople[18+i] <- exdf2$people[i]*3
            }
            if(is.element(exdf2$year[length(exdf2$year)],cyears)){
              exdf2$rolldeath[length(exdf2$year)-36+i] <- exdf2$deaths[length(exdf2$year)-18+i]+exdf2$deaths[length(exdf2$year)-36+i]+exdf2$deaths[length(exdf2$year)-54+i]
              exdf2$rollpeople[length(exdf2$year)-36+i] <- exdf2$people[length(exdf2$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(expre2$deaths,exdf2$rolldeath,expost2a$deaths,expost2b$deaths)
          plotdata$people <- c(expre2$people,exdf2$rollpeople,expost2a$people,expost2b$people)
        }
      }
      if(input$extend3 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre3a <- plotdata[which(plotdata$pop == '1'),]
          expre3b <- plotdata[which(plotdata$pop == '2'),]
          exdf3 <- plotdata[which(plotdata$pop == '3'),]
          expost3 <- plotdata[which(plotdata$pop == '4'),]
          exdf3$rolldeath <- NA
          exdf3$rollpeople <- NA
          exdf3$rolldeath[19:(length(people)/upto-18)] <- (exdf3$deaths[19:(length(people)/upto-18)-18]+exdf3$deaths[19:(length(people)/upto-18)]+exdf3$deaths[19:(length(people)/upto-18)+18])/3
          exdf3$rollpeople[19:(length(people)/upto-18)] <- exdf3$people[19:(length(people)/upto-18)]
          plotdata$deaths <- c(expre3a$deaths,expre3b$deaths,exdf3$rolldeath,expost3$deaths)
          plotdata$people <- c(expre3a$people,expre3b$people,exdf3$rollpeople,expost3$people)
        }
      }else if(input$extend3 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre3a <- plotdata[which(plotdata$pop == '1'),]
          expre3b <- plotdata[which(plotdata$pop == '2'),]
          exdf3 <- plotdata[which(plotdata$pop == '3'),]
          expost3 <- plotdata[which(plotdata$pop == '4'),]
          exdf3$rolldeath <- NA
          exdf3$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            exdf3$rolldeath[i] <- exdf3$deaths[i-18]+exdf3$deaths[i]+exdf3$deaths[i+18]
            exdf3$rollpeople[i] <- exdf3$people[i-18]+exdf3$people[i]+exdf3$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf3$rolldeath[i] <- NA
              exdf3$rollpeople[i] <- NA
            }else{
            }
          }
          plotdata$deaths <- c(expre3a$deaths,expre3b$deaths,exdf3$rolldeath,expost3$deaths)
          plotdata$people <- c(expre3a$people,expre3b$deaths,exdf3$rollpeople,expost3$people)
        }
      }else if(input$extend3 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre3a <- plotdata[which(plotdata$pop == '1'),]
          expre3b <- plotdata[which(plotdata$pop == '2'),]
          exdf3 <- plotdata[which(plotdata$pop == '3'),]
          expost3 <- plotdata[which(plotdata$pop == '4'),]
          exdf3$rolldeath <- NA
          exdf3$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            
            if(is.element(exdf3$year[i],cyears) | (input$country == "Cook Islands" & exdf3$year[i] == 2011)){
              exdf3$rolldeath[i] <- exdf3$deaths[i-18]+exdf3$deaths[i]+exdf3$deaths[i+18]
              exdf3$rollpeople[i] <- exdf3$people[i]*3
            }else{

            }
          }
          for(i in 1:18){
            if(is.element(exdf3$year[1],cyears)){
              exdf3$rolldeath[18+i] <- exdf3$deaths[i]+exdf3$deaths[18+i]+exdf3$deaths[36+i]
              exdf3$rollpeople[18+i] <- exdf3$people[i]*3
            }
            if(is.element(exdf3$year[length(exdf3$year)],cyears)){
              exdf3$rolldeath[length(exdf3$year)-36+i] <- exdf3$deaths[length(exdf3$year)-18+i]+exdf3$deaths[length(exdf3$year)-36+i]+exdf3$deaths[length(exdf3$year)-54+i]
              exdf3$rollpeople[length(exdf3$year)-36+i] <- exdf3$people[length(exdf3$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(expre3a$deaths,expre3b$deaths,exdf3$rolldeath,expost3$deaths)
          plotdata$people <- c(expre3a$people,expre3b$people,exdf3$rollpeople,expost3$people)
        }
      }
      if(input$extend4 == '3-year Rolling Average'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre4 <- plotdata[which(plotdata$pop != '4'),]
          exdf4 <- plotdata[which(plotdata$pop == '4'),]
          exdf4$rolldeath <- NA
          exdf4$rollpeople <- NA
          exdf4$rolldeath[19:(length(people)/upto-18)] <- (exdf4$deaths[19:(length(people)/upto-18)-18]+exdf4$deaths[19:(length(people)/upto-18)]+exdf4$deaths[19:(length(people)/upto-18)+18])/3
          exdf4$rollpeople[19:(length(people)/upto-18)] <- exdf4$people[19:(length(people)/upto-18)]
          plotdata$deaths <- c(expre4$deaths,exdf4$rolldeath)
          plotdata$people <- c(expre4$people,exdf4$rollpeople)
        }
      }else if(input$extend4 == '3-year Period'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre4 <- plotdata[which(plotdata$pop != '4'),]
          exdf4 <- plotdata[which(plotdata$pop == '4'),]
          exdf4$rolldeath <- NA
          exdf4$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            exdf4$rolldeath[i] <- exdf4$deaths[i-18]+exdf4$deaths[i]+exdf4$deaths[i+18]
            exdf4$rollpeople[i] <- exdf4$people[i-18]+exdf4$people[i]+exdf4$people[i+18]
          }
          for(i in 19:(length(people)/upto-18)){
            if((i+17) %% 54 < 36){
              exdf4$rolldeath[i] <- NA
              exdf4$rollpeople[i] <- NA
            }else{
            }
          }
          plotdata$deaths <- c(expre4$deaths,exdf4$rolldeath)
          plotdata$people <- c(expre4$people,exdf4$rollpeople)
        }
      }else if(input$extend4 == 'Census year +/- 1'){
        if(length(counts[,1]) < -1){
          
        }else{
          expre4 <- plotdata[which(plotdata$pop != '4'),]
          exdf4 <- plotdata[which(plotdata$pop == '4'),]
          exdf4$rolldeath <- NA
          exdf4$rollpeople <- NA
          for(i in 19:(length(people)/upto-18)){
            
            if(is.element(exdf4$year[i],cyears) | (input$country == "Cook Islands" & exdf4$year[i] == 2011)){
              exdf4$rolldeath[i] <- exdf4$deaths[i-18]+exdf4$deaths[i]+exdf4$deaths[i+18]
              exdf4$rollpeople[i] <- exdf4$people[i]*3
            }else{

            }
          }
          for(i in 1:18){
            if(is.element(exdf4$year[1],cyears)){
              exdf4$rolldeath[18+i] <- exdf4$deaths[i]+exdf4$deaths[18+i]+exdf4$deaths[36+i]
              exdf4$rollpeople[18+i] <- exdf4$people[i]*3
            }
            if(is.element(exdf4$year[length(exdf4$year)],cyears)){
              exdf4$rolldeath[length(exdf4$year)-36+i] <- exdf4$deaths[length(exdf4$year)-18+i]+exdf4$deaths[length(exdf4$year)-36+i]+exdf4$deaths[length(exdf4$year)-54+i]
              exdf4$rollpeople[length(exdf4$year)-36+i] <- exdf4$people[length(exdf4$year)-18+i]*3
            }
          }
          plotdata$deaths <- c(expre4$deaths,exdf4$rolldeath)
          plotdata$people <- c(expre4$people,exdf4$rollpeople)
        }
      }
    }
    #################################################################
    #bulk of technical code below
    #crude rate calculation and empty rates set up
    percent <- (plotdata$deaths/plotdata$people)
    plotdata$percent <- percent
    plotdata$rate <- 0
    plotdata$ratelow <- 0
    plotdata$rateupp <- 0
    plotdata$rategammalow <- 0
    plotdata$rategammaupp <- 0
    #summing vectors for total asr and var(asr) and if internal finding base population
    summer <- 0
    varasr <- 0
    w <- rep(0,18)
    w_max <- 0
    z <- 0
    v <- rep(0,18)
    v_tot <- 0
    adjrate <- 0
    popint <- 0
    if(input$stdtype == "Internal"){
      if(upto == 1){
        popint <- 1
      }else if(upto == 2){
        if(input$int2 == "Population 2"){
          popint <- 2
        }else{
          popint <- 1
        }
      }else if(upto == 3){
        if(input$int3 == "Population 3"){
          popint <- 3
        }else if(input$int3 == "Population 2"){
          popint <- 2
        }else{
          popint <- 1
        }
      }else{
        if(input$int4 == "Population 4"){
          popint <- 4
        }else if(input$int4 == "Population 3"){
          popint <- 3
        }else if(input$int4 == "Population 2"){
          popint <- 2
        }else{
          popint <- 1
        }
      }
    }
    #finding length of dataset by year
    #yeardiff <- as.numeric(plotdata$year[length(plotdata$year)])-as.numeric(plotdata$year[1])+1
    #setting up data frame of repeated internal data if needed for internal std
    popintdf <- data.frame(plotdata[which(plotdata$pop == popint),])
    if(upto == 1){
      intdf <- popintdf
    }else if(upto == 2){
      intdf <- rbind(popintdf,popintdf)
    }else if(upto == 3){
      intdf <- rbind(popintdf,popintdf,popintdf)
    }else{
      intdf <- rbind(popintdf,popintdf,popintdf,popintdf)
    }
    #this loops starts rate calculations, it is done in reverse order as total is wanted
    #as the top value and needs other calculations done first
      for(i in 1:length(plotdata$rate)){
        r <- length(plotdata$rate)+1-i
        if(is.na(deaths[r]) ){#|| deaths[r] == 0){
          #rates are 0 if no death but has upper approximation
          #THIS IS THE ISSUE
          ################
          ##############
          ##############
          #############
          #################
          #plotdata$rate[r] <- 0.001
          #plotdata$rateupp[r] <- (3/people[r])*100000
          #plotdata$ratelow[r] <- 0
        }else{
          
          if(input$stdtype == "External"){
            plotdata$rate[r] <- percent[r]*100000#*as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))])
            #rate gets externally standardised in line above and below adds
            #up rates to get the asr easily
            if(deaths[r] == 0){
              plotdata$rate[r] <- 0.001
            }
            if(plotdata$ageband[r] == "Total"){
              plotdata$rate[r] <- summer
              summer <- 0
            }else{
              summer <- summer + plotdata$rate[r]*(as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))]))
            }
            #if(input$agegroup == "Total"){
            if(plotdata$ageband[r] == "Total"){
                #calculate CI if a total row and uses varasr as calculated below
                plotdata$rateupp[r] <- plotdata$rate[r]+1.96*sqrt(varasr)
                plotdata$ratelow[r] <- max(plotdata$rate[r]-1.96*sqrt(varasr),0)
                w_max <- mean(w)
                z <- mean(w^2)
                v_tot <- sum(v)
                adjrate <- plotdata$rate[r]/100000
                plotdata$rategammalow[r] <- v_tot/2/adjrate*qchisq(0.05/2,2*((adjrate^2)/v_tot))*100000
                plotdata$rategammaupp[r] <- (v_tot+z)/2/(w_max+adjrate)*qchisq(1-0.05/2,2*(((adjrate+w_max)^2)/v_tot+z))*100000
                varasr <- 0
                w_max <- 0
                z <- 0
                v_tot <- 0
                adjrate <- 0
                w <- rep(0,18)
                v <- rep(0,18)
              }else{
                varasr <- varasr + 100000*(plotdata$percent[r]*100000/plotdata$people[r])*((as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))]))^2)#(((plotdata$rate[r]/as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))])*as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))])^2))/plotdata$deaths[r])
                w[(i-1)%%18+1] <- (as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))]))/plotdata$people[r]
                v[(i-1)%%18+1] <- (w[(i-1)%%18+1]^2)*plotdata$deaths[r]
                #otherwise a simple proportion based CI is used for the age group
                plotdata$rategammalow[r] <- 1/2*qchisq(0.05/2,2*plotdata$deaths[r])/plotdata$people[r]*100000#*(as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))]))
                plotdata$rategammaupp[r] <- 1/2*qchisq(1-0.05/2,2*(plotdata$deaths[r]+1))/plotdata$people[r]*100000#*(as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))]))
                plotdata$rateupp[r] <- plotdata$rate[r] + 1.96*(sqrt(plotdata$percent[r]/plotdata$people[r])*100000)  #(percent[r] + 1.96*sqrt((percent[r]*(1-percent[r]))/people[r]))*100000*as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))])#(3/people[i])*100000#(percent[i] + 1.96*sqrt((percent[i]*(1-percent[i]))/people[i]))*100000
                plotdata$ratelow[r] <- max(0,plotdata$rate[r] - 1.96*(sqrt(plotdata$percent[r]/plotdata$people[r])*100000))    #max((percent[r] - 1.96*sqrt((percent[r]*(1-percent[r]))/people[r]))*100000*as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))]),0)#0#(percent[i] - 1.96*sqrt((percent[i]*(1-percent[i]))/people[i]))*100000
                
              }
            #}else{
            #}
          }else{
            #calculates internal standardisation, uses similar process as above
            plotdata$rate[r] <- percent[r]*100000
            if(deaths[r] == 0){
              plotdata$rate[r] <- 0.001
            }
            
            if(plotdata$ageband[r] == "Total"){
              plotdata$rate[r] <- sum(v)/sum(w)*(as.numeric(intdf[r,4])/as.numeric(intdf[r,5]))*100000
              #summer <- 0
              varasr <- (sum(v)/(sum(w)^2))*((as.numeric(intdf[r,4])/as.numeric(intdf[r,5]))^2)
              plotdata$rateupp[r] <- plotdata$rate[r]+1.96*sqrt(varasr)*100000
              plotdata$ratelow[r] <- max(plotdata$rate[r]-1.96*sqrt(varasr)*100000,0)
              varasr <- 0
              w <- rep(0,18)
              v <- rep(0,18)
            }else{
              w[(i-1)%%18+1] <- plotdata$people[r]*(as.numeric(intdf[r,4])/as.numeric(intdf[r,5]))
              v[(i-1)%%18+1] <- plotdata$deaths[r]
              plotdata$rateupp[r] <- plotdata$rate[r] + 1.96*(sqrt(plotdata$percent[r]/plotdata$people[r])*100000)  #(percent[r] + 1.96*sqrt((percent[r]*(1-percent[r]))/people[r]))*100000*as.numeric(data_standard[which(data_standard$Standard==input$extstd),(which(selectgrouptext==plotdata$ageband[r]))])#(3/people[i])*100000#(percent[i] + 1.96*sqrt((percent[i]*(1-percent[i]))/people[i]))*100000
              plotdata$ratelow[r] <- max(0,plotdata$rate[r] - 1.96*(sqrt(plotdata$percent[r]/plotdata$people[r])*100000))
              #summer <- summer + w[(i-1)%%18+1]
            }
          }
        }
      }
    #with rates calculated now rr is calculated
    #baseline is repeated in data sets to allow easy data frame division
    rr <- rep(0,length(percent))
    upp <- rep(0,length(percent))
    low <- rep(0,length(percent))
    densub <- plotdata[which(plotdata$pop==base),7]
    denrep <- rep(densub,num)
    deathsub <- plotdata[which(plotdata$pop==base),4]
    deathrep <- rep(deathsub,num)
    peoplesub <- plotdata[which(plotdata$pop==base),5]
    peoplerep <- rep(peoplesub,num)
    
    #rrl <- length(rr)
    for(i in 1:length(rr)){
      if(is.na(denrep[i]) || denrep[i] == 0 || is.na(deathrep[i]) || deathrep[i] == 0 || is.na(peoplerep[i]) || peoplerep[i] == 0){
        #0 rates
        denrep[i] <- 1
        deathrep[i] <- 1
      }
      if(is.na(percent[i]) || percent[i] == 0 || plotdata$pop[i] == base){
        rr[i] <- NA
        upp[i] <- NA
        low[i] <- NA
      }else{
        #as already standardised can divide rates then calculate their CI's
        rr[i] <- plotdata$rate[i]/denrep[i]
        upp[i] <- exp(log(rr[i]) + 1.96*sqrt(1/deaths[i] + 1/deathrep[i] - 1/people[i] - 1/peoplerep[i]))
        low[i] <- exp(log(rr[i]) - 1.96*sqrt(1/deaths[i] + 1/deathrep[i] - 1/people[i] - 1/peoplerep[i]))
      }
    }
    
    #add them to data frame
    plotdata$rr <- rr
    plotdata$lower <- low
    plotdata$upper <- upp
    
    #remove all rows which aren't totals
    if(input$agegroup == "Total"){
      if(input$proj == TRUE) {
        
      }else{
        plotdata <- plotdata[which(plotdata$ageband == "Total"),]
      }
    }
    
    #below are conditions that are data specific, the following as missing data in gastric for maori in 95
    if(input$issue1 == "Cancer - Gastric Death" & (input$ethnic1 == "Maori" | input$ethnic1 == "Non-Maori")){
      plotdata[which(plotdata$year == 1995),4:14] <- NA # & plotdata$pop == 1
      if(input$extend1 == '3-year Rolling Average' | input$extend1 == '3-year Period'){
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 1)-1,4:12] <- NA
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 1)+1,4:12] <- NA
      }
    }
    if(input$issue2 == "Cancer - Gastric Death" & (input$ethnic2 == "Maori" | input$ethnic2 == "Non-Maori")){
      plotdata[which(plotdata$year == 1995),4:14] <- NA # & plotdata$pop == 2
      if(input$extend2 == '3-year Rolling Average' | input$extend2 == '3-year Period'){
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 2)-1,4:12] <- NA
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 2)+1,4:12] <- NA
      }
    }
    if(input$issue3 == "Cancer - Gastric Death" & (input$ethnic3 == "Maori" | input$ethnic3 == "Non-Maori")){
      plotdata[which(plotdata$year == 1995),4:14] <- NA # & plotdata$pop == 3
      if(input$extend3 == '3-year Rolling Average' | input$extend3 == '3-year Period'){
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 3)-1,4:12] <- NA
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 3)+1,4:12] <- NA
      }
    }
    if(input$issue4 == "Cancer - Gastric Death" & (input$ethnic4 == "Maori" | input$ethnic4 == "Non-Maori")){
      plotdata[which(plotdata$year == 1995),4:14] <- NA # & plotdata$pop == 4
      if(input$extend4 == '3-year Rolling Average' | input$extend4 == '3-year Period'){
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 4)-1,4:12] <- NA
        plotdata[which(plotdata$year == 1995 & plotdata$pop == 4)+1,4:12] <- NA
      }
    }
    
    #the following remove first and last observations
    if(input$extend1 == '3-year Rolling Average' | input$extend1 == '3-year Period'){
      plotdata[1,4:14] <- NA 
      plotdata[length(plotdata$pop)/upto,4:14] <- NA 
    }
    if(input$extend2 == '3-year Rolling Average' | input$extend2 == '3-year Period'){
      plotdata[length(plotdata$pop)/upto+1,4:14] <- NA 
      plotdata[2*length(plotdata$pop)/upto,4:14] <- NA 
    }
    if(input$extend3 == '3-year Rolling Average' | input$extend3 == '3-year Period'){
      plotdata[2*length(plotdata$pop)/upto+1,4:14] <- NA
      plotdata[3*length(plotdata$pop)/upto,4:14] <- NA
    }
    if(input$extend4 == '3-year Rolling Average' | input$extend4 == '3-year Period'){
      plotdata[3*length(plotdata$pop)/upto+1,4:14] <- NA
      plotdata[length(plotdata$pop),4:14] <- NA
    }
    
    #the following is a flagging system based on variance of rates and rr
    #using 30, 50 and 100% as the levels 1,2,3
    plotdata$flagrate <- 0
    plotdata$flagrr <- 0
    for(i in 1:length(plotdata$rate)){
      if(is.na(plotdata$rate[i]) || is.na(plotdata$rateupp[i])){
      }else if((plotdata$rateupp[i] - plotdata$rate[i])/plotdata$rate[i] > 1){
        plotdata$flagrate[i] <- 3
      }else if((plotdata$rateupp[i] - plotdata$rate[i])/plotdata$rate[i] > 0.5){
        plotdata$flagrate[i] <- 2
      }else if((plotdata$rateupp[i] - plotdata$rate[i])/plotdata$rate[i] > 0.3){
        plotdata$flagrate[i] <- 1
      }
      if(is.na(plotdata$rr[i]) || is.na(plotdata$upper[i])){
      }else if((plotdata$upper[i] - plotdata$rr[i])/plotdata$rr[i] > 1){
        plotdata$flagrr[i] <- 3
      }else if((plotdata$upper[i] - plotdata$rr[i])/plotdata$rr[i] > 0.5){
        plotdata$flagrr[i] <- 2
      }else if((plotdata$upper[i] - plotdata$rr[i])/plotdata$rr[i] > 0.3){
        plotdata$flagrr[i] <- 1
      }
    }
    messagerate <- 0
    messagerate <- max(plotdata$flagrate)
    messagerr <- 0
    messagerr <- max(plotdata$flagrr)
    plotdata$flagrate <- 0
    plotdata$flagrr <- 0
    #supposedly finding the max of the flagging and will output a warning message
    ###
    ###not fully implemented
    ###
    
    #data containing all the plots need

    plotdata$Population <- NA
    for(i in 1:length(plotdata$pop)){
      if(plotdata$pop[i] == 1){
        if(input$gender1 == "Total"){
          if(input$ethnic1 == "Total"){
            pop1 <- "Total Pop 1"
          }else{
            pop1 <- paste(input$ethnic1,"Pop 1",sep=" ")
          }
        }else{
          if(input$ethnic1 == "Total"){
            pop1 <- paste(input$gender1,"Pop 1",sep=" ")
          }else{
            pop1 <- paste(input$gender1,input$ethnic1,"Pop 1",sep=" ")
          }
        }
        plotdata$Population[i] <- pop1
        pop1 <- NULL
      }else if(plotdata$pop[i] == 2){
        if(input$gender2 == "Total"){
          if(input$ethnic2 == "Total"){
            pop2 <- "Total Pop 2"
          }else{
            pop2 <- paste(input$ethnic2,"Pop 2",sep=" ")
          }
        }else{
          if(input$ethnic2 == "Total"){
            pop2 <- paste(input$gender2,"Pop 2",sep=" ")
          }else{
            pop2 <- paste(input$gender2,input$ethnic2,"Pop 2",sep=" ")
          }
        }
        plotdata$Population[i] <- pop2
        pop2 <- NULL
      }else if(plotdata$pop[i] == 3){
        if(input$gender3 == "Total"){
          if(input$ethnic3 == "Total"){
            pop3 <- "Total Pop 3"
          }else{
            pop3 <- paste(input$ethnic3,"Pop 3",sep=" ")
          }
        }else{
          if(input$ethnic3 == "Total"){
            pop3 <- paste(input$gender3,"Pop 3",sep=" ")
          }else{
            pop3 <- paste(input$gender3,input$ethnic3,"Pop 3",sep=" ")
          }
        }
        plotdata$Population[i] <- pop3
        pop3 <- NULL
      }else if(plotdata$pop[i] == 4){
        if(input$gender4 == "Total"){
          if(input$ethnic4 == "Total"){
            pop4 <- "Total Pop 4"
          }else{
            pop4 <- paste(input$ethnic4,"Pop 4",sep=" ")
          }
        }else{
          if(input$ethnic4 == "Total"){
            pop4 <- paste(input$gender4,"Pop 4",sep=" ")
          }else{
            pop4 <- paste(input$gender4,input$ethnic4,"Pop 4",sep=" ")
          }
        }
        plotdata$Population[i] <- pop4
        pop4 <- NULL
      }
    }
    if(input$agegroup == "Total"){
      newdata <- data.frame(plotdata$rr,plotdata$upper,plotdata$lower,unique(loopingyear),plotdata$ageband,as.factor(plotdata$pop),plotdata$rate,plotdata$rateupp,plotdata$ratelow,plotdata$Population,plotdata$rategammalow,plotdata$rategammaupp,plotdata$deaths,plotdata$people)
    }else{
      newdata <- data.frame(plotdata$rr,plotdata$upper,plotdata$lower,loopingyear,plotdata$ageband,as.factor(plotdata$pop),plotdata$rate,plotdata$rateupp,plotdata$ratelow,plotdata$Population,plotdata$rategammalow,plotdata$rategammaupp,plotdata$deaths,plotdata$people)
    }
    #naming output table for it to make sense
    colnames(newdata)[1] <- "rr"
    colnames(newdata)[2] <- "upp"
    colnames(newdata)[3] <- "low"
    colnames(newdata)[4] <- "year"
    colnames(newdata)[5] <- "ageband"
    colnames(newdata)[6] <- "pop"
    colnames(newdata)[7] <- "rate"
    colnames(newdata)[8] <- "rateupp"
    colnames(newdata)[9] <- "ratelow"
    colnames(newdata)[10] <- "Population"
    colnames(newdata)[11] <- "gammalow"
    colnames(newdata)[12] <- "gammaupp"
    colnames(newdata)[13] <- "deaths"
    colnames(newdata)[14] <- "people"
    
    #giving names for plot legend based on gender and ethnicity
    #takes gender and ethnicity and pastes it with a number on the end
    #data frame for AR, ardf pulls non base data
    #arbase contains what to subtract from others
    ardf <- newdata[which(newdata$pop != base),]
    arbase <- newdata[which(newdata$pop == base),]
    if(upto == 2){
      arsub <- arbase
    }else if(upto == 3){
      arsub <- rbind(arbase,arbase)
    }else if(upto == 4){
      arsub <- rbind(arbase,arbase,arbase)
    }else{
      arsub <- ardf
    }
    #creates equal data frames by repeating the base so a simple subtraction can be used
    varar <- (((ardf$deaths*(ardf$people-ardf$deaths))/((ardf$people^2)*(ardf$people-1)))+((arsub$deaths*(arsub$people-arsub$deaths))/((arsub$people^2)*(arsub$people-1))))
    ardf$rateupp <- ardf$rate-arsub$rate + 1.96*sqrt(varar)*100000#sqrt(((ardf$rateupp-ardf$rate)/1.96)^2 + ((arsub$rateupp-arsub$rate)/1.96)^2)
    ardf$ratelow <- ardf$rate-arsub$rate - 1.96*sqrt(varar)*100000#sqrt(((ardf$rate-ardf$ratelow)/1.96)^2 + ((arsub$rate-arsub$ratelow)/1.96)^2)
    ardf$rate <- ardf$rate-arsub$rate
    
    #data for plots set up
    fullRate <<- newdata
    fullRR <<- newdata[which(newdata$pop != base),]
    fullAR <<- ardf
    
    plotdata$deaths <- as.factor(round(plotdata$deaths,0))
    fullSumm <<- rbind(plotdata[c(2,3,4,7,15)])
    
    if (input$proj == TRUE) {
      #if (max(as.numeric(as.character(plotdata$year))) > min(as.numeric(as.character(plotdata$year)))) {
        #print(plotdata)
#        plotdata <- plotdata[which(as.numeric(as.character(plotdata$year)) == max(as.numeric(as.character(plotdata$year)))), ]
      #}
      #print(plotdata)
      plotdata <- createProjections(plotdata)
      plotdata
    } else {
      plotdata#[-c(5,6,15,16)]
    }
  }
  
  createProjections <- function (lastyear) {
    #print(lastyear)
    #deaths <- plotdata$deaths
    npop <- as.numeric(as.character(lastyear$pop[length(lastyear$pop)]))
    rates <- lastyear$rate[which(as.numeric(as.character(lastyear$year)) == max(as.numeric(as.character(lastyear$year))))]#1:npop * length(lastyear$rate) / npop]
    #print(rates)
    if (input$geog == "National") {
      place <- "nat"
    } else if (input$subgeog == "Regional Council") {
      place <- "reg"
    } else if (input$subgeog == "TLA") {
      place <- "tla"
    } else if (input$subgeog == "DHB") {
      place <- "dhb"
    }
    e1 <- tolower(getEth(1))
    e1 <- gsub("[-]", "", e1)
    dat1 <- read.csv(paste("data/denominator/proj/proj", e1, place, ".csv", sep = ""), header = TRUE, sep = ";")
    g1 <- getGender(1)
    dat1 <- dat1[which(dat1$Factor == g1), ]
    dat1$pop <- 1
    data <- dat1[complete.cases(dat1), ]
    if (npop > 1) {
      e2 <- tolower(getEth(2))
      e2 <- gsub("[-]", "", e2)
      dat2 <- read.csv(paste("data/denominator/proj/proj", e2, place, ".csv", sep = ""), header = TRUE, sep = ";")
      g2 <- getGender(2)
      dat2 <- dat2[which(dat2$Factor == g2), ]  
      dat2$pop <- 2
      data <- rbind(data, dat2[complete.cases(dat2), ])
    }
    if (npop > 2) {
      e3 <- tolower(getEth(3))
      e3 <- gsub("[-]", "", e3)
      dat3 <- read.csv(paste("data/denominator/proj/proj", e3, place, ".csv", sep = ""), header = TRUE, sep = ";")
      g3 <- getGender(3)
      dat3 <- dat3[which(dat3$Factor == g3), ]
      dat3$pop <- 3
      data <- rbind(data, dat3[complete.cases(dat3), ])
    }
    if (npop > 3) {
      e4 <- tolower(getEth(4))
      e4 <- gsub("[-]", "", e4)
      dat4 <- read.csv(paste("data/denominator/proj/proj", e4, place, ".csv", sep = ""), header = TRUE, sep = ";")
      g4 <- getGender(4)
      dat4 <- dat4[which(dat4$Factor == g4), ]
      dat4$pop <- 4
      data <- rbind(data, dat4[complete.cases(dat4), ])
    }
    if (place != "nat") {
      if (place == "reg") {
        data <- data[which(data$Region == input$reg), ]
      } else if (place == "tla") {
        data <- data[which(data$Region == input$tla), ]
      } else if (place == "tla") {
        data <- data[which(data$Region == input$dhb), ]
      }
      data <- data[-2]
    }
    data <- data[-2]
    #print(melt(data, id.vars = 1))
    meltdata <- melt(data, id.vars = 1)
    #print(meltdata)
    meltdata$ageband <- gsub("X0.4", "00-04", meltdata$variable)
    meltdata$ageband <- gsub("X5.9", "05-09", meltdata$ageband)
    meltdata$ageband <- gsub("X", "", meltdata$ageband)
    meltdata$ageband <- gsub("[.]", "-", meltdata$ageband)
    meltdata$ageband <- gsub("80-and-over", "80+", meltdata$ageband)
    meltdata <- meltdata[-2]
    meltdata <- meltdata[-which(meltdata$ageband == "pop"), ]
    #print(meltdata)
    ratestot <- rates
    ratestot[which((1:length(ratestot) - 1) %% 18 == 0)] <- 0
    ratestot <- ratestot[c(2:length(ratestot), 1)]
    #print(ratestot)
    
    #ratesrep <- rep(rates, each = 6)
    #okay screwing up the indexing here, annoying total positioning
    #ratesrep <- ratesrep[rep(rep(0:(npop - 1), each = 6) * 108, 18) + 1:6 + rep(6 * 0:17, each = 6 * npop)]
    #print(ratesrep)
    #ratesrep <- ratesrep[c(7:length(ratesrep), 1:6)]
    #print(lastyear$rate)
    
    #print(meltdata$value)
    ratesrep <- ratestot[rep(1:18, each = npop) + 0:(npop - 1) * 18]
    #print(ratesrep)
    ratesrep <- rep(ratesrep, each = 6)
    #print(ratesrep)
    #rates are being iffy here too, try to set all those at a total -> 0
    meltdata$deaths <- meltdata$value * ratesrep / 100000
    #print(meltdata$deaths)
    meltdata <- meltdata[-2]
    #print(meltdata)
    
    #print(meltdata)
    meltdata$pop <- rep(rep(1:npop, each = 6), length(meltdata[ , 1]) / (6 * npop))
    meltdata$pop <- as.factor(meltdata$pop)
    

    #print(meltdata)
    deathsplit <- meltdata$deaths[1:(length(meltdata$deaths) - 6 * npop)]
    deathdone <- aggregate(deaths ~ Year + pop, meltdata, FUN = sum)$deaths
    meltdata$deaths[(length(meltdata$deaths) - 6 * npop + 1):length(meltdata$deaths)] <- deathdone 
    
    meltdata <- meltdata[-which(meltdata$Year == "2013"), ]
    
    #print(meltdata)
    #print(length(meltdata$deaths))
    #meltdata$deaths <- meltdata$deaths[rep(rep(0:(npop - 1), each = 5) * 90, 18) + 1:5 + rep(5 * 0:17, each = 5 * npop)]
    #print(meltdata)
    meltdata <- rbind(meltdata, data.frame(ageband = lastyear$ageband, Year = lastyear$year, deaths = as.numeric(as.character(lastyear$deaths)), pop = lastyear$pop))
    colnames(meltdata)[4] <- "Population"
    meltdata$Year <- as.numeric(as.character(meltdata$Year))
    #print(meltdata)
    meltdata <- meltdata[order(meltdata$Population, meltdata$Year), ]
    #print(meltdata)
    if (input$agegroup == "Total") {
      meltdata <- meltdata[which(meltdata$ageband == "Total"), ]
    }    
    ####Need to remove if 5-year bands specifically selected
    meltdf <<- meltdata
    meltdata[-which(is.element(meltdata$Year, c(1948:2012, 2013))), ]#"2013"))), ]
  }
  
  output$plotp <- renderPlot({
    if(input$updater == 0){
      return()
    }
    plotterProj(input$updater)
  })
  plotterProj <- function(n) {
    print(meltdf)
    ggplot(data = meltdf, aes(x = Year, y = deaths, col = Population)) +
       geom_line() + 
       geom_point() +
#       geom_errorbar(aes(ymin = ratelow, ymax = rateupp, col=Population), lty = 1, width = 0.2) +
#       geom_ribbon(aes(ymin = ratelow, ymax = rateupp, fill=Population), alpha = 0.2, lty = 0) +
#       geom_hline(aes(yintercept=0),size = 0.001) +
#       scale_x_continuous(name = "Year", breaks = naming, labels = naming) +
       facet_wrap(~ageband)+#.~ageband) +
       theme_bw() +
#       
       ggtitle("Projected deaths 2013-2038") +
       ylab("Projected deaths")
  }
  
  
  output$summary <- renderTable({
    if(input$updater == 0){
      return()
    }
    getTable(input$updater)
  })
  getTable <- function(n){
    localpop <- fullSumm$pop[length(fullSumm$pop)]
    summarydf <- data.frame(Year=unique(fullSumm$year))
    popnames <- unique(fullSumm$Population)
    if(input$format == "by pop"){
      if(localpop == 1){
        summarydf$Count <- fullSumm$deaths
        summarydf$Rate <- fullSumm$rate
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "))
        summarydf
      }else if(localpop == 2){
        summarydf$Count1 <- fullSumm$deaths[fullSumm$pop==1]
        summarydf$Rate1 <- fullSumm$rate[fullSumm$pop==1]
        summarydf$Count2 <- fullSumm$deaths[fullSumm$pop==2]
        summarydf$Rate2 <- fullSumm$rate[fullSumm$pop==2]
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "),
                                 paste(popnames[2],"Count",sep=" "),
                                 paste(popnames[2],"Rate",sep=" "))
        summarydf
      }else if(localpop == 3){
        summarydf$Count1 <- fullSumm$deaths[fullSumm$pop==1]
        summarydf$Rate1 <- fullSumm$rate[fullSumm$pop==1]
        summarydf$Count2 <- fullSumm$deaths[fullSumm$pop==2]
        summarydf$Rate2 <- fullSumm$rate[fullSumm$pop==2]
        summarydf$Count3 <- fullSumm$deaths[fullSumm$pop==3]
        summarydf$Rate3 <- fullSumm$rate[fullSumm$pop==3]
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "),
                                 paste(popnames[2],"Count",sep=" "),
                                 paste(popnames[2],"Rate",sep=" "),
                                 paste(popnames[3],"Count",sep=" "),
                                 paste(popnames[3],"Rate",sep=" "))
        summarydf
      }else if(localpop == 4){
        summarydf$Count1 <- fullSumm$deaths[fullSumm$pop==1]
        summarydf$Rate1 <- fullSumm$rate[fullSumm$pop==1]
        summarydf$Count2 <- fullSumm$deaths[fullSumm$pop==2]
        summarydf$Rate2 <- fullSumm$rate[fullSumm$pop==2]
        summarydf$Count3 <- fullSumm$deaths[fullSumm$pop==3]
        summarydf$Rate3 <- fullSumm$rate[fullSumm$pop==3]
        summarydf$Count4 <- fullSumm$deaths[fullSumm$pop==4]
        summarydf$Rate4 <- fullSumm$rate[fullSumm$pop==4]
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "),
                                 paste(popnames[2],"Count",sep=" "),
                                 paste(popnames[2],"Rate",sep=" "),
                                 paste(popnames[3],"Count",sep=" "),
                                 paste(popnames[3],"Rate",sep=" "),
                                 paste(popnames[4],"Count",sep=" "),
                                 paste(popnames[4],"Rate",sep=" "))
        summarydf
      }
    }else{
      if(localpop == 1){
        summarydf$Count <- fullSumm$deaths
        summarydf$Rate <- fullSumm$rate
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "))
        summarydf
      }else if(localpop == 2){
        summarydf$Count1 <- fullSumm$deaths[fullSumm$pop==1]
        summarydf$Count2 <- fullSumm$deaths[fullSumm$pop==2]
        summarydf$Rate1 <- fullSumm$rate[fullSumm$pop==1]
        summarydf$Rate2 <- fullSumm$rate[fullSumm$pop==2]
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[2],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "),
                                 paste(popnames[2],"Rate",sep=" "))
        summarydf
      }else if(localpop == 3){
        summarydf$Count1 <- fullSumm$deaths[fullSumm$pop==1]
        summarydf$Count2 <- fullSumm$deaths[fullSumm$pop==2]
        summarydf$Count3 <- fullSumm$deaths[fullSumm$pop==3]
        summarydf$Rate1 <- fullSumm$rate[fullSumm$pop==1]
        summarydf$Rate2 <- fullSumm$rate[fullSumm$pop==2]
        summarydf$Rate3 <- fullSumm$rate[fullSumm$pop==3]
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[2],"Count",sep=" "),
                                 paste(popnames[3],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "),
                                 paste(popnames[2],"Rate",sep=" "),
                                 paste(popnames[3],"Rate",sep=" "))
        summarydf
      }else if(localpop == 4){
        summarydf$Count1 <- fullSumm$deaths[fullSumm$pop==1]
        summarydf$Count2 <- fullSumm$deaths[fullSumm$pop==2]
        summarydf$Count3 <- fullSumm$deaths[fullSumm$pop==3]
        summarydf$Count4 <- fullSumm$deaths[fullSumm$pop==4]
        summarydf$Rate1 <- fullSumm$rate[fullSumm$pop==1]
        summarydf$Rate2 <- fullSumm$rate[fullSumm$pop==2]
        summarydf$Rate3 <- fullSumm$rate[fullSumm$pop==3]
        summarydf$Rate4 <- fullSumm$rate[fullSumm$pop==4]
        colnames(summarydf) <- c("Year",paste(popnames[1],"Count",sep=" "),
                                 paste(popnames[2],"Count",sep=" "),
                                 paste(popnames[3],"Count",sep=" "),
                                 paste(popnames[4],"Count",sep=" "),
                                 paste(popnames[1],"Rate",sep=" "),
                                 paste(popnames[2],"Rate",sep=" "),
                                 paste(popnames[3],"Rate",sep=" "),
                                 paste(popnames[4],"Rate",sep=" "))
        summarydf
      }
    }
  }
  #plotting process has a rendering that calls its plot method based on the generate button
  output$plot1 <- renderPlot({
    if(input$updater == 0){
      return()
    }
    plotterRate(input$updater)
  })
  #uses ggplot2 to plot the rate, layers its lines and points and labels, pop dependent
  plotterRate <- function(n){
    localpop <- fullRate$pop[length(fullRate$pop)]
    if(upperlim - lowerlim > 30){
      naming = lowerlim:upperlim * c(1:0, 0)
    }else{
      naming = lowerlim:upperlim
    } 
    ggplot(data=fullRate,aes(year,y=rate,col=Population)) +
      geom_line(aes(year,y=rate,col=Population),linetype=1) + 
      geom_point(aes(year,y=rate,col=Population),size=1) +
      geom_errorbar(aes(ymin = ratelow, ymax = rateupp, col=Population), lty = 1, width = 0.2) +
      geom_ribbon(aes(ymin = ratelow, ymax = rateupp, fill=Population), alpha = 0.2, lty = 0) +
      geom_hline(aes(yintercept=0),size = 0.001) +
      scale_x_continuous(name = "Year", breaks = naming, labels = naming) +
      facet_wrap(~ageband)+#.~ageband) +
      theme_bw() +
      
      ggtitle(title) +
      ylab("Rate per 100,000")
  }
  #rest of the flagging system, needs work
  output$msg1 <- renderText({
    if(input$updater == 0){
      return()
    }
    #messageRate(input$updater)
  })
  messageRate <- function(n){
      if(messagerate == 0){
        #paste("No warnings",sep="")
      }else if(messagerate == 1){
        #paste("Flag 1"," - Confidence bands wider than 30% of the value",sep="")
      }else if(messagerate == 2){
        #paste("Flag 2"," - Confidence bands wider than 50% of the value",sep="")
      }else if(messagerate == 3){
        #paste("Flag 3"," - Confidence bands wider than 100% of the value",sep="")
      }else{
        #paste("No warnings",sep="")
      }
      #paste("No warnings",sep="")
  }
  #same plot process as above
  output$plot3 <- renderPlot({
    if(input$updater == 0){
      return()
    }
    plotterAR(input$updater)
  })
  plotterAR <- function(n){
    localpop <- fullAR$pop[length(fullAR$pop)]
    if(upperlim - lowerlim > 30){
      naming = lowerlim:upperlim * c(1:0, 0)
    }else{
      naming = lowerlim:upperlim
    } 
    ggplot(data=fullAR,aes(year,y=rate,col=Population)) +
      geom_line(aes(year,y=rate,col=Population),linetype=1) + 
      geom_point(aes(year,y=rate,col=Population),size=1) +
      geom_errorbar(aes(ymin = ratelow, ymax = rateupp, col=Population), lty = 1, width = 0.2) +
      geom_ribbon(aes(ymin = ratelow, ymax = rateupp, fill=Population), alpha = 0.2, lty = 0) +
      geom_hline(aes(yintercept=0), size = 1, lty = "22", col = "orange") +
      scale_x_continuous(name = "Year", breaks = naming, labels = naming) +
      facet_wrap(~ageband)+#.~ageband) +
      theme_bw() +
      
      ggtitle(title) +
      ylab("Attributable Risk Difference")
  }
  output$msg3 <- renderText({
    if(input$updater == 0){
      return()
    }
    #messageRate(input$updater)
  })
  #same process again, last plot
  output$plot2 <- renderPlot({
    if(input$updater == 0){
      return()
    }
    plotterRR(input$updater)
  })
  plotterRR <- function(n){
    localpop <- fullRR$pop[length(fullRR$pop)]
    if(upperlim - lowerlim > 30){
      naming = lowerlim:upperlim * c(1:0, 0)
    }else{
      naming = lowerlim:upperlim
    } 
    ggplot(data=fullRR,aes(year,y=rr,col=Population)) +
      geom_line(aes(year,y=rr,col=Population),linetype=1) + 
      geom_point(aes(year,y=rr,col=Population),size=1) +
      geom_errorbar(aes(ymin = low, ymax = upp, col=Population), lty = 1, width = 0.2) +
      geom_ribbon(aes(ymin = low, ymax = upp, fill=Population), alpha = 0.2, lty = 0) +
      geom_hline(aes(yintercept=1), size = 1, lty = "22", col = "orange") +
      scale_x_continuous(name = "Year", breaks = naming, labels = naming) +
      facet_wrap(~ageband) +
      theme_bw() +
      
      ggtitle(title) +
      ylab("Relative Risk")
  }
  output$msg2 <- renderText({
    if(input$updater == 0){
      return()
    }
    #messageRR(input$updater)
  })
  messageRR <- function(n){
    if(messagerr == 0){
      #paste("",sep="")
    }else if(messagerr == 1){
      #paste("Flag 1"," - Confidence bands wider than 30% of the value",sep="")
    }else if(messagerr == 2){
      #paste("Flag 2"," - Confidence bands wider than 50% of the value",sep="")
    }else if(messagerr == 3){
      #paste("Flag 3"," - Confidence bands wider than 100% of the value",sep="")
    }
    #messagerr <- 0
  }
  output$intra <- renderText({
    if(input$updater == 0){
      return()
    }
    
      pops <- input$numpop
      sub1 <- input$ethnic1
      warn <- "Care should be taken when interpreting the following selections, as there may be issues of partially overlapping ethnic groups. For most accurate comparisons look at intra-ethnic differences"
      g0 <- c("Total","")
      g1 <- c("Non-Maori","Maori")
      g2 <- "Pacific"
      g3 <- "Asian"
      problem <- FALSE
      if(pops<2){
        sub <- c(sub1,"","","")
      }else if(pops<3){
        sub2 <- input$ethnic2
        sub <- c(sub1,sub2,"","")
      }else if(pops<4){
        sub2 <- input$ethnic2 
        sub3 <- input$ethnic3
        sub <- c(sub1,sub2,sub3,"")
      }else{
        sub2 <- input$ethnic2 
        sub3 <- input$ethnic3
        sub4 <- input$ethnic4
        sub <- c(sub1,sub2,sub3,sub4)
      }
      if(is.element(sub[1],g0)){
        if(is.element(sub[2],g0)){
          if(is.element(sub[3],g0)){
          }else if(is.element(sub[3],g1)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else if(is.element(sub[3],g2)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g2)) problem = TRUE
          }else if(is.element(sub[3],g3)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g3)) problem = TRUE
          }
        }else if(is.element(sub[2],g1)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else if(is.element(sub[3],g1)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else{problem = TRUE}
        }else if(is.element(sub[2],g2)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g2)) problem = TRUE
          }else if(is.element(sub[3],g2)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g2)) problem = TRUE
          }else{problem = TRUE}
        }else if(is.element(sub[2],g3)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g3)) problem = TRUE
          }else if(is.element(sub[3],g3)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g3)) problem = TRUE
          }else{problem = TRUE}
        }      
      }else if(is.element(sub[1],g1)){
        if(is.element(sub[2],g0)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else if(is.element(sub[3],g1)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else{problem = TRUE}
        }else if(is.element(sub[2],g1)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else if(is.element(sub[3],g1)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else{problem = TRUE}
        }else{problem = TRUE}
      }else if(is.element(sub[1],g1)){
        if(is.element(sub[2],g0)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else if(is.element(sub[3],g1)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else{problem = TRUE}
        }else if(is.element(sub[2],g1)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else if(is.element(sub[3],g1)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g1)) problem = TRUE
          }else{problem = TRUE}
        }else{problem = TRUE}
      }else if(is.element(sub[1],g2)){
        if(is.element(sub[2],g0)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g2)) problem = TRUE
          }else if(is.element(sub[3],g2)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g2)) problem = TRUE
          }else{problem = TRUE}
        }else if(is.element(sub[2],g2)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g2)) problem = TRUE
          }else if(is.element(sub[3],g2)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g2)) problem = TRUE
          }else{problem = TRUE}
        }else{problem = TRUE}
      }else if(is.element(sub[1],g3)){
        if(is.element(sub[2],g0)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g3)) problem = TRUE
          }else if(is.element(sub[3],g3)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g3)) problem = TRUE
          }else{problem = TRUE}
        }else if(is.element(sub[2],g3)){
          if(is.element(sub[3],g0)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g3)) problem = TRUE
          }else if(is.element(sub[3],g3)){
            if(is.element(sub[4],g0)){}else if(!is.element(sub[4],g3)) problem = TRUE
          }else{problem = TRUE}
        }else{problem = TRUE}
      }
    #warn <- c(sub,"Care should be taken when interpreting the following selections, as there may be issues of partially overlapping ethnic groups. For most accurate comparisons look at intra-ethnic differences")
    
    if(problem) warn #else bands()
    
  })
  
  
  #BYO functionality, below gets the input csv and reads it before returning it for processing
  read <- function(byopop){
    if(byopop == 1){
      inFile <- input$file1
      if(is.null(inFile)){
        return(NULL)
      }
    }else if(byopop == 2){
      inFile <- input$file2
      if(is.null(inFile)){
        return(NULL)
      }
    }else if(byopop == 3){
      inFile <- input$file3
      if(is.null(inFile)){
        return(NULL)
      }
    }else{
      inFile <- input$file4
      if(is.null(inFile)){
        return(NULL)
      }
    }
    inputdata <<- read.csv(inFile$datapath, header=T, sep=',')
    if(dim(inputdata)[1]==1){
      inputdata <<- read.csv(inFile$datapath, header=T, sep=';')
    }
    inputdata
  }
  
  #here are all the buttons below graphs along with the methods to download them
  output$csv <- renderUI({
    downloadButton('downloadData','Download data as .csv')
  })
  output$png1 <- renderUI({
    downloadButton('downloadPlot1','Download Rate graph as .png')
  })
  output$png3 <- renderUI({
    downloadButton('downloadPlot3','Download AR graph as .png')
  })
  output$png2 <- renderUI({
    downloadButton('downloadPlot2','Download RR graph as .png')
  })
  
  output$txt <- renderUI({
    downloadButton('downloadSummary','Download summary as .txt')
  })
  output$csvemp <- renderUI({
    wellPanel(
      sliderInput("yearemp","Year range (start and end)",
                min = 1948,
                max = 2013,
                step = 1,
                sep = "",
                value=c(1948,2013)),
      downloadButton('downloadEmp','Download template as .csv')
    )
    
  })
  #downloaders, data writes table as a csv as data.csv
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('data','.csv',sep='')
    },
    content = function(file) {
      write.csv(tocsv,file)
    })
  output$downloadEmp <- downloadHandler(
    filename = function(){
      paste('data','.csv',sep='')
    },
    content = function(file) {
      ydiff = input$yearemp[2] - input$yearemp[1] + 1
      blankm = matrix(NA, nrow = ydiff * 3, ncol = 21)
      blankm[, 1] = rep(input$yearemp[1]:input$yearemp[2], each = 3)
      blankm[, 2] = rep(c("Male", "Female", "Total"), ydiff)
      blank = data.frame(blankm)
      colnames(blank) = c("Year",	"Factor",	"0.4",	"5.9",	"10.14",	"15.19",	"20.24",	"25.29",	"30.34",	"35.39",	"40.44",	"45.49",	"50.54",	"55.59",	"60.64",	"65.69",	"70.74",	"75.79",	"80 and over",	"Total", "Ethnicity")
      write.csv(blank,file)
    })
  #plot 1 pastes as png as graph.png
  output$downloadPlot1 <- downloadHandler(
    filename = function(){ 
      paste('graph','.png', sep='') },
    content = function(file) {
      ggsave(file,plotterRate(1))
    })
  #plot 2 and 3 are output the same as plot 1
  output$downloadPlot2 <- downloadHandler(
    filename = function(){ 
      paste('graph','.png', sep='') },
    content = function(file) {
      ggsave(file,plotterRR(1))
    })
  output$downloadPlot3 <- downloadHandler(
    filename = function(){ 
      paste('graph','.png', sep='') },
    content = function(file) {
      ggsave(file,plotterAR(1))
    })
  
  output$help1 <- renderText({
    "The first image shows the data input form. It has repeated years, splits by gender and has age bands separated by a fullstop. There is also the option of ethnicity which can be added with a column appended to the end of the repeated string, as shown in the second image"
  })
  
  
  
})