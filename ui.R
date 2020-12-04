library(shiny)
library(shinythemes)
library(shinydashboard)

#Page one: Network Graph
page_one <-
  tabPanel(
    "Network Graph",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        sliderInput(
          "n.people",
          "Number of people who lived near campus:",
          min = 1,
          max = 1000,
          value = 500
        ),
        sliderInput(
          "n.workers",
          "Number of people have in-person jobs:",
          min = 1,
          max = 10,
          value = 5
        ),
        sliderInput(
          "n.roommates",
          "Number of rommates to live with:",
          min = 1,
          max = 10,
          value = 5
        ),
        plotOutput("hist")
      ),
      # mainPanel
      mainPanel(
        h1("Network Simulation"),
        h4("How connected are we?"),
        verbatimTextOutput("txtout"),
      )
    )
  ) 


#Page two: Disease Simulation
page_two <- tabPanel(
  "Disease Simulation",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput(
        "max.time",
        "The duration of estimation",
        min = 1,
        max = 1000,
        value = 500
      ),
      sliderInput(
        "pct.starting.infected",
        "The percentage of people initially infected",
        min = 1,
        max = 10,
        value = 5
      ),
      sliderInput(
        "pmask",
        "percentage of people wearing a mask",
        min = 1,
        max = 10,
        value = 5
      ),
      sliderInput(
        "pparty",
        "percentage of people going to party",
        min = 1,
        max = 10,
        value = 5
      ),
      sliderInput(
        "partyDay",
        "which day people go to a party",
        min = 1,
        max = 60,
        value = 5
      ),
      plotOutput(
        outputId = "simulation"
      )
    ),
    #mainPanel
    mainPanel(
      h1("Header 1"),
      
      h4("Output 1"),
      verbatimTextOutput("txtout")
  )
  )
)

#Page three: Simulation Network by days
page_three <- tabPanel(
  "Simulation Network by days",
  sidebarPanel(
    width = 3,
    sidebarLayout(
      sliderInput(
        "timeToPlot",
        "Which day during the simulation",
        min = 1,
        max = 16,
        value = 8
      ),
      #selectInput(
        #inputId = "party_no_party",
        #label = h5("Party or no party?"),
        #choices = list(
        #  "Party",
        #  "no Party",
        #),
        #selected = "no Party"
      #),
      plotOutput("networkDay")
    )
  ),
  # sidebarPanel
  mainPanel(
    h1("Header 1"),

    h4("Output 1"),
    verbatimTextOutput("txtout"),
    helpText("if you can't see any graph, make sure your parameter is in the right range!")
  )
) 
#Page four: Acknowledgement

page_four <- tabPanel("Acknowledgement", "I am deeply grateful to DRP for giving me this wonderful opportunity to study more about Network modeling and the SIR model in disease simulation.
Special thanks goes to my mentor Anna, who has been patient, compassionate and helpful in mentoring me.  I would never be able to make as much progress as I have without her. 
")

#Define UI
ui <- fluidPage(
  includeCSS("style.css"),
  navbarPage(
    theme = shinytheme("flatly"),
    "Can't I just go to one party?",
    page_one,
    page_two,
    page_three,
    page_four
  )
)
