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
          max = 500,
          value = 200
        ),
        sliderInput(
          "n.workers",
          "Number of coworkers in a workplace:",
          min = 1,
          max = 10,
          value = 5
        ),
        sliderInput(
          "n.roommates",
          "Number of rommates in a dorm room:",
          min = 1,
          max = 10,
          value = 5
        ),
      ),
      # mainPanel
      mainPanel(
        h1("Network Simulation"),
        h4("How connected are we?"),
        verbatimTextOutput("txtout"),
        plotOutput("hist", height=800, width = 800)
      )
    )
  )

#Page two: Disease Simulation
page_two <- tabPanel(
  "Disease Simulation",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = "is_party",
        label = h5("Is there a party?"),
        choices = list(
          "Yes",
          "No"
        ),
        selected = "No"
      ),
      sliderInput(
        "max.time",
        "The duration of estimation",
        min = 1,
        max = 365,
        value = 100
      ),
      sliderInput(
        "pct.starting.infected",
        "The percentage of people initially infected",
        min = 0,
        max = 1,
        value = 0.05
      ),
      sliderInput(
        "pmask",
        "percentage of people wearing a mask",
        min = 0,
        max = 1,
        value = 0.5
      ),
      sliderInput(
        "pparty",
        "percentage of people going to party",
        min = 0,
        max = 1,
        value = 0.5
      ),
      sliderInput(
        "partyDay",
        "which day people go to a party",
        min = 0,
        max = 1,
        value = 0.5
      ),
      
    ),
    #mainPanel
    mainPanel(
      h1("Infection over time"),
      #verbatimTextOutput("txtout"),
      plotOutput(outputId = "simulation",width = "100%")
      #plotOutput(outputId = "simulation",width = "100%", click = "plot_click")
  )
  )
)

#Page three: Simulation Network by days
page_three <- tabPanel(
  "Simulation Network by days",
    sidebarLayout(
      sidebarPanel(
        width = 3,
      sliderInput(
        "timeToPlot",
        "Which day during the simulation",
        min = 1,
        max = 16,
        value = 8
      ),
      # selectInput(
      #   inputId = "is_party",
      #   label = h5("Is there a party?"),
      #   choices = list(
      #     "Yes",
      #     "No"
      #   ),
      #   selected = "No."
      # )
      ),
      mainPanel(
        h1("How will infections look like in the network Graph?"),
        #verbatimTextOutput("txtout"),
        #helpText("if you can't see any graph, make sure your parameter is in the right range!"),
        plotOutput("networkDay",height=800, width = 800)
      )
    )
)

#Page four: Acknowledgment
 
 page_four <- tabPanel(
   "Acknowledgement",
   sidebarLayout(
     sidebarPanel(h6("© 2020 Harper Zhu, Anna Neufeld, Directed Reading Program in University of Washington")),
     mainPanel(
       h4("I am deeply grateful to DRP for giving me this wonderful opportunity to study more about Network modeling and the SIR model in disease simulation.
          Special thanks goes to my mentor Anna, 
          who has been patient, compassionate and helpful.
          I would never be able to make as much progress as I have without her.
          "),
     )
   )
 )
 
# #Define UI
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