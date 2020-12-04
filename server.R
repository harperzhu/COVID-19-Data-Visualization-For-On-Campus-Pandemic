# Define server function
server <- function(input, output) {
        source("Final_Project.R")
        output$network <- renderPlot({
                distribution_graph <- initiateNet(input$n.roommates, input$n.workers, input$n.people)
                plot(distribution_graph)
        })
        output$simulation <- renderPlot({
                infection_graph <- simulateDisease(input$distribution_graph, input$pct.starting.infected, input$max.time, input$infected, input$pparty, input$pmask, input$partyDay)
                ggplot(infection_graph)
        })
        output$networkDay <- renderPlot({
                networkDay_graph <- plotNetworkGraphDisease(input$results, input$timeToPlot, input$distribution_graph)
                ggplot(networkDay_graph)
        })
        
}