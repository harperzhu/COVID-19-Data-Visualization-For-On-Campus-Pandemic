# Define server function
server <- function(input, output) {
        source("Final_Project.R")
        output$hist <- renderPlot({
                #distribution_graph <- initiateNet(n.roommates, n.workers, n.people)
                distribution_graph <- initiateNet(input$n.roommates, input$n.workers, input$n.people)
                plot(distribution_graph)
        })

         output$simulation <- renderPlot({
                 #fullResults <- simulateDisease(distribution_graph, pct.starting.infected, max.time, pparty, pmask, partyDay)
                 fullResults <- simulateDisease(distribution_graph, input$pct.starting.infected, input$max.time, input$pparty, input$pmask, is_party, input$partyDay)
                 infections.by.time = fullResults[[1]]
                 results = fullResults[[2]]
                 #print(class(infections.by.time))
                 ggplot(data = infections.by.time, aes(x = t, y = S, col="S")) + geom_line() +
                        geom_line(aes(x = t, y = S, col="E")) +
                        geom_line(aes(x = t, y = I, col="I")) +
                       geom_line(aes(x = t, y = R, col="R"))
         })
         output$networkDay <- renderPlot({
                 ### To do: simulateDisease probably needs to return both infections.by.time AND results in order for this to work
                 #net.layout.by.time <- plotNetworkGraphDisease(results, timeToPlot, distribution_graph)
                 net.layout.by.time %>% 
                         filter(t %in% c(5, 6, 7, 20, 40)) %>%
                         ggplot(aes(xend = xend, yend = yend, x = x, y = y)) + 
                         geom_edges(color = "lightgray") +
                         geom_nodes(aes(color = is_infected)) + 
                         facet_wrap(~ t) + 
                         theme_blank()+scale_color_manual(values=c("deep sky blue","indianred1"))
         })
         
}
        
