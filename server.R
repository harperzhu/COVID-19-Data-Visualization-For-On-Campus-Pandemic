# Define server function

server <- function(input, output) {
        source("Final_Project.R")

        #Network graph
        output$hist <- renderPlot({
                distribution_graph <- initiateNet(input$n.roommates, input$n.workers, input$n.people)
                #plot(distribution_graph)
                l <- layout.fruchterman.reingold(distribution_graph, niter=1000)

                plot(distribution_graph, layout=l,
                     edge.arrow.size=0.5,
                     vertex.label.cex=0.1,
                     vertex.label.family="Helvetica",
                     vertex.label.font=0.5,
                     vertex.shape="circle",
                     vertex.size=1,
                     vertex.label.color="black",
                     edge.width=1)
        })
        #infections over time graph

        model <- eventReactive(c(input$n.roommates,input$n.workers,input$n.people,
                input$max.time,input$pparty,input$pct.starting.infected,
                input$pmask, input$partyDay)
        ,{
                distribution_graph <- initiateNet(input$n.roommates, input$n.workers, input$n.people)
                fullResults <- simulateDisease(distribution_graph, input$pct.starting.infected, input$max.time, input$pparty, input$pmask, 
                                               FALSE,
                                       input$partyDay, input$n.people, input$n.roommates)
                fullResults_party <- simulateDisease(distribution_graph, input$pct.starting.infected, input$max.time, input$pparty, input$pmask, 
                                               TRUE,
                                               input$partyDay, input$n.people, input$n.roommates)
                #plot(distribution_graph)
                infections.by.time = fullResults[[1]]
                infections.by.time.party = fullResults_party[[1]]
                results = fullResults[[2]]
                results_party = fullResults_party[[2]]
                net.layout.by.time <- plotNetworkGraphDisease(results, distribution_graph)
                p1 <- ggplot(data = infections.by.time, aes(x = t, y = S, col="S")) +
                        ylab("number of people") +
                        geom_line() +
                        geom_line(aes(x = t, y = E, col="E")) +
                        geom_line(aes(x = t, y = I, col="I")) +
                        geom_line(aes(x = t, y = R, col="R")) +
                        theme_bw()+ggtitle("No Party")
                p2 <- ggplot(data = infections.by.time.party, aes(x = t, y = S, col="S")) +
                        ylab("number of people") +
                        geom_line() +
                        geom_line(aes(x = t, y = E, col="E")) +
                        geom_line(aes(x = t, y = I, col="I")) +
                        geom_line(aes(x = t, y = R, col="R")) +
                        theme_bw()+ggtitle("Party")
                list(myplot1=p1, myplot2=p2, netlayout = net.layout.by.time)
        })

        output$simulation <- renderPlot({model()$myplot1})

        output$simulationParty <- renderPlot({model()$myplot2})

        #network by day graph
        output$networkDay <- renderPlot({
                model()$netlayout %>% 
                        filter(t %in% c(1, input$partyDay-2, input$partyDay+2, input$partyDay+6, input$partyDay+10)) %>%
                        ggplot(aes(xend = xend, yend = yend, x = x, y = y)) + 
                        geom_edges(color = "lightgray") +
                        geom_nodes(aes(color = is_infected)) + 
                        facet_wrap(~ t) + 
                        theme_blank()+scale_color_manual(values=c("deep sky blue","indianred1"))
                
        })

}
