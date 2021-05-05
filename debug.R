library(patchwork)
source("Final_Project.R")
input <- list()
input$n.roommates <- 3
input$n.workers <- 5
input$n.people <- 200
input$pct.starting.infected <- 0.1
input$max.time <- 100
input$pparty <- 0.5
input$pmask  <- 0.1
input$partyDay <- 8





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
p1+p2+plot_layout(nrow=2)
