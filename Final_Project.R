#load the packages
library(statnet)
library(tidyverse)
library(magrittr)
library(ggnetwork)
library(EpiModel)
library(netdiffuseR)
library(igraph)
library(ergm)
library(tergm)
library(network)
library(dplyr)
library(ggplot2)

initiateNet <- function(n.roommates, n.workers, n.people, random_seed) {
        set.seed(random_seed) 
        distribution <- matrix(0, nrow = n.people, ncol = n.people)
        
        ### If 0.5*n.people is the number of people who work, and each person who works
        ### has n.workers coworkers on average, we can assume that there are around (0.5*n.people)/n.workers
        ### different workplaces
        n.workplaces <- floor(0.5 * n.people / n.workers)
        
        ### randomly assign each person to either workplace==0 (not a worker) or a workplace in 1:nworkplaces!
        ### There is a 50% chance you are not a worker (workplace=0)
        ### There is a 50% chance that you ARE a worker, and if you are you have an equal chance of being assigned to
        ### any workplace
        workplaces <-        sample(
                        0:n.workplaces,
                        size = n.people,
                        prob = c(0.5, rep(1 / n.workplaces, n.workplaces)),
                        replace = TRUE
                )
        
        ## Everyone in a certain workplace is attached to eachother
        for (workplace in 1:n.workplaces) {
                workers <- which(workplaces == workplace)
                distribution[workers, workers] <- 1
        }
        
        # Make blocks for roommates where everyone in a house is attached to eachother
        for (i in seq(1, n.people - n.roommates, by = n.roommates)) {
                distribution[(i:(i + n.roommates - 1)), (i:(i + n.roommates - 1))] <-
                        1
        }
        
        # Subtract 1s from the diagonal of "distibution" because we didn't mean to make each person a
        # neighbor of themselves
        distribution <- distribution - diag(1, n.people, n.people)
        
        
        
        ### Everone either has 3 edges or 1 edge
        ### Workers have 3, nonworkers have 1
        
        distribution_graph <-
        graph_from_adjacency_matrix(distribution, "undirected")
        
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
        return(distribution_graph)
}










simulateDisease <-
        function(distribution_graph,
                 pct.starting.infected,
                 max.time,
                 pparty,
                 pmask,
                 is_party,
                 partyDay,
                 n.people,
                 n.roommates,
                 random_seed) {

                set.seed(random_seed) 
                infected <- sample(
                        x = c(1, 0),
                        # people can be infected (T) or susceptible (F)
                        size = n.people,
                        # create a vector that is n.people long
                        replace = T,
                        # with replacement, so that >1 person can be infected
                        prob = c(pct.starting.infected, 1 - pct.starting.infected)
                )
                
                
                ### RUN A BIG LOOP TO SIMUALTE THE DISEASE
                
                # Set up a list that tracks infections over time.
                # At time step 1, the infections are just the initial infections.
                infections <- vector(length = max.time, mode = "list")
                infections[[1]] <- infected
                
                # Get the edgelist and add a column that stores whether or not people are roommates
                el <-   as_edgelist(distribution_graph) %>%
                        as.data.frame %>%
                        set_names(value = c("from", "to"))
                
                el <- el %>% mutate(roommates = FALSE)
                for (i in 1:(n.roommates-1)) {
                        el <- el %>% mutate(roommates = ifelse(to == from +i, TRUE, roommates))  
                }
                
                # Next, run the loop
                for (t in 2:max.time) {
                        ### Every day something happens!!!!!
                        ### First, anyone who has ever been in contact with disease (anyone for who infections[[t]] !=0)
                        ### has their status incremented by a day.
                        ### NOTE: days 7-11 are latent
                        ### days 1-6 are infectious
                        ### days 12+ are recovered
                        infections[[t]] <- infections[[t - 1]]
                        infections[[t]][infections[[t]] != 0] <-  infections[[t]][infections[[t]] != 0] + 1 
                        
                        ### For every edge in EL, find out if it is at risk of spreading the disease
                        # This will store edges where the "from" person is susceptible
                        at_risk_edges_1 <- rep(FALSE, nrow(el))
                        # This will store edges where the "to" person is susceptible
                        at_risk_edges_2 <- rep(FALSE, nrow(el))
                        
                        for (i in 1:nrow(el)) {
                                person1 <- el[i,1]
                                person2 <- el[i,2]
                                ### If person 1 is S and person 2 is Infectious
                                if (infections[[t]][person1] == 0 & (infections[[t]][person2] > 0) & (infections[[t]][person2] <= 6)) {
                                        at_risk_edges_1[i] <- TRUE
                                }
                                ### If person 2 is S and person 1 is Infectious
                                if (infections[[t]][person2] == 0 & (infections[[t]][person1] > 0) & (infections[[t]][person1] <= 6)) {
                                        at_risk_edges_2[i] <- TRUE
                                }
                        }
                        
                        
                        ### For each edge in at_risk_edges_1, did the disease spread???
                        ### If the people are roommates, it spreads with probability 0.7
                        ### If the people are NOT roommates,spreads with probability 3/7*0.015
                        ### The 3/7 is because there is only a 3/7 probability that you went to work today
                        
                        infections_roommate <- sample(c(TRUE, FALSE), size=nrow(el), prob=c(0.7, 0.3), replace=TRUE)
                        infections_coworkers <- sample(c(TRUE, FALSE), size=nrow(el), prob=c(3/7*0.015, 1-3/7*0.015), replace=TRUE)
                        
                        new_infected_roommates_1 <- at_risk_edges_1 &  el$roommates & infections_roommate
                        new_infected_roommates_2  <- at_risk_edges_2 &  el$roommates & infections_roommate
                        new_infected_coworkers_1 <- at_risk_edges_1 & !el$roommates & infections_coworkers
                        new_infected_coworkers_2  <- at_risk_edges_2 & !el$roommates & infections_coworkers
                        
                        #### Need to actually make these new people infected!!!
                        new_infections <- c(el[which(new_infected_roommates_1==TRUE),1],
                                            el[which(new_infected_roommates_2==TRUE),2],
                                            el[which(new_infected_coworkers_1==TRUE),1],
                                            el[which(new_infected_coworkers_2==TRUE),2])
                        
                        infections[[t]][new_infections] <- 1
                        #### IF today is the party day, we need to suddenly make a lot of people infected
                        if(is_party == "TRUE"){
                                if (t == partyDay) {
                                        infections[[t]] <-
                                                simulateParty(infections[[t]],
                                                              pparty,
                                                              pmask)
                                }  
                        }
                }
                
                
                
                (results <- infections %>%
                                lapply(FUN = as.data.frame) %>%
                                lapply(FUN = set_names, value = "infected") %>%
                                bind_rows(.id = "t") %>%
                                mutate(id = rep(1:n.people, times = max.time),
                                       t = as.integer(t)) %>%
                                tibble::as_tibble())

                # This dataset is the raw results of our simulation, but it's usually easier to
                # look at a summary.  Let's look at the number of people infected over time
                infections.by.time <- results %>%
                        group_by(t) %>%
                        summarize(
                                S = sum(infected==0),
                                E= sum(infected > 0 & infected < 6),
                                I= sum(infected >= 6 & infected <= 11),
                                R = sum(infected > 11))
                
                #infection_graph <- ggplot(data = infections.by.time, aes(x = t, y = S, col="S")) + geom_line() +
                #       geom_line(aes(x = t, y = S, col="E")) +
                #       geom_line(aes(x = t, y = I, col="I")) +
                #      geom_line(aes(x = t, y = R, col="R"))
                
                # return(c(results, infection_graph))
                
                return(list(infections.by.time, results))
        }















plotNetworkGraphDisease <-
        function(results, distribution_graph) {
                distgraph2 <- distribution_graph %>% intergraph::asNetwork(.)
                net.layout <- ggnetwork(distgraph2) %>%
                        mutate(id = rep(vertex.names))
                results <- results %>% mutate(
                        S = infected == 0,
                        E = 0 < infected & infected < 6,
                        I = infected >= 6 & infected <= 11,
                        R = infected > 11,
                        is_infected = infected > 0
                )
                net.layout.by.time <-
                        split(results, f = results$t) %>%
                        lapply(FUN = right_join,
                               y = net.layout,
                               by = "id") %>%
                        bind_rows
                
                # net.layout.by.time %>% 
                #         filter(t %in% c(5, 6, 7, 20, 40)) %>%
                #         ggplot(aes(xend = xend, yend = yend, x = x, y = y)) + 
                #         geom_edges(color = "lightgray") +
                #         geom_nodes(aes(color = is_infected)) + 
                #         facet_wrap(~ t) + 
                #         theme_blank()+scale_color_manual(values=c("deep sky blue","indianred1"))
                return(net.layout.by.time)
        }









simulateParty <- function(infected, pparty, pmask) {
        
        ### Might be interesting to study how a change in %s impacts the results
        n.people <- length(infected)
        
        partygoer <-
                sample(
                        c(TRUE, FALSE),
                        size = n.people,
                        replace = TRUE,
                        prob = c(pparty, 1 - pparty)
                )
        maskwearer <-
                sample(
                        c(TRUE, FALSE),
                        size = n.people,
                        replace = TRUE,
                        prob = c(pmask, 1 - pmask)
                )
        
        
        partygoers <- which(partygoer == TRUE)
        #maskwearers <- which(maskwearer=TRUE)
        #partygoers <- partygoers[infected[partygoers] <= 11 & infected[partygoers] <= 5]
        
        sus_partygoers <- partygoers[infected[partygoers] == 0]
        inf_partygoers <-
                partygoers[infected[partygoers] <= 6 &
                                   infected[partygoers] >0]
        
        #print(infected[partygoers])
        
        party_el <- as.matrix(expand.grid(sus_partygoers, inf_partygoers))
        #print(dim(party_el))
        if (!is.null(nrow(party_el)) & nrow(party_el)>0) {
                for (i in 1:nrow(party_el)) {
                        edge <- party_el[i, ]
                        ##### Both wear mask
                        if (maskwearer[edge[1]] == TRUE &
                            maskwearer[edge[2]] == TRUE) {
                                spread <- sample(
                                        c(TRUE, FALSE),
                                        size = 1,
                                        prob = c(0.015, 1 - 0.015)
                                )
                                if (spread) {
                                        infected[edge[1]] <- 1
                                }
                        }
                        
                        ##### susceptible person wear a mask, infected do not
                        if (maskwearer[edge[1]] == TRUE &
                            maskwearer[edge[2]] == FALSE) {
                                spread <- sample(
                                        c(TRUE, FALSE),
                                        size = 1,
                                        prob = c(0.21, 1 - 0.21)
                                )
                                if (spread) {
                                        infected[edge[1]] <- 1
                                }
                        }
                        
                        ##### both infectious and susceptible do not wear a mask
                        
                        if (maskwearer[edge[1]] == FALSE &
                            maskwearer[edge[2]] == FALSE) {
                                spread <- sample(
                                        c(TRUE, FALSE),
                                        size = 1,
                                        prob = c(0.7, 1 - 0.7)
                                )
                                if (spread) {
                                        infected[edge[1]] <- 1
                                }
                        }
                        ##### infected person wear a mask, susceptible do not
                        
                        if (maskwearer[edge[1]] == FALSE &
                            maskwearer[edge[2]] == TRUE) {
                                spread <- sample(
                                        c(TRUE, FALSE),
                                        size = 1,
                                        prob = c(0.05, 1 - 0.05)
                                )
                                if (spread) {
                                        infected[edge[1]] <- 1
                                }
                        }
                }
        }
        return(infected)
}