library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)

fnPlotRankvsTF <- function(dfInTerms, idField){
    dfInTerms %>% 
        ggplot(aes(rank, tf, color=!!as.symbole(id))) + 
        scale_x_log10() + scale_y_log10() + 
        geom_line(size=1.1, alpha=0.8) + 
        theme(legend.position = 'bottom')
}

fnPlotNetwork <- function(dfInPairs, minN){
    dfInPairs %>% 
        filter ( n >= minN ) %>%
        graph_from_data_frame () %>%
        ggraph ( layout = "fr" ) +
        geom_edge_link ( aes ( edge_alpha = n , edge_width = n ), edge_colour = "cyan4" ) +
        geom_node_point ( size = 5 ) +
        geom_node_text ( aes ( label = name ), repel = TRUE , point.padding = unit ( 0.2 , "lines" )) +
        theme_void ()
}