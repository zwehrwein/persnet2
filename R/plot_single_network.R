#' Plot a personal network graph using ggraph
#'
#' Visualizes a personal network graph using ggraph, distinguishing between strong and weak ties 
#' and highlighting the ego node. The plot uses different colors and line styles:
#' - **Strong ties** are represented with solid red edges.
#' - **Weak ties** are represented with dashed blue edges.
#' - **Ego node** is represented with a black point.
#' - **Alters** are represented with grey points.
#'
#' @param tidygra A tidygraph object representing a personal network.
#'
#' @return A ggplot object visualizing the network structure.
#'         Returns NA if the input is invalid.
#' @importFrom magrittr %>%
#' @importFrom tidygraph %N>%
#' @import ggraph 
#' @import ggplot2 
#' @export

plot_single_network <- function(tidygra) {
  ##########
  # Function: Plots a personal network graph using ggraph, distinguishing 
  #           between strong and weak ties and highlighting the ego node.
  # Inputs: tidygra = A tidygraph object representing a personal network
  # Outputs: A ggplot object visualizing the network structure
  ##########
  
  # Test if valid tidygra input
  if (is.null(tidygra) || !"tbl_graph" %in% class(tidygra)) {
    warning("Warning: Graph is missing or invalid. Skipping.")
    return(NA)  # Skip invalid graphs
  }
  
  # Test whether the network is an isolate (no weight edge attribute)
  edge_attributes <- tidygra %>% tidygraph::activate(edges) %>% tibble::as_tibble()
  if (!"weight" %in% colnames(edge_attributes)) {
    # Plot isolate (ego only)
    tg_plot <- ggraph(tidygra, layout = "fr") +
      geom_node_point(size = 4, color = 'black', show.legend = FALSE) +
      theme_graph()
    
    return(tg_plot)
  } else {
    
    # Check if ego is present in the graph, and if so, focus layout on ego
    node_names <- unique(tidygra %N>% dplyr::pull(name))
    
    # Transform tie strength into strong/weak strings and create an alter dummy variable
    tidygra <- tidygra %>%
      tidygraph::activate(edges) %>%
      dplyr::mutate(strength_of_tie = ifelse(weight == 1, "weak", "strong")) %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(alter_dummy = ifelse(name != 'ego', 1, 0))
    
    if ("ego" %in% node_names) {
      focus_index <- which(node_names == "ego")
      
      # Plot network with ego as focal point
      tg_plot <- ggraph(tidygra, layout = "focus", focus = focus_index) +
        geom_edge_link(aes(color = strength_of_tie, linetype = strength_of_tie),
                       edge_width = 0.75, show.legend = FALSE) +
        scale_edge_linetype_manual(values = c("weak" = "dashed", "strong" = "solid")) +
        scale_edge_colour_manual(values = c("weak" = "#5B8FA8FF", "strong" = "#800000FF")) +
        geom_node_point(aes(color = factor(alter_dummy)), size = 4, show.legend = FALSE) +
        scale_colour_manual(values = c('black', 'grey66')) +
        theme_graph()
      
      return(tg_plot)
    } else {
      
      # Plot network without ego as focal point
      tg_plot <- ggraph(tidygra, layout = "fr") +
        geom_edge_link(aes(color = strength_of_tie, linetype = strength_of_tie),
                       edge_width = 0.75, show.legend = FALSE) +
        scale_edge_linetype_manual(values = c("weak" = "dashed", "strong" = "solid")) +
        scale_edge_colour_manual(values = c("weak" = "#5B8FA8FF", "strong" = "#800000FF")) +
        geom_node_point(size = 4, color = 'grey66', show.legend = FALSE) +
        theme_graph()
      
      return(tg_plot)
    }
  }
}