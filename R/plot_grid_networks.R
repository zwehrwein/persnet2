#' Plot a grid of personal network graphs
#'
#' Visualizes multiple personal network graphs in a grid format using ggraph. 
#' Each individual network graph distinguishes between strong and weak ties and highlights the ego node.
#'
#' - **Strong ties** are represented with solid red edges.
#' - **Weak ties** are represented with dashed blue edges.
#' - **Ego node** is represented with a black point.
#' - **Alters** are represented with grey points.
#'
#' The function allows customization of the grid:
#' - **number_cols**: Specifies the number of columns in the grid 
#' (default = ceiling of the square root of the total number of plots to make).
#'
#' Uses should subset the list networks fed into function to customize the number of plots.
#'
#' @param list_tidygra A list of tidygraph objects.
#' @param number_cols Number of columns in the grid (default = ceiling of the square root of the total number of plots).
#'
#' @return A combined grid plot of sociograms
#' @export
#'
#' @examples
#' plot_grid_networks(list_tidygra)
#' plot_grid_networks(list_tidygra, number_cols = 4, nets_per_plot = 10)
#' plot_grid_networks(list_tidygra[10:25], number_cols = 3)
plot_grid_networks <- function(list_tidygra, number_cols = NULL) {
  ##########
  # Function: Plots a grid of personal network graphs from a list of tidygraph 
  #           objects, displaying multiple networks at once.
  # Inputs: 
  #   list_tidygra   = A list of tidygraph objects
  #   number_cols    = Number of columns in the grid (defaults to the ceiling 
  #                    of the square root of the total number of networks)
  # Output: A combined grid plot of all personal networks
  ##########
  
  # Check if the input is a list of tidygraph objects
  if (!is.list(list_tidygra)) {
    stop("Input must be a list of tidygraph objects.")
  }
  
  # Default columns = ceiling of the sqrt of the total number of networks
  if (is.null(number_cols)) {
    number_cols <- ceiling(sqrt(length(list_tidygra)))
  }
  
  # Generate individual network plots
  net_plots <- lapply(list_tidygra, plot_single_network)
  
  # Combine the plots into a grid with 'number_cols' columns
  combined_plot <- cowplot::plot_grid(plotlist = net_plots, ncol = number_cols)
  
  # Return the combined plot
  return(combined_plot)
}