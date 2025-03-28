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
#' - **number_cols**: Specifies the number of columns in the grid (default = ceiling of the square root of the total number of plots to make).
#' - **nets_per_plot**: Defines how many networks are displayed per plot (default = 20).
#'
#' @param list_tidygra A list of tidygraph objects.
#' @param number_cols Number of columns in the grid (default = ceiling of the square root of the total number of plots).
#' @param nets_per_plot Number of networks per plot (default = 25 to ensure clarity).
#'
#' @return A combined grid plot of personal networks.
#' @export
#'
#' @examples
#' plot_grid_networks(list_tidygra)
#' plot_grid_networks(list_tidygra, number_cols = 4, nets_per_plot = 10)
#' plot_grid_networks(list_tidygra[10:25], number_cols = 3, nets_per_plot = 15)
plot_grid_networks <- function(list_tidygra, number_cols = NULL, nets_per_plot = NULL) {
  ##########
  # Function: Plots a grid of personal network graphs from a list of tidygraph 
  #           objects, displaying multiple networks at once.
  # Inputs: 
  #   list_tidygra   = A list of tidygraph objects
  #   number_cols    = Number of columns in the grid (default = ceiling of the square root of the total)
  #   nets_per_plot  = Number of networks per plot (default = 20)
  # Outputs: A combined grid plot of personal networks
  ##########
  
  # Check if the input is a list of tidygraph objects
  if (!is.list(list_tidygra)) stop("Input must be a list of tidygraph objects.")
  
  # Set default values if parameters are NULL
  if (is.null(number_cols)) number_cols <- ceiling(sqrt(length(list_tidygra)))
  if (is.null(nets_per_plot)) nets_per_plot <- 25
  
  # Split list of networks into chunks of size 'nets_per_plot'
  chunks_nets_visual <- split(list_tidygra, ceiling(seq_along(list_tidygra) / nets_per_plot))
  
  # Loop through chunks, create and combine plots
  for (i in seq_along(chunks_nets_visual)) {
    chunk <- chunks_nets_visual[[i]]
    
    # Generate individual network plots
    net_plots <- lapply(chunk, plot_single_network)  
    
    # Combine the plots into a grid with 'number_cols' columns
    combined_plot <- cowplot::plot_grid(plotlist = net_plots, ncol = number_cols)
    
    # Return the combined plot
    return(combined_plot)
  }
}