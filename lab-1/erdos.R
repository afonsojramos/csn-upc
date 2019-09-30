library(emdbook)
library(tidyr)
library(igraph)

erdos_renyi_average_path_length <- function(n, epsilon=0.0001) {

    # Generate p such that G(n, p) will almost surely be connected
    value <- ((1 - epsilon) * log(n)) / n
    p <- ifelse(value <= 0, 1, 1.1 * value)

    print(glue::glue('Building Erdos-Renyi graph for: p = {p}, n = {n}'))
    graph <- erdos.renyi.game(n, p)
    return (average.path.length(graph))
}

number_of_nodes <- 2 ^ (0 : 15)
average_path_lengths <- lapply(number_of_nodes, erdos_renyi_average_path_length)

plot(number_of_nodes, average_path_lengths, xlab = 'Number of nodes', ylab = 'Average shortest path length')
