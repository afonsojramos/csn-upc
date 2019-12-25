requiredPackages <- c("igraph", "ggplot2", "ggthemes", "esquisse", "data.table")

for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}

computeSummaryTable <- function(graphs){
  
  graphsNames = c("Erdos-Renyi", "Full graph", "Barabasi-Albert", "Watts-Strogatz")
  table <- data.table("Graph" = character(),
                      "N" = numeric(),
                      "E" = numeric(),
                      "k" = numeric(),
                      "delta" = numeric(),
                      "diameter" = numeric(),
                      stringsAsFactors = FALSE)
  
  for (x in 1:length(graphsNames)){
    
    g = graphs[[x]]
    gName = graphsNames[x]
    
    E = length(E(g))
    N = length(V(g))
    k = 2*E/N
    delta = 2*E/(N * (N-1))
    diameter = diameter(g, directed = FALSE, unconnected = TRUE, weights = NULL)
    
    
    table <- rbind(table, list(gName, N, E, round(k, 2), round(delta, 2), round(diameter, 2)))
  }
  return(table)
}



nodes = 1000

er.graph <- erdos.renyi.game(nodes, 0.5)
full.graph <- make_full_graph(nodes, directed=F)
ba.graph <-barabasi.game(nodes, 1, directed=F)
ws.graph <- watts.strogatz.game(1,nodes,4,0.5)

graphs = list(er.graph, full.graph, ba.graph, ws.graph)
computeSummaryTable(graphs)