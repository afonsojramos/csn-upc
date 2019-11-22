# Load and install necessary packages
requiredPackages <-
  c("igraph",
    "ggplot2",
    "data.table",
    "knitr",
    "rstudioapi",
    "xtable")

for (pac in requiredPackages) {
  if (!require(pac,  character.only = TRUE)) {
    install.packages(pac, repos = "http://cran.rstudio.com")
    library(pac,  character.only = TRUE)
  }
}
rm(pac)
rm(requiredPackages)

# set pwd to current directory, must load rstudioapi before.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##exploring community structure
karate <- graph.famous("Zachary")
g <- graph.famous("Zachary")
##view friendship relation among members of Karate club
plot(karate) 
##Find cluster partition according to Walktrap algorithm:
#distance is based on random walks, and similarity==shortest random walk
communities <- walktrap.community(karate)
plot(wc, karate)

## HELPERS

same.community <- function(memb, a, b) {
  return(memb[a] == memb[b])
}

increase_cluster <- function(com_edges, memb, v) {
  v_com <- memb[v]
  com_edges[v_com] = com_edges[v_com] + 1
  return(com_edges)
}

# METRICS

get_expansion <- function(g_vn, outer_com_edges) {
  return(sum(outer_com_edges)/g_vn)
}

get_conductance <- function(g_vn, c_vns, outer_com_edges, inner_com_edges) {
  n <- length(communities[])
  
  value <- 0
  for(i in 1:n) {
    value = value + outer_com_edges[i]*c_vns[i]/(inner_com_edges[i] + outer_com_edges[i])
  }

  return(value/g_vn)
}

get_tpt <- function(g_vn, c_vns, g, communities) {
  n <- length(communities[])
  
  value <- 0
  for(i in 1:n) {
    value = value + sum(count_triangles(induced.subgraph(g, unname(unlist(communities[i])))))
  }
  
  return(value/g_vn)
}

get_metrics <- function(g, communities) {
  n <- length(communities[])
  
  g_vn <- gsize(g)
  memb <- membership(communities)
  edges <- E(g)
  c_vns <- unname(sizes(communities))
    
  outer_com_edges <- rep(0, n)
  inner_com_edges <- rep(0, n)
  
  for(i in 1:length(edges)) {
    e <- ends(g, edges[i])
    a <- e[1]
    b <- e[2]
    
    if(same.community(memb, a, b)) {
      # same community.. adding 2 times to the same 
      inner_com_edges <- increase_cluster(inner_com_edges, memb, a)
      inner_com_edges <- increase_cluster(inner_com_edges, memb, b)
    }
    else {
      outer_com_edges <- increase_cluster(outer_com_edges, memb, a)
      outer_com_edges <- increase_cluster(outer_com_edges, memb, b)
    }
  }
  
  expansion <- get_expansion(g_vn, outer_com_edges)
  conductance <- get_conductance(g_vn, c_vns, outer_com_edges, inner_com_edges)
  tpt <- get_tpt(g_vn, c_vns, g, communities)
  
  return(list("modularity" = modularity(communities), "expansion" = expansion,  "conductance" = conductance, "TPT" = tpt))
}

# MAIN

metrics <- get_metrics(g, communities)

print(metrics)



