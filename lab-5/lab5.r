# Load and install necessary packages
requiredPackages <-
  c("igraph",
    "ggplot2",
    "data.table",
    "knitr",
    "rstudioapi",
    "xtable",
    "DT")

for (pac in requiredPackages) {
  if (!require(pac,  character.only = TRUE)) {
    install.packages(pac, repos = "http://cran.rstudio.com")
    library(pac,  character.only = TRUE)
  }
}

# You need to install this library manually
library(Rglpk) # https://stackoverflow.com/questions/25114771/glpk-no-such-file-or-directory-error-when-trying-to-install-r-package

rm(pac)
rm(requiredPackages)

# set pwd to current directory, must load rstudioapi before.
if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

## PRINTING

print_graph_results <- function(result) {
  algorithm_names <- names(result)
  measure_names <- names(result[[algorithm_names[[1]]]])
  
  cat(measure_names, "\n")
  
  for(i in 1:length(algorithm_names)) {
    algorithm_name <- algorithm_names[[i]]
    cat(algorithm_name, ":")
    
    for(j in 1:length(measure_names)) {
      measure_name <- measure_names[[j]]
      
      cat(" ", result[[algorithm_name]][[measure_name]], sep="")
    }
    cat("\n")
  }
}

result_to_dd <- function(result) {
  algorithm_names <- names(result)
  measure_names <- names(result[[algorithm_names[[1]]]])
  
  dd<-  as.data.frame(matrix(unlist(result), nrow=length(unlist(result[1]))))
  dd <- transpose(dd)
  
  colnames(dd) <- measure_names
  rownames(dd) <- algorithm_names
  
  dd <- format(round(dd, 3), nsmall = 3)
  
  return(dd)
}

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
  n <- length(c_vns)
  
  value <- 0
  for(i in 1:n) {
    value = value + outer_com_edges[i]*c_vns[i]/(inner_com_edges[i] + outer_com_edges[i])
  }

  return(value/g_vn)
}

get_tpt <- function(g_vn, c_vns, g, comm) {
  n <- length(comm[])
  
  value <- 0
  for(i in 1:n) {
    value = value + sum(count_triangles(induced.subgraph(g, unname(unlist(comm[i])))))
  }
  
  return(value/g_vn)
}

get_metrics <- function(g, comm) {
  n <- length(comm[])
  
  g_vn <- gsize(g)
  memb <- membership(comm)
  edges <- E(g)
  c_vns <- unname(sizes(comm))
    
  outer_com_edges <- rep(0, n)
  inner_com_edges <- rep(0, n)
  
  for(i in 1:length(edges)) {
    e <- ends(g, edges[i])
    a <- e[1]
    b <- e[2]
    
    if(same.community(memb, a, b)) {
      # !!same community!! adding 2 times to the same community
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
  tpt <- get_tpt(g_vn, c_vns, g, comm)
  
  return(list("modularity" = modularity(comm), "expansion" = expansion,  "conductance" = conductance, "TPT" = tpt))
}

get_graph_metrics <- function(algorithm_name, g) {
  comm <- get(algorithm_name)(g)
  metrics <- get_metrics(g, comm)
  return(metrics)
}


# INPUT, add any graph or any community algorithm
karate <- graph.famous("Zachary")
meredith <- graph.famous("Meredith")

gs <- list(karate, meredith)
graph_names <- c("Zachary", "Meredith")

algorithms <- c("edge.betweenness.community", "fastgreedy.community", "label.propagation.community", 
                "leading.eigenvector.community", "multilevel.community", "optimal.community",
                "spinglass.community", "walktrap.community", "infomap.community")

# MAIN

result <- list()

for(i in 1:length(gs)) {
  cat("Graph:", graph_names[i], "\n")
  
  g <- gs[[i]]
  
  for(j in 1:length(algorithms)) {
    algorithm_name <- algorithms[[j]]
    metrics <- get_graph_metrics(algorithm_name, g)
    result[algorithm_name] <- list(metrics)
  }
  
  # print_graph_results(result)
  
  dd_result <- result_to_dd(result)
  
  print(dd_result)
  
  if(rstudioapi::isAvailable()) {
    datatable(dd_result)
  } 
  cat("\n -------------------- \n\n")
}

