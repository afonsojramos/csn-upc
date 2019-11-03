source("./metrics.r")

generateER <- function(metrics_table,t){
  for (x in 1:nrow(metrics_table)){
    language <- paste(unlist(metrics_table[x,1]), collapse='')
    N = as.numeric(unlist(metrics_table[x,2]))
    E = as.numeric(unlist(metrics_table[x,3]))
    
    for (it in 1:t){
      # graph = erdos.renyi.game(N, (2*E)/(N*(N-1)))
      graph = erdos.renyi.game(N, E, type = "gnm")
      # Save the generated graph
      out_file <- paste("./data/ER/",language,"/", language, "_", it, ".txt", sep = "")
      write_graph(graph, out_file, format = "edgelist")
    }
  }
}

t = 20
generateER(metrics_table, t)

# run ./scripts/process_all_er.sh before proceeding

results <- read.csv("./results/ER.csv")
baseline_c <- read.table("./results/baseline.csv")
for (x in 1:nrow(metrics_table)){
  language <- paste(unlist(metrics_table[x,1]), collapse='')
  cat(paste("\n",language,"\n"))
  C <- as.numeric(unlist(baseline_c[x,2]))
  sum = 0
  for(y in unlist(results[language])){
    if(y>=C){
      sum = sum + 1
    }
  }
  cat(paste("pval ", sum/t))
}
