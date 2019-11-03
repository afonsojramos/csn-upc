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

source("./switching.r")

## ---- Generate Metrics Table ----
## languages = languages to evaluate
generateMetricsTable <- function(languages) {
    metrics_table <- data.table(
        "Language" = character(),
        "N" = numeric(),
        "E" = numeric(),
        "k" = numeric(),
        "delta" = numeric(),
        stringsAsFactors = FALSE
    )
    
    for (x in 1:length(languages)) {
        language <- languages[x]
        file <-
            paste(
                "./data/dependency_networks/",
                language,
                "_syntactic_dependency_network.txt",
                sep = ""
            )
        
        deg_seq = read.table(
            file,
            header = FALSE,
            stringsAsFactors = FALSE,
            sep = " ",
            quote = ""
        )
        deg_seq = na.omit(deg_seq)
        
        graph = graph.data.frame(deg_seq[-1,])
        graph = simplify(graph,
                         remove.multiple = TRUE,
                         remove.loops = TRUE)
        
        E = gsize(graph)
        
        N = as.numeric(deg_seq[1, 1])
        k = 2 * E / N
        delta = 2 * E / (N * (N - 1))
        
        metrics_table <-
            rbind(metrics_table, list(language, N, E, k, delta))
        
        # Save the processed graph
        out_file <-
            paste(
                "./data/preprocessed/",
                language,
                "_syntactic_dependency_network.txt",
                sep = ""
            )
        write_graph(graph, out_file, format = "edgelist")
    }
    return(metrics_table)
}

## ---- Generate Erdos Renyi ----
## metrics_table = Metrics table generated with generateMetricsTable()
## t = T value
generateER <- function(metrics_table, t) {
    for (x in 1:nrow(metrics_table)) {
        language <- paste(unlist(metrics_table[x, 1]), collapse = '')
        N = as.numeric(unlist(metrics_table[x, 2]))
        E = as.numeric(unlist(metrics_table[x, 3]))
        
        for (it in 1:t) {
            graph = erdos.renyi.game(N, (2 * E) / (N * (N - 1)))
            # Save the generated graph
            out_file <-
                paste("./data/ER/",
                      language,
                      "/",
                      language,
                      "_",
                      it,
                      ".txt",
                      sep = "")
            write_graph(graph, out_file, format = "edgelist")
        }
    }
}

## ---- Generate Final Table ----
## languages = languages to evaluate
generateFinalTables <- function(languages) {
    start_time <- Sys.time()
    finalTable <- data.table(
        "Language" = character(),
        "Metric" = numeric(),
        "p-val.(Switching)" = numeric(),
        stringsAsFactors = FALSE
    )
    
    for (x in 1:length(languages)) {
        language <- languages[x]
        print(language)
        file <-
            paste(
                "./data/dependency_networks/",
                language,
                "_syntactic_dependency_network.txt",
                sep = ""
            )
        
        deg_seq = read.table(
            file,
            header = FALSE,
            stringsAsFactors = FALSE,
            sep = " ",
            quote = ""
        )
        deg_seq = na.omit(deg_seq)
        
        graph = graph.data.frame(deg_seq[-1,])
        graph = simplify(graph,
                         remove.multiple = TRUE,
                         remove.loops = TRUE)
        
        N = as.numeric(deg_seq[1, 1])
        
        metric <- calculateGraphCloseness(graph, N)
        switch <- calculateSwitchingPValue(metric, graph, N, 20)
        
        finalTable <-
            rbind(finalTable, list(language, metric, switch))
    }
    
    end_time <- Sys.time()
    cat("Elapsed time: ", end_time - start_time, "\n")
    return(finalTable)
}

languages = c(
    "Arabic",
    "Basque",
    "Catalan",
    "Chinese",
    "Czech",
    "English",
    "Greek",
    "Hungarian",
    "Italian",
    "Turkish"
)
metrics_table = generateMetricsTable(languages)

# Print table in LaTex format
print(xtable(
    metrics_table,
    type = 'latex',
    digits = c(0, 0, 0, 0, 6, 10)
),
include.rownames = FALSE)

t = 20
generateER(metrics_table, t)

generateFinalTables(languages)
