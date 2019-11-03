## ---- Calculate Switching C ----
## graph = graph to switch,
## N = number of nodes,
## origC = C metric of actual language data
calculateSwitchingCloseness <- function(graph, N, origC) {
    succ_count = 0
    fail_count = 0
    
    # Create edgelist
    edgelist = as_edgelist(graph, names = FALSE)
    
    E = nrow(edgelist)
    Q = 10
    
    # Create adjacency matrix
    m = as_adjacency_matrix(graph, names = FALSE, type = "both")
    rstyle_matrix <- as.matrix(m)
    
    # Calculating all potential switches simultaneously
    edge1_id = sample(1:E, Q * E, replace = TRUE)
    edge2_id = sample(1:E, Q * E, replace = TRUE)
    
    # Switching for Q*E times
    for (i in seq(1, floor(E * Q))) {
        # Extract trial edges from edge list
        edge1 = edgelist[edge1_id[i], ]
        edge2 = edgelist[edge2_id[i], ]
        
        # Retain original edges
        edge1_original = edge1
        edge2_original = edge2
        
        # Check if Switching is possible
        # Check they are not the same vertices + Check for multiedges + Check for loops
        if (edge1[1] != edge2[1] &&
            edge1[2] != edge2[2] &&
            edge1[1] != edge2[2] &&
            edge2[1] != edge1[2] && # Vertices are all different
            rstyle_matrix[edge2[1], edge1[2]] == 0 &&
            rstyle_matrix[edge1[2], edge2[1]] == 0 &&
            rstyle_matrix[edge1[1], edge2[2]] == 0 &&
            rstyle_matrix[edge2[2], edge1[1]] == 0) {
            # No loop or hyperedges
            
            # Make the switch
            temp = edge1[2]
            edge1[2] = edge2[2]
            edge2[2] = temp
            
            # Reassign edges
            edgelist[edge1_id[i], ] = edge1
            edgelist[edge2_id[i], ] = edge2
            
            # Delete adjacencies
            rstyle_matrix[edge1_original[1], edge1_original[2]] = 0
            rstyle_matrix[edge2_original[1], edge2_original[2]] = 0
            rstyle_matrix[edge1_original[2], edge1_original[1]] = 0
            rstyle_matrix[edge2_original[2], edge2_original[1]] = 0
            
            # Add new adjacencies
            rstyle_matrix[edge1[1], edge1[2]] = 1
            rstyle_matrix[edge2[1], edge2[2]] = 1
            rstyle_matrix[edge1[2], edge1[1]] = 1
            rstyle_matrix[edge2[2], edge2[1]] = 1
            
            # Increment success counter
            succ_count = succ_count + 1
        } else {
            # Increment failure counter
            fail_count = fail_count + 1
        }
    }
    cat("# Fails: ", fail_count, "\n")
    cat("# Success: ", succ_count, "\n")
    
    # Create new graph from edgelist
    new_graph = graph_from_edgelist(edgelist, directed = FALSE)
    
    x = calculateGraphClosenessBounds(new_graph, N, origC, N / 1000)
    return(x)
}


## ---- Monte Carlo Algorithm ----
## x = C metric,
## graph = language data graph,
## N = num nodes,
## Tval = Num iters in Monte Carlo
calculateSwitchingPValue <-
    function(x, graph, N, Tval = 20) {
        cat("X: ", x, "\n")
        results = c()
        # X is the closeness of the original language
        f_bin = 0
        for (i in seq(1, Tval)) {
            x_nh = calculateSwitchingCloseness(graph, N, x)
            
            cat("X_nh: ", x_nh, "x: ", x, "\n")
            if (x_nh >= x) {
                f_bin = f_bin + 1
            }
            
            results = append(results, x_nh)
        }
        
        cat("Results switching: ", results, "\n")
        Pval = f_bin / Tval
        cat("p-value: ", Pval, "\n")
        return(Pval)
    }

## ---- Calculate Closeness using Bounds functions ----
## graph = language graph,
## N = num nodes,
## origC = original C
## M_max = Max sum iteration
calculateGraphClosenessBounds <- function(graph, N, origC, M_max) {
    closeness = 0
    M = M_max
    m_sample = sample(1:N, N, replace = FALSE)
    
    for (i in seq(1:M)) {
        temp = distances(graph,
                         m_sample[i],
                         V(graph),
                         mode = "all",
                         algorithm = "dijkstra")
        temp[which(!is.finite(temp))] <- 0
        temp <- temp[temp > 0]
        temp <- sapply(temp, function(x)
            1 / x)
        if (length(temp) > 0) {
            temp <- sum(temp) / (N - 1)
        } else {
            temp <- 0
        }
        
        # Current closness measure after first "i" iterations
        closeness <- closeness + temp
        
        # Comput Cnh_min and Cnh_max
        Cnh_min = ((1 / N) * closeness)
        Cnh_max = ((1 / N) * closeness) +  1 - (i / N)
        
        # If Cnh_min is greater than actual C, return 1
        if (Cnh_min >= origC) {
            return(1)
        }
        
        # Compute Cnh_max and check if: C <= Cnh_max
        if (Cnh_max < origC) {
            return(0)
        }
        
    }
    closeness = closeness / (M)
    
    return(closeness)
}

## ---- Calculate closeness estimate (no bounds function) ----
calculateGraphCloseness <- function(graph_simple, N) {
    closeness = 0
    M = N / 50 # Estimate the closeness
    m_sample = sample(1:N, N, replace = FALSE)
    for (i in seq(1:M)) {
        temp = distances(
            graph_simple,
            m_sample[i],
            V(graph_simple),
            mode = "all",
            algorithm = "dijkstra"
        )
        temp[which(!is.finite(temp))] <- 0
        temp <- temp[temp > 0]
        temp <- sapply(temp, function(x)
            1 / x)
        if (length(temp) > 0) {
            temp <- sum(temp) / (N - 1)
        } else {
            temp <- 0
        }
        closeness <- closeness + temp
    }
    closeness = closeness / (M)
    
    return(closeness)
}
