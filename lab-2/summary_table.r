write_summary <- function(language, file) {
    degree_sequence = read.table(file, header = FALSE)
    
    return (
        c(
            Language = language,
            N = length(degree_sequence$V1),
            'Maximum Degree' = max(degree_sequence$V1),
            'M/N' = sum(degree_sequence$V1) / length(degree_sequence$V1),
            'N/M' = length(degree_sequence$V1) / sum(degree_sequence$V1)
        )
    )
}

# header: Used to indicate that the first line is a header wihtout real data
# as.it: Used to tell that cells treated as real strings and not as categorial data
source = read.table("./list.txt",
                    header = TRUE,
                    as.is = c("language", "file"))
