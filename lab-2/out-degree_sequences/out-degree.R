# source("summary_table.R")

degree_sequence = read.table("./data/English_out-degree_sequence.txt", header = FALSE)
degree_spectrum = table(degree_sequence, length(degree_sequence$V1),max(degree_sequence$V1),sum(degree_sequence$V1)/length(degree_sequence$V1),length(degree_sequence$V1)/sum(degree_sequence$V1))
#barplot(degree_spectrum, main = "English", xlab = "degree", ylab = "number of vertices", log = "xy")
