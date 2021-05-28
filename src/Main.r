data <- read.csv("../Datasets/JobChanges/aug_train.csv", stringsAsFactors=TRUE, header=T, sep=",")
df <- data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = TRUE)
print(df)


# ## ---- data exploration-----------------------------------------------
# library(DataExplorer)
# plot_missing(data) ## Are there missing values, and what is the missing data profile? None in this case
# plot_bar(data) ## How does the categorical frequency for each discrete variable look like?
# plot_histogram(data) ## What is the distribution of each continuous variable?
