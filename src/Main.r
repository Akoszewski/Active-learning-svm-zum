data <- read.csv("../Datasets/JobChanges/aug_train.csv", stringsAsFactors=TRUE, header=T, sep=",")
df <- data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = TRUE)
print(df)

df <- data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = FALSE)

print(paste0("### head: ", head(data)));
print(paste0("### class: ",class(df)));
print(paste0("### dim: ",  dim(df)));
print(paste0("### names: ",names(df)));

print(paste0("### df: "));
print(df[1:5,c(1:14)])
str(df)
CRITERIA <- df[c(1),-c(1:2)]
DATA <- df[-c(1), -c(1)]

# ## ---- data exploration-----------------------------------------------
# library(DataExplorer)
# plot_missing(data) ## Are there missing values, and what is the missing data profile? None in this case
# plot_bar(data) ## How does the categorical frequency for each discrete variable look like?
# plot_histogram(data) ## What is the distribution of each continuous variable?
