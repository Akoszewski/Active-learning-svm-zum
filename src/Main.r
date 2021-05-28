data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=";")
df = data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())
print(df)