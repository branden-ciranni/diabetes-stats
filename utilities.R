
# count the number of null values in each column of the DataFrame
count_null <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}