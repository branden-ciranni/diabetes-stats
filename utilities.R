# Utility functions, left out of the main `analysis.Rmd` notebook for cleanliness.

# count the number of null values in each column of the DataFrame
count_null <- function(df) {
  null.count <- sapply(df, function(x) sum(is.na(x)))
  null.perc <- sapply(df, function(x) sum(is.na(x))/nrow(df))
  df.null <- data.frame(feature=names(null.count),
                        null.count=null.count,
                        null.perc=null.perc) 
  return (df.null %>% arrange(desc(null.perc)))
}


# plot percentage graph of missing values
plot_missing_values <- function(df) {
  df.null = count_null(df)
  null.plot <- ggplot(df.null) + 
                geom_col(aes(x=reorder(feature, desc(null.perc)), y=null.perc))
  return (null.plot)
}

