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

# transform categorical to ordinal
# Inputs:
#   df  -- data.frame
#   c   -- column to modify
#   ord -- ordered list of values in increasing magnitude
cat_to_ord <- function(df, c, ord) {
  h = hash(ord, c(1:length(ord)))
  h['NA'] = NA
  return (apply(df[c], 1, function(x) h[[x]]))
}

