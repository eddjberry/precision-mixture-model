# Function for applying the JV10_error function from the precision modelling functions
# to a data frame

JV10_df_error <- function(d, id.var = "id", tar.var = "target", res.var = "response"){
  id <- d[, id.var]
  
  l <- split(d, id)
  
  paras <- data.frame(id = FALSE, precision = FALSE, bias = FALSE)
  
  for(i in seq_along(l)) {
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))
    
    X <- as.matrix(df[, tar.var])
    Tg <- as.matrix(df[res.var])
    
    B <- JV10_error(X, Tg)
    
    id <- as.character(df[1, id.var])
    
    paras[i, 1] <- id
    paras[i,2:3] <- B
  }
  return(paras)
}