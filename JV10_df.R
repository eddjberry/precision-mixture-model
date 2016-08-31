# This (admittedly ugly) function takes a dataframe and returns parameters
# estimates for each level of the id variable and the given
# target and response variables.

# If there are non-target variables then nt.vars should be either a list column 
# names or a character vectorsi.e. nt.vars = c("nt1", "nt2")

JV10_df <- function(df, id.var = "id", tar.var = "target", res.var = "response", nt.vars = NULL){
  id <- d[, id.var]
  
  l <- split(d, id)
  
  paras <- data.frame(id = FALSE, K = FALSE, Pt = FALSE, Pn = FALSE, Pu = FALSE)
  
  for(i in seq_along(l)) {
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))
    
    X <- as.matrix(df[, tar.var])
    Tg <- as.matrix(df[res.var])
    
    if(is.null(nt.vars)) {
      B <- JV10_fit(X, Tg, return.ll = FALSE)
    } else {
      NT = as.matrix(df[,nt.vars])
      B <- JV10_fit(X, Tg, NT, FALSE)
    }
    id <- as.character(df[1, id.var])
    paras[i, 1] <- id
    paras[i,2:5] <- B
  }
  return(paras)
}