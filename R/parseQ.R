# parseQ.R

#' @importFrom stringr str_split str_match

# Take a glpk model object a Q equation and return a long-form DF of variable/coef pairs
parseQ <- function(model, q) {
  vars <- getVarNames(model)
  terms <- tokenizeQ(q)
  coefs <- getCoefNames(terms)
  createVarPairs(vars, coefs)
}

getVarNames <- function(model) {
  attr(model, "objective_vars_names")
}

# Tokenize the QP equation, keeping signs per term
tokenizeQ <- function(qEquation) {
  qEquation %>%
    str_split('(?=[+-])') %>%
    unlist %>%
    str_match("(^[+-]?\\d?)\\*?(.*$)")
}


getCoefNames <- function(tokenMatrix) {
  coefs <- setNames(tokenMatrix[,2], tokenMatrix[,3])
  # Manually add a coefficient of 1 if no coef is specified (e.g x^2)
  coefs[coefs == ''] <- 1
  coefs
}

# Create a long matrix of variable pairs and coefficients
createVarPairs <- function(varList, coefs) {
  varMat <- expand.grid(x1 = varList, x2 = varList)
  varMat$x3 <- ifelse(
    varMat$x1 == varMat$x2,
    paste0(varMat$x1, "^2"),
    paste0(varMat$x1, "*", varMat$x2)
  )
  varMat$values <- coefs[varMat$x3]
  varMat
}
