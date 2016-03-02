# processQP.R

getRemainder <- function(file) {
  str_extract(file, regex("Subject To.+$", dotall = TRUE))
}

getGoalAndObj <- function(file) {
  res <- list()
  file %>%
    remove_newLines %>%
    remove_spaces %C>%
    max_min %T>% {
      .$o ->> res$goal
    } %C>%
    get_objective %>% {
      fullObj <- .$o
      fullObj %C>%
        get_c %T>% {
          str_replace(fullObj, coll(.$i), '') ->> res$lp_obj
        } %C>%
        get_Q %>% {
          .$o ->> res$qp_obj
        }
    }
  return(res)
}

getModelComponents <- function(file) {
  tmp.rem <- getRemainder(file)
  res <- getGoalAndObj(file)
  res$rem <- tmp.rem
  return(res)
}

rebuildLP <- function(comps) {
  tmpContents <- paste(comps$goal, comps$lp_obj, comps$rem, sep = "\n")
  tmpFile <- tempfile("tmp_lp", fileext = ".lp")
  writeLines(tmpContents, tmpFile)
  return(tmpFile)
}

#' @export
processQP <- function(modelPath, type = "CPLEX_LP") {
  # Read the original CPLEX file as a string
  file <- readQP(modelPath)
  comps <- getModelComponents(file)
  tmp.path <- rebuildLP(comps)
  tmp.model <- Rglpk::Rglpk_read_file(tmp.path, type = type)
  parseQ(tmp.model, comps$qp_obj)
}
