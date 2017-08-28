#' Evaluate function to calculate raster statistics based on a mapping table
#' providing a function to be evaluated
#'
#' @param raster raster input for function evaluation
#' @param mapping mapping table containing a function description
#' @param column column name from mapping table with function to be applied on raster
#' @return mapping table with attached results ($value)
#' @export
EvalRasterFun <- function(raster, mapping, column) {

  #parallel execution with foreach
  if (requireNamespace("doSNOW", quietly=TRUE)) {
    start.time <- Sys.time()
    #init parallel environment
    cl <- snow::makeCluster(as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")) - 1)
    doSNOW::registerDoSNOW(cl)

    #evaluation function
    mapping["value"] <- foreach::foreach(i=1:nrow(mapping), .combine=rbind, .export=c("raster"), .packages=c("raster")) %dopar% {
      return(eval(parse(text = mapping[i,column])))
    }

    #stop cluster
    snow::stopCluster(cl)
    print(Sys.time() - start.time)

  #sequential execution with apply
  } else {

    #evaluation function
    mapping["value"] <- apply(mapping, 1, function(row){
      eval(parse(text = row[column]))
    })

  }

  return(mapping)

}
