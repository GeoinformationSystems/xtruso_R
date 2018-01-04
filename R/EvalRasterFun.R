#' Evaluate function to calculate raster statistics based on a mapping table
#' providing a function to be evaluated
#'
#' @param stack raster or raster stack as input for function evaluation
#' @param mapping mapping table containing a functional description to be evaluated
#' @param column column name from mapping table with function to be evaluated
#' @param raster.name name of the raster
#' @return mapping table with attached results ($value_$raster@title$)
#' @export
EvalRasterFun <- function(stack,
                          mapping,
                          column,
                          parallel = TRUE,
                          raster.name = "raster") {

  #add column with internal raster name
  .raster.name <- "raster.i"
  mapping[.raster.name] <- apply(mapping, 1, FUN = function(row) {
    gsub(raster.name, .raster.name, row[column])
  })

  #parallel execution with foreach
  if (parallel && "doParallel" %in% installed.packages()[, "Package"]) {

    require(doParallel, quietly = TRUE)

    #init parallel environment
    cl <- makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)

    #evaluation function
    for(i in 1:nlayers(stack)) {
      raster.i <- stack[[i]]
      mapping[paste("value", raster.i@title, sep="_")] <- foreach::foreach(j=1:nrow(mapping), .combine=rbind, .export=c(.raster.name), .packages=c("raster")) %dopar% {
        return(eval(parse(text = mapping[j,.raster.name])))
      }
    }

    #stop cluster
    parallel::stopCluster(cl)

  #sequential execution with apply
  } else {

    #evaluation function
    for(i in 1:nlayers(stack)) {
      raster.i <- stack[[i]]
      mapping[paste("value", raster.i@title, sep="_")] <- apply(mapping, 1, function(row){
        eval(parse(text = row[.raster.name]))
      })
    }

  }

  mapping <- mapping[ , !(names(mapping) %in% c(.raster.name))]

  return(mapping)

}
