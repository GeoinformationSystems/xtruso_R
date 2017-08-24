#' Evaluate function to calculate raster statistics based on a mapping table providing a function
#' column to be applied to an input raster
#'
#' @param raster raster that serves as input for function evaluation
#' @param mapping mapping table containing  a function column
#' @param column column name with function to be applied
#' @return mapping table with attached results ($value)
#' @export
raster_evalRasterFun <- function(raster, mapping, column) {

  #execute function for each feature and append it as "value"
  mapping["value"] <- apply(mapping, 1, function(row){
    eval(parse(text = row[column]))
  })

  return(mapping)

}
