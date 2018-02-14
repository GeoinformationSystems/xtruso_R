#' Create catchment graph from input shapefile using a specified connection field
#'
#' @param data input dataframe
#' @param name.id object identifier
#' @param name.conn attribute that contains link to connected object identifier
#' @param conn.NA name.conn value, if the object is not linked
#' @return igraph graph object 
#' @export
CatchmentGraph.initGraph <- function(data,
                                     name.id,
                                     name.conn,
                                     conn.NA) {

  #package igrapg must be available
  if (!"igraph" %in% installed.packages()[, "Package"])
    stop("Package igraph must be availabe to use this function.")
  
  #set object relations dataframe
  relations <- data.frame(from=data[name.conn], id=data[name.id])
  relations <- relations[relations[name.conn] != conn.NA, ]
  
  #set first column to name.id
  if(names(data)[1] != name.id){
    idx = which(names(data) == name.id)
    data <- data[,c(idx, setdiff(1:ncol(data), idx))]
  }
    
  
  #create graph
  return(igraph::graph.data.frame(relations, directed=TRUE, vertices=data))
  
}


#' get vertex by name
#'
#' @param graph input graph
#' @param name vertex name
#' @return vertex from graph
CatchmentGraph.getVertex <- function(graph,
                                     name) {
  
  #get index for requested vertex name
  idx <- which(igraph::V(graph)$name == name)
  
  #retrieve vertex by index
  return(igraph::V(graph)[idx])
  
}


#' get connected vertices
#'
#' @param graph input graph 
#' @param name input vertex identifier
#' @param order max vertex order
#' @param mode igraph search direction
#' @return neighborhood graph for input vertex neighborhood
CatchmentGraph.getNeighborhood <- function(graph,
                                           name,
                                           order = igraph::vcount(graph),
                                           mode = c("all", "out", "in")) {
  
  #get vertex
  vertex <- CatchmentGraph.getVertex(graph, name)
  
  #get subgraph
  return(igraph::make_ego_graph(graph, order, vertex, mode)[[1]])
  
}


#' get upstream catchment graph
#'
#' @param graph input graph
#' @param name input vertex identifier
#' @return upstream graph for input vertex
#' @export
CatchmentGraph.getUpstream <- function(graph,
                                       name) {
  
  return(CatchmentGraph.getNeighborhood(graph, name, mode="out"))
  
}


#' get downstream catchment graph
#'
#' @param graph input graph
#' @param name input vertex identifier
#' @return downstream graph for input vertex
#' @export
CatchmentGraph.getDownstream <- function(graph,
                                         name) {
  
  return(CatchmentGraph.getNeighborhood(graph, name, mode="in"))
  
}
