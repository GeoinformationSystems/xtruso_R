#' Create graph from input dataframe using a specified connection field
#'
#' @param data input dataframe
#' @param colname.id column that specifies object identifier
#' @param colname.conn.in column that specifies link to incomming object identifier(s)
#' @param colname.conn.out column that specifies link to outgoing object identifier(s)
#' @param conn.sep value separator for multiple incomming or outcoming connections
#' @param conn.none no connection value
#' @param conn.directed flag: connections are directed
#' @return igraph graph implementation
#' @export
#' 
x.graph.init <- function(data,
                         colname.id = names(data[1]),
                         colname.conn.in = NA,
                         colname.conn.out = NA,
                         conn.sep = ",",
                         conn.none = NA,
                         conn.directed = TRUE) {

  #package igraph must be available
  if (!"igraph" %in% installed.packages()[, "Package"])
    stop("Package igraph must be availabe to use graph structures.")
  
  if(missing(data))
    stop("Need to specify input connection dataframe.")
  
  #init object relations dataframe
  relations <- data.frame(stringsAsFactors=F)
  
  #check for incomming relations
  if(!is.na(colname.conn.in) && colname.conn.in %in% names(data)){
    #set no connection value to NA
    if(!is.na(conn.none))
      data[!is.na(data[colname.conn.in]) & data[colname.conn.in] == conn.none, colname.conn.in] <- NA
    #init relations
    relations <- rbind(relations, do.call("rbind", apply(data[!is.na(data[colname.conn.in]), c(colname.id, colname.conn.in)], 1, function(row){
      x.graph.connections(conn.in = row[colname.conn.in], 
                          conn.out = row[colname.id],
                          conn.sep = conn.sep)
    })))
  }
  
  #check for outgoing relations
  if(!is.na(colname.conn.out) && colname.conn.out %in% names(data)){
    #set no connection value to NA
    if(!is.na(conn.none))
      data[!is.na(data[colname.conn.out]) & data[colname.conn.out] == conn.none, colname.conn.out] <- NA
    #init relations
    relations <- rbind(relations, do.call("rbind", apply(data[!is.na(data[colname.conn.out]), c(colname.id, colname.conn.out)], 1, function(row){
      x.graph.connections(conn.in = row[colname.id], 
                          conn.out = row[colname.conn.out],
                          conn.sep = conn.sep)
    })))
  }
  
  #set first column in data to col.id
  if(names(data)[1] != colname.id){
    idx = which(names(data) == col.id)
    data <- data[, c(idx, setdiff(1:ncol(data), idx))]
  }
    
  #create graph
  return(igraph::graph.data.frame(relations, directed=conn.directed, vertices=data))
  
}


#' get connection for object based on provided input and output identifiers
#'
#' @param conn.in incomming object identifier(s)
#' @param conn.out outgoing object identifier(s)
#' @param conn.sep value separator for multiple incomming or outcoming connections
#' @return dataframe with connections
#' 
x.graph.connections <- function(conn.in,
                                conn.out,
                                conn.sep = ",") {
  
  c.from <- unlist(ifelse(typeof(conn.in) == "character", strsplit(conn.in, conn.sep), conn.in))
  c.to <- unlist(ifelse(typeof(conn.out) == "character", strsplit(conn.out, conn.sep), conn.out))
  
  return(data.frame(from=c.from, to=c.to, stringsAsFactors=F))
  
}


#' get vertex by name
#'
#' @param graph input graph
#' @param name vertex name
#' @return vertex from graph
#' 
x.graph.vertex <- function(graph,
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
#' @export
#' 
x.graph.neighborhood <- function(graph,
                                 name,
                                 order = igraph::vcount(graph),
                                 mode = c("all", "out", "in")) {
  
  if(missing(graph))
    stop("Need to specify input graph.")
  
  if(missing(name))
    stop("Need to specify input vertex identifier")
  
  #get vertex
  vertex <- x.graph.vertex(graph, name)
  
  #get subgraph
  return(igraph::make_ego_graph(graph, order, vertex, mode)[[1]])
  
}


#' get outward connection graph
#'
#' @param graph input graph
#' @param name input vertex identifier
#' @return outward graph starting at input vertex
#' @export
#' 
x.graph.neighborhood.out <- function(graph,
                                     name) {
  
  return(x.graph.neighborhood(graph, name, mode="out"))
  
}


#' get inward connection graph
#'
#' @param graph input graph
#' @param name input vertex identifier
#' @return inward graph starting at input vertex
#' @export
#' 
x.graph.neighborhood.in <- function(graph,
                                    name) {
  
  return(x.graph.neighborhood(graph, name, mode="in"))
  
}
