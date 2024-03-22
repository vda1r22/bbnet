#' @title Bayesian Belief Model v4.0.1 - Network diagrams
#'
#' @description
#'27 Feb 2024
#'Copyright Rick Stafford
#'
#'Requires all functions loaded and:
#'   1) BBN interaction grid - named bbn.model
#'   2) list of scenarios indicating changes (1-12 can be used). Named priors1, priors2, ... priors12
#' Nodes can also be colour coded by theme.
#' It is easiest to create a slightly different csv file to produce these networks, which allows for the colour coding.
#'
#' Required arguments:
#' bbn.network - a csv file as described above, with note paid to the first three column names.
#'
#' Optional arguments:
#'    1) font.size - default = 0.7. Changes the font in the figure produced.
#'       The value here is a multiplier of the default font size used in the igraph package and does not correspond to the font.size argument in the bbn.timeseries() function.
#'    2) arrow.size - default = 4. Changes the size of the arrows.
#'       Note, sizes do vary based on interaction strength, so this is a multiplier for visualisation purposes.
#'       Negative interactions are shown by red arrows, and positive interactions by black arrows.
#'    3) arrange - this describes how the final diagram looks.
#'       Default is `layout_on_sphere` but `layout_on_grid` provides the same layout as in the bbn.visualise() function and ensures nodes are structured in the order specified in the network.
#'       Other layouts, including `layout_on_sphere` are more randomly determined, and better/clearer diagrams may occur if you run these multiple times.
#'       Other options are from the igraph package

#' @param bbn.network.diagram
#' To illustrate a ‘complex system’.

#'
#' @return
#' It visualises all nodes and interactions in a full network.
#'
#'
#' @export

bbn.network.diagram <- function(bbn.network, font.size=0.7, arrow.size=4, arrange = layout_on_sphere){

  dta <- bbn.network

  edges <-  data.frame(from=character(),to=character(),weight=integer(),type=integer(),stringsAsFactors=FALSE)

  nodes <- dta[,1:3]

  size.data <- dim(dta)[1]

  dta <- dta[,4:(size.data+3)]

  edge.count<-0

  for(x in 1:size.data){
    for(y in 1:size.data){
      if(is.na(dta[x,y])==F){
        edge.count <- edge.count+1
        edges[edge.count,1] <- as.character(nodes$id[x])
        edges[edge.count,2] <- as.character(nodes$id[y])
        if(dta[x,y]>0){
          edges[edge.count,3] <- dta[x,y]
          edges[edge.count,4] <- 1 # 1 = positive interaction, 2 = negative interaction

        }
        if(dta[x,y]<0){

          edges[edge.count,3] <- dta[x,y]*-1
          edges[edge.count,4] <- 2}


      }

    }
  }

  #write.csv(edges, 'EDGES_dept.csv')

  links <- edges

  net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)

  # Generate colors based on node type:
  colrs <- c("gray50", "tomato", "gold", "white")
  V(net)$color <- colrs[V(net)$node.type]

  colrs2 <- c("black", "red")
  E(net)$color <- colrs2[E(net)$type]

  E(net)$width <- E(net)$weight*(arrow.size/4)
  V(net)$label.cex <-font.size
  V(net)$label.color <- 'blue'

  #l <- layout_on_sphere(net)

  print(plot(net, edge.arrow.size=0.3,vertex.label=nodes$node.name,edge.curved=.2, layout  =arrange))

}
