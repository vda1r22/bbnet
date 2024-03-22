#' @title Bayesian Belief Model v4.0.1 - Visualise function
#'
#' @description
#'27 Feb 2024
#'Copyright Rick Stafford
#'
#'Requires all functions loaded and:
#'   1) BBN interaction grid - named bbn.model
#'   2) list of scenarios indicating changes (1-12 can be used). Named priors1, priors2, ... priors12
#'
#' Required arguments:
#'    1) bbn.model - a matrix or dataframe of interactions between different model nodes.
#'    2) priors1 - an X by 2 array of initial changes to the system under investigation.
#'       The first column should be a -4 to 4 (including 0) integer value for each node in the network with negative values indicating a decrease and positive values representing an increase.
#'       0 represents no change.
#'
#' Optional Arguments:
#'    3) timesteps - default = 5. This is the number of timesteps the model performs.
#'       Note, timesteps are arbitrary and non-linear. However, something occurring in timestep 2, should occur before timestep 3.
#'    4) disturbance - default = 1.
#'       where 1 - creates a prolonged or press disturbance as per the bbn.predict() function. Essentially prior values for each manipulated node are at least maintained (if not increased through reinforcement in the model) over all timesteps.
#'       where 2 - shows a brief pulse disturbance, which can be useful to visualise changes as peaks and troughs in increase and decrease of nodes can propagate through the network.
#'    5) threshold - default = 0.2. Nodes which deviate from 0 by more than this threshold value will display interactions with other nodes.
#'       As mentioned, values in these visualisation functions don't directly correspond to those in the bbn.predict() function.
#'       This value can be tweaked from 0 to 4 to create the most useful visualisations.
#'    6) font.size - default = 0.7. Changes the font in the figure produced.
#'       The value here is a multiplier of the default font size used in the igraph package and does not correspond to the font.size argument in the bbn.timeseries() function.
#'    7) arrow.size - default = 4. Changes the size of the arrows.
#'       Note, sizes do vary based on interaction strength, so this is a multiplier for visualisation purposes.
#'
#' @param bbn.visualise
#' This function produces a network diagram in each timestep of the model.
#'
#' @return
#' Network diagram where the most important interactions at that timestep are listed
#' and the colour of nodes changes from black (showing the highest increase) to white (showing the lowest increase / largest decrease).
#'
#' @export

bbn.visualise <- function(bbn.model, priors1, timesteps=5, disturbance = 1, threshold=0.2, font.size=0.7, arrow.size=4){

  node <- priors1 # read in single scenario

  colnames(node)<- c('Increase','name') # setting fixed column names as these are refered to later on

  ## Bayesian Belief network relies on values between 0 and 1, but -4 to 4 is much more intuitive, so converted at this stage
  node <- node %>%
    mutate(Increase = recode(Increase, '4' = '0.9', '3' = '0.8', '2' = '0.7', '1' ='0.6','-1'='0.4', '-2'='0.3', '-3'='0.2', '-4'='0.1', '0'='0.5'))

  node$Increase <- as.numeric(node$Increase)

  node$Decrease <- 1-node$Increase  # the decrease is 1- the increase once in probability format

  node <- node[,c('Increase','Decrease','name')]

  number.nodes <- dim(node)[1]

  #### edge strengths of the network, read in from the main csv file - again, converted from -4 to 4 into 0 to 1 format



  node.x.increase.if.node.y.increase <- bbn.model # input of bbm matrix

  #### convert from +4 to -4 scale to 1 to 0 scale
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==4]<- 0.9
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==3]<- 0.8
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==2]<- 0.7
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==1]<- 0.6
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-1]<- 0.4
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-2]<- 0.3
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-3]<- 0.2
  node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-4]<- 0.1


  node.x.increase.if.node.y.increase <- node.x.increase.if.node.y.increase[,-1] # drops first column which was text

  node.x.increase.if.node.y.increase <- as.data.frame(node.x.increase.if.node.y.increase)

  node.x.increase.if.node.y.increase2 <- (0 + node.x.increase.if.node.y.increase) # force contents to act as numbers, as initially one column was text

  row.names(node.x.increase.if.node.y.increase)<-node$name # ensuring rows and columns in the model have the same names as the list of nodes
  colnames(node.x.increase.if.node.y.increase)<-node$name

  # create decrease matrix - for Bayes calculation
  node.x.increase.if.node.y.decrease <- (1 - node.x.increase.if.node.y.increase2)

  #fix(node.x.increase.if.node.y.decrease)


  rep <- timesteps # number of repititions of the time series model
  disturbancetype <- disturbance # prolonged pulse disturbance (1 = prolonged, 2 = pulse)

  ## storage of probabilities over time

  node.store <- array(NA, c(number.nodes,rep)) # time series model
  node.store2 <- array(NA, c(number.nodes,rep)) # original model
  #  boot.node.store <- array(NA,c(number.nodes,rep2,boot_max))

  for(aaa in 1:number.nodes){node.store[aaa,1] <- node[aaa,1]}
  for(aaa in 1:number.nodes){node.store2[aaa,1] <- node[aaa,1]}

  ### set up order to look at connections - specifies node 1 on node 2, node 1 on node 3 etc

  ord <- array(NA, dim=c(2,(number.nodes*number.nodes)))

  aaa <-1
  for(i in seq(1, (number.nodes*number.nodes), number.nodes)){
    ord[1,i:(i+number.nodes-1)]<-aaa
    aaa<-aaa+1
    ord[2,1:(i+number.nodes-1)] <- 1:number.nodes
  }

  yyy<- 1:(number.nodes*number.nodes)

  #### ordering complete

  ################################## Time series Calcs ##############################################

  post.node <- array(NA, c(number.nodes, number.nodes))

  for(x.rep in 2:rep){

    for(bbb in 1:(number.nodes*number.nodes)){
      temp1 <- (node.x.increase.if.node.y.increase[(ord[1,yyy[bbb]]),ord[2,yyy[bbb]]] * node[ord[1,yyy[bbb]],1]) + (node.x.increase.if.node.y.decrease[ord[1,yyy[bbb]],ord[2,yyy[bbb]]] * node[ord[1,yyy[bbb]],2])
      post.node[ord[2,yyy[bbb]],ord[1,yyy[bbb]]] <- temp1
    }

    for (f in 1:number.nodes){

      post.node.calc <- c(post.node[f,1:number.nodes])
      post.node.calc <- na.omit(post.node.calc)

      if(node[f,1]>=0.5){q = 1}
      if(node[f,1]<0.5){q=2}

      if(all(is.na(post.node.calc))){
        post.node.calc <- 0.5
      }

      if(disturbancetype == 1){
        post.node1 <- (node[f,q]) #### disturbance is prolonged and remains at prior value or greater throughout
      }
      if(disturbancetype == 2){
        post.node1 <- mean(post.node.calc) # pulse disturbance likely to dissipate rapidly
      }

      post.node1.try2 <- mean(post.node.calc) # this section works out whether something is more certain if the prior is not used
      if(node[f,1]> 0.5 & post.node1.try2 > post.node1) {post.node1 <- post.node1.try2}
      if(node[f,1]< 0.5 & post.node1.try2 < post.node1) {post.node1 <- post.node1.try2}
      if(node[f,1] == 0.5) {post.node1 <- post.node1.try2}
      node[f,1:2]<- c(post.node1 , 1-post.node1)
      node.store[f, x.rep] <- node[f,1]
    }

  }

  # ## visualisation

  dta <- bbn.model

  temp.array <- (1:number.nodes)
  if(number.nodes<=9){temp.array <- paste("s0",temp.array[1:number.nodes],  sep="")}

  if(number.nodes>9){
    temp.array1 <- paste("s0",temp.array[1:9],  sep="")
    temp.array2 <- paste("s",temp.array[10:number.nodes],  sep="")
    temp.array <- c(temp.array1, temp.array2)
  }

  strength.array <- node.store[,x.rep] # prob of increasing per timestep
  dta<- add_column(dta, node.type=strength.array, .before = 1)
  dta<- add_column(dta, node.name=node$name, .before = 1)
  dta<- add_column(dta, id=temp.array, .before = 1)

  edges <-  data.frame(from=character(),to=character(),weight=integer(),type=integer(),stringsAsFactors=FALSE)

  nodes <- dta[,1:3]
  size.data <- dim(dta)[1]

  dta <- dta[,5:(size.data+4)]

  for(x.rep in 1:rep){

    strength.array <- node.store[,x.rep] # prob of increasing per timestep


    nodes[,3] <- strength.array


    edge.count<-1

    for(x in 1:size.data){
      for(y in 1:size.data){
        if(is.na(dta[x,y])==F){
          edges[edge.count,1] <- as.character(nodes$id[x])
          edges[edge.count,2] <- as.character(nodes$id[y])
          if(dta[x,y]>0){
            if(dta[x,y]==4){
              edges[edge.count,3] <- 4
              edges[edge.count,4] <- 1 # 1 = positive interaction, 2 = negative interaction
            }
            if(dta[x,y]==3){
              edges[edge.count,3] <- 3
              edges[edge.count,4] <- 1
            }
            if(dta[x,y]==2){
              edges[edge.count,3] <- 2
              edges[edge.count,4] <- 1
            }
            if(dta[x,y]==1){
              edges[edge.count,3] <- 1
              edges[edge.count,4] <- 1
            }
          }
          if(dta[x,y]<0){
            if(dta[x,y]==-4){
              edges[edge.count,3] <- 4
              edges[edge.count,4] <- 2
            }
            if(dta[x,y]==-3){
              edges[edge.count,3] <- 3
              edges[edge.count,4] <- 2
            }
            if(dta[x,y]==-2){
              edges[edge.count,3] <- 2
              edges[edge.count,4] <- 2
            }
            if(dta[x,y]==-1){
              edges[edge.count,3] <- 1
              edges[edge.count,4] <- 2
            }
          }
          edge.count <- edge.count+1
        }
      }
    }


    links <- edges

    ## now remove links from any nodes less than threshold

    up_thresh <- (threshold/8)+0.5
    low_thresh <- 0.5-(threshold/8)

    nodes22<-nodes[nodes$node.type > up_thresh | nodes$node.type < low_thresh,]


    for(x in dim(links)[1]:1){
      remove=0
      if(edges$from[x] %in% nodes22$id ==F){
        remove = 1
      }
      if(edges$to[x] %in% nodes22$id ==F){
        remove = 1
      }
      if(remove==1){
        links<-links[-x,]
      }
    }


    #print(rank(nodes$node.type))
    nodes$node.type<-rank(nodes$node.type)

    net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)

    # Generate colors based on node type:
    colrs <- gray.colors(size.data, start=1, end=0)

    V(net)$color <- colrs[V(net)$node.type]

    colrs2 <- c("black", "red")
    E(net)$color <- colrs2[E(net)$type]

    E(net)$width <- (E(net)$weight)*(arrow.size/4)
    V(net)$label.cex <-font.size

    #l <- layout_on_sphere(net)

    print(plot(net, edge.arrow.size=0.5,vertex.label=nodes$node.name,edge.curved=.3,layout=layout_on_grid))

  }
}
