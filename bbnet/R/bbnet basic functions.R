#' @title Bayesian Belief Model v4.0.1 - Basic test function - error bar changes made
#'
#' @description
#'27 Feb 2024
#'Copyright Rick Stafford
#'Requires all functions loaded and:
#'   1) BBN interaction grid - named bbn.model
#'   2) list of scenarios indicating changes (1-12 can be used). Named priors1, priors2, ... priors12



#' @param multiplot
#'Plot function from
#'http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'CC0 licence
#'
#' @return ggplot
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#'    cols:   Number of columns in layout
#'    layout: A matrix specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.
#'
#' @export

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page``
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' @param isEmpty
#'
#' @return is empty function
#'
#' @export

isEmpty <- function(x) {
  return(length(x)==0)
}



#' @param bbn.predict
#' Run a predictive model on up to 12 scenarios at a time.
#' As a minumum, we need to pass the network model and one scenario to the function.
#'
#' Required arguments:
#'   1) bbn.model - a matrix or dataframe of interactions between different model nodes
#'   2) priors1 - an X by 2 array of initial changes to the system under investigation.
#'      The first column should be a -4 to 4 (including 0) integer value for each node in the network
#'      with negative values indicating a decrease and positive values representing an increase.
#'      0 represents no change.
#'
#' Optional Arguments:
#'   3) priors2 - priors12 - as above, but additional scenarios.
#'   4) boot_max - the number of bootstraps to perform.
#'      Suggested range for exploratory analysis 1-1000.
#'      For final analysis recommended size = 1000 - 10000 - note, this can take a long time to run.
#'      Default value is 1, running with no bootstrapping - suitable for exploration of data and error checking.
#'   5) values - default value 1. This provides a numeric output of posterior values and any confidence intervals.
#'      Set to 0 to hide this output
#'   6)figure - default value 1. Sets the figure options.
#'      0 = no figures produced.
#'      1 = figure is saved in working directory as a PDF file (note, this is overwritten if the name is not changed, and no figure is produced if the existing PDF is open when the new one is generated).
#'      2 = figure is produced in a graphics window. All figures are combined on a single plot where scenario 2 is below scenario 1 (i.e. scenarios work in columns then rows)
#'   7)font.size - default = 5. This sets the font size on the figures.
#'
#' @return The output given above shows the posterior outcome of the model.
#'This is a short-term prediction model.
#'There are also confidence intervals of our predictions.
#'
#' @export

bbn.predict <- function(bbn.model, priors1, ..., boot_max=1, values = 1, figure = 1, font.size=5){

  plot.keep= list() # to display plots at the end of the function
  policyno = 1 + length(list(...))


  if(length(list(...))>12){
    print('Limit of 12 scenarios at once. Graphs may fail to print properly')
  }

  #print(list(...)[1])

  for(policy in 1:policyno){

    # if uploading priors

    if(policy==1){node <- priors1}  # read.csv(priors1, header = T)}# need to read in files as priors for each tested scenario
    if(policy==2){node <- as.data.frame(list(...)[1])}  #read.csv(priors2, header = T)}
    if(policy==3){node <- as.data.frame(list(...)[2])}  #read.csv(priors3, header = T)}
    if(policy==4){node <- as.data.frame(list(...)[3])}  #read.csv(priors4, header = T)}
    if(policy==5){node <- as.data.frame(list(...)[4])}  #read.csv(priors5, header = T)}
    if(policy==6){node <- as.data.frame(list(...)[5])}  #read.csv(priors6, header = T)}
    if(policy==7){node <- as.data.frame(list(...)[6])}  #read.csv(priors7, header = T)}
    if(policy==8){node <- as.data.frame(list(...)[7])}  #read.csv(priors8, header = T)}
    if(policy==9){node <- as.data.frame(list(...)[8])}  #read.csv(priors9, header = T)}
    if(policy==10){node <- as.data.frame(list(...)[9])}  #read.csv(priors10, header = T)}
    if(policy==11){node <- as.data.frame(list(...)[10])}  #read.csv(priors11, header = T)}
    if(policy==12){node <- as.data.frame(list(...)[11])}  #read.csv(priors12, header = T)}

    colnames(node)<- c('Increase','name') # setting fixed column names as these are refered to later on

    ## Bayesian Belief network relies on values between 0 and 1, but -4 to 4 is much more intuitive, so converted at this stage
    node <- node %>%
      mutate(Increase = recode(Increase, '4' = '0.9', '3' = '0.8', '2' = '0.7', '1' ='0.6','-1'='0.4', '-2'='0.3', '-3'='0.2', '-4'='0.1', '0'='0.5'))

    node$Increase <- as.numeric(node$Increase)

    node$Decrease <- 1-node$Increase  # the decrease is 1- the increase once in probability format

    node <- node[,c('Increase','Decrease','name')]

    number.nodes <- dim(node)[1]

    #### edge strengths of the network, read in from the main csv file - again, converted from -4 to 4 into 0 to 1 format



    node.x.increase.if.node.y.increase <- bbn.model #read.csv(bbn.model, header = T)  # bbn.model is matrix of interaction, read in from csv file

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


    ###################### to here  -see additional paramters to add to the input.params function


    rep2 <- 4 # number of repetitions of the standard model


    ## storage of probabilities over time

    #node.store <- array(NA, c(number.nodes,rep)) # time series model
    node.store2 <- array(NA, c(number.nodes,rep2)) # original model
    boot.node.store <- array(NA,c(number.nodes,rep2,boot_max))

    #for(aaa in 1:number.nodes){node.store[aaa,1] <- node[aaa,1]}
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

    ######################################## as per Stafford et al. 2015 Excel Calculations ########################################
    for(boot in 1:boot_max){
      node2 <- node
      node.x.increase.if.node.y.increase <- node.x.increase.if.node.y.increase2
      adjust.number <- round(sum(apply(node.x.increase.if.node.y.increase,  1, function(x.33) length(which(!is.na(x.33)))))/10) # adjusting 10th of the parameters  -this picks which parameters to adjust in each bootsrap run, but doesn't apply to first run


      adjust.count<-1
      size.adjust<-length(node.x.increase.if.node.y.increase)  ## changing some of the parameters by +/-0.1 to create bootstrapping
      while(adjust.count<adjust.number+1){
        x_a1 <- round(runif(1,1,size.adjust))
        y_a1 <- round(runif(1,1,size.adjust))
        if(!is.na(node.x.increase.if.node.y.increase[x_a1,y_a1])){
          ## get value, and randomly increase by +/-0.1]
          if(boot==1){
            node.x.increase.if.node.y.increase[x_a1,y_a1]<-node.x.increase.if.node.y.increase[x_a1,y_a1]} ## note if bootstrap size is set to 1, then no bootstrapping takes place
          if(boot > 1){
            node.x.increase.if.node.y.increase[x_a1,y_a1]<-node.x.increase.if.node.y.increase[x_a1,y_a1] + runif(1,-0.1,0.1)}
          adjust.count <- adjust.count+1
        }
      }

      post.node2 <- array(NA, c(number.nodes, number.nodes))

      for(x.rep in 2:rep2){

        for(bbb in 1:(number.nodes*number.nodes)){   ### main Bayes equation
          temp1 <- (node.x.increase.if.node.y.increase[(ord[1,yyy[bbb]]),ord[2,yyy[bbb]]] * node2[ord[1,yyy[bbb]],1]) + (node.x.increase.if.node.y.decrease[ord[1,yyy[bbb]],ord[2,yyy[bbb]]] * node2[ord[1,yyy[bbb]],2])
          post.node2[ord[2,yyy[bbb]],ord[1,yyy[bbb]]] <- temp1
        } # this multiples each value in the probability table by each prior to create 'posterior' probability table

        for (f in 1:number.nodes){ ## converted via Bayes equation at this point, but testing  whether multiple nodes are pulling in the same direction - if so, combining probabilities, rather than averaging - see Stafford et al. 2015 for details

          post.node.calc <- c(post.node2[f,1:number.nodes])
          post.node.calc <- na.omit(post.node.calc)

          if(isEmpty(post.node.calc)) {
            temp2<-node2[f,1]
            #print('empty')
            #print(temp2)
            #print(f)
          }
          else if(all(post.node.calc == 0.5) == T){
            temp2 <- 0.5#mean(post.node.calc) # if all calculations of prior are 0.5, then take mean (set at 0.5)
            #print('0.5')
            #print(temp2)
            #print(f)
          }
          else if(all(post.node.calc >= 0.5) == T){  ## if all 0.5 or above, then sort by biggest first. take difference between current prob and multiply this up to increase cerinty when everything agrees
            post.node.calc <- sort(post.node.calc, decreasing = T)
            temp2 <- post.node.calc[1]
            if(length(post.node.calc)>1){
              for(cert in 2:length(post.node.calc)){
                temp2 <- temp2 + (post.node.calc[cert]-0.5)*(1-(temp2))
              }}
            #print('>0.5')
            #print(temp2)
            #print(f)
          }
          else if(all(post.node.calc <= 0.5) == T){ ##
            post.node.calc <- sort(post.node.calc, decreasing = F)
            temp2 <- post.node.calc[1]
            if(length(post.node.calc)>1){
              for(cert in 2:length(post.node.calc)){
                temp2 <- temp2 - (0.5-post.node.calc[cert])*((temp2))
              }}
            #print('<0.5')
            ##print(temp2)
            #print(f)
          }
          else{
            temp2 <- mean(post.node.calc)
            #print('none of above')
            #print(temp2)
            #print(f)
          }


          # if(all(is.na(post.node.calc))){
          #   post.node.calc <- 0.5
          #   print('all NA')
          #   print(f)
          # }
          #
          # if(is.na(temp2)){temp2<-mean(post.node.calc)}



          if(node2[f,1]>=0.5){q = 1}
          if(node2[f,1]<0.5){q=2}

          post.node1 <- (node2[f,q]) # if nothing further affecting it, then it will revert to prior - note in time series calc it is pulled towards 0.5 which is why values can differ
          post.node1.try2 <- temp2 # this section works out whether something is more certain if the prior is not used again
          #post.node1 <- mean(post.node.calc) # this one immediately reduces probability to post Bayes, so immediate changes, such as increasing reef, will only occur at intial time frame
          if(node2[f,1]> 0.5 & post.node1.try2 > post.node1) {post.node1 <- post.node1.try2}
          if(node2[f,1]< 0.5 & post.node1.try2 < post.node1) {post.node1 <- post.node1.try2}
          if(node2[f,1] == 0.5) {post.node1 <- post.node1.try2}
          node2[f,1:2]<- c(post.node1 , 1-post.node1)
          node.store2[f, x.rep] <- node2[f,1]

        }

      }
      boot.node.store[,,boot]<-node.store2
    }

    ## calculating bootstrap values - removing top and bottom 2.5% for 95% CI

    boot.node.mean <- apply(boot.node.store, c(1,2), mean)
    boot.node.upper<- apply(boot.node.store, c(1,2), quantile, probs=0.975)
    boot.node.lower<- apply(boot.node.store, c(1,2), quantile, probs=0.025)
    boot.node.actual <- boot.node.store[,,1] # note, this is the value from the first run, where no parameters were changed and forms the point on the final graph

    boot.node.upper.CI<- boot.node.upper - boot.node.mean
    boot.node.lower.CI <- boot.node.mean - boot.node.lower

    boot.node.actual.CI.upper <- boot.node.actual + boot.node.upper.CI
    boot.node.actual.CI.lower <- boot.node.actual - boot.node.lower.CI


    ########################################### end of Excel Version ######################################

    ##################### Calculation of values as per Stafford et al., 2015 and Excel model

    final.output <- node
    final.output.CI <- node
    colnames(final.output.CI)<-c('LowerCI','UpperCI','name')
    for(x in 1:dim(node)[1]){
      if((0.5 - min(boot.node.actual[x,]))>(max(boot.node.actual[x,])-0.5)){#if < 0.5 i.e. decreasing picks the lowest value across the four time steps
        final.output[x,1] <- min(boot.node.actual[x,])
        final.output[x,2] <- 1-final.output[x,1]
        final.output.CI[x,1] <- min(boot.node.actual.CI.lower[x,])
        final.output.CI[x,2] <- min(boot.node.actual.CI.upper[x,])

      }

      if((0.5 - min(boot.node.actual[x,]))<(max(boot.node.actual[x,])-0.5)){ #if>0.5 picks highest value across all time steps
        final.output[x,1] <- max(boot.node.actual[x,])
        final.output[x,2] <- 1-final.output[x,1]
        final.output.CI[x,1] <- max(boot.node.actual.CI.lower[x,])
        final.output.CI[x,2] <- max(boot.node.actual.CI.upper[x,])

      }
    }


    final.output <- cbind(final.output, final.output.CI[,1:2])



    final.output$name <- factor(final.output$name,levels=node$name)

    ### convert back to -4 to 4 scale


    final.output$Increase <- 10*(final.output$Increase - 0.5)
    #final.output$Decrease <- 8*(final.output$Decrease - 0.5) - no longer needed
    final.output$LowerCI <- 10*(final.output$LowerCI - 0.5)
    final.output$UpperCI <- 10*(final.output$UpperCI - 0.5)

    final.output <- subset(final.output, select = -Decrease)

    if(values==1){ ## print out outputs by scenario
      print(paste('Scenario number ', policy))
      print(final.output)
    }

    #### change extreme values so error bars print properly on figure

    ff <- function(a) {
      ifelse(a > 3.99, 3.99, a)
    }
    gg <- function(a) {
      ifelse(a < -3.99, -3.99, a)
    }


    final.output$Increase<-ff(final.output$Increase)
    final.output$Increase<-gg(final.output$Increase)
    final.output$LowerCI<-ff(final.output$LowerCI)
    final.output$LowerCI<-gg(final.output$LowerCI)
    final.output$UpperCI<-ff(final.output$UpperCI)
    final.output$UpperCI<-gg(final.output$UpperCI)

    p0<-ggplot(data=final.output, aes(x=name, y=Increase)) +
      geom_point(stat="identity", size=0.5) +
      geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width=0.1) +
      xlab("") + ylab("Predicted change") +
      theme(text = element_text(size=font.size)) +  scale_y_continuous(limits = c(-4, 4))


    p0<-p0 + coord_flip()



    plot.keep[[policy]] <- p0

  }

  if(figure==1){## plot pdf figure to working directory

    pdf("BBN_Output_RenameMe.pdf")

    if(policyno==1){multiplot(plot.keep[[1]])}
    if(policyno==2){multiplot(plot.keep[[1]],plot.keep[[2]])}
    if(policyno==3){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], cols=2)}
    if(policyno==4){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]], cols=2)}
    if(policyno==5){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], cols=2)}
    if(policyno==6){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], plot.keep[[6]], cols=2)}
    if(policyno==7){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], plot.keep[[6]],plot.keep[[7]], cols=2)}
    if(policyno==8){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]], cols=2)}
    if(policyno==9){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],  plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],cols=2)}
    if(policyno==10){multiplot(plot.keep[[1]],plot.keep[[2]],plot.keep[[3]], plot.keep[[4]], plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],plot.keep[[10]],cols=2)}
    if(policyno==11){multiplot(plot.keep[[1]],plot.keep[[2]],plot.keep[[3]], plot.keep[[4]], plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],plot.keep[[10]],plot.keep[[11]],cols=3)}
    if(policyno==12){multiplot(plot.keep[[1]],plot.keep[[2]],plot.keep[[3]], plot.keep[[4]], plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],plot.keep[[10]],plot.keep[[11]], plot.keep[[12]],cols=3)}

    dev.off()
  }

  if(figure==2){ # plot figure to console
    if(policyno==1){multiplot(plot.keep[[1]])}
    if(policyno==2){multiplot(plot.keep[[1]],plot.keep[[2]])}
    if(policyno==3){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], cols=2)}
    if(policyno==4){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]], cols=2)}
    if(policyno==5){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], cols=2)}
    if(policyno==6){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], plot.keep[[6]], cols=2)}
    if(policyno==7){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], plot.keep[[6]],plot.keep[[7]], cols=2)}
    if(policyno==8){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]], cols=2)}
    if(policyno==9){multiplot(plot.keep[[1]],plot.keep[[2]], plot.keep[[3]], plot.keep[[4]],  plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],cols=2)}
    if(policyno==10){multiplot(plot.keep[[1]],plot.keep[[2]],plot.keep[[3]], plot.keep[[4]], plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],plot.keep[[10]],cols=2)}
    if(policyno==11){multiplot(plot.keep[[1]],plot.keep[[2]],plot.keep[[3]], plot.keep[[4]], plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],plot.keep[[10]],plot.keep[[11]],cols=3)}
    if(policyno==12){multiplot(plot.keep[[1]],plot.keep[[2]],plot.keep[[3]], plot.keep[[4]], plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],plot.keep[[10]],plot.keep[[11]], plot.keep[[12]],cols=3)}
  }
}



#' @param bbn.timeseries
#' We need to pass the function a network model and a scenario as a minimum.
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
#'       where 2 - shows a brief pulse disturbance, which can be useful to visualise changes as peaks and troughs in increase and decrease of nodes can propagate through the network
#'
#' @return
#' Graph of each node in the network, visualised across the different timesteps in the model.
#'
#' @export

bbn.timeseries <- function(bbn.model, priors1, timesteps=5, disturbance = 1){ ### reads in a single scenario and makes figures of how parameters change over time for all or selected nodes

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

  node.store<-(node.store-0.5)*10 # convert back to -4 to 4 scale

  for (graphmake in 1: number.nodes){

    # if(mean(node.store[graphmake,])>=0.5){
    #
    # y.vals <- node.store[graphmake,]/max(node.store[graphmake,])}
    #
    # if(mean(node.store[graphmake,])<0.5){
    #
    #   y.vals <- 1-(min(node.store[graphmake,])/node.store[graphmake,])}

    x.vals <- 1:dim(node.store)[2]

    y.vals <- node.store[graphmake,]

    vals <- cbind.data.frame(x.vals, y.vals)

    colnames(vals) <- c('x', 'y')

    name_y <- node[graphmake,3]


    p<-ggplot(vals, aes(x=x, y=y)) + geom_point(size=2, shape=23) + geom_smooth(se=F, method="loess") +
      labs(x="Time Step", y = name_y)+theme_classic() + scale_color_grey()

    print(p)


  }

}



#' @param bbn.visualise
#' This function produces a network diagram in each timestep of the model.
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



#' @param bbn.network.diagram
#' Network diagrams will only serve to illustrate a ‘complex system’.
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



#' @param bbn.sensitivity
#' The function works by bootstrapping with multiple changes to prior values and interaction strengths in the network.
#' The frequency shows the number of times a modified interaction shows up as important in causing a change to the listed nodes.
#' As such, those interactions showing as more frequent in the table or figure are likely to be most influential in any predictions.
#' These should be subject to closer scrutiny in terms of values used.
#' (note, this does not mean the values are incorrect or should be reduced from more extreme values - i.e. from 4 to 3, just that they should be carefully checked)
#'
#' Required arguments:
#'   1) bbn.model - a matrix or dataframe of interactions between different model nodes.
#'      One or more nodes (recommended no more than 3) which would be the main outcomes of interest in the model.
#'      The spelling of these nodes needs to be identical (including capital letters) to that in the imported csv file (note, you should include spaces if these are in your csv file, rather than the dot notation used once imported into R).
#'
#' Optional arguments:
#'   2) boot_max - the number of bootstraps to perform.
#'      Suggested range for exploratory analysis 100-1000.
#'      For final analysis recommended size = 1000 - 10000 - note, this can take a long time to run. Default value is 1000.
#'
#' @return
#' This function produces a list of the most important parameters/interaction strengths to examine.
#'
#' @export

bbn.sensitivity <- function(bbn.model, boot_max = 1000, ...){

  x<-length(list(...))
  if(x < 1){print('Need to input at least one key variable')}
  if(x > 3){print('Recommend a maximum of three key variables')}

  node.x.increase.if.node.y.increase <- bbn.model  # bbn.model is matrix of interaction, read in from csv file

  names_node <- node.x.increase.if.node.y.increase[,1]
  number.nodes <- length(names_node)

  node <- array(NA,c(number.nodes,3))  ### setting up the node variable  -set at 0.5 as will be used like this throughout
  node[,3]<-names_node
  node[1:number.nodes,1]<-0.5
  node[1:number.nodes,2]<-0.5
  colnames(node)<- c('Increase','Decrease','name')
  node<-as.data.frame(node)
  node[,1]<-as.numeric(node[,1])
  node[,2]<-as.numeric(node[,2])

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


  ###################### to here  -see additional paramters to add to the input.params function


  rep2 <- 4 # number of repetitions of the standard model


  ## storage of probabilities over time

  #node.store <- array(NA, c(number.nodes,rep)) # time series model
  node.store2 <- array(NA, c(number.nodes,rep2)) # original model
  boot.node.store <- array(NA,c(number.nodes,rep2,boot_max))

  #for(aaa in 1:number.nodes){node.store[aaa,1] <- node[aaa,1]}
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

  ######################################## Stafford et al. 2015 Excel Calculations ########################################
  node.x.increase.if.node.y.increase <- node.x.increase.if.node.y.increase2
  adjust.number <- round(sum(apply(node.x.increase.if.node.y.increase,  1, function(x.33) length(which(!is.na(x.33)))))/10) # adjusting 10th of the parameters

  sens.names <- node.x.increase.if.node.y.increase

  rownames(sens.names) <- colnames(sens.names) ## needs row and column names

  sens.store <- array(NA,c(adjust.number,(boot_max-1)))

  for(boot in 1:boot_max){
    node2 <- node

    for(x in 1:number.nodes){
      node2$Increase[x]<-node2$Increase[x]+runif(1,-0.1,0.1)
    }

    node2$Decrease <- 1-node2$Increase

    #### set up dataframe to store sensitivity values

    adjust.count<-1
    size.adjust<-length(node.x.increase.if.node.y.increase)  ## changing by +/-0.1
    while(adjust.count<adjust.number+1){
      x_a1 <- round(runif(1,1,size.adjust))
      y_a1 <- round(runif(1,1,size.adjust))
      if(!is.na(node.x.increase.if.node.y.increase[x_a1,y_a1])){
        ## get value, and randomly increase by +/-0.1]
        if(boot==1){
          node.x.increase.if.node.y.increase[x_a1,y_a1]<-node.x.increase.if.node.y.increase[x_a1,y_a1]} ## note if bootstrap size is set to 1, then no bootstrapping takes place
        if(boot > 1){
          node.x.increase.if.node.y.increase[x_a1,y_a1]<-node.x.increase.if.node.y.increase[x_a1,y_a1] + runif(1,-0.1,0.1)}
        sens.store[adjust.count,(boot-1)]<-paste(rownames(sens.names)[x_a1],colnames(sens.names)[y_a1],sep="->")
        adjust.count <- adjust.count+1
      }
    }

    post.node2 <- array(NA, c(number.nodes, number.nodes))

    for(x.rep in 2:rep2){

      for(bbb in 1:(number.nodes*number.nodes)){   ### main Bayes equation
        temp1 <- (node.x.increase.if.node.y.increase[(ord[1,yyy[bbb]]),ord[2,yyy[bbb]]] * node2[ord[1,yyy[bbb]],1]) + (node.x.increase.if.node.y.decrease[ord[1,yyy[bbb]],ord[2,yyy[bbb]]] * node2[ord[1,yyy[bbb]],2])
        post.node2[ord[2,yyy[bbb]],ord[1,yyy[bbb]]] <- temp1
      } # not sure how the 'ord' works, but basically this multiples each value in the probability table by each prior to create 'posterior' probability table

      for (f in 1:number.nodes){

        post.node.calc <- c(post.node2[f,1:number.nodes])
        post.node.calc <- na.omit(post.node.calc)

        if(isEmpty(post.node.calc)) {
          temp2<-node2[f,1]
          #print('empty')
          #print(temp2)
          #print(f)
        }
        else if(all(post.node.calc == 0.5) == T){
          temp2 <- 0.5#mean(post.node.calc) # if all calculations of prior are 0.5, then take mean (set at 0.5)
          #print('0.5')
          #print(temp2)
          #print(f)
        }
        else if(all(post.node.calc >= 0.5) == T){  ## if all 0.5 or above, then sort by biggest first. take difference between current prob and multiply this up to increase cerinty when everything agrees
          post.node.calc <- sort(post.node.calc, decreasing = T)
          temp2 <- post.node.calc[1]
          if(length(post.node.calc)>1){
            for(cert in 2:length(post.node.calc)){
              temp2 <- temp2 + (post.node.calc[cert]-0.5)*(1-(temp2))
            }}
          #print('>0.5')
          #print(temp2)
          #print(f)
        }
        else if(all(post.node.calc <= 0.5) == T){ ##
          post.node.calc <- sort(post.node.calc, decreasing = F)
          temp2 <- post.node.calc[1]
          if(length(post.node.calc)>1){
            for(cert in 2:length(post.node.calc)){
              temp2 <- temp2 - (0.5-post.node.calc[cert])*((temp2))
            }}
          #print('<0.5')
          ##print(temp2)
          #print(f)
        }
        else{
          temp2 <- mean(post.node.calc)
          #print('none of above')
          #print(temp2)
          #print(f)
        }


        # if(all(is.na(post.node.calc))){
        #   post.node.calc <- 0.5
        #   print('all NA')
        #   print(f)
        # }
        #
        # if(is.na(temp2)){temp2<-mean(post.node.calc)}



        if(node2[f,1]>=0.5){q = 1}
        if(node2[f,1]<0.5){q=2}

        post.node1 <- (node2[f,q]) # if nothing further affecting it, then it will revert to prior - in time series it is pulled towards 0.5
        post.node1.try2 <- temp2 # this section works out whether something is more certain if the prior is not used again
        #post.node1 <- mean(post.node.calc) # this one immediately reduces probability to post Bayes, so immediate changes, such as increasing reef, will only occur at intial time frame
        if(node2[f,1]> 0.5 & post.node1.try2 > post.node1) {post.node1 <- post.node1.try2}
        if(node2[f,1]< 0.5 & post.node1.try2 < post.node1) {post.node1 <- post.node1.try2}
        if(node2[f,1] == 0.5) {post.node1 <- post.node1.try2}
        node2[f,1:2]<- c(post.node1 , 1-post.node1)
        node.store2[f, x.rep] <- node2[f,1]

      } ### going wrong here, as on 2nd and third run through, those lower than 0.5 are turning into NaN

    }
    boot.node.store[,,boot]<-node.store2
  }

  ### sensitivity analysis ######

  key.variable1 <- c(...)

  max.variable <- length(key.variable1)

  #### calculate which parameters changed in the top 25% of bootstrapped replications

  display.number <- round(boot_max*0.25)
  if(display.number<1){
    display.number=1
  }

  sens.output <- array(NA,c(adjust.number,display.number,max.variable))


  for(var1 in 1:max.variable){


    find.col <- which(node[,3]==key.variable1[var1])
    key.values1<-boot.node.store[find.col,rep2,]

    dif.val <- abs(key.values1 - key.values1[1])### work out the differences between actual and bootstrapped
    dif.val.adjust <- runif(boot_max,-0.0001, 0.0001) # to adjust the numbers to prevent ties
    dif.val <- dif.val+dif.val.adjust

    dif.val.rank <- rank(dif.val) # rank biggest



    for(output.param in 1:display.number){
      output.index <- boot_max - (output.param-1)
      output.save <- which(dif.val.rank==output.index)
      sens.output[,output.param,max.variable] <- sens.store[,(output.save-1)] ### -1 as there is no storage of first set of parameters as these don't change
    }

  }

  tab<-table(sens.output)

  tab<-tab[order(tab)]

  tab<-as.data.frame(tab)

  p<- ggplot(tab,                                   # Draw barchart of table
             aes(x = sens.output,
                 y = Freq)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0))

  print(p)

  print(tab)

}
