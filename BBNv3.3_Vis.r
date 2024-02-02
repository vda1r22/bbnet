### Bayesian Belief Model v3.4 - with bootstrap on excel method, revised from 3.0 to adjust bootstrap to give actual value and variability.
### Revised from v3.2 to account for incorrect negative interaction calculations
### Revised from version v3.3 to allow integer based interactions and priors, rather than probabilities
### 23 Jan 2023
### Copyright Rick Stafford

rm(list=ls(all=TRUE)) # clear any background

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tibble")) install.packages("tibble")
if (!require("igraph")) install.packages("igraph")
if (!require("dplyr")) install.packages("dplyr")

boot_max <- 1
### number of bootstraps to perform
plot.keep= list()

############################# Plot function only Copied from internet http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ - copyright not mine #########################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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

################################################# end of plot function ##################################################


################################### is empty function ################################

isEmpty <- function(x) {
  return(length(x)==0)
}

###########################################################################

for(policy in 1:10){

#number.nodes <- 6 # number of nodes in the network
### this would either be determined by a numeric entry or by uploading a csv file of priors
#node <- array(0.5, c(number.nodes,3))
#node <- array(0.5, c(number.nodes,3))
#node<-as.data.frame(node)
#node[,3] <- as.character(1:number.nodes) # this can be used as the name of a node if needed
#node[,3] <- as.character(node[,3])
#node[,1] is prob increase
#node[,2]is prob decrease

#node[1,1:2] <- c(0.8, 0.2) # to add values automatically

# if uploading priors
  
if(policy==1){node <- read.csv('priors1.csv', header = T)}
if(policy==2){node <- read.csv('priors2.csv', header = T)}
if(policy==3){node <- read.csv('priors3.csv', header = T)}
if(policy==4){node <- read.csv('priors4.csv', header = T)}
if(policy==5){node <- read.csv('priors5.csv', header = T)}
if(policy==6){node <- read.csv('priors6.csv', header = T)}
if(policy==7){node <- read.csv('priors7.csv', header = T)}
if(policy==8){node <- read.csv('priors8.csv', header = T)}
if(policy==9){node <- read.csv('priors9.csv', header = T)}
if(policy==10){node <- read.csv('priors10.csv', header = T)}
  
colnames(node)<- c('Increase','name')
#fix(node) #allows values to be changed

node <- node %>%
  mutate(Increase = recode(Increase, '4' = '0.9', '3' = '0.8', '2' = '0.7', '1' ='0.6','-1'='0.4', '-2'='0.3', '-3'='0.2', '-4'='0.1', '0'='0.5'))

node$Increase <- as.numeric(node$Increase)

node$Decrease <- 1-node$Increase

node <- node[,c('Increase','Decrease','name')]

number.nodes <- dim(node)[1]

#### connections

# either this section, or read in csv of interaction increase probabilities

#node.x.increase.if.node.y.increase <- array(NA, c(number.nodes,number.nodes))

## or read it in

node.x.increase.if.node.y.increase <- read.csv('Bali_BBN.csv', header = T)

#### convert from +4 to -4 scale to 1 to 0 scale
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==4]<- 0.9
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==3]<- 0.8
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==2]<- 0.7
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==1]<- 0.6
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-1]<- 0.4
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-2]<- 0.3
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-3]<- 0.2
node.x.increase.if.node.y.increase[node.x.increase.if.node.y.increase==-4]<- 0.1


node.x.increase.if.node.y.increase <- node.x.increase.if.node.y.increase[,-1]
  
node.x.increase.if.node.y.increase <- as.data.frame(node.x.increase.if.node.y.increase)

node.x.increase.if.node.y.increase2 <- (0 + node.x.increase.if.node.y.increase) # don't know why - seems to force contents to act as numbers

# adding data to save time at moment
#node.x.increase.if.node.y.increase[1,] <- c(NA,0.2,0.15)
#node.x.increase.if.node.y.increase[2,] <- c(0.7, NA, 0.3)
#node.x.increase.if.node.y.increase[3,] <- c(0.8, 0.35, NA)

row.names(node.x.increase.if.node.y.increase)<-node$name
colnames(node.x.increase.if.node.y.increase)<-node$name

#fix(node.x.increase.if.node.y.increase)

# create decrease matrix, then display it if changes are wanted
node.x.increase.if.node.y.decrease <- (1 - node.x.increase.if.node.y.increase2) # as default

#fix(node.x.increase.if.node.y.decrease)

rep <- 8 # number of repititions of the time series model
rep2 <- 4 # number of repititions of the standard model
disturbancetype <- 2 # prolonged pulse disturbance (1 = prolonged, 2 = prolonged pulse, 3= short pulse)

## storage of probabilities over time

node.store <- array(NA, c(number.nodes,rep)) # time series model
node.store2 <- array(NA, c(number.nodes,rep2)) # original model
boot.node.store <- array(NA,c(number.nodes,rep2,boot_max))

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

######################################## Stafford et al. 2015 Excel Calculations ########################################
for(boot in 1:boot_max){
  node2 <- node
  node.x.increase.if.node.y.increase <- node.x.increase.if.node.y.increase2
  adjust.number <- round(sum(apply(node.x.increase.if.node.y.increase,  1, function(x.33) length(which(!is.na(x.33)))))/10) # adjusting 10th of the parameters
  
  
  
  
  
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

boot.node.mean <- apply(boot.node.store, c(1,2), mean)
boot.node.upper<- apply(boot.node.store, c(1,2), quantile, probs=0.975)
boot.node.lower<- apply(boot.node.store, c(1,2), quantile, probs=0.025)
boot.node.actual <- boot.node.store[,,1]

boot.node.upper.CI<- boot.node.upper - boot.node.mean
boot.node.lower.CI <- boot.node.mean - boot.node.lower

boot.node.actual.CI.upper <- boot.node.actual + boot.node.upper.CI
boot.node.actual.CI.lower <- boot.node.actual - boot.node.lower.CI


########################################### end of Excel Version ######################################
  
########### production of graphs from time series outputs

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

  rhs<- ggplot(vals, aes(x=x, y=y)) + geom_point(size=2, shape=23) + geom_smooth(se=F, method="loess") +
    labs( x="",y = "")+theme(axis.text.y=element_blank(),axis.ticks=element_blank())  +
    scale_color_grey() + geom_hline(yintercept=0.5)

  lhs <- paste("p", graphmake, "<- rhs", sep="")
  eval(parse(text=lhs))
}



##################### Calculation of values as per Stafford et al., 2015 and Excel model

final.output <- node
final.output.CI <- node
colnames(final.output.CI)<-c('LowerCI','UpperCI','name')
for(x in 1:dim(node)[1]){
  if((0.5 - min(boot.node.actual[x,]))>(max(boot.node.actual[x,])-0.5)){#if < 0.5 - note, previously [x,3] in both cases on this line, if broken
    final.output[x,1] <- min(boot.node.actual[x,])
    final.output[x,2] <- 1-final.output[x,1]
    final.output.CI[x,1] <- min(boot.node.actual.CI.lower[x,])
    final.output.CI[x,2] <- min(boot.node.actual.CI.upper[x,])
    
  }
  
  if((0.5 - min(boot.node.actual[x,]))<(max(boot.node.actual[x,])-0.5)){ #if>0.5 - note, previously [x,3] if broken
    final.output[x,1] <- max(boot.node.actual[x,])
    final.output[x,2] <- 1-final.output[x,1]
    final.output.CI[x,1] <- max(boot.node.actual.CI.lower[x,])
    final.output.CI[x,2] <- max(boot.node.actual.CI.upper[x,])
    
  }
}


final.output <- cbind(final.output, final.output.CI[,1:2])


final.output$name <- factor(final.output$name,levels=node$name)

### convert back to -4 to 4 scale

final.output$Increase <- 8*(final.output$Increase - 0.5)
final.output$Decrease <- 8*(final.output$Decrease - 0.5)
final.output$LowerCI <- 8*(final.output$LowerCI - 0.5)
final.output$UpperCI <- 8*(final.output$UpperCI - 0.5)

p0<-ggplot(data=final.output, aes(x=name, y=Increase)) +
  geom_point(stat="identity", size=0.5) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width=0.1) + 
  xlab("") + ylab("Predicted change") +
  theme(text = element_text(size=5)) +  scale_y_continuous(limits = c(-4, 4))
  

p0<-p0 + coord_flip()

#lhs <- (paste0("p",1:number.nodes))
#lhs<-toString(lhs)
#lhs <- paste0("multiplot(",lhs,",cols=3)")


#pdf("rplot.pdf") 

#eval(parse(text=lhs))
#multiplot(p0, cols=1)

#dev.off() 

plot.keep[[policy]] <- p0

}

pdf("rplot.pdf") 

multiplot(plot.keep[[1]], plot.keep[[2]], plot.keep[[3]], plot.keep[[4]], plot.keep[[5]], plot.keep[[6]],plot.keep[[7]],plot.keep[[8]],plot.keep[[9]],plot.keep[[10]], cols=2)

dev.off()

# ## visualisation
# 
# dta <- node.x.increase.if.node.y.increase
# 
# temp.array <- (1:number.nodes)
# temp.array1 <- paste("s0",temp.array[1:9],  sep="")
# temp.array2 <- paste("s0",temp.array[10:number.nodes],  sep="")
# temp.array <- c(temp.array1, temp.array2)
# 
# strength.array <- final.output[,1] # prob of increasing
# 
# dta<- add_column(dta, node.type=strength.array, .before = 1)
# dta<- add_column(dta, node.name=node$name, .before = 1)
# dta<- add_column(dta, id=temp.array, .before = 1)
# 
# 
# 
# edges <-  data.frame(from=character(),to=character(),weight=integer(),type=integer(),stringsAsFactors=FALSE) 
# 
# nodes <- dta[,1:3]
# 
# size.data <- dim(dta)[1]
# 
# dta <- dta[,4:(size.data+3)]
# 
# edge.count<-1
# 
# for(x in 1:size.data){ 
#   for(y in 1:size.data){
#     if(is.na(dta[x,y])==F){
#       edges[edge.count,1] <- as.character(nodes$id[x])
#       edges[edge.count,2] <- as.character(nodes$id[y])
#       if(dta[x,y]>0.5){
#         if(dta[x,y]>=0.85){
#           edges[edge.count,3] <- 3
#           edges[edge.count,4] <- 1 # 1 = positive interaction, 2 = negative interaction
#         }
#         if(dta[x,y]<0.85){
#           edges[edge.count,3] <- 2
#           edges[edge.count,4] <- 1
#         }
#         if(dta[x,y]<=0.65){
#           edges[edge.count,3] <- 1
#           edges[edge.count,4] <- 1
#         }
#       }
#       if(dta[x,y]<0.5){
#         if(dta[x,y]<=0.15){
#           edges[edge.count,3] <- 3
#           edges[edge.count,4] <- 2
#         }
#         if(dta[x,y]>0.15){
#           edges[edge.count,3] <- 2
#           edges[edge.count,4] <- 2
#         }
#         if(dta[x,y]>=0.35){
#           edges[edge.count,3] <- 1
#           edges[edge.count,4] <- 2
#         }
#       }
#       edge.count <- edge.count+1
#     }
#   }
# }
# 
# 
# links <- edges
# 
# ## now remove any nodes less than threshold
# 
# up_thresh <- 0.7
# low_thresh <- 0.3
# 
# nodes<-nodes[nodes$node.type < 0.5 | nodes$node.type > up_thresh,]
# 
# nodes<-nodes[nodes$node.type > 0.5 | nodes$node.type < low_thresh,]
# 
# for(x in dim(links)[1]:1){
#   remove=0
#   if(edges$from[x] %in% nodes$id ==F){
#     remove = 1
#   }
#   if(edges$to[x] %in% nodes$id ==F){
#     remove = 1
#   }
#   if(remove==1){
#     links<-links[-x,]
#   }
# }
# 
# nodes$node.type<-round(nodes$node.type*20)
# 
# net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
# 
# # Generate colors based on node type:
# colrs <- gray.colors(20, start=1, end=0)
# 
# V(net)$color <- colrs[V(net)$node.type]
# 
# colrs2 <- c("black", "red")
# E(net)$color <- colrs2[E(net)$type]
# 
# E(net)$width <- E(net)$weight
# V(net)$label.cex <-0.7
# 
# #l <- layout_on_sphere(net)
# 
# plot(net, edge.arrow.size=0.5,vertex.label=nodes$node.name,edge.curved=.3)
# 
# 
# pdf("visualise.pdf") 
# 
# plot(net, edge.arrow.size=0.5,vertex.label=nodes$node.name,edge.curved=.3)
# 
# dev.off()
