library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(data.table)
library(igraph)
library(visNetwork)
library(htmlwidgets)

#setwd("C:/Users/Raza/OneDrive - University of Toronto/UK Plasma Systematic Review/Nodes Visualization/NodeVisualizer")

#https://kateto.net/networks-r-igraph

#import dataframe 
list.files()
nodes <- "nodes.csv" 
fullpath <- file.path(getwd(), nodes) #creates the full path for you depending on OS
file.exists(nodes)
st <- read_csv(fullpath)
st_df <- as.data.frame(st)


st_indication <- st_df$Indication
st_category <- st_df$Category
stforks <- length(unique(st_category))
st_study <- st_df$Study
#here i am creating some experimental vectors


nodes3 <- read.csv("nodes2.csv", header=T, as.is=T)
nodes3$weight <- 1


links3 <- read.csv("edges2.csv", header=T, as.is=T)
links3 <- links3 %>% group_by(from) %>% mutate(fromCount=n()) #here I am adding 'weights' to each category based on number of links to the links df

nodes3$weight<-links3$fromCount[match(unlist(nodes3$media),links3$from)]
nodes3 <- replace(nodes3,is.na(nodes3),1) #transferrign weights from links df to nodes df

net2 <- graph_from_data_frame(d=links3, vertices=nodes3, directed=F) #creating graph 


V(net2)$color<-nodes3$X #adding color to graph
V(net2)$weight<-nodes3$weight #adding weights to graph

edge.start <- ends(net2, es=E(net2), names=F)[,1]
edge.col <- V(net2)$color[edge.start] #set link color to source 

plot(net2, edge.arrow.size=.2, edge.color=edge.col, edge.curved=.1, vertex.size=(V(net2)$weight)*2,vertex.label.cex	=(V(net2)$weight)*1.1*(0.8^((V(net2)$weight)/1.3)), vertex.label.dist=(V(net2)$weight)*0.1, vertex.color=V(net2)$color, asp=0.35) #displaying graph weighed for several things, and aspect ratio of 0.35


#sample code for outcomes

outcomes <- read.csv("outcomes.csv", header=T, as.is=T)
final_df<-as.data.frame(outcomes)

count =0

final_li <- list()

#loop through dataframe above to create new list with outcomes and corresponding papers
for (i in 3:ncol(final_df)) {

  temp_li <- list()

  temp_li <- append(temp_li, colnames(final_df)[i])#appendoutcome
  
  #print(final_df[,i])

  for (j in 1:length(final_df[,i])) {

    
    #print(final_df[j,i])
    
    if (final_df[j,i] ==1) {
      
      
      #print(final_df[j,2])
      
      temp_li <- append(temp_li, final_df[j,2])
      

    }
  }

  final_li <- append(final_li,list(temp_li))
  
}

print(final_li)


#sample code for outcomes with OUTCOMES FIRST 




#create a new df with from->to relationships from outcomes , 

out_df<-data.frame(from = character(), to = character()) #df for from->to relationships
count=1


for (i in 1:length(final_li)) {
  
 
  for (j in 2:length(final_li[[i]])) { #here starting with 2 makes it ignore the name of the clinical outcome itself
    
    out_df[count,] <- c(final_li[[i]][1],final_li[[i]][j]) #creating a from->to list and appending to a dataframe
    
    print(final_li[[i]][1])
    print(final_li[[i]][j])
    
    count = count+1
    
  }
  
}
  
out_df <- out_df %>% group_by(from) %>% mutate(fromCount=n()) #here I am adding 'weights' to outcome based on # of studies

linksx <- out_df


#########network generation##############

linksx <- read.csv("new_links4.csv", header=T, as.is=T) #copying code from above

nodesx <- read.csv("new_nodes3.csv", header=T, as.is=T) #copying code from above

nodesx$weight<-linksx$fromCount[match(unlist(nodesx$media),linksx$from)]
nodesx <- replace(nodesx,is.na(nodesx),1) #transferrign weights from links df to nodes df

net3 <- graph_from_data_frame(d=linksx, vertices=nodesx, directed=F) #creating graph 
V(net3)$category<-nodesx$category #adding color to graph

V(net3)$color<-nodesx$color #adding color to graph
#V(net3)$weight<-nodesx$weight #adding weights to graph
V(net3)$weight<-nodesx$nodeweight #adding weights to graph

V(net3)$font<-nodesx$font #adding font style (bold/plain) to graph
V(net3)$fontsize<-nodesx$fontsize #adding font size to graph
V(net3)$fontcolor<-nodesx$fontcolor #adding font size to graph
V(net3)$edgewidth<-nodesx$edgewidth #adding font size to graph
V(net3)$edgecolor<-nodesx$edgecolor #adding font size to graph

#V(net3)$edgewidth<-linksx$edgewidth #adding color to graph

#V(net3)$name <- ifelse ((V(net3)$weight > 10),paste0(V(net3)$name, "=", V(net3)$weight),(V(net3)$name)) #add sample weights to the name


edge.start <- ends(net3, es=E(net3), names=F)[,1] #what's happenign is that you are using 'ends' to get the edges of this network,which gives you a 2 column list of pairs of numbers for start and end of the edges. you then pick [,2] which represents the end (you could've picked the start which is [,1]), which you can then use to feed into edge.start for later use in the next
edge.col <- V(net3)$color[edge.start] #set link color to source (edge.start) or target (edge.target)

edge.end <- ends(net3, es=E(net3), names=F)[,2] #what's happenign is that you are using 'ends' to get the edges of this network,which gives you a 2 column list of pairs of numbers for start and end of the edges. you then pick [,2] which represents the end (you could've picked the start), which you can then use to feed into edge.start for later use in the next

edge.width <- V(net3)$edgewidth[edge.end] #set link color to source (edge.start) or target (edge.target)


#tail_of(fg, edge.of.interest)$value

#edge.start <- ends(net3, es=E(net3), names=F)[,1]
#edge.col <- V(net3)$color[edge.target] #set link color to source (edge.start) or target (edge.target)

normalize_01 <- function(x) (x - min(x)) / (max(x) - min(x)) + 0.25


#####################

pdf("plot50.pdf",20,16)

#pdf("plot34.pdf",10,7)

fr <- layout_with_fr(net3, start.temp=100, niter = 1000 )


plot(net3, edge.arrow.size=.2,edge.curved=.1,edge.color=edge.col,vertex.label = V(net3)$medialabel , vertex.size=V(net3)$weight/9,vertex.label.cex	=(V(net3)$fontsize/10)+0.4, vertex.label.dist=1, vertex.label.degree = -pi/2, vertex.color=V(net3)$color, vertex.frame.color =V(net3)$color,  asp=0.7, layout=fr, vertex.label.font=V(net3)$font, vertex.label.family="sans",  vertex.label.color=c(V(net3)$fontcolor), vertex.shape="circle", edge.width=edge.width, edge.curved=1) #displaying graph weighed for several things, and aspect ratio of 0.35



#this one is for studies included
#plot(net3, edge.arrow.size=.2,edge.curved=.1,edge.color=edge.col,vertex.size=((2+((V(net3)$weight)/3))/2),vertex.label.cex	=V(net3)$fontsize/5, vertex.label.dist=0, vertex.color=V(net3)$color, vertex.frame.color =V(net3)$color,  asp=0, layout=layout.fruchterman.reingold, vertex.label.font=normalize_01(c(V(net3)$font)), vertex.label.family="sans",  vertex.label.color=c(V(net3)$fontcolor), vertex.shape="circle", edge.width=edge.width, edge.curved=1) #displaying graph weighed for several things, and aspect ratio of 0.35

#saveWidget(visIgraph(g), file = "test.html")


dev.off()


#############

net3plot <- toVisNetworkData(net3) # convert the graph (or use visIgraph)


net3vis <- visNetwork(nodes = net3plot$nodes ,
                          color = net3plot$nodes$color,
                          edges = net3plot$edges,
                          main = "Plasma Systematic Review",
                          submain = "Interactive Figure",
                          footer = "") 


net3vis <- net3vis %>%
  visIgraphLayout(layout = "layout_with_kk", # or use igraph's `layout_*`s in quotes
                  # layout = "layout.norm",  # using saved coords? set this!
                  # layoutMatrix = coords,   # our previous coords
                  smooth = FALSE,            # set to F when bogged by bigger graphs
                  physics = TRUE             # set to F when bogged by bigger graphs
  ) 


net3vis
###############

#saving default
#plot(net3, edge.arrow.size=.2,edge.curved=.1,edge.color=edge.col,vertex.size=((2+((V(net3)$weight)/3))/2),vertex.label.cex	=0.5+(555^(-1/(V(net3)$weight))), vertex.label.dist=0.5, vertex.color=V(net3)$color, asp=0.4) #displaying graph weighed for several things, and aspect ratio of 0.35


# 
# colrs<-c("red", "blue","gold","yellow","orange","brown","violet","green","purple", "lightblue", "darkred", "darkgreen")
# V(net2)$color <- colrs[V(net2)$category]
# V(net2)$label.color <- "black"
#   


#tr <- make_tree(length(st_indication), children = 20, mode = "undirected")
#plot(tr, vertex.size=10, vertex.label=st_study, vertex.label.cex=0.8,vertex.label.color="black") 

#links3 <- links3 %>% group_by(from) %>% mutate(fromCount=n())

#> which(links3$from ==nodes3$media[39])[1]
#links3 <- links3 %>% group_by(from) %>% mutate(fromCount=n())










