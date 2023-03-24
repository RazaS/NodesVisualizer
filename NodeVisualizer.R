library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(data.table)
library(igraph)

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


33333333333333333333333333333333333333333333333
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

linksx <- read.csv("linksx2.csv", header=T, as.is=T) #copying code from above

nodesx <- read.csv("outcomes_nodesx2.csv", header=T, as.is=T) #copying code from above
nodesx$weight <- 1 #assigning default weight

nodesx$weight<-linksx$fromCount[match(unlist(nodesx$media),linksx$from)]
nodesx <- replace(nodesx,is.na(nodesx),1) #transferrign weights from links df to nodes df

net3 <- graph_from_data_frame(d=linksx, vertices=nodesx, directed=F) #creating graph 

V(net3)$color<-nodesx$color #adding color to graph
V(net3)$weight<-nodesx$weight #adding weights to graph

edge.start <- ends(net3, es=E(net3), names=F)[,2] #what's happenign is that you are using 'ends' to get the edges of this network,which gives you a 2 column list of pairs of numbers for start and end of the edges. you then pick [,2] which represents the end (you could've picked the start), which you can then use to feed into edge.start for later use in the next line
edge.col <- V(net3)$color[edge.start] #set link color to source (edge.start) or target (edge.target)

#tail_of(fg, edge.of.interest)$value

#edge.start <- ends(net3, es=E(net3), names=F)[,1]
#edge.col <- V(net3)$color[edge.target] #set link color to source (edge.start) or target (edge.target)

plot(net3, edge.arrow.size=.2,edge.curved=.1,edge.color=edge.col,vertex.size=2+((V(net3)$weight)/3),vertex.label.cex	=0.3+(2^(-1/(V(net3)$weight))), vertex.label.dist=1, vertex.color=V(net3)$color, asp=0.5) #displaying graph weighed for several things, and aspect ratio of 0.35



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
