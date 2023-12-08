library (readr)
library(igraph)
library (readr)
library (haven)
library (ggplot2)
library(readxl)
dataset_DBLP <- read_excel("E:\\UCINET PROJECTS\\final_ungraph_-_Copy_2.xlsx")
dataset_DBLP


#2. Manage dataset
#M_DATA<-as.data.frame(table(dataset_DBLP)) # Create an edge weight column named "Freq"
#M2_DATA<-subset(M_DATA,Freq>0) # Delete all the edges having weight equal to 0


dblp<-graph_from_data_frame(dataset_DBLP, directed = FALSE)
#E(dblp)$weight<-E(dblp)$Freq # Assigning edge attribute to each edge
dblp

dblp
#1. igraph summary
#number of edge
gsize(dblp)
#nunber of vertix
gorder(dblp)

#2. Nodelist
V(dblp)

#3. Edgelist
E(dblp)
#Adjacency List
adj_list <- as_adj_list(g)

#5. Adjacency matrix
dblp[c(1:18162),c(1:18162)]

#1. Degree centrality
dblp_deg<-degree(dblp,mode=c("All"))
V(dblp)$degree<-dblp_deg
max(V(dblp)$degree)
which.max(V(dblp)$degree)
freq=table(V(dblp)$degree[1:20])
normalized_freq=freq/sum(freq)
barplot(freq)

barplot(normalized_freq)



dblp_deg <- degree(dblp, mode = "all")
V(dblp)$degree <- dblp_deg

max_degrees <- max(dblp_deg, 2)  # Get the two maximum degree values
indices <- which(dblp_deg %in% max_degrees)  # Find the indices of vertices with maximum degrees

max_degrees_values <- dblp_deg[indices]  # Retrieve the values of the maximum degrees

result <- data.frame(Vertex = indices, Degree = max_degrees_values)
print(result)
x=(18161*18160)/2




#2. Eigenvector centrality
dblp_eig <- evcent(dblp)$vector
dblp_eig
V(dblp)$Eigen<-dblp_eig
which.max(dblp_eig)


#3. Betweenness centrality
dblp_bw<-betweenness(dblp, directed = FALSE,normalized = TRUE)
V(dblp)$betweenness<-dblp_bw
max(V(dblp)$betweenness)
which.max(dblp_bw)

DF1<-as_long_data_frame(dblp)
DF1

#Create a graph object from the dataset
graph <- graph_from_data_frame(dataset_DBLP, directed = FALSE)

#Calculate the average shortest path
avg_shortest_path <- average.path.length(graph)

#Print the result
print(avg_shortest_path)


all_shortest_paths(graph,from=1)

install.packages("DirectedClustering")
library(DirectedClustering)

paths <- paths(graph)

for (i in 1:length(3)) {
  print(shortest_paths[[i]])
  print("-----------------------------------")
}








#closeness_centrality
closeness_centrality <- closeness(graph, mode = "all", normalized = TRUE)
min(closeness_centrality)

#Clustering coefficient
cluster_coeffs <- transitivity(graph,type='local')
global_cluster_coeff <- mean(cluster_coeffs,na.rm = TRUE)
global_cluster_coeff
cluster_coeffs

max(cluster_coeffs,na.rm=TRUE)
min(cluster_coeffs,na.rm=TRUE)



#connectivity
# Check connectivity
connectivity <- edge_connectivity(graph)
connectivity
# Check if connected
if (connectivity > 0) {
  print("Graph is connected")
} else {
  print("Graph is not connected")
}

components <- clusters(graph)
sum(components$csize)
min(components$csize)
max(components$csize)
plot(graph, vertex.color = components$membership, vertex.label = V(graph)$name)

sum(components$csize==2)
#BFS Travers
bfs=bfs(graph,root=1,mode='all')
(bfs$order)

start_vertex <- 1
#Perform BFS
bfs_result <- bfs(graph, root = start_vertex, order = TRUE, unreachable = FALSE)
#Get the order in which vertices were reached
order_reached <- bfs_result$order
order_reached
#Print the order
print(length(c(order_reached)))




update.packages("igraph")
#Calculate Diameter
diameter(graph)

# Create the graph object
dblp_graph <- graph_from_data_frame(dataset_DBLP, directed = FALSE)

# Set the centrality measures as node attributes
dblp_deg <- degree(dblp_graph, mode = "all")
V(dblp_graph)$degree <- dblp_deg

dblp_eig <- evcent(dblp_graph)$vector
V(dblp_graph)$eigenvector <- dblp_eig

dblp_bw <- betweenness(dblp_graph, directed = FALSE)
V(dblp_graph)$betweenness <- dblp_bw

# Plotting the network with degree centrality
plot_degree <- function(graph) {
  plot(graph, vertex.size = V(graph)$degree * 0.5, vertex.label = NA,
        main = "Degree Centrality")
}
plot_degree
# Plotting the network with eigenvector centrality
plot_eigenvector <- function(graph) {
  plot(graph, vertex.size = V(graph)$eigenvector * 1000, vertex.label = NA,
        main = "Eigenvector Centrality")
}

# Plotting the network with betweenness centrality
plot_betweenness <- function(graph) {
  plot(graph, vertex.size = V(graph)$betweenness * 10, vertex.label = NA,
 main = "Betweenness Centrality")
}

# Call the plotting functions
plot_degree(dblp_graph)
plot_eigenvector(dblp_graph)
plot_betweenness(dblp_graph)

# Create a graph from the undirected data set
#graph1 <- graph_from_data_frame(g_undirect, directed = FALSE, vertices = NULL)
plot(dblp_graph)



lc <- cluster_louvain(graph) # Create a cluster based on the Louvain method
communities(lc) # You can check which vertices belongs to which clusters.
c=communities(lc)
sizes(lc)
#2. Plotting the Betweenness Centrality network with the community detection

set.seed(1001) # To duplicate the computer process and create exactly the same network repetitively you should set the seed.
plot(lc, graph, edge.color = 'black',vertex.label.cex =0.5,
layout = layout.fruchterman.reingold)


