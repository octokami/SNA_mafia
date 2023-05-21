#   ____________________________________________________________________________
#   SNA Project                                                 ####

# Installing packages
# remotes::install_github("SNAnalyst/DF", dependencies = TRUE)
# remotes::install_github("SNAnalyst/SNA4DSData", dependencies = TRUE, force=TRUE)
#install.packages("intergraph")
# Help with packages
#help(package = "snafun")
#help(package = "DF")
library(igraph)
library(dplyr)
# Setting margins
par(mar = c(0, 0, 0, 0))

# Loading the data
data(Montagna_mafia, package = "DF")
# More information on the network
?Montagna_mafia
# or
help(Montagna_mafia)
# or
?DF::Montagna_mafia
# Check if igraph, network or list
class(g)
# Choosing a network from the list
g <- Montagna_mafia[[1]]
g2 <- Montagna_mafia[[2]]
# Check the attributes
g

# The description of an igraph object starts with up to four letters:
# 
# -   **D** or **U**, for a directed or undirected graph
# -   **N** for a named graph (= where nodes have a name attribute)
# -   **W** for a weighted graph (= where edges have a weight attribute)
# -   **B** for a bipartite (two-mode) graph: consists of two types of vertices,
# where edges can only occur between vertices of a different type.
# 
# The two numbers that follow refer to the number of nodes and edges in
# the graph. The description also lists node & edge attributes:
# 
# `(g/c)`: graph-level character attribute<br> `(v/c)`: vertex-level
# character attribute<br> `(e/n)`: edge-level numeric attribute

# Name of the graph
snafun::extract_graph_attribute(g, "name")
# Vertex attribute
snafun::extract_vertex_attribute(g, "Relationship")
# Edge attribute
snafun::extract_edge_attribute(g, "weight")

# Vertex attributes: Role, Relationship, Request, weight
df <- data.frame(attr = V(g)$Request)
# Calculate value counts
df %>% count(attr) %>% arrange(desc(n))


## Plotting the network
plot(g, 
     vertex.color = "yellow",       # fill color of the vertices
     vertex.label.cex = .5,         # size of the labels
     vertex.size = 5, 
     edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"), # edge width proportional to the edge weight attribute
     layout = layout_with_lgl(g)) # Large Graph Layout

plot(g2, 
     vertex.color = "yellow",       # fill color of the vertices
     vertex.label.cex = .5,         # size of the labels
     vertex.size = 10, 
     edge.width = 1.5*snafun::extract_edge_attribute(g2, "weight"), # edge width proportional to the edge weight attribute
     layout = layout_nicely(g2)) 

# Removing loops and multiple edges
g <- simplify(g, remove.loops = TRUE)
g2 <- simplify(g2, remove.loops = TRUE)

# There are many isolates, these are not relevant for the analysis
g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0))
# Get the subgraph containing nodes with degree > 2 and their neighbors
#highest_degree_nodes <- which(degree(g) > 2)
#g <- induced_subgraph(g, c(highest_degree_nodes, neighbors(g, highest_degree_nodes)))
# Plot the subgraph
node_index <- V(g)$name %in% c("N0", "N1", "N2", "N16", "N17", "N56", "N57", "N73", "N74")
g <- delete_vertices(g, node_index)
plot(g, layout = layout_nicely(g), vertex.label.cex = 0.75, vertex.size = 15, edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"))
g

g2 <- igraph::delete_vertices(g2, which(igraph::degree(g2) == 0))
node_index <- V(g2)$name %in% c("N11", "N12", "N14", "N52", "N101", "N106", "N107", "N124", "N143", "N144", "N145")
g2 <- delete_vertices(g2, node_index)
plot(g2, layout = layout_nicely(g2), vertex.label.cex = .75, vertex.size = 15, edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"))
g2


# Plotting with the Fruchterman-Reingold layout algorithm
# Get the names of the top 5 nodes with the highest degree
top_names <- names(head(sort(degree(g), decreasing = TRUE), 5))
# Plot
minC <- rep(-Inf, igraph::vcount(g))
maxC <- rep(Inf, igraph::vcount(g))
minC[1] <- maxC[1] <- 0
co <- igraph::layout_with_fr(g,
                     minx = minC, maxx = maxC,
                     miny = minC, maxy = maxC)
plot(g,
     layout = co, 
     vertex.size = 25, vertex.label.cex = 1, vertex.label.dist = 0.2, vertex.label.color = "black",
     #vertex.label = c(rep("", vcount(g))), # Hide labels for visibility
     vertex.label = ifelse(V(g)$name %in% top_names, V(g)$name, NA),
     #edge.arrow.size = 0.2,
     edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"),
     xlim = range(co[, 1]), ylim = range(co[, 2]),
     rescale = FALSE)

#   ____________________________________________________________________________
#   Descriptive measures                                                     ####
# summary statistics
# mean distance or Average path length
igraph::mean_distance(g)
# diameter
igraph::diameter(g) 
# dyad census
igraph::dyad.census(g) 
# reciprocity
igraph::reciprocity(g) 
# transitivity:  Higher transitivity indicates a network where relationships tend to form closed groups or cliques, 
# indicating a higher level of cooperation or collaboration among individuals. 
# Lower transitivity suggests a more fragmented or loosely connected network.
igraph::transitivity(g, type = "global")
# Clustering coefficient
mean(transitivity(g, type = "local"))
# density: A higher density suggests a more tightly interconnected network, 
# where a significant number of individuals are linked to each other. 
# Conversely, a lower density suggests a more fragmented or decentralized network with fewer connections. 
# The density measure helps us understand the overall level of cohesion and connectivity within the mafia network.
igraph::edge_density(g)
# Average degree
mean(degree(g))
# Assortativity
#V(g)$Request <- replace(V(g)$Request, is.na(V(g)$Request), "Unknown")
#assortativity(g, V(g)$Request)
assortativity(g, degree(g))

sna::cug.test(intergraph::asNetwork(g), mode = "graph", FUN = sna::gtrans, 
              cmode = "edges", reps = 1000, 
              FUN.args = list(mode = "graph"))

sna::cug.test(intergraph::asNetwork(g),
              sna::centralization,
              FUN.arg=list(FUN = sna::betweenness), 
              mode="graph", 
              cmode="edges")

# Find common nodes
length(intersect(V(g)$name, V(g2)$name))

#   ____________________________________________________________________________
#   Centrality measures                                                     ####
##  ............................................................................
##  Degree                                                                  ####
# The *degree* of a vertex is the number of edges going in (*indegree*),
# going out (*outdegree*), or the total number of neighbors a of vertex
# (*total degree*).
# 
# Here, we have an undirected network, so there is no difference between
# *indegree* and *outdegree*.

#snafun::v_degree(g, mode = "in")
#snafun::v_degree(g, mode = "out")
sort(snafun::v_degree(g, mode = "all"), decreasing = TRUE)
sort(snafun::v_degree(g2, mode = "all"), decreasing = TRUE)


# Define a color palette based on degrees
color_palette <- colorRampPalette(c("lightblue", "red"))(max(degree(g)) + 1)
# Set the vertex color based on degrees
V(g)$color <- color_palette[degree(g) + 1]
#layout <- layout_with_fr(g, niter = 0, weights = NULL)
layout <- layout_with_lgl(g) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
#par(mar = c(0,0,0,0))
png("meetings_degree.png", width = 8000, height = 8000, res = 1000)
plot(g, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"))
dev.off()

# Phone
color_palette <- colorRampPalette(c("lightblue", "red"))(max(degree(g2)) + 1)
V(g2)$color <- color_palette[degree(g2) + 1]
layout <- layout_with_lgl(g2) #layout_nicely(g)
layout <- layout * 4
png("phone_degreee.png", width = 8000, height = 8000, res = 1000)
plot(g2, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"))
dev.off()

##  ............................................................................
##  Betweenness                                                             ####
# Centrality measures the specific advantage that a vertex has by
# occupying a specific position in the graph.
# 
# -   The *geodesic* is the shortest possible path between two specific
#     vertices.
# -   The *geodesic length* is the length of the geodesic (ie.: number of
#     edges) between two specific vertices.
# 
# The betweenness centrality of node *i* is the proportion of all shortest
# paths in the network that pass through *i*. Betweenness shows which
# nodes have information access advantage and are important to the
# network's efficiency.

sort(snafun::v_betweenness(g), decreasing = TRUE)

#edge_betweenness(g)
# if you want to use weights
#sort(snafun::v_betweenness(g, directed == FALSE), decreasing = TRUE)

# If Betweenness is zero for all vertex: 
# no node in the network acts as a bridge or intermediary between other nodes. 
# In other words, there are no shortest paths that pass through any particular node in the network.
# It is a Disconnected Network: 
# there is no path between any pair of nodes from different components. 
# In such cases, each component will have its own betweenness centrality,
# but within each component, the betweenness centrality of individual nodes will be zero.

betweenness(g)

# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(betweenness(g)) + 1)
V(g)$color <- color_palette[betweenness(g) + 1]
layout <- layout_with_lgl(g) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("meetings_betweenness.png", width = 8000, height = 8000, res = 1000)
plot(g, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"))
dev.off()

# Phone
# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(betweenness(g2)) + 1)
V(g2)$color <- color_palette[betweenness(g2) + 1]
layout <- layout_with_lgl(g2) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("phone_betweenness.png", width = 8000, height = 8000, res = 1000)
plot(g2, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"))
dev.off()


# if Betweenness is also zero for the sub_network.
# Highly Regular Network: a complete graph, where every node is directly connected to all other nodes. 
# There are multiple shortest paths between any pair of nodes,
# and therefore no node acts as a bottleneck or intermediary.
# There are alternative paths between nodes, 
# allowing information or flows to bypass individual nodes. 
# This can result in lower betweenness centrality values for all nodes, including zeros.
##  ............................................................................
##  k-path centrality                                                       ####

# *Geodesic K-path centrality* counts the number of vertices that can be
# reached from a specific vertex through a geodesic path of length less
# than *k*.
# 
# The default is *k == 3*
#   
#   In other words: how many nodes can node *i* reach within no more than
# *k* steps. The higher this number, the *i* is "close to the fire."
sort(snafun::v_geokpath(g, k = 3), decreasing = TRUE)
sort(snafun::v_geokpath(g2, k = 3), decreasing = TRUE)

# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(snafun::v_geokpath(g, k = 2)) + 1)
V(g)$color <- color_palette[snafun::v_geokpath(g, k = 2) + 1]
layout <- layout_with_lgl(g) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("meetings_k2.png", width = 8000, height = 8000, res = 1000)
plot(g, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"))
dev.off()

# Phone
# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(snafun::v_geokpath(g2, k = 2)) + 1)
V(g2)$color <- color_palette[snafun::v_geokpath(g2, k = 2) + 1]
layout <- layout_with_lgl(g2) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("phone_k2.png", width = 8000, height = 8000, res = 1000)
plot(g2, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"))
dev.off()

# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(snafun::v_geokpath(g, k = 3)) + 1)
V(g)$color <- color_palette[snafun::v_geokpath(g, k = 3) + 1]
layout <- layout_with_lgl(g) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("meetings_k.png", width = 8000, height = 8000, res = 1000)
plot(g, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"))
dev.off()

# Phone
# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(snafun::v_geokpath(g2, k = 3)) + 1)
V(g2)$color <- color_palette[snafun::v_geokpath(g2, k = 3) + 1]
layout <- layout_with_lgl(g2) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("phone_k.png", width = 8000, height = 8000, res = 1000)
plot(g2, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"))
dev.off()

##  ............................................................................
##  Closeness                                                               ####

# Closeness measures how much effort it takes a node to reach **all other
# nodes** in the network. It is calculated as the inverse of the sum of
# the geodesic lengths from that node to all others.
# 
# You can again decide to only consider incoming, outgoing, or all ties.
# The default is to use outgoing ties (ie: you only follow the direction
#               of the edges--this makes no difference for an undirected graph).
# # (note that this measure is not obviously defined when there are isolates
#  in the graph, because there is no obvious distance to them--here they
#  are assigned distance *n*, with *n* being the number of vertices in the
#  graph)
sort(snafun::v_closeness(g), decreasing = TRUE)
sort(snafun::v_closeness(g2), decreasing = TRUE)

color_palette <- colorRampPalette(c("lightblue", "red"))(max(snafun::v_closeness(g)) + 1)
V(g)$color <- color_palette[snafun::v_closeness(g) + 1]
layout <- layout_with_lgl(g) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("meetings_closeness.png", width = 8000, height = 8000, res = 1000)
plot(g, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"))
dev.off()

# Phone
# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(snafun::v_closeness(g2)) + 1)
V(g2)$color <- color_palette[snafun::v_closeness(g2) + 1]
layout <- layout_with_lgl(g2) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("phone_closeness.png", width = 8000, height = 8000, res = 1000)
plot(g2, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"))
dev.off()


##  ............................................................................
##  Bottleneck centrality                                                   ####

# Vertex *j* is a bottleneck for vertex *i* when *j* is on at least 1/4 of
# the shortest paths from *i* to everybody else. Bottleneck centrality for
# *j* is the number of vertices that *j* is a bottleck for.
# 
# In other words, it measures how many actors rely on *j* quite a lot in
# order to get their information to others in the network.
# 
# This also shows possible points in the network where the "information
# chain" can be broken (if *j* becomes is, gets apprehended, or dies) or
# which actors tend to be well-informed regarding the activities of
# others.
# 
# (note: the measure does **not** take edge weight into consideration)
#   
# NOTE: Often, especially when densely connected subgroups exist (such as
# in this network), multiple shortest paths are possible between two
# vertices. If a vertex appear on at least one of them, that path counts
# as part of the calculation. This implies that being a *bottleneck* does
# not take into account whether there are alternative geodesics between
# the two vertices; in other words, it is not determined whether a path is
# redundant or not. Hence, removal of the bottleneck node may not affect
# the efficiency of the graph in these cases.

#DF::v_bottleneck(g)   # regardless of edge direction
# take the direction of edges into account and "go with the flow"
# doesn't matter here, of course, since the graph is undirected
#DF::v_bottleneck(g, mode = c("all", "out", "in"), vids = igraph::V(g), n = 4)

bottleneck_score <- DF::v_bottleneck(g)
bottleneck_score[order(bottleneck_score$bottleneck, decreasing = TRUE),]

bottleneck_score <- DF::v_bottleneck(g2)
bottleneck_score[order(bottleneck_score$bottleneck, decreasing = TRUE),]

color_palette <- colorRampPalette(c("lightblue", "red"))(max(DF::v_bottleneck(g)) + 1)
V(g)$color <- color_palette[DF::v_bottleneck(g) + 1]
layout <- layout_with_lgl(g) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("meetings_bottleneck.png", width = 8000, height = 8000, res = 1000)
plot(g, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g, "weight"))
dev.off()

# Phone
# Define a color palette
color_palette <- colorRampPalette(c("lightblue", "red"))(max(DF::v_bottleneck(g2)) + 1)
V(g2)$color <- color_palette[DF::v_bottleneck(g2) + 1]
layout <- layout_with_lgl(g2) #layout_nicely(g)
layout <- layout * 4
# Save the plot with increased quality
png("phone_bottleneck.png", width = 8000, height = 8000, res = 1000)
plot(g2, layout = layout, vertex.label.cex = 0.75, vertex.size = 15, vertex.label.color = "black", edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"))
dev.off()
# cleanup
rm(bottleneck_score)

#   ____________________________________________________________________________
#   Subgroups                                                               ####

# Here, we are interested in learning whether the network we are analyzing
# is just a giant "blob," or whether it falls apart into subgroups (that
# may or may not be connected between each other).
# 
# When subgroups exist, this means that information doesn't spread
# easily/equally across the network, but tends to be contained more within
# pockets of the network. This is quite common in terrorist networks,
# where most members are only aware of a small part of the operation and
# have no (or few) links to the other groups in the network. This is
# useful to mitigate the risk that the entire network is exposed when a
# member gets detected by Law Enforcement.

##  ............................................................................
##  Fast greedy algorithm                                                   ####
# 
# There are many ways to determine subgroups in a graph, we will focus on
# one method that is quite flexible and relatively efficient for large
# networks and that works for both directed and undirected networks.
#snafun::extract_comm_fastgreedy(g)
snafun::extract_comm_walktrap(g)

##  ............................................................................
##  Graph modularity                                                        ####

# Graphs with a high modularity score will have many connections within a subgroup
# but only few pointing outwards to other subgroups. 
# More formally, modularity is the fraction of the edges that fall within the 
# given groups minus the expected fraction if edges were distributed at random. 
# The value of the modularity for unweighted and undirected graphs lies in the range [-1/2,1]. 
# 
# It is positive if the number of edges within groups exceeds the number expected 
# on the basis of chance. 
# 
# As a rule of thumb, a value greater than 0.30 implies that there is significant 
# community structure.
# 
# Value close to 1 indicates strong community structure. 
# When modularity = 0, the community division is not better than random.
# 
# Modularity can be used in two ways:
# 1. to quantify the level of segmentation into sub-group of a specific graph
# 2. to quantify how strong the subgroups are that were found by a specific 
# subgroup algorithm (or subgroups defined by the researcher herself).
# 
# Let's see if the subgroups that were found by the algorithm are meaningful:
g_subgroups <- snafun::extract_comm_fastgreedy(g)
# Alternativerly
igraph::communities(g_subgroups)
igraph::modularity(g_subgroups)
# there is significant community structure.
# check out the subgrouping visually
png("meetings_subgroup.png", width = 8000, height = 8000, res = 1000)
plot(g_subgroups, g,
     vertex.label = c(rep("", vcount(g))),
     vertex.size = 3, vertex.label.cex = .5, vertex.label.dist = 0,
     #main = "Subgroups"
     )
dev.off()


# Phone
g_subgroups <- snafun::extract_comm_fastgreedy(g2)
# Alternativerly
igraph::communities(g_subgroups)
igraph::modularity(g_subgroups)
# there is significant community structure.
# check out the subgrouping visually
png("phone_subgroup.png", width = 8000, height = 8000, res = 1000)
plot(g_subgroups, g2,
     vertex.label = c(rep("", vcount(g))),
     vertex.size = 3, vertex.label.cex = .5, vertex.label.dist = 0,
     edge.width = 0.75*snafun::extract_edge_attribute(g2, "weight"),
     #main = "Subgroups"
)
dev.off()

#   ____________________________________________________________________________
#   EFFICIENCY OF A NETWORK                                                 ####

# The efficiency of a network is defined as the extent to which the network has 
# just enough edges to keep the components connected. 
# Any additional edge between components makes the network less efficient.
# 
# Efficiency runs from 0 to 1 (= perfect efficiency)

# It is useful to see how quickly a network can circulate important information
# to all of its members. Highly efficient networks also have a higher capacity 
# to remain "secretive" as there are no redundant connections between people 
# (so criminals who are arrested will know less of the overall operation and will 
# know fewer others directly and will expose fewer others when tapped/surveyed).
##  ............................................................................
modularity(g_subgroups)

# Krackhardt efficiency: efficiency of 1 has precisely as many edges as are needed to connect
# its components; as additional edges are added, efficiency gradually falls towards 
# 0: completely inefficient graph or the network is disconnected.
DF::g_efficiency(intergraph::asNetwork(g), diag = FALSE)
DF::g_efficiency(intergraph::asNetwork(g2), diag = FALSE)

#   ____________________________________________________________________________
#   NETWORK VULNERABILITY                                                   ####

# Network vulnerability refers to the extent to which a network can continue to
# operate, when vertices or edges fail/vanish.

# This requires a definition of "operate" and there are several. 
# The most common ones for data forensics purposes are:
# * the reachability of vertices from other vertices
# * the average geodesic length between vertices
##  ............................................................................
## Reachability of vertices from other vertices                            ####

#' When a vertex is removed from the network, some other vertices 
#' might not be able to reach each other anymore. 
#' This algorithm calculates the number of pairs of vertices 
#' that can no longer reach each other without vertex i
#' in the network, but that were able to reach each other 
#' with i in the network. 
#' 
#' More formally, we compare the number of pairs of vertices that 
#' do not have some connecting path before and after removing vertex i.
#' The i-th value is the number of pairs of vertices 
#' that become disconnected when vertex i is removed 
#' from the network (leaving everything else the same).
DF::vuln_paths(g, mode = c("all", "out", "in"), weight = snafun::extract_edge_attribute(g, "weight"), digits = 3)

# the vertices with positive vulnerability colored blue
# vertices sized according to their vuln. proportion
# vuln_prop is the ratio of the number of disconnected pairs after removing vertex
# i to that before removing i (correcting for the paths to and from i).
#vulnerability_prop <- DF::vuln_paths(g)$vuln_prop
#vulnerable <- DF::vuln_paths(g)$vulnerability > 0
# col <- DF::vuln_paths(g)$vulnerability == 0

##  ............................................................................
##  Average path length                                                     ####

#' When a vertex is removed from the network, some other vertices 
#' might not be able to reach each other as efficiently 
#' as before anymore. For example, when A-B-C, the removal of 
#' vertex B requires A and C to connect through other vertices 
#' than through B, which might require more steps.
#' 
#' The "efficiency" of a graph can be defined in many ways, 
#' here we adopt two of the common definitions: the sum 
#' of the geodesic lengths between each pair of vertices or 
#' the harmonic mean of these lengths. 

#' Note that this definition of 'efficiency' is not the same as above.
#' Higher vulnerability would increase the mean path length if removed.
#DF::vuln_efficiency(g, method = c("harmonic", "sum"), mode = c("all", "out", "in"),
#  weight = NA, disconnected = c("size", "max", "infinite"), digits = 3)

# top 20 of vulnerability-causing vertices
vulnerable <- DF::vuln_paths(g)$vulnerability > 0
eff_g <- DF::vuln_efficiency(g)
vulnerability_prop <- eff_g$vuln_prop
eff_g <- eff_g[order(eff_g$vulnerability, decreasing = TRUE), ]
# Plot
png("meetings_vulnerable.png", width = 8000, height = 8000, res = 1000)
plot(g,
     vertex.label = ifelse(V(g)$name %in% eff_g[1:11,]$name, V(g)$name, NA),
     vertex.label.color = "black",
     vertex.color = vulnerable + 1,
     vertex.size = 150 * (vulnerability_prop - 1) + 5,
     layout = layout_nicely(g),
     main = "Vulnerable vertices in blue")
dev.off()
eff_g



# Phone
vulnerable <- DF::vuln_paths(g2)$vulnerability > 0
eff_g <- DF::vuln_efficiency(g2)
vulnerability_prop <- eff_g$vuln_prop
eff_g <- eff_g[order(eff_g$vulnerability, decreasing = TRUE), ]
# Plot
png("phone_vulnerable.png", width = 8000, height = 8000, res = 1000)
plot(g2,
     vertex.label = ifelse(V(g2)$name %in% eff_g[1:13,]$name, V(g2)$name, NA),
     vertex.label.color = "black",
     vertex.color = vulnerable + 1,
     vertex.size = 25 * (vulnerability_prop - 1) + 5,
     layout = layout_nicely(g2),
     main = "Vulnerable vertices in blue")
dev.off()
eff_g

#   ____________________________________________________________________________
#   LET'S ATTACK THE NETWORK AT A GRANDER SCALE!                           ####

#' This approach drops vertices until the network has become empty. 
#' 
#' The network performance measure here is the total number of vertices 
#' that can be reached by all of the vertices in the network.

#' This algorithm is useful to show which (groups of vertices) are most critical 
#' to the functioning of the graph and which attack approach yields the best results.

#' Scenario 1: vertices are removed based on their betweenness score in the 
#' original graph. 
#' First the vertex is removed with the highest betweenness, then the one with 
#' the second highest betweenness, etc.
#' 
#' Scenario 2: vertices are removed based on their degree in the original graph. 
#' First the vertex is removed with the highest degree, then the one with 
#' the second highest degree, etc.
#' 
#' Scenario 3: First the vertex is removed with the highest betweenness. 
#' Then, betweenness scores are recalculated for the new, smaller graph.
#' Then, the vertex with the highest betweenness in this new graph is removed.
#' Then, betweenness scores are recalculated for the new, smaller graph and 
#' the vertex with the highest betweenness in this new graph is removed. Etc.
#' This approach is often used in computer attacks.
#' 
#' Scenario 4: vertices are removed at random. 
#' This is done k times and the average effect is determined of 
#' removing 1 random vertex, 2 random vertices, etc. 
#' This is useful to check how vulnerable a network is against a random attack 
#' or random drop-out. 

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..

# which (groups of vertices) are most critical to the functioning of the graph and
# which attack approach yields the best results.
# % of the reachable paths that remain after the removal of a vertex
DF::vuln_attack(g, k = 10, digits = 3) #, weight = snafun::extract_edge_attribute(g, "weight"))
DF::vuln_attack(g2, k = 10, digits = 3) #, weight = snafun::extract_edge_attribute(g, "weight"))
# Degree-based seems to be the best attack approach


#   ____________________________________________________________________________
#   WHO IS GOING TO FILL THE VOID?                                          ####

# When a node is removed from the network, the network will suffer from degraded 
# performance. However, when someone else takes over de position of the removed 
# node, then the network can operate well again.
# This is often seen in terrorist networks, where removing the leader has only 
# temporary effect. This reflects the resilience of a social network. 
# 
# As has been found in research, this resilience of terrorist networks is often due
# to whether or not there is a natural successor to the removed leader. 
# The most natural successor is a node that is highly structurally equivalent 
# to the removed node. 
# Two nodes are exactly structurally equivalent if they have the exact same 
# ties to and from others (except the two actors themselves).

# For the meetings network: N68
se_g <- snafun::d_structural_equivalence(g)
sort(se_g[, "N18"], decreasing = TRUE)

# For the meetings network: N68
se_g <- snafun::d_structural_equivalence(g2)
sort(se_g[, "N18"], decreasing = TRUE)

# Removing a group
main_guys <- sort(snafun::v_betweenness(g), decreasing = TRUE)[1:7] |> names()
print(main_guys)
snafun::d_structural_equivalence(g)[main_guys, ]

# an easier way to see what is going on
# The central guys are in the rows
# In the columns are those who might follow them up
se_g_top <- snafun::d_structural_equivalence(g)
diag(se_g_top) <- 0
se_g_top <- se_g_top[main_guys, ]
se_g_top <- se_g_top[, apply(se_g_top, 2, function(x) any(x > .7, na.rm = TRUE))]
# remove any values lower than .7 from the printout
se_g_top[(se_g_top <= .7) | is.na(se_g_top)] <- ""
print(as.data.frame(se_g_top))

#   ____________________________________________________________________________
#   SECRECY                                                                 ####

#' The secrecy of a graph is defined as the fraction of the network that 
#' remains unexposed if a single member of the network is detected. 
#' Hence, the score runs between 0 (= everybody gets exposed as soon as 1 
#' person is exposed) to 1 (= nobody gets exposed when 1 person is exposed).

#' Three scenario's are usually considered:
#' S1 assumes that every actor has the same probability of being 
#' detected under a surveillance (with prob. = 1/number_of_vertices) and 
#' all of the actor's connections are exposed.

#' S2 assumes that every actor has the same probability being detected. 
#' The actor then snitches on each connected with probability p (to be 
#' set by the researcher).
#' 
#' S3 assumes that individuals with a more central position are more likely 
#' to be discovered. This is captured by the equilibrium distribution of a random 
#' walk on the graph. 
#' As in S1, once an individual has been captured, all of the actor's connections are 
#' exposed as well.


##  ............................................................................
##  Graph secrecy                                                           ####
# Secrecy: the fraction of the network that remains unexposed if a single member of the network is detected. 
# Hence, the score runs between 0 (= everybody gets exposed as soon as 1 person is exposed) to 
# 1 (= nobody gets exposed when 1 person is exposed).
DF::g_secrecy(g, type = 0, p = 0.25, digits = 3)
# Assumption: individuals that are more central position in the network, are more likely to be discovered
# More likely in a more mature investigation than in the initial phase
DF::g_secrecy(g, type = 3, digits = 3)
DF::g_secrecy(g2, type = 3, digits = 3)

##  ............................................................................
##  Individual secrecy                                                      ####

#' The function secrecy_vertex returns the secrecy score per vertex, 
#' this is the fraction of vertices in the graph that are to remain 
#' unexposed under a surveillance when that vertex is exposed multiplied by 
#' the probability of that vertex being exposed to begin with. 
#' In other words: if 1 vertex in the graph is exposed, what is the 
#' fraction of vertices that is likely to remain unexposed, due to vertex i? 
#' This is indeed determined by the number of neighbors of i and the risk of 
#' i being the one to be exposed.

# This approach is useful for small-ish graphs, but often not very informative 
# for larger ones, since each individual then will often only expose a very limited 
# fraction of the network. However, it can be used to search for a combination of 
# persons to arrest.
# For leaders in the network, this measure can be used to see where the "weak"/"risky"
# points of the network are (in terms of exposure risk) and the network can then 
# be (re-)organized to improve its secrecy capacity.

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..

sort(DF::v_secrecy(g, type = 1), decreasing = FALSE)

# as a histogram
hist(DF::v_secrecy(g, type = 1), 
     main = "Network secrecy",
     xlab = "Secrecy of each vertex")