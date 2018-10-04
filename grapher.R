

##########################
####### FINAL STAGE ########
##########################

# Turn it into igraph object
network=graph_from_data_frame(d=masterEdges, directed=T) 

# Count the number of degree for each node:
deg=degree(network, mode="all")

# Plot
# plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )
png("my_plot.png", 1000, 1000)
plot(network, vertex.color=rgb(0.1,0.7,0.8,0.5) )
dev.off()





##########################
####### FINAL STAGE ########
##########################

library(igraph)
library(lsa)

g = make_graph("Zachary")
coords = layout_with_fr(g)
# plot the graph
plot(g, layout=coords)

c1 = cluster_fast_greedy(g)

# modularity measure
modularity(c1)

B = modularity_matrix(g, membership(c1))
round(B[1,],2)

membership(c1)
length(c1)
sizes(c1)
crossing(c1, g)

plot(c1, g, layout=coords)