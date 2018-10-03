

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