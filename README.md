#Co-authorship Network Analysis  in R

(more details on https://amirhosblog.wordpress.com/2016/09/29/co-authorship-analysis-in-r/)

Nowadays in every field, people collobrate with each other. There are alot of intersing collobation network analysis projects out there like rappers, co-stardom analysis, and scientific collaboration.

The colloration network is formed as indvidulas with different skills/knowledge/professions interact with each other to facilitate fulfillment of tasks to a mutally shared goal. Collboration provides opportunity to all the parties involved to discover new knowledge/experinece/skills and consequenty combine them to address a complex challenge.

Since for I already have a few thousands journal articles on ethics of Synthetic Biology written from 2004 to 2014, I decided to write this walkthrough on how to do the co-authorship analysis. Scientific collaborative networks are a hallmark of contemporary academic research. Scientists are no longer independent players, but members of scientific cooperation networks looking for solutions to social, political, economic and technological problems, which, usually, require multidisciplinary approaches (Sonnenwald, D). Scientific collaboration can also help broaden the scope of a research project and foster innovation as it provides access to different disciplines.

Co-authorship analysis enables us to understand and disvocer scientific collaboration patterns. It can be used to asses the extend of collaboration within a research area, evaluate the relation between authors and their significance in a given network, contirbution of insitutions or larger organisaitons, formation of research groups, and so on.

Regardless of what kind of collaboration network you want to analysis, you’ll need an edge list where individulas are unique nodes with edges connecting them to each other if they have colloborated. In scientific co-authorship networks, nodes represent authors, organizations or countries, which are connected when they share the authorship of a paper.

##ANALYSIS
It is useful to review a few major centrality measures in social networks before starting the analysis.

**Degree Centrality** : Is the number of ties (edges) a node has without considering their directions. It shows the extend a node holds all of the ties in a network.

The degree centrality of a vertex v, for a given graph G := (V,E) with |V| vertices and |E| edges, is calculated as

Cd(v) = deg(v)

 It is measured by number of edges of a node. It can be also normalized by dividing by total number of edges.

**Betweenness Centrality** : Betweenness centrality of node v is equal to the number of shortest paths from all vertices to all other that pass v.

It is a measure of the extent to which a node is connected to other nodes that are not connected to each other. In the other words the total amount of flow it carries if flow between all other nodes passes it.

The betweenness centrality of a vertex v for a given connected graph G is:

g(v)= ∑_(s≠v≠t)▒(σst(v))/σst

Where σst is the total number of shortest paths from node s to node t and σst(v) is the number of those paths that pass through v. It is between 0 and 1 with 1 being the highest centrality.

**Closeness Centrality** : It is based on the length of the average shortest path between a vertex and all vertices in the graph.

Is a measure of the degree to which an individual is near all other individuals in a network.

They all measure the consequences of having (or not having) a certain node in a graph which shows the importance/power/influence of the node in the network structure.

**High Degree Centrality**:

A node with high degree centrality, has more autonomy from its alters (comparing to a node with lower degree), therefore, it has more flexibility/power/opportunities/choices to pick any of those nodes in for example a trade context.

***High Betweenness Centrality***:

A node with high betweenness centrality has a large influence on the transfer of items through the network, under the assumption that item transfer follows the shortest paths. Betweenness Centrality is related to connectivity in the graph, in so much as high betweenness vertices have the potential to disconnect graphs if removed. Therefore, a node with high betweenness centrality has the capacity to broker contacts among other nodes, and isolate other nodes or prevent contact.

**High Closeness Centrality**:

A node with high acts as a reference point by which can reach a larger number of other nodes at shorter path lengths. It emphasizes the distance of an actor to all others in the network (as oppose to direct ties in degree centrality).

**Potential Limitation**:

Degree centrality is particularly useful when we want to compare networks cohesiveness as in how the ties have been distributed. It addresses the problem that can rise when graph density is not adequate measure (for example if in a network certain node has disproportionately high degree).

However, It’s main limitation is that it only takes into account the immediate ties that a node has to its egos, rather than indirect ties to all other nodes. A node with a high total degree centrality might be connected to some whole other nodes, but those nodes might be rather disconnected from the network as a whole, thus the ego node is only central locally and therefore doesn’t have the ability to broker between groups (or information/flow originated in other parts of the network is likely not to reach it).

Betweennesss centrality is a good measure to find nodes which bridge subgroups in a network, and consequently indicate structural holes in a network. It can also identify levels of hierarchy in a network. If one eliminates all the actors with no betweenness (that is, the “subordinates”), some of the remaining actors will then have 0 betweenness—they are at the second level of the hierarchy.

While it is useful for analyses of spread of disease and pandemics, it can be misleading for simpler analysis.

As closeness Centrality relies on the sum of the geodesic distances form each actor to all the others, it can be misleading in complicated graphs. A node that is very close to a relatively small subset of a network, can have the same score as a moderately close node to every actor in a large subset of the network.

