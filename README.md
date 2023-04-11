# OUTLIER-DETECTION-IN-SOCIAL-NETWORKS-LEVERAGING-COMMUNITY-STRUCTURE
For a given social network G(V,E), with known community structures, the goal is to identify the set of outlier nodes Vo ∈ V, that have the tendency to belong in more than one community or whose neighbors either belong to only one community or do not belong to any community.
Community detection is done using Louvain.
Pendent nodes are considered to be outliers.
Permanence value is calculated for the boundary nodes only.
If the value crosses the threshold, it is an outlier.
