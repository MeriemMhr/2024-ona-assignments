# Introduction

This document provides an analysis of the seating arrangement on the
Fakebook company bus. As a new intern, selecting the right seat is
critical for fostering connections within the company. The seats are
analyzed based on their centrality measures within the network.

# Network Centrality Measures

First, we calculate the centrality measures for each seat to understand
their importance within the network.

    # Define the edges of the seating arrangement
    edges <- tribble(
      ~from, ~to,
      "1", "2", "1", "A", "1", "B",
      "2", "1", "2", "3", "2", "A",
      "3", "2", "3", "4", "3", "B", "3", "C",
      "4", "3", "4", "5", "4", "C",
      "5", "4", "5", "6", "5", "D",
      "6", "5", "6", "B", "6", "D",
      "A", "1", "A", "2", "A", "B",
      "B", "A", "B", "1", "B", "3", "B", "6", "B", "D",
      "C", "3", "C", "4", "C", "D",
      "D", "C", "D", "5", "D", "6", "D", "B"
    ) %>%
      as_tbl_graph(directed = FALSE) %>%
      activate(nodes) %>%
      mutate(name = as.character(name))

    # Calculate centrality measures for the network
    centrality_measures <- edges %>%
      mutate(degree = centrality_degree(),
             closeness = centrality_closeness(),
             betweenness = centrality_betweenness(),
             color = case_when(
               name %in% c("A", "B", "C", "D") ~ 'red',
               TRUE ~ 'blue'
             ))

    # Print centrality measures for chosen seats
    chosen_seats <- centrality_measures %>%
      filter(name %in% c("A", "B", "C", "D")) %>%
      arrange(name)

## Seating Arrangement Network Plot

Next, we visualize the network plot with the calculated centrality
measures.

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : font family not found in Windows font database

<img src="Exercise-2_files/figure-markdown_strict/network-plot-1.png" alt="Network plot showing the centrality measures for the seating arrangement on the Fakebook bus."  />
<p class="caption">
Network plot showing the centrality measures for the seating arrangement
on the Fakebook bus.
</p>

# Interpretation of Centrality Measures

The network plot highlights Seats A, B, C, and D as potential choices
for the new intern at Fakebook, shown in red. These seats are analyzed
based on their centrality within the bus social network.

-   **Seat A**: High betweenness but lower degree centrality. Ideal for
    an intern looking to bridge different social clusters, potentially
    influencing the network’s communication flow.

-   **Seat B**: Offers the highest degree centrality, suggesting it is
