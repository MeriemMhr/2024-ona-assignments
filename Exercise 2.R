# Load necessary libraries
library(tidygraph)
library(ggraph)
library(dplyr)
library(ggplot2)

# Define the edges directly as there's no external CSV file in this context
edges <- tribble(
  ~from, ~to,
  "1", "2",   "1", "A",   "1", "B",
  "2", "1",   "2", "3",   "2", "A",
  "3", "2",   "3", "4",   "3", "B",   "3", "C",
  "4", "3",   "4", "5",   "4", "C",
  "5", "4",   "5", "6",   "5", "D",
  "6", "5",   "6", "B",   "6", "D",
  "A", "1",   "A", "2",   "A", "B",
  "B", "A",   "B", "1",   "B", "3",   "B", "6",   "B", "D",
  "C", "3",   "C", "4",   "C", "D",
  "D", "C",   "D", "5",   "D", "6",   "D", "B"
) %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  mutate(name = as.character(name))


# Calculate centrality measures and annotate the nodes
centrality_measures <- mutate(edges,
                              degree = centrality_degree(),
                              closeness = centrality_closeness(),
                              betweenness = centrality_betweenness(),
                              color = case_when(
                                name %in% c("A", "B", "C", "D") ~ 'red',
                                TRUE ~ 'blue'
                              )
)

# Extract only the chosen seats with their centrality measures for printing
chosen_seats_centrality <- centrality_measures %>%
  filter(name %in% c("A", "B", "C", "D")) %>%
  select(name, degree, closeness, betweenness) %>%
  arrange(name)

# Print the centrality measures as a table
print(chosen_seats_centrality)

# Simple manual color assignment
color_assignment <- c('red', 'blue')

# Plot the network
network_plot <- ggraph(centrality_measures, layout = 'stress') +
  geom_edge_link(edge_width = 1, color = 'gray') +
  geom_node_point(aes(color = color), size = 8) + # Ensure 'color' column is a factor or character
  geom_node_text(aes(label = name), repel = TRUE, fontface = "bold", color = "black") +
  theme_graph() +
  labs(title = "Bus Seating Arrangement and Centrality Measures",
       subtitle = "Highlighting Seats A, B, C, D") +
  scale_color_manual(values = c('red', 'blue'), labels = c("Chosen Seats", "Other Seats")) +
  guides(color = guide_legend(title = "Seat Type")) + # Adding a legend for colors
  theme(legend.position = "bottom", legend.title = element_text(size = 10), legend.text = element_text(size = 8))

# Print the network plot
print(network_plot)

# Save the plot
ggsave("bus_network_centrality_plot_v2.png", network_plot, width = 10, height = 7, dpi = 300)