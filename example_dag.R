# Load libraries
library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(grid)  # Needed for arrow specification

# Define DAG
dag <- dagitty("dag {
  age -> smoking
  age -> lung_cancer
  age -> death

  sex -> smoking
  sex -> lung_cancer
  sex -> death

  ethnicity -> smoking
  ethnicity -> lung_cancer
  ethnicity -> death

  deprivation -> smoking
  deprivation -> obesity
  deprivation -> lung_cancer
  deprivation -> death

  smoking -> lung_cancer
  smoking -> death

  obesity -> lung_cancer
  obesity -> death

  lung_cancer -> death
}")

# Set coordinates manually
coordinates(dag) <- list(
    x = c(age = 0, sex = 0, ethnicity = 0, deprivation = 0,
          smoking = 1, obesity = 1,
          lung_cancer = 2, death = 3),
    y = c(age = 3, sex = 2, ethnicity = 1, deprivation = 0,
          smoking = 2.5, obesity = 1.5,
          lung_cancer = 2, death = 2)
)

# Convert to tidy format
tidy_dag <- tidy_dagitty(dag)

# Add node type for coloring
tidy_dag <- tidy_dag %>%
    mutate(node_type = case_when(
        name %in% c("age", "sex", "ethnicity", "deprivation") ~ "Demographic",
        name %in% c("smoking", "obesity") ~ "Behavioral",
        name == "lung_cancer" ~ "Health Outcome",
        name == "death" ~ "Final Outcome",
        TRUE ~ "Other"
    ))

# Plot DAG with arrows
ggplot(tidy_dag) +
    geom_dag_edges_link(
        aes(x = x, y = y, xend = xend, yend = yend, ),
        arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    ) +
    geom_dag_node(aes(x = x, y = y, fill = node_type), shape = 21, size = 20, alpha = 0.8) +
    geom_dag_text(aes(x = x, y = y, label = name), color = "black", size = 2.2) +
    scale_fill_manual(values = c(
        "Demographic" = "#66c2a5",
        "Behavioral" = "#fc8d62",
        "Health Outcome" = "#8da0cb",
        "Final Outcome" = "#e78ac3"
    )) +

    theme_void() +
    theme(legend.position = "none")

