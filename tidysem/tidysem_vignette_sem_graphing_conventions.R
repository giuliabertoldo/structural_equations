# Source: https://cjvanlissa.github.io/tidySEM/articles/sem_graph.html

library(tidySEM)
library(ggplot2)
library(tidyverse)
library(lavaan)

# Observed variables
graph_sem(layout = matrix("x")) +
  coord_fixed()

# Latent variables
graph_sem(nodes = data.frame(name = "x", shape = "oval"),
          layout = matrix("x"),
          fix_coord = TRUE)

# Path models
df <- iris[, 1:2]
names(df) <- c("x", "y")
sem("y ~ x", df) %>%
  graph_sem(spacing_x = 2.5, fix_coord = TRUE)

# Measurement model
df <- iris[ , c(1,3:4)]
names(df) <- paste0("y_", 1:3)

tidy_sem(df) %>%
  measurement() %>%
  estimate_lavaan() %>%
  graph_sem()

# SEM
df <- iris[ , 1:4]
names(df) <- c("y_1", "x", "y_2", "y_3")

tidy_sem(df) %>%
  measurement() %>%
  add_paths(y ~ x, x ~~ x, x ~1) %>%
  estimate_lavaan() %>%
  graph_sem()

## we can define a more pleasing layout for the model above using the get_layout() function:
tidy_sem(df) %>%
  measurement() %>%
  add_paths(y ~ x, x ~~ x) %>%
  estimate_lavaan() %>%
  graph_sem(layout =
              get_layout("",     "x",    "",
                         "",     "y",    "",
                         "y_1", "y_2", "y_3", rows = 3))
