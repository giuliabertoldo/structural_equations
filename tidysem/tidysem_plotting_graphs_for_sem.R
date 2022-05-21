# Source: https://cjvanlissa.github.io/tidySEM/articles/Plotting_graphs.html

# Load packages
library(tidySEM)
library(lavaan)
library(ggplot2)
library(dplyr)


# 1. Example: Graphing a CFA ----------------------------------------------


# * Step 1: Run an analysis -----------------------------------------------
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model,
           data=HolzingerSwineford1939)


# * Step 2: Plot the graph ------------------------------------------------
graph_sem(model = fit)


# * Step 3: Customizing the layout ----------------------------------------
## Option 1: Automatically generate layout for a model
get_layout(fit)
get_layout(fit, layout_algorithm = "layout_in_circle")
get_layout(fit, layout_algorithm = "layout_on_grid")

## Option 2: SPecifying layout manually in R
get_layout("c", NA,  "d",
           NA,  "e", NA, rows = 2)

## Layout for two hypothetical nodes x and y
get_layout("x", "y", rows = 1)

## Layout for a mediation model
get_layout("", "m", "",
           "x", "", "y", rows = 2)

## Layout for a CFA model
get_layout("", "F", "",
           "y1", "y2", "y3", rows = 2)

## Layout for the CFA model estimated above
lay <- get_layout("", "", "visual","","textual","","speed","", "",
                  "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", rows = 2)
## Plot using the custom layout
graph_sem(fit, layout = lay)


# * Step 4: Customize nodes and edges -------------------------------------
## See what nodes are available
get_nodes(fit)

## See what edges are available
get_edges(fit)

