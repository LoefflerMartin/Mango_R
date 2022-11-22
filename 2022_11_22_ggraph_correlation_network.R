# Example Script to visualize correlation structure in a dataset.
# ML 2022-11-22
# see: https://drsimonj.svbtle.com/how-to-create-correlation-network-plots-with-corrr-and-ggraph
# see also here: https://corpling.hypotheses.org/3341
# see also here for more sophisticated graphs: http://blog.schochastics.net/post/ggraph-tricks-for-common-problems/


####################### 1) HEADER ########################################################################## 
rm(list=ls(all=TRUE)) # clear workspace

# load (and install) packages
packs = c("igraph", "ggraph", "tidygraph", "tidyverse", "corrr", "rstatix")
if (length(setdiff(packs, rownames(installed.packages())))>0) {
  install.packages(setdiff(packs, rownames(installed.packages())))
}
lapply(packs, require, character.only=TRUE); rm(packs)



####################### 2) PREPARE DATA #################################################################### 
# create correlation matrix in long format
tidy_cors <- mtcars %>% 
  cor_mat() %>%
  cor_gather()
#cor_get_pval(mtcars %>% cor_mat()) # this is how the p-matrix could be accessed

# create and igraph object from dataframe
graph_cors <- tidy_cors %>%
  filter(p < .001) %>%                      # keep only correlations above a p-threshold
  filter(abs(r) > .5) %>%                   # keep only correlations above an r-threshold
  graph_from_data_frame(directed = FALSE)

# create a vector of weights for the node size later on based on the sum of all correlations
cor_matrix <- mtcars %>% correlate()
cor_matrix$point_size <- cor_matrix %>%
  select_if(is.numeric) %>% 
  abs() %>% 
  rowSums(na.rm = TRUE)



####################### 3) PLOT DATA ####################################################################### 
# plot correlation network using ggraph
ggraph(graph_cors, layout = "auto") +
  geom_edge_bend(aes(edge_alpha = abs(cor),
                     edge_width = abs(cor),
                     color = cor),
                 strength = .5) +                                         # add edges to graph
  guides(edge_alpha = "none") +                                           # some legend formatting
  scale_edge_colour_gradientn(limits = c(-1, 1),
                              colors = c("blue", "white", "gold")) +      # format edges colour
  geom_node_point(size=cor_matrix$point_size*2,
                  color = "grey70", size = 5) +                           # add nodes
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding  = unit(15, "lines"),
                 size=cor_matrix$point_size, 
                 colour = 'black') +                                      # add text to nodes
  theme_graph(background = 'white', text_colour = 'black') +              # set the themes
  labs(title = "Correlation Network", subtitle = "of the mtcars dataset") # add title

