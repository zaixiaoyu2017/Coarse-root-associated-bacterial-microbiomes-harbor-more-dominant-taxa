library(ggplot2)
library(ggtern)

#data: A community table with taxa as rows, and columns including the relative abundance for each taxa of samples groups, rich and size.

ggtern(data, aes( RD1,RD2,RD3)) +
  geom_mask() +
  geom_point(aes(color = rich, size = size), alpha = 0.8, show.legend = FALSE) +
  scale_size(range = c(0, 6)) +
  scale_colour_manual(values  = c( 'red','blue', 'green3', 'gray'), limits = c('RD1', 'RD2', 'RD3', '0')) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

