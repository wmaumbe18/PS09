library(tidyverse)
library(proxy)

set.seed(76)
points <- data_frame(
  x1 = c(runif(25), runif(25, min=1, max=2)),
  x2 = c(runif(25), runif(25, min=1, max=2))
)

centroids <- data_frame(
  x1 = c(0.25, 0.25),
  x2 = c(0.75, 0.75)
)

# Hint
D <- proxy::dist(x=points[, c("x1","x2")], y=centroids[, c("x1","x2")])

# Sanity check

# Using kmeans(), the resulting clusters
results <- kmeans(points, centers=2)
results$cluster
points$cluster <- as.factor(results$cluster)

# the resulting centers
centers <- results$centers %>%
  as_tibble() %>%
  mutate(cluster=as.factor(1:n()))

ggplot(NULL, aes(x=x1, y=x2, col=cluster)) +
  geom_point(data=points) +
  geom_point(data=centers, size=5)
