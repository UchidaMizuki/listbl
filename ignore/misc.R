# library(devtools)
# library(rlang)
# library(vctrs)
# library(dplyr)

# res <- tibble::tibble(name1 = sample(letters[1:3], 10,
#                                      replace = T),
#                       name2 = sample(letters[1:3], 10,
#                                      replace = T),
#                       value = as.list(sample(1:3, 10,
#                                              replace = T))) |>
#   dplyr::group_by(name1, name2) |>
#   as_listbl()
#
# res
#
# res |>
#   select(name2)
#
# res |>
#   as_tibble()

# library(data.tree)
# library(treemap)
#
# data(GNI2014)
# head(GNI2014)
#
# GNI2014$pathString <- paste("world",
#                             GNI2014$continent,
#                             GNI2014$country,
#                             sep = "/")
#
# population <- as.Node(GNI2014)
# print(population, "iso3", "population", "GNI", limit = 20)
#
# getAnywhere(print.Node)
# population$Get("population", filterFun = isLeaf)
