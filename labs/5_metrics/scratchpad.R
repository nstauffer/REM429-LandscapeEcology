# Early settlement
early <- rbind(c("g","f","a","f","f","f","f","a","a","f","f","g"),
               c("g","f","a","g","g","f","f","a","a","a","f","f"),
               c("f","f","f","f","f","f","f","g","g","f","a","f"),
               c("f","f","f","f","f","f","f","a","f","f","f","f"),
               c("f","f","g","f","g","f","g","f","g","f","g","g"),
               c("g","g","g","f","a","a","g","f","g","f","f","f"),
               c("f","f","f","g","g","f","f","g","a","f","f","f"),
               c("f","f","f","f","f","f","f","f","f","a","f","f"),
               c("f","g","f","f","f","f","f","f","a","f","f","f"),
               c("a","a","g","g","f","g","f","f","g","g","f","f"),
               c("g","g","f","f","g","f","g","f","f","f","g","g"),
               c("g","g","g","f","f","f","f","f","f","f","g","f"))



# 1 = forest, 2 = grassland, 3 = agriculture
early.matrix <- matrix(byrow = TRUE,
                       nrow = 12,
                       ncol = 12,
                       data = c("g","f","a","f","f","f","f","a","a","f","f","g",
                                "g","f","a","g","g","f","f","a","a","a","f","f",
                                "f","f","f","f","f","f","f","g","g","f","a","f",
                                "f","f","f","f","f","f","f","a","f","f","f","f",
                                "f","f","g","f","g","f","g","f","g","f","g","g",
                                "g","g","g","f","a","a","g","f","g","f","f","f",
                                "f","f","f","g","g","f","f","g","a","f","f","f",
                                "f","f","f","f","f","f","f","f","f","a","f","f",
                                "f","g","f","f","f","f","f","f","a","f","f","f",
                                "a","a","g","g","f","g","f","f","g","g","f","f",
                                "g","g","f","f","g","f","g","f","f","f","g","g",
                                "g","g","g","f","f","f","f","f","f","f","g","f"))

# The outermost cells are all buffer for edge metrics, the landscape is [2:11, 2:11]
early.landscape <- early[2:11, 2:11]

# Patch sizes in the LANDSCAPE
early.forest.patch4.sizes <- c(19,2,13,2,1,24)
early.grass.patch4.sizes <- c(2,2,3,1,2,2,1,2,1,1,2,1,1,1,2,1)
early.ag.patch4.sizes <- c(1,3,1,1,2,1,1,1,1)
# Edges, taking into account the ones beyond the edge of the landscape frame
early.forest.patch4.edges <- c(22,2,20,6,5,3,29)
early.grass.patch4.edges <- c(6,6,3,6,4,6,4,6,7,4,6,2,4,4,4,6,2)
early.ag.patch4.edges <- c(3,6,4,6,4,4,4,4,3)


patch.df.early <- data.frame("Landscape" = "Early",
                             "Patch.Type" = c("Forestland", "Grassland", "Agricultural"),
                             "Landscape.Proportion" = c(length(grep(early.landscape, pattern = "f"))/(ncol(early.landscape)*nrow(early.landscape)),
                                                        length(grep(early.landscape, pattern = "g"))/(ncol(early.landscape)*nrow(early.landscape)),
                                                        length(grep(early.landscape, pattern = "a"))/(ncol(early.landscape)*nrow(early.landscape))),
                             "Patch.Count" = c(length(early.forest.patch4.sizes), length(early.grass.patch4.sizes), length(early.ag.patch4.sizes)),
                             "Patch.Size.Mean" = c(mean(early.forest.patch4.sizes), mean(early.grass.patch4.sizes), mean(early.ag.patch4.sizes)),
                             "Edge.Area.Ratio" = c(sum(early.forest.patch4.edges)/sum(early.forest.patch4.sizes),
                                                   sum(early.ag.patch4.edges)/sum(early.ag.patch4.sizes),
                                                   sum(early.grass.patch4.edges)/sum(early.grass.patch4.sizes)))

# Late settlement
late <- rbind(c("g","f","a","a","f","g","f","a","a","f","f","g"),
              c("g","f","a","g","a","g","f","a","a","a","f","f"),
              c("a","f","f","f","f","f","f","g","f","f","a","f"),
              c("a","f","f","f","g","g","f","a","a","g","f","a"),
              c("a","f","f","a","g","f","g","f","g","f","f","g"),
              c("g","g","g","f","a","g","g","a","g","a","g","a"),
              c("a","g","f","a","g","f","a","g","a","f","f","f"),
              c("f","a","f","a","f","a","f","a","f","a","a","a"),
              c("f","g","f","f","a","a","f","f","a","a","g","f"),
              c("a","a","f","g","a","g","f","f","g","g","a","g"),
              c("g","g","g","g","g","a","g","f","f","f","g","g"),
              c("g","g","g","f","f","a","f","a","a","f","g","f"))

late.matrix <- matrix(byrow = TRUE,
                     nrow = 12,
                     ncol = 12,
                     data = c("g","f","a","a","f","g","f","a","a","f","f","g",
                              "g","f","a","g","a","g","f","a","a","a","f","f",
                              "a","f","f","f","f","f","f","g","f","f","a","f",
                              "a","f","f","f","g","g","f","a","a","g","f","a",
                              "a","f","f","a","g","f","g","f","g","f","f","g",
                              "g","g","g","f","a","g","g","a","g","a","g","a",
                              "a","g","f","a","g","f","a","g","a","f","f","f",
                              "f","a","f","a","f","a","f","a","f","a","a","a",
                              "f","g","f","f","a","a","f","f","a","a","g","f",
                              "a","a","f","g","a","g","f","f","g","g","a","g",
                              "g","g","g","g","g","a","g","f","f","f","g","g",
                              "g","g","g","f","f","a","f","a","a","f","g","f"))

# The outermost cells are all buffer for edge metrics, the landscape is [2:11, 2:11]
late.landscape <- late[2:11, 2:11]

# Patch sizes in the LANDSCAPE
late.forest.patch4.sizes <- c(14,5,1,1,1,1,8,1,2,1,3,2,1)
late.grass.patch4.sizes <- c(1,1,1,1,2,2,1,1,3,1,1,3,1,1,5,1,3,1)
late.ag.patch4.sizes <- c(1,1,1,1,2,1,1,4,1,1,3,2,1,1,1,4,1,1,1)

# Edges, taking into account the ones beyond the edge of the landscape frame
late.forest.patch4.edges <- c(22,12,4,4,4,4,16,4,6,4,8,5,2)
late.grass.patch4.edges <- c(7,4,9,4,8,4,3,8,4,4,4,4,6,6,4,4,4,2)
late.ag.patch4.edges <- c(4,3,3,4,6,4,4,10,3,4,6,6,4,4,4,9,4,4,4)


patch.df.late <- data.frame("Landscape" = "Late",
                            "Patch.Type" = c("Forestland", "Grassland", "Agricultural"),
                             "Landscape.Proportion" = c(length(grep(late.landscape, pattern = "f"))/(ncol(late.landscape)*nrow(late.landscape)),
                                                        length(grep(late.landscape, pattern = "g"))/(ncol(late.landscape)*nrow(late.landscape)),
                                                        length(grep(late.landscape, pattern = "a"))/(ncol(late.landscape)*nrow(late.landscape))),
                             "Patch.Count" = c(length(late.forest.patch4.sizes), length(late.grass.patch4.sizes), length(late.ag.patch4.sizes)),
                             "Patch.Size.Mean" = c(mean(late.forest.patch4.sizes), mean(late.grass.patch4.sizes), mean(late.ag.patch4.sizes)),
                             "Edge.Area.Ratio" = c(sum(late.forest.patch4.edges)/sum(late.forest.patch4.sizes),
                                                   sum(late.ag.patch4.edges)/sum(late.ag.patch4.sizes),
                                                   sum(late.grass.patch4.edges)/sum(late.grass.patch4.sizes)))

patch.df <- rbind(patch.df.early, patch.df.late)
