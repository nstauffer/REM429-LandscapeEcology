################# DATA -----
## Creating matrix versions of the rasters from the .pdf

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

################# EVALUATE -----
## Identify the patches and edges

# The landscape in question is [2:11,2:11] because the outer edge of the data is context for edge math
early.patches <- patches.4way(early.matrix,
                              colmin = 2,
                              colmax = 11,
                              rowmin = 2,
                              rowmax = 11)
# It doesn't hurt to get edges per cell for the few cells technically outside the landscape, so I'm not specifying min/max
early.edges <- edges.4way(early.matrix)

late.patches <- patches.4way(late.matrix,
                              colmin = 2,
                              colmax = 11,
                              rowmin = 2,
                              rowmax = 11)

late.edges <- edges.4way(late.matrix)

################# SUMMARIZE -----
## Time to calculate some values from our patch and edge information!

# So I do a lapply() across the unique values on the landscape (so it'll turn up the types and use those)
# For each patch type, I'll make a vector of the values I wanted to calculate
# Note that for the lapply() inputs I sliced out the non-landscape cells. They'll screw up this math
patch.summary.early <- dplyr::bind_rows(lapply(unique(dfcolwise.vector(early.matrix)),
                                               FUN = function(X, patches, edges){
                                                 # Output is the list I'm storing the values in
                                                 output <- list()
                                                 # The type is the current landscape type
                                                 output[["type"]] <- X
                                                 # Turn the matrices into vectors. This lets me use one-dimensional indices to find things in them
                                                 # And 1D indices are way easier to deal with than 2D
                                                 patch.vector <- dfcolwise.vector(patches)
                                                 edge.vector <- dfcolwise.vector(edges)
                                                 # This just gives me a summary of how many cells were in each patch
                                                 patch.table <- table(patch.vector)
                                                 
                                                 # Proportion is [number of cells in type]/[total cells]
                                                 # So, getting the length of the result of searching patch.vector for my current type + ID suffix tells me how many cells were in patches of that type
                                                 output[["patch.proportion"]] <- length(grep(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}")))/length(patch.vector)
                                                 # Number of patches is the length of unique values that matched current type + ID suffix
                                                 output[["patch.n"]] <- length(unique(patch.vector[grepl(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}"))]))
                                                 # Mean size is the mean of the patch.table values that had names that matched current type + ID suffix
                                                 output[["patch.size.m"]] <- mean(patch.table[names(patch.table)[grepl(names(patch.table), pattern = paste0("^", output$type, "\\d{1,100}"))]])
                                                 # Here's where the 1D indices are really helpful. I can use the patch identities from patch.vector to slice to the relevant values in edge.vector
                                                 # That lets me quickly get the number of edges for each patch!
                                                 output[["edge.area.ratio"]] <- sum(edge.vector[grepl(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}"))])/length(grep(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}")))
                                                 
                                                 return(output)
                                               }, patches = early.patches[2:11,2:11], edges = early.edges[2:11,2:11]))
patch.summary.early$landscape <- "Early"

patch.summary.late <- dplyr::bind_rows(lapply(unique(dfcolwise.vector(late.matrix)),
                                               FUN = function(X, patches, edges){
                                                 output <- list()
                                                 output[["type"]] <- X
                                                 patch.vector <- dfcolwise.vector(patches)
                                                 edge.vector <- dfcolwise.vector(edges)
                                                 patch.table <- table(patch.vector)
                                                 output[["patch.proportion"]] <- length(grep(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}")))/length(patch.vector)
                                                 output[["patch.n"]] <- length(unique(patch.vector[grepl(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}"))]))
                                                 
                                                 output[["patch.size.m"]] <- mean(patch.table[names(patch.table)[grepl(names(patch.table), pattern = paste0("^", output$type, "\\d{1,100}"))]])
                                                 
                                                 output[["edge.area.ratio"]] <- sum(edge.vector[grepl(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}"))])/length(grep(patch.vector, pattern = paste0("^", output$type, "\\d{1,100}")))
                                                 
                                                 return(output)
                                               }, patches = late.patches[2:11,2:11], edges = late.edges[2:11,2:11]))
patch.summary.late$landscape <- "Late"

# Bind the patch summaries together!
patch.summary <- rbind(patch.summary.early, patch.summary.late)

# Doing the landscape as a single data frame step
# For each variable, I calculate early then late
landscape.summary <- data.frame(landscape = c("Early", "Late"),
                                # Number of patches is the length of the unique values in the vector of the landscape cells
                           patch.n = c(length(unique(dfcolwise.vector(early.patches[2:11,2:11]))),
                                       length(unique(dfcolwise.vector(late.patches[2:11,2:11])))),
                           # The Shannon-Wiener diversity index is calculated with a custom function using the patch.summary$proportion
                           sw.diversity = c(diversity.sw(patch.summary$patch.proportion[patch.summary$landscape == "Early"]),
                                            diversity.sw(patch.summary$patch.proportion[patch.summary$landscape == "Late"])),
                           # The Shannon-Wiener evenness index is calculated with a custom function using the patch.summary$proportion
                           sw.evenness = c(evenness.sw(patch.summary$patch.proportion[patch.summary$landscape == "Early"]),
                                           evenness.sw(patch.summary$patch.proportion[patch.summary$landscape == "Late"])))

# Write out the files
write.csv(patch.summary, "patch.csv")
write.csv(landscape.summary, "landscape.csv")