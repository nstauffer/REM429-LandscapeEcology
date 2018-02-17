# Convert a data frame into a vector by concatenating columns sequentially
# Great for when you want to be able to refer to things by a 1D index instead of 2D
dfcolwise.vector <- function(df) {
  unlist(lapply(1:ncol(df), function(X, df){
    unlist(df[,X])
  }, df = df))
}

# Identify patches in a data frame or matrix of values
# All values must be character strings not ending in digits
# The min/max arguments describe the edges of the landscape (important if your data extend past the landscape!)
patches.4way <- function(df, colmin = NULL, colmax = NULL, rowmin = NULL, rowmax = NULL) {
  if (is.null(colmin)) {
    colmin <- 1
  }
  if (is.null(colmax)) {
    colmax <- ncol(df)
  }
  if (is.null(rowmin)) {
    rowmin <- 1
  }
  if (is.null(rowmax)) {
    rowmax <- ncol(df)
  }
  
  # Make the columns into a vector
  colwise <- dfcolwise.vector(df)
  # What are the unique patch types?
  patch.types <- unique(colwise)
  
  
  # Just make a copy of df we can mutilate
  patch.type.df <- df
  # This will be the ID number, to be incremented on each search
  n <- 0
  # For each column in the range
  for (col in colmin:colmax) {
    # For each row in that column
    for (row in rowmin:rowmax) {
      # Work with the cell
      current.cell <- patch.type.df[row,col]
      
      # Reset this in case it escaped another loop
      current.n <- NA
      
      # If the cell already has a patch ID number, make it current.n
      if (grepl(current.cell, pattern = "\\d{1,100}$")) {
        current.n <- stringr::str_extract(current.cell, pattern = "\\d{1,100}$")
      } else {
        # Otherwise, increment the ID number, make it the current ID number, and update the current cell to include it
        n <- n + 1
        current.n <- n
        current.cell <- paste0(patch.type.df[row,col], current.n)
        patch.type.df[row,col] <- current.cell
      }
      
      
      # Get the VALID column indices to the left and right
      lr <- c(left = (col - 1),
              right = (col + 1))
      lr <- lr[(lr >= colmin & lr <= colmax)]
      # Get the VALID row indices above and below
      ud <- c(up = (row - 1),
              down = (row + 1))
      ud <- ud[(ud >= rowmin & ud <= rowmax)]
      
      # For the indices in lr
      for (cell in lr) {
        # Find out what's there
        replace.cell <- patch.type.df[row, cell]
        # If the non-ID parts of the cell value (the landscape type) are identical, they're part of the same patch
        if (gsub(replace.cell, pattern = "\\d{1,100}", replacement = "") == gsub(current.cell, pattern = "\\d{1,100}", replacement = "")) {
          # If the cell being checked already has a patch ID number appended
          if (grepl(replace.cell, pattern = "\\d{1,100}$")) {
            # If the "root" cell already has an ID number, replace all of that patch's cell values with the new one
            if (grepl(current.cell, pattern = "\\d{1,100}$")) {
              patch.type.df[patch.type.df == current.cell] <- replace.cell
            }
            current.cell <- replace.cell
            current.n <- stringr::str_extract(current.cell, pattern = "\\d{1,100}$")
          } else {
            # Otherwise make the cell being checked part of the "root" patch
            patch.type.df[row, cell] <- current.cell
          }
        }
      }
      # Same as above but up, up and down
      for (cell in ud) {
        replace.cell <- patch.type.df[cell, col]
        if (gsub(replace.cell, pattern = "\\d{1,100}", replacement = "") == gsub(current.cell, pattern = "\\d{1,100}", replacement = "")) {
          if (grepl(replace.cell, pattern = "\\d{1,100}$")) {
            if (grepl(current.cell, pattern = "\\d{1,100}$")) {
              patch.type.df[patch.type.df == current.cell] <- replace.cell
            }
            current.cell <- replace.cell
            current.n <- stringr::str_extract(current.cell, pattern = "\\d{1,100}$")
          } else {
            patch.type.df[cell, col] <- current.cell
          }
        }
      }
    }
  }
  output <- patch.type.df
  
  output[1:{if ((rowmin - 1) > 0) {rowmin - 1} else {1}},] <- NA
  output[{if ((rowmax + 1) <= nrow(df)) {rowmax + 1} else {nrow(df)}}:nrow(df),] <- NA
  output[,1:{if ((colmin - 1) > 0) {colmin - 1} else {1}}] <- NA
  output[,{if ((colmax + 1) <= ncol(df)) {colmax + 1} else {ncol(df)}}:ncol(df)] <- NA
  return(output)
}

# Identify edges in a data frame or matrix of values
# You can specify the min/max boundaries for the landscape, but why bother?
# It doesn't hurt to have edge counts calculated beyond the landscape, at least not for so few cells
edges.4way <- function(df, colmin = NULL, colmax = NULL, rowmin = NULL, rowmax = NULL) {
  if (is.null(colmin)) {
    colmin <- 2
  }
  if (is.null(colmax)) {
    colmax <- ncol(df) - 1
  }
  if (is.null(rowmin)) {
    rowmin <- 2
  }
  if (is.null(rowmax)) {
    rowmax <- ncol(df) - 1
  }
  
  # Just make an empty copy of df we can mutilate
  edges.df <- matrix(data = NA, ncol = ncol(df), nrow = nrow(df))
  
  # For each column in the range
  for (col in colmin:colmax) {
    # For each row in that column
    for (row in rowmin:rowmax) {
      # Work with the cell
      current.cell <- df[row,col]
      
      # Get the VALID column indices to the left and right
      lr <- c(left = (col - 1),
              right = (col + 1))
      lr <- lr[(lr >= 1 & lr <= ncol(df))]
      # Get the VALID row indices above and below
      ud <- c(up = (row - 1),
              down = (row + 1))
      ud <- ud[(ud >= 1 & ud <= nrow(df))]
      
      cell.edgecount <- 0
      # For the indices in lr
      for (cell in lr) {
        # If it's the same patch type, don't increment the edge count
        # If running this on a patch-IDed matrix, you need to strop the ID numbers off the ends
        if (stringr::str_replace(df[row, cell], pattern = "\\d{1,100}", replacement = "") != stringr::str_replace(current.cell, pattern = "\\d{1,100}", replacement = "")) {
          cell.edgecount <- cell.edgecount + 1
        }
      }
      # Same as above but up, up and down
      for (cell in ud) {
        # If it's the same patch, don't increment the edge count
        if (stringr::str_replace(df[cell, col], pattern = "\\d{1,100}", replacement = "") != stringr::str_replace(current.cell, pattern = "\\d{1,100}", replacement = "")) {
          cell.edgecount <- cell.edgecount + 1
        }
      }
      edges.df[row,col] <- cell.edgecount
    }
  }
  output <- edges.df

  return(output)
}

# Calculate the Shannon-Wiener Diversity Index
# prop.vec is the vector of proportions of the types on the landscape
diversity.sw <- function(prop.vec) {
  -sum(unlist(lapply(prop.vec,
              FUN = function(X){
                X*log(X)
              })))
}

# Calculate the Shannon-Wiener Evenness Index
# prop.vec is the vector of proportions of the types on the landscape
evenness.sw <- function(prop.vec) {
  diversity.sw(prop.vec)/log(length(prop.vec))
}