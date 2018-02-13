library(tidyverse)
# Make a metadata data frame from assorted lines in a log file
# text is the raw log file from readLines()
# key is a string unique to this log file. I use the log filename without extension
meta.extract <- function(text,
                         key){
  log <- text
  values <- list()
  # Put the key in there!
  values$key <- key
  # Get the seed number
  values$seed.number <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                      pattern = "Random number seed")],
                                                        pattern = "\\d{1,100}"))
  
  # Get the percent of the area cut each decade
  values$decade.pct.cut <- as.numeric(stringr::str_extract(stringr::str_split(log[grepl(log,
                                                                                            pattern = "Percent of forested area to be cut")],
                                                                              pattern = "\\(")[[1]][1],
                                                           pattern = "\\d{1,2}\\.\\d{1,3}"))
  
  # Get the cell rowcount
  values$row.count <- as.numeric(stringr::str_extract(stringr::str_split(log[grepl(log,
                                                                                       pattern = "Number of rows and columns")],
                                                                         pattern = ",")[[1]][1],
                                                      pattern = "\\d{1,100}"))
  
  # Get the cell columncount
  values$col.count <- as.numeric(stringr::str_extract(stringr::str_split(log[grepl(log,
                                                                                       pattern = "Number of rows and columns")],
                                                                         pattern = ",")[[1]][2],
                                                      pattern = "\\d{1,100}"))
  
  # Get the oldest decade class
  values$class.oldest.decade <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                              pattern = "Oldest age class")],
                                                                pattern = "\\d{1,100}"))
  
  # Get the map area
  values$map.ha <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                 pattern = "Total map area")],
                                                   pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  
  # Get the forested area
  values$forested.ha <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                      pattern = "Total forested area")],
                                                        pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  
  # Get the mean harvest area
  # The multiplication by ten is because this dropped by an order of magnitude somehow in the reading????
  values$harvest.hectares.mean <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                                pattern = "Average harvest size \\(hectares\\)")],
                                                                  pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  
  # Get the harvest area standard deviation
  values$harvest.hectares.mean <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                                pattern = "Standard deviation in harvest size \\(hectares\\)")],
                                                                  pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  
  # Get the mim harvestable age class (in decades)
  values$harvest.decade.min <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                             pattern = "Minimum age class \\(decades\\) where harvest is allowed")],
                                                               pattern = "\\d{1,100}"))
  
  # Get the dispersion method
  values$dispersion <- trimws(stringr::str_split(log[grepl(log,
                                                               pattern = "Spatial dispersion method")],
                                                 pattern = "\\:")[[1]][2])
  
  # Get buffer width
  values$buffer <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                 pattern = "Edge-buffer width")],
                                                   pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  
  # Get persistence
  values$persistence.decades <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                              pattern = "Number of decades openings persist")],
                                                                pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  return(data.frame(values))
}

# Make a data frame from a stretch of a log file
# text is the raw log file from readLines()
# header.regex is a regular expression used to find the line with the header for the data in the log file
# key is a string unique to this log file. I use the log filename without extension
table.extract <- function(text,
                          header.regex,
                          key){
  # Find the empty lines
  empty.lines <- grep(trimws(text), pattern = "^$")
  
  # Find the line with the header
  header <- grep(text, pattern = header.regex)
  
  empty.lines[empty.lines > header][1]
  # Get just the lines that fall from the header to the next empty line
  text.trim <- text[header:(empty.lines[empty.lines > header][1] - 1)]
  
  # Cut the white space off the ends of the lines then split each into columns based on repetition of spaces
  text.list <- stringr::str_split(trimws(text.trim), pattern = "[ ]{2,100}")
  
  # Ridiculous, but for each column that can be discerned, get in and grab everything after the header
  # This will make a list of vectors, each corresponding to a column
  cols <- lapply(X = 1:length(text.list[[1]]),
                 FUN = function(X, text){
                   col <- X
                   unlist(lapply(X = text[2:length(text)],
                                 FUN = function(X, col){
                                   return(as.numeric(X[col]))
                                 },
                                 col = col))
                 },
                 text = text.list)
  # Hacky as hell, but I can't be bothered to fight dplyr::bind_cols() any more
  # Combine the columns into a data frame and name them
  df <- eval(parse(text = paste0("data.frame('key' = key,", paste0("cols[[", 1:length(cols), "]]", collapse = ", "), ")")))
  names(df)[2:ncol(df)] <- text.list[[1]]
  
  return(df[!is.na(df[[2]]),])
}

# Using meta.extract() and table.extract() make a list of data frames for the output from a harvestlite run
# file is the full filepath and filename for the log file to be read in
log.read <- function(file){
  # Get the filename without the extension to use as the key
  key <- gsub(stringr::str_split(file, pattern = "/")[[1]][length(stringr::str_split(file, pattern = "/")[[1]])], pattern = "\\.[A-z]{3}", replacement = "")
  
  # Read in the file
  log <- readLines(file)
  
  # Get the metadata
  metadata <- meta.extract(text = log,
                           key = key)
  
  # Get the various tables
  area.harvested <- table.extract(text = log,
                                  header.regex = "Area harvested \\(ha\\)$",
                                  key = key)
  
  age.class.distribution <- table.extract(text = log,
                                          header.regex = "Area \\(ha\\)$",
                                          key = key)
  
  patch.sizes <- table.extract(text = log,
                               header.regex = "Avg\\. size \\(ha\\)$",
                               key = key)
  
  patch.size.distribution <- table.extract(text = log,
                                           header.regex = ">1000 ha$",
                                           key = key)
  
  # Get the other values that aren't metadata but also aren't in table format because they're single metrics
  assorted <- list()
  assorted$key <- key
  # Get interior habitat acreage
  assorted$habitat.ha.interior <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                                pattern = "Area of interior habitat")],
                                                                  pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  # Get edge habitat acreage
  assorted$habitat.ha.edge <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                            pattern = "Area of edge habitat")],
                                                              pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  # Get boundary length between different-aged cells
  assorted$age.boundary.length.km <- as.numeric(stringr::str_extract(log[grepl(log,
                                                                                   pattern = "Total boundary length between different-aged cells")],
                                                                     pattern = "\\d{1,100}[\\.]{0,1}\\d{0,3}"))
  
  
  other <- data.frame(assorted)
  
  return(list(metadata = metadata,
              area.harvested = area.harvested,
              age.class.distribution = age.class.distribution,
              patch.sizes = patch.sizes,
              patch.size.distribution = patch.size.distribution,
              other = other))
}

# A wrapper for log.read() that will take just a filepath and read in all the logs from that filepath,
# combining the dataframes (e.g. all the metadata dataframes from the logs into a single output metadata frame)
# It'll return a list of the combined dataframes
# filepath is the full path to the folder containing the files
# extension is the file extension on the logs and defaults to "txt" because that's what I saved mine as, but "log" is what harvestlite does if left to its own devices
logs.read <- function(filepath,
                      extension = "txt"){
  # Get all the file locations
  files <- list.files(path = filepath,
                      pattern = paste0("\\.", extension, "$"),
                      ignore.case = TRUE,
                      full.names = TRUE)
  # Create a list of all the data frames!
  data <-lapply(files, log.read)
  
  output <- lapply(names(data[[1]]),
                   FUN = function(X, data){
                     name <- X
                     dfs <- lapply(data, FUN = function(X, name){
                       X[[name]]
                     }, name = name)
                     return(dplyr::bind_rows(dfs))
                   }, data = data)
  
  names(output) <- names(data[[1]])
  
  return(output)
}

# There will be NA warnings! That's normal and I just couldn't be bothered to silence them



