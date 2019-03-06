library(tidyverse)

# Read in data and add a source field
redtail.raw <- readxl::read_excel(path = "C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/11_patterns/data/REM429 point data.xlsx",
                                  sheet = "Red-Tail Ridge")
redtail.raw$source <- "Red-Tail Ridge"

caffey.raw <- readxl::read_excel(path = "C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/11_patterns/data/REM429 point data.xlsx",
                                 sheet = "Caffey Hill")
caffey.raw$source <- "Caffey Hill"

# Bind them together
raw <- rbind(redtail.raw, caffey.raw)

raw[raw == "NA" | raw == "N" | raw == "NR"] <- NA

raw <- dplyr::select(dplyr::mutate(raw,
                     DBH99 = round(DBH99in*2.54),
                     DBH13 = round(as.numeric(DBH13cm))), -dplyr::matches("(in|cm)$"))



data <- dplyr::select(raw,
                      source, X, Y,
                      dplyr::matches("99"),
                      dplyr::matches("13"))

data$uid <- 1:nrow(data)

# Remove all the rows where there isn't a valid value for critical variables
data <- dplyr::filter(data,
                      !is.na(X),
                      !is.na(Y))
# No shrinking trees
bad.dbh <- dplyr::filter(data,
                         # Be aggressive. If the tree "shrank" then the data can't be trusted!
                         !(DBH99 <= DBH13))
data <- dplyr::filter(data,
                      !(uid %in% bad.dbh$uid))

# At least one has a status
data <- dplyr::filter(data,
                      !is.na(Status99) | !is.na(Status13))

# A tree that was dead in 99 can't be alive in 13
data <- dplyr::filter(data,
                      !(Status99 %in% c("D") & !(Status13 %in% c("D"))))

write.csv(data, "C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/11_patterns/data/clean_wide.csv")

# Get the 2013 data
raw.13 <- dplyr::select(raw, source, X, Y, dplyr::matches("13")) %>% mutate(Year = 2013)
names(raw.13) <- gsub(names(raw.13), pattern = "13(cm){0,1}", replacement = "")

# Get the 1999 data
raw.99 <- dplyr::select(raw, source, X, Y, dplyr::matches("99")) %>% mutate(Year = 1999)
names(raw.99) <- gsub(names(raw.13), pattern = "99(in){0,1}", replacement = "")

# Bind the two years
data.tall <- dplyr::select(rbind(raw.99, raw.13),
                           source, X, Y, TreeID, Year, Status, DBH, Notes)
data.tall <- dplyr::filter(data.tall,
                           !is.na(X),
                           !is.na(Y),
                           !is.na(TreeID),
                           !is.na(Status),
                           !is.na(DBH))

write.csv(data.tall, "C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/11_patterns/data/clean_tall.csv")

spatialpoints <- sp::SpatialPoints(coords = data.tall[, c("X", "Y"),])
spdf <- sp::SpatialPointsDataFrame(coords = data.tall[, c("X", "Y"),], 
                                   data = data.tall)
plot(spatialpoints)
