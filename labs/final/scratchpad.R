library(tidyverse)
# Read in the indicator values
lpi.2013 <- readxl::read_excel("C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/2013_LPI_Plot.XLSX")
lpi.2017 <- readxl::read_excel("C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/2017_LPI_Plot.XLSX")

lpi.2013$year <- 2013
lpi.2017$year <- 2017

# Read in the metadata
metadata.2011to2015 <- readxl::read_excel("C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/2011to2015_PlotDetails.XLSX")
metadata.2017 <- readxl::read_excel("C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/2017_PlotDetails.XLSX")

# Combine the indicator values into one data set
lpi <- rbind(lpi.2013, lpi.2017)

# Combine the metadata into one data frame
metadata <- rbind(metadata.2011to2015, metadata.2017)

# Filter out all rows where the plot key doesn't show up in both data frames
# Just to be neat
metadata <- metadata[metadata$PlotKey %in% lpi$PlotKey,]
lpi <- lpi[lpi$PlotKey %in% metadata$PlotKey,]


### Assign seasonal use ####
metadata$day <- lubridate::yday(lubridate::as_date(metadata$EstablishDate))

# Check to see if a date falls between two dates, ignoring years. Returns a logical value
# This is inclusive (e.g. if earliest and date both are "01-01-2018", it'll return TRUE)
check.season <- function(date, earliest, latest) {
  early.day <- lubridate::yday(lubridate::as_date(earliest))
  late.day <- lubridate::yday(lubridate::as_date(latest))
  day <- lubridate::yday(lubridate::as_date(date))
  output <- early.day <= day & day <= late.day
  return(output)
}

# Dates of seasonal use
dates <- list(lekking = c(earliest = "01-03-2013",
                          latest = "15-05-2013"),
              nesting = c(earliest = "01-04-2013",
                          latest = "30-06-2013"),
              spring = c(earliest = "01-04-2013",
                          latest = "30-06-2013"),
              summer = c(earliest = "15-05-2013",
                         latest = "15-09-2013"),
              brood.early = c(earliest = "15-05-2013",
                          latest = "15-06-2013"),
              brood.late = c(earliest = "15-06-2013",
                          latest = "15-09-2013"),
              # Winter wraps around, so we'll do it in two steps
              winter.1 = c(earliest = "01-11-2013",
                          latest = "31-12-2013"),
              winter.2 = c(earliest = "01-01-2013",
                           latest = "28-02-2013"))

# For each of the date windows in the list dates, run check.season() on all of the dates in the dataframe
sampling.seasons <- lapply(dates,
                           FUN = function(X, visit.dates){
                             sapply(visit.dates,
                                    check.season,
                                    earliest = as.Date(X[1], "%d-%m-%Y"),
                                    latest = as.Date(X[2], "%d-%m-%Y"))
                           },
                           visit.dates = metadata$EstablishDate)
# If either of the winter windows was TRUE, then it was in winter use sampling time
sampling.seasons[["winter"]] <- sampling.seasons$winter.1 | sampling.seasons$winter.2

# Add these to metadata!
metadata <- cbind(metadata, dplyr::select(as.data.frame(sampling.seasons), -winter.1, -winter.2))

#### Assigning precip regime ####
## Get the 30 year normals for precip in mm read in
prism <- raster::raster("C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/PRISM_ppt_30yr_normal_4kmM2_annual_asc.asc")

## I need this as a spatial points data frame but I'm running into issues with sp::SpatialPointsDataFrame()
# so I'm writing it out as a .csv and using Arc to convert it into a shapefile then reading that in
write.csv(metadata, "C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/metadata.csv")

# Read it back in
metadata.spdf <- rgdal::readOGR(dsn = "C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data",
                                layer = "metadata",
                                stringsAsFactors = FALSE)
# Read in the GRSG polygons too
grsg.spdf <- rgdal::readOGR(dsn = "C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data",
                                layer = "Patterson Pass Sage Grouse Habitat Improvement Project",
                                stringsAsFactors = FALSE)
# Add the GRSG information to the metadata
metadata <- cbind(metadata, sp::over(x = metadata.spdf, y = sp::spTransform(grsg.spdf, metadata.spdf@proj4string)))

# Now we need to know which were sampled pre-treatment
metadata$relative.sampling[sapply(as.numeric(stringr::str_extract(metadata$Season, pattern = "^\\d{4}")) < as.numeric(stringr::str_extract(metadata$Year_Comp, pattern = "\\d{4}$")), isTRUE)] <- "Pre-treatment"
metadata$relative.sampling[sapply(as.numeric(stringr::str_extract(metadata$Season, pattern = "^\\d{4}")) > as.numeric(stringr::str_extract(metadata$Year_Comp, pattern = "\\d{4}$")), isTRUE)] <- "Post-treatment"
    

## Extract the raster value for each point and write it into the metadata dataframe in a variable called prism.precip.mm
metadata$prism.precip.mm <- raster::extract(x = prism, y = metadata.spdf)

## Define the precip regimes by their ranges (taken from page 79 of the HAF)
precip <- list(arid = c(least = 250, greatest = 300), mesic = c(least = 300.0000001, greatest = Inf))

## Assign assign precip regime to the points based on the prism.precip.mm values
for (regime in names(precip)) {
  metadata$precip.regime[metadata$prism.precip.mm >= precip[[regime]][1] & metadata$prism.precip.mm <= precip[[regime]][2]] <- regime
}


#### BENCHMARK ####
# Read in the benchmarks
# benchmarks <- readxl::read_excel("C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/AIMTerrestrialBenchmarkTool_rem429_20171031.xlsx",
#                                  sheet = "Monitoring Objectives",
#                                  range = "A1:N22")

benchmarks <- read.csv("C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/final/data/benchmark.csv",
                       stringsAsFactors = FALSE)

# Combine the metadata and LPI indicator values
combined.data <- merge(x = metadata,
                       y = lpi,
                       by = "PlotKey")
combined.data <- dplyr::filter(combined.data, HitCategory == "Any Hit")
combined.data$HitPctAvg <- 100*combined.data$HitPctAvg

combined.data$relative.sampling[sapply(combined.data$year < as.numeric(stringr::str_extract(combined.data$Year_Comp, pattern = "\\d{4}$")), isTRUE)] <- "Pre-treatment"
combined.data$relative.sampling[sapply(combined.data$year > as.numeric(stringr::str_extract(combined.data$Year_Comp, pattern = "\\d{4}$")), isTRUE)] <- "Post-treatment"


# BENCHMARK
# This happens in two steps: once for benchmarks that care about precip regime and once for those that don't
benchmarked.points.list <- lapply(1:nrow(benchmarks),
                            FUN = function(X, data, benchmarks){
                              # Assign the names back to this vector from the data frame
                              X <- benchmarks[X,]
                              if (X$indicator == "") {
                                return(NULL)
                              }
                              
                              # Get just the subset of data for the current indicator
                              relevant.data <- dplyr::filter(combined.data,
                                                             Indicator == X$indicator)
                              # For the duration, season, and precip regime, filter as appropriate
                              # if (X$Duration != "All") {
                                relevant.data <- relevant.data[relevant.data$Duration == X$Duration,]
                              # }
                              if (X$season != "any") {
                                relevant.data <- relevant.data[relevant.data[[X$season]],]
                              }
                              if (X$precip.regime != "any") {
                                relevant.data <- relevant.data[relevant.data$precip.regime == X$precip.regime,]
                              }
                              
                              if (nrow(relevant.data) < 1) {
                                return(NULL)
                              }
                              ## Check to see if the lower limit expression is met, the upper is met, and then that both are
                              # Build the inequality expressions and evaluate them
                              ll.expr <- paste0(paste0(X$Lower.Limit, X$LL.Relation), relevant.data$HitPctAvg)
                              lower.satisfied <- sapply(trimws(ll.expr),
                                                        FUN = function(X){eval(parse(text = X))})
                              ul.expr <- paste0(relevant.data$HitPctAvg, paste0(X$UL.Relation, X$Upper.Limit))
                              upper.satisfied <- sapply(trimws(ul.expr),
                                                        FUN = function(X){eval(parse(text = X))})
                              # Check to see if both are satisfied
                              satisfied <- mapply(lower.satisfied,
                                                  upper.satisfied,
                                                  FUN = all)
                              
                              # Return the rows where they were both satisfied, adding in some stuff from the benchmark table!
                              output <- cbind(relevant.data[satisfied,],
                                              category = X$Condition.Category,
                                              indicator.name = X$Indicator,
                                              season = X$season)
                              
                              return(output)
                            },
                            data = combined.data,
                            benchmarks = benchmarks)

# Turn that list into a single data frame
benchmarked.points.df <- dplyr::bind_rows(benchmarked.points.list)

#### PLOT ####
# Summarizing, not really necessary
benchmarked.points.summarized <- dplyr::summarize(.data = dplyr::group_by(benchmarked.points.df,
                                                                          category,
                                                                          indicator.name,
                                                                          precip.regime,
                                                                          season),
                                                  count = n())

# Make a figure
ggplot(dplyr::filter(benchmarked.points.df, !is.na(relative.sampling), !is.na(Treatment))) +
  geom_bar(aes(x = indicator.name,
               fill = category),
           position = "fill") +
  # This is where we should tweak coloration
  scale_fill_brewer() +
  coord_flip() +
  # Faceting can be on two dimensions, if we want to make a grid that's season ~ precip or something
  facet_grid(Treatment ~ relative.sampling) +
  labs(fill = "Condition Category",
       y = "Proportion of plots",
       x = "Indicator")

ggplot(benchmarked.points.df[grepl(benchmarked.points.df$Season.x, pattern = "^2017"),]) +
  geom_bar(aes(x = indicator.name,
               fill = category),
           position = "fill") +
  # This is where we should tweak coloration
  scale_fill_brewer() +
  coord_flip() +
  # Faceting can be on two dimensions, if we want to make a grid that's season ~ precip or something
  facet_grid(~ Treatment) +
  labs(fill = "Condition Category",
       y = "Proportion of plots",
       x = "Indicator")

library(tidyverse)
summary.plot <- benchmarked.points.df %>% filter(!is.na(PlotKey), !is.na(Treatment)) %>%
  group_by(PlotKey, year, Treatment) %>% summarize(meeting.count = length(category[category == "Meeting"]),
                                        notmeeting.count = length(category[category == "Not Meeting"]),
                                        total = n())
summary.plot$class[(summary.plot$meeting.count / summary.plot$notmeeting.count < 0.5)] <- "Poor"
summary.plot$class[(summary.plot$meeting.count / summary.plot$notmeeting.count >= 0.5)] <- "Acceptable"

write.csv(summary.plot, "summary_by_plotkey.csv")

summary.treatment <- summary.plot %>% ungroup() %>%
  group_by(Treatment) %>% summarize(poor.count = length(class[class == "Poor"]),
                                   acceptable.count = length(class[class == "Acceptable"]),
                                   total = n())
write.csv(summary.plot, "summary_by_treatment.csv")
