gg2WayMosaic <- function(dfInput, air=0) {
  # This function takes as input a data frame with two columns
  # Each column should be of class 'factor' and it is probably wise that they should be 'ordered'
  # By default, the first column will be represented along the x-axis and the second along the y-axis
  # The function only produces 2-way mosaic plots (i.e. not n-way mosaic plots).
  # The function returns a ggplot object, which means the plot can be manipulated further as per usual
  # Read more about Mosiac plots at e.g. http://www.math.yorku.ca/SCS/sugi/sugi17-paper.html
  
  # Example:
  # testData <- data.frame(
  #                   x = sample(c("A", "B", "C", "C", "C"), 200, replace=TRUE), 
  #                   y = sample(c("i", "j", "k", "l", "m"), 200, replace=TRUE)
  #             )
  # gg2WayMosaic(testData, air=0.005)
  
  require(ggplot2)
  require(dplyr)
  
  # Check that valid data was received .. or stop execution
  if (!is.data.frame(dfInput)) stop("plotTwoWayMosaic expects a dfInputframe")
  if (nrow(dfInput)<1) stop("plotTwoWayMosaic expects a dfInputframe with dfInput")
  if (ncol(dfInput)>2) dfInput <- dfInput[,1:2]
  colnames(dfInput) <- c("x", "y")
  if (!("factor" %in% class(dfInput$x))) stop("plotTwoWayMosaic expects a dfInputframe with two columns of class 'factor")
  if (!("factor" %in% class(dfInput$y))) stop("plotTwoWayMosaic expects a dfInputframe with two columns of class 'factor")
  dfInput <- na.omit(dfInput) # Throw away any NA's
  
  # Create a table where each row represents location and dimensions of each box in the mosaic plot.
  
  # First calculate the absolute frequency of each combination of the x and y categorical variables
  # This is similar to the 'table' function, but note that 'count' produces a long data frame with new column 'n'
  dfResult <- count(dfInput, x, y) 
  
  
  # Now calculate the width (w) and position (xmin , xmax) of each column (x values), as:
  # width = the relative frequency of each x-value .. i.e. if there are 100 observations and 20 of those 
  #         have x="A", the column 'A' should have a width of 0.2
  # position = the cumulutive width of the columns
  colDimensions <- dfResult %>% 
    group_by(x) %>% 
    summarise(w=sum(n)/sum(dfResult$n)) %>% 
    mutate(xmin = cumsum(w)-w) %>% 
    mutate(xmax = xmin + w)
  
  # Now merge the new data on column dimensions (width and x-position) into the table of absolute frequencies
  dfResult <- left_join(dfResult, colDimensions, by=c("x"))
  
  # And now calculate the height (h) and position (ymin , ymax) of each x*y combination
  # After this the dfResult data frame will contain all the data needed to plot all the rectangles
  dfResult <- dfResult %>% group_by(x) %>% mutate(h = n/sum(n)) %>% mutate(ymin=cumsum(h)-h) %>% mutate(ymax = ymin + h)
  
  # Where to place x-axis breaks? .. The x-variable should be placed under the corresponding column of rectangles
  # and in the middel of the rectangle
  xBreaks <- dfResult %>% select(xmin, w, x) %>% unique() %>% mutate(breaks = xmin+0.5*w) %>% select(breaks, x)
  
  # Now simply create a ggplot .. making sure to add/subtract a little 'air' between rectangles
  plot <- ggplot() +
    geom_rect(data=dfResult, mapping=aes(xmin=xmin+air, xmax=xmax-air, ymin=ymin+air, ymax=ymax-air, fill=factor(y)), color="black", alpha=1) +
    scale_x_continuous(limits=c(0,1), breaks=xBreaks$breaks, labels = xBreaks$x) +
    labs(title="Mosaic", x="Var x", y="Relative frequency", fill="Var y")
  
  return(plot)
  
}