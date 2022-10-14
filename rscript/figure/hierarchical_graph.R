library(collapsibleTree) 

# input data must be a nested data frame:
head(warpbreaks)

# Represent this tree:
p <- collapsibleTree( warpbreaks, 
                      c("wool", "tension", "breaks"))

Total <- incident_ready %>% 
  select(region, lhb,category,nature)

p <- collapsibleTree( Total, 
                      c("region","lhb","category","nature"))
p



widgetToPng <- function(widget, file = "widget.pdf",  ...) {
  temp <- tempfile(fileext = ".html")
  file <- R.utils::getAbsolutePath(file)
  htmlwidgets::saveWidget(widget, temp)
  webshot(
    temp, file,
    selector = "#htmlwidget_container",
    zoom = 2,
    delay = 0.5,
    ...
  )
}

widgetToPng(
  collapsibleTreeSummary(warpbreaks, c("wool", "tension", "breaks"), 
                         maxPercent = 50, collapsed = FALSE),
  "collapsibleTree.png"
)
