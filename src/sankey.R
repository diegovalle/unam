# On the left are all the students who applied to the UNAM, by major. 
# On the right all those who gained admittance (June 2011 - June 2013)

ddply(subset(unam, accepted == "A"), .(year(date)), summarise,
      sum = length(area))
ddply(unam, .(year(date)), summarise,
      sum = length(area))


t <- ddply(unam, .(major, cu), summarise,
           sum = length(area))
t2 <- ddply(subset(unam, accepted == "A"), .(cu,major), summarise,
            sum = length(area))
names(t) <- c("source", "target", "value")
names(t2) <- c("source", "target", "value")
t2$target <- str_c(t2$target, "(accepted)")
t3 <- rbind(t2,t)


sankeyPlot <- rCharts$new()
##with this simplification
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
#sankeyPlot$setTemplate(script = "layouts/chart.html")
sankeyPlot$set(
  data = t3,
  nodeWidth = 10,
  nodePadding = 10,
  layout = 1,
  width = 760,
  height = 1500,
  units = "persons",
  title = "UNAM"
)
sankeyPlot$save(file.path("..", "html", "major-major.html"))


# 
# On the left are all the students who applied to the UNAM, by area of study. 
# On the right all those who gained admittance (June 2011 - June 2013)

t <- ddply(unam, .(area, cu), summarise,
           sum = length(area))
t2 <- ddply(subset(unam, accepted == "A"), .(cu, area), summarise,
            sum = length(area))
names(t) <- c("source", "target", "value")
names(t2) <- c("source", "target", "value")
t2$target <- str_c(t2$target, " (accepted)")
t3 <- rbind(t,t2)

# plot(
#   gvisSankey(t3, from="source", 
#              to="target", weight="value",
#              options=list(
#                height=550,
#                width=560,
#                sankey="{link:{color:{fill:'lightblue'}}}"
#              ))
# )

sankeyPlot <- rCharts$new()
##with this simplification
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
#sankeyPlot$setTemplate(script = "layouts/chart.html")
sankeyPlot$set(
  data = t3,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 560,
  height = 500,
  units = "",
  title = "UNAM"
)
sankeyPlot$save(file.path("..", "html", "area-area.html"))

# To enter the UNAM all students must declare a major, based on this choice they then 
# take an admission exam in one of four basic areas. This is a flow chart of majors chosen 
# by the students who passed the admission exam (June 2011 - June 2013). 
# The leftmost nodes area the areas of study of the admission exam, 
# then the campus locations (note that CU is itself divided into faculties) 
# and then the majors
t <- ddply(subset(unam, accepted == "A"), .(area, cu), summarise,
           sum = length(area))
t2 <- ddply(subset(unam, accepted == "A" & cu == "CU"), .(cu, faculty), summarise,
            sum = length(area))
t3 <- ddply(subset(unam, accepted == "A"), .(faculty, major), summarise,
            sum = length(area))
names(t) <- c("source", "target", "value")
names(t2) <- c("source", "target", "value")
names(t3) <- c("source", "target", "value")
t4 <- rbind(t,t2)
t4 <- rbind(t4,t3)




S <- gvisSankey(t4, from="source", 
                to="target", weight="value",
                options=list(
                  height=2050,
                  width=960,
                  sankey="{link:{color:{fill:'lightblue', stroke: '#999', strokeWidth: .2, fillOpacity: 0.5 }}}"
                ))

print(S, "html", file = file.path("..", "html", "all-unam.html"))
# 
# sankeyPlot <- rCharts$new()
# ##with this simplification
# sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
# #sankeyPlot$setTemplate(script = "layouts/chart.html")
# sankeyPlot$set(
#   data = t4,
#   nodeWidth = 15,
#   nodePadding = 10,
#   layout = 32,
#   width = 960,
#   height = 500,
#   units = "",
#   title = "UNAM"
# )
# sankeyPlot$save("all.html")


#The size of a square is proportional to the number of students admitted, 
#the greener the color the higher the median score. The data corresponds to 
#all who applied to take the test from June 2011 to June 2013. 
#Click on a square to view the data by major.
t <- ddply(subset(unam, accepted == "A"), .(major, cu), summarise,
           sum = length(area), median = median(score))
t$major <- str_c(t$major, " - ", t$median, " - ", t$cu)
t <- rbind(t, data.frame(major = ddply(t, .(cu), summarise,
                                       sum = sum(sum))$cu,
           cu = "Admitted",
           sum= ddply(t, .(cu), summarise,
                      sum = sum(sum))$sum,
           median = ddply(subset(unam, accepted == "A"), .(cu), summarise,
                          median = median(score))$median))
t <- rbind(t, data.frame(major = "Admitted",
                         sum = nrow(subset(unam, accepted == "A")),
                         cu = NA,
                         median = NA))

colors <- brewer.pal(9, "Greens")


Tree <- gvisTreeMap(t, idvar="major", parentvar="cu",
                    sizevar="sum", colorvar="median",
                    options = list(width=960, height=500,
                                   minColor = colors[1], 
                                   midColor = colors[5],
                                   maxColor = colors[9],
                                   showScale = TRUE))
print(Tree, "chart", file = file.path("..", "html", "treemap-unam.html"))
