library(ggplot2)
library(zoo)
library(stringr)
library(plyr)
options(stringsAsFactors = FALSE)
library(directlabels)
library(scales)
library(gridExtra)
library(car)
##require(devtools)
##install_github('rCharts', 'ramnathv')
library(rCharts)
theme_set(theme_bw())


addSource <- function(plot, text = "Data Source: Dirección General de Administración Escolar - UNAM") {
  plot <- arrangeGrob(plot, 
              sub = textGrob(text,
                x = 0, hjust = -0.1, vjust=0.1,
                gp = gpar(fontface = "italic", fontsize = 9,
                  col = "gray50")))
  return(plot)
}

saveChart <- function(p, filename, width = 9.60, height = 6.00) {
  ggsave(filename = filename, plot = p, dpi = 100,
         width = width, height = height)
}


median_cl_boot <- function(x, conf.int = 0.95, B = 1000, na.rm = TRUE, reps = FALSE) {
    if (na.rm)
        x <- x[!is.na(x)]
    n <- length(x)
    xm <- median(x)
    if (n < 2)
        return(data.frame(y = xm, ymin = NA, ymax = NA))
    resamples <- lapply(1:B, function(i) sample(x, replace=T))
    r.median <- sapply(resamples, median)
    quant <- quantile(unlist(r.median),
                      c((1 - conf.int)/2, (1 + conf.int)/2))
    names(quant) <- NULL
    Median <- median(x)
    data.frame(y = Median,
               ymin = quant[1],
               ymax = quant[2])
}



plotMajors <- function(unam, area_sub, shapes = c(15:18,0:10), palette) {
  df <- subset(unam, area == area_sub &
              accepted == "A")
  df <- ddply(df, .(major, faculty), transform,
              median = median(score))
  df <- ddply(df, .(major), transform, median = max(median))
  df$faculty <- with(df, reorder(faculty, -score, mean))
  df$major <- with(df, reorder(major, median))
  
  ggplot(df,
         aes(major, score, color = cu, 
             group = faculty, shape = faculty)) +
    geom_jitter(alpha = .8,) +
    stat_summary(fun.data = median_cl_boot, alpha=1,
                   color = "red", geom = "linerange") +
    stat_summary(fun.data = median_cl_boot, color = "black",
                 geom = "point", show_guide = FALSE) +
    coord_flip() +
    guides(color = guide_legend("individual scores,\nby campus",
             override.aes = list(alpha = 1)),
           shape = guide_legend("median score (95% CI),\nby faculty/campus",
             override.aes = list(alpha = 1))) +
    scale_shape_manual(values= shapes) +
    scale_colour_manual(values = palette) +
        scale_fill_manual(values = palette) +
    labs(title =str_c("Admission Scores - ", area_sub, " (Jun 2011-Jun 2013)")) +
    ylab("exam score")
}

plotFaculties<- function(unam, area_sub) {
  df <- subset(unam, area == area_sub &
              accepted == "A")
  ggplot(df, aes(reorder(faculty, score, median), score)) +
    geom_jitter(alpha = .3) +
    geom_boxplot(fill = "transparent", color = "red") +
    ##stat_summary(fun.data = median_cl_boot, color = "red",
    ##             geom = "crossbar", show_guide = FALSE) +
    coord_flip() +
    labs(title = str_c("Admission Scores - ", area_sub, " (Jun 2011‐Feb 2013)")) +
    ylab("exam score") +
    xlab("faculty")
}

addSave <- function(p, filename, width = 9.60, height = 6.00,
                    text = "Data Source: Dirección General de Administración Escolar - UNAM") {
  saveChart(addSource(p, text),
            file.path("..", "graphs", filename),
            width, height)
}

isCU <- function(vec, asbool = FALSE) {
  ifelse(str_detect(vec, "FACULTAD") |
         vec == "ESCUELA NACIONAL DE TRABAJO SOCIAL",
         "CU",
         if(asbool)
           "Not CU"
         else
           vec)
}


##Load and clean the unam admission scores database
unam <- read.csv("../clean-data/unam-admission.csv",
                 stringsAsFactors = FALSE)
## salaries <- read.csv("../data/salaries.csv", skip = 1)
unam$faculty <- str_replace(unam$faculty, "\\([0-9]*\\)  ", "")
unam$major <- str_replace(unam$major, "\\([0-9]*\\)  ", "")
unam$date <- as.Date(as.yearmon(as.character(unam$date), "%b %Y"))
unam$score <- as.numeric(unam$score)
unam$cu <- isCU(unam$faculty)
ddply(unam, .(date), summarise, sum = length(date))





unam <- subset(unam,
               !faculty %in%
               c("U.  MULT.  DE DOCEN.  E INV. DE LA FAC. DE C., QRO.",
                 "UMDI, JURIQUILLA, QRO.",
                 "ESCUELA NACIONAL DE ESTUDIOS SUPERIORES UNIDAD LEON",
                 "ESCUELA NACIONAL DE ESTUDIOS SUP. UNIDAD LEON",
                 "ESCUELA NACIONAL DE ARTES PLASTICAS, PLANTEL TAXCO",
                 "ESCUELA NACIONAL DE ESTUDIOS SUP. UNIDAD MORELIA",
                 "CENTRO PENINSULAR EN HUMANIDADES Y CIENCIAS SOCIALES",
                 "PLANTEL TAXCO, GRO"))
unam$area <- car::recode(unam$area,
                    '"Biológicas, Químicas y de la Salud"="Biological Sciences and Health";
                    "Ciencias Físico-Matemáticas y las Ingenierías"="Physical Sciences, Mathematics and Engineering";
                    "Ciencias Sociales"="Social Sciences";
                    "Humanidades y Artes"="Humanities and Arts"')



meds <- ddply(subset(unam, 
                     accepted == "A"),
      .(major),
      summarise,
      score = mean(score, na.rm = TRUE))
meds[order(-meds$score),]

top.majors <-  c("ARTES VISUALES",
                                 "MEDICO CIRUJANO",
                                 "INGENIERIA MECATRONICA",
                                 "RELACIONES INTERNACIONALES")
exam.dates <- c("Jun 2011", "Feb 2012", "Jun 2012",
                     "Feb 2013", "Jun 2013")
p <- ggplot(subset(unam, major %in% top.majors &
             accepted == "A"),
       aes(date, score, group = major, color = major)) +
  stat_summary(fun.y = median, geom = "line") +
  scale_x_date(breaks = sort(unique(unam$date)),
                     labels = exam.dates) +
  ylab("median exam score")
addSave(p, "top-majors.svg")






meds <- ddply(subset(unam, 
             accepted == "A"),
      .(major, area, date),
      summarise,
      score = min(score, na.rm = TRUE))
meds <- ddply(meds, .(major, area), function (df) {
  df$score <- c(NA, diff(df$score))
  df})
p <- ggplot(na.omit(meds), aes(date,score, group = major)) +
  geom_line(alpha = .6) +
  facet_wrap(~area)+
  scale_x_date(breaks = sort(unique(na.omit(meds)$date)),
                     labels = c("Feb 2012", "Jun 2012",
                     "Feb 2013", "Jun 2013"))  
addSave(p, "change-in-trend.svg")
  

## ggplot(subset(unam, area == "Ciencias Físico-Matemáticas y las Ingenierías" &
##               accepted == "A"),
##        aes(reorder(faculty, score, median), score)) +
##   geom_jitter(alpha = .5) +
##   geom_boxplot(fill = "transparent", color = "red") +
##   ##stat_summary(fun.data = median_cl_boot, color = "red",
##   ##             geom = "crossbar")+
##   coord_flip() 


shapes <- list(c(15:20,0:10), c(0:1, 15, 2:10),
               c(15:18,0:10), c(15:18,20,0:10))
location.colors <- c("CU" = "#1F78B4",
             "ESCUELA NACIONAL DE ARTES PLASTICAS" = "#8DD3C7",
             "ESCUELA NACIONAL DE ENFERMERIA Y OBSTETRICIA" = "#BEBADA",
             "ESCUELA NACIONAL DE MUSICA" = "#FB8072",
             "FES ACATLAN" = "#80B1D3",
             "FES ARAGON" = "#FDB462", 
             "FES CUAUTITLAN" = "#B3DE69",
             "FES IZTACALA" = "#FCCDE5",
             "FES ZARAGOZA" = "#FFFFB3")
bio <- location.colors[c("CU",
                         "ESCUELA NACIONAL DE ENFERMERIA Y OBSTETRICIA",
                         "FES CUAUTITLAN",
                         "FES IZTACALA",
                         "FES ZARAGOZA"
                  )]
humanities <- location.colors[c("CU",
                                "ESCUELA NACIONAL DE ARTES PLASTICAS",
                                "ESCUELA NACIONAL DE MUSICA",             
                                "FES ACATLAN",
                                "FES ARAGON",
                                "FES CUAUTITLAN"
                  )]
stem <- location.colors[c("CU",
                          "FES ACATLAN",
                          "FES ARAGON",
                          "FES CUAUTITLAN",
                          "FES ZARAGOZA"
                  )]
ss <- location.colors[c("CU",
                          "FES ACATLAN",
                          "FES ARAGON",
                          "FES CUAUTITLAN"
                  )]
palettes <- list(bio, humanities,
                 stem, ss)
i <- 1
for(major in levels(as.factor(unam$area))){
  p <- plotMajors(unam, major, shapes[[i]], palettes[[i]])
  addSave(p, str_c(major, "-majors.svg"), 14, 7)
  i <- i + 1
}


for(area in levels(as.factor(unam$area))) {
  p <- plotFaculties(unam, area)
  addSave(p, str_c(area, "-faculty.svg"))
}

#plotFaculties(unam, levels(as.factor(unam$area))[1])

p <- ggplot(unam,
            aes(score, group = accepted,
                fill = accepted), color = "transparent") +
  geom_histogram(binwidth = 5, alpha= .5, position = "identity") +
  facet_wrap(~ area) +
  scale_fill_manual("exam\nresult",
                    breaks = c("A", "R"), values = c("blue", "red"),
                    labels = c("accepted", "rejected")) +
  labs(title = "Rejected and accepted students, by area of study (Jun 2011‐Feb 2013)")
addSave(p, "rejected-area.svg")

df <- ddply(unam, .(major, faculty, area), summarise,
            yield = length(accepted[accepted == "A"]) / length(accepted),
            median = min(score[accepted == "A"]))
## for(field in levels(as.factor(df$area))) {
##   p <- ggplot(subset(df, area == field), aes(yield, reorder(major, yield, max))) +
##     geom_point(aes(shape = faculty)) +
##     scale_x_continuous(labels = percent)
##   addSave(p, str_c(field, "-yield.png"), 14, 7)
## }
df$location <- isCU(df$faculty, TRUE)
p <- ggplot(df, aes(yield, median)) +
  geom_point(aes(color = location), size = 5) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), aes(group = location)) +
  facet_wrap(~area)+
  scale_x_continuous(labels = percent)
## g4.svg <- gridToSVG(file.path("..", "graphs", "plot1.svg"))

r1 <- rPlot(median ~ yield | area,
            data = df, type = "point", color = "location",
                tooltip="function(item){return item.major +'\n' + 'Location: ' + item.faculty + '\n' + 'Minimum score: ' + item.median + '\n' + 'Percent accepted: ' + Math.round(item.yield*1000)/10 + '%'}",
                title = "Enlace", size =  list(const = 3.5))
r1$facet(var = 'area', type = 'wrap', cols = 2)
r1$guides(x = list(title = "percent admitted",
                min = min(df$yield)-.03,
                max = max(df$yield)+.03),
              y = list(title = "minimum score",
                min = min(df$median)-5,
                max = max(df$median)+15))
df <- df[order(df$yield),]
df.loess <- ddply(df, .(area, location), summarise,
                  median = predict(lm(median ~ I(poly(yield,2)))),
                  yield = yield)
## df.loess <- df.loess[,c("loess", "yield", "location", "area")]
## names(df.loess)[1] <- "median"
r1$layer(data = subset(df.loess, location == "CU"), type = 'line', 
             color = list(const = 'black'), copy_layer = TRUE, tooltip = NULL,
             size = list(const = 2))
r1$layer(data = subset(df.loess, location != "CU"), type = 'line', 
             color = list(const = 'black'), copy_layer = TRUE, tooltip = NULL,
             size = list(const = 2))
r1$addParams(title = "")
r1$set(width = 600, height = 600)
r1

df <- unam
df$cu <- isCU(unam$faculty, TRUE)
p <- ggplot(ddply(df, .(date, cu), summarise,
             sum = length(date[accepted == "A"]) / length(date)),
       aes(date, sum, group = cu, color = cu)) +
  geom_line() +
  geom_point(size = 4) +
  scale_x_date(breaks = sort(unique(unam$date)),
               labels = exam.dates) +
  scale_y_continuous(labels = percent, limits = c(0,.15)) +
  labs(title = "Percentage admitted to the UNAM") +
  ylab("percent admitted")
addSave(p, "percent-admit.svg",
       width = 8, height = 5)


p <- ggplot(ddply(df, .(date, cu), summarise,
             sum =  median(score[accepted == "A"], na.rm = TRUE)),
       aes(date, sum, group = cu, color = cu)) +
  geom_line() +
  geom_point(size = 4) +
  scale_x_date(breaks = sort(unique(unam$date)),
               labels = exam.dates) +
  scale_y_continuous(labels = comma) +
  labs(title = "Median scores of those admitted to the UNAM") +
  ylab("median score")
addSave(p, "median-admit.svg")

demand <- ddply(unam, .(major, cu), summarise, sum = length(date))
demand[order(demand$sum),]
p <- ggplot(demand, aes(sum, reorder(major, sum, max), color = cu)) +
  geom_point(size = 4) +
  scale_color_manual("location", values = location.colors) +
  scale_x_continuous(label = comma) +
  xlab("number of applicants") +
  ylab("major") +
  labs(title = "Number of applicants at the UNAM (Jun 2011‐Feb 2013)")
addSave(p, "demand.svg",
        width = 12, height = 15)

## ing.mecatro <- subset(unam, major %in% c( "INGENIERIA MECATRONICA",
##                                           "INGENIERIA ELECTRICA Y ELECTRONICA",
##                                           "INGENIERIA MECANICA"))
## ing.mecatro <- ing.mecatro[str_detect(ing.mecatro$faculty, "FACULTAD"),]
## ggplot(ddply(ing.mecatro, .(date), summarise,
##              sum = length(date[accepted == "A"]) / length(date)),
##        aes(date, sum)) +
##   geom_line()+
##   scale_x_date(breaks = sort(unique(unam$date)),
##                labels = exam.dates) 

nrow(subset(unam, date == "2013-06-01"))


sd(unam[unam$accepted == "A" &
        unam$major == "RELACIONES INTERNACIONALES","score"])
max(unam[unam$accepted == "R","score"], na.rm = TRUE)


nrow(unam[unam$accepted == "R" & unam$score > 80, ])
nrow(unam[unam$accepted == "A" & unam$score > 80, ])


wasted <- subset(subset(unam, date == "2013-02-01"), (major == "INGENIERIA MECATRONICA") |
       (major == "INGENIERIA ELECTRICA Y ELECTRONICA" &
              accepted == "A") |
       (major == "INGENIERIA MECANICA" &
              accepted == "A") )
wasted <- wasted[str_detect(wasted$faculty, "FACULTAD"), ]
minimum <- min(wasted[wasted$accepted == "A" &
                      wasted$major == "INGENIERIA MECANICA","score"],
               na.rm = TRUE)
nrow(wasted[wasted$accepted == "R" & wasted$score > minimum  ,])
nrow(wasted[wasted$accepted == "A" ,])

p <- ggplot(wasted, aes(major, score, color = accepted)) +
  geom_jitter(alpha = .6) +
  ##stat_summary(fun.data = median_cl_boot, color = "red",
  ##             geom = "crossbar") +
  coord_flip() +
  scale_color_manual("exam\nresult", values = c("blue", "red"),
                     labels = c("accepted", "rejected")) +
  labs(title = "Accepted or Rejected Students")
addSave(p, "mecatronica.svg")



yield <- ddply(unam, .(major, accepted, date), summarise,
      total = length(accepted))
yield <- ddply(yield, .(major, date), summarise, yield = total[1]/ total[2])

tail(yield[order(-yield$yield), ], 20)
head(yield[order(-yield$yield), ])

p <- ggplot(subset(yield, major %in% top.majors),
       aes(date, yield, group = major, color = major)) +
  geom_line() +
  ylab("acceptance rate") +
  scale_y_continuous(labels = percent) +
  scale_x_date(breaks = sort(unique(yield$date)),
                     labels = exam.dates,
               limits = c(as.Date("2011-06-01"), as.Date("2014-08-01")))
p <- direct.label(p, "last.bumpup")
addSave(p, "percent-admitted-top.png")

allareas.a <- subset(unam, accepted == "A")
allareas.cu.a <- allareas.a[grep("FACULTAD",allareas.a$faculty),]
avscore <- function(str, df){
    mean(df[grep(str, df$major),]$score)
}
majors <- c("MEDICO", "INGENIERIA EN COMPUTACION",
            "DERECHO","MECATRONICA", "ARQUITECTO","INGENIERIA CIVIL",
            "INGENIERIA INDUSTRIAL","PSICOLOGIA","RELACIONES INT",
            "ADMINISTRACION","COMUNICACION","CONTADURIA" )
scores <- sapply(majors, avscore, allareas.cu.a)
salaries <- c(13364,12636,10969,10902,10870,10821,9747,9708,9704,9567,9372,8151)
ss <- data.frame(scores, log.salaries = log(salaries))
p <- ggplot(ss, aes(scores, log.salaries, label = rownames(ss))) +
    geom_point() +
    xlab("Number of correct answers") +
    #scale_y_log10() +
  ylab("log starting salary") +
    geom_smooth(method = lm) +
    geom_text(hjust=-0.05, angle = -50, size = 4) +
    coord_cartesian(xlim = c(80, 115)) +
    labs(title = "Starting Salary vs Entrance Exam Score") +
    theme_bw()
addSave(p, "score_vs_salary.png",
        text = "Data Source: Suplemento Universitarios Reforma 2008 / Dirección General de Administración Escolar - UNAM")
