library(ggplot2)
library(zoo)
library(stringr)
library(plyr)
options(stringsAsFactors = FALSE)
library(directlabels)
library(scales)
library(gridExtra)
library(car)
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



plotMajors <- function(unam, area_sub, shapes = c(15:18,0:10)) {
  df <- subset(unam, area == area_sub &
              accepted == "A")
  df <- ddply(df, .(major, faculty), transform,
              median = median(score))
  df <- ddply(df, .(major), transform, median = max(median))
  df$faculty <- with(df, reorder(faculty, -score, mean))
  df$major <- with(df, reorder(major, median))
  
  ggplot(df,
         aes(major, score, color = faculty,
             group = faculty, shape = faculty)) +
    geom_jitter(alpha = .8) +
    stat_summary(fun.data = median_cl_boot, alpha=1,
                   color = "red", geom = "linerange") +
    stat_summary(fun.data = median_cl_boot, color = "black",
                 geom = "point", show_guide = FALSE) +
    coord_flip() +
    guides(color = guide_legend("individual scores",
             override.aes = list(alpha = 1)),
           shape = guide_legend("median score (95% CI)",
             override.aes = list(alpha = 1))) +
    scale_shape_manual(values= shapes) +
    scale_colour_brewer(palette="Set3") +
    labs(title =str_c("Admission Scores - ", area_sub))+#, " (Jun 2011-Jun 2013)")) +
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

addSave <- function(p, filename, width = 9.60, height = 6.00) {
  saveChart(addSource(p), filename, width, height)
}


##Load and clean the unam admission scores database
unam <- read.csv("../clean-data/unam-admission.csv")
salaries <- read.csv("../data/salaries.csv", skip = 1)
unam$faculty <- str_replace(unam$faculty, "\\([0-9]*\\)  ", "")
unam$major <- str_replace(unam$major, "\\([0-9]*\\)  ", "")
unam$date <- as.Date(as.yearmon(as.character(unam$date), "%b %Y"))
unam$score <- as.numeric(unam$score)
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
unam$area <- recode(unam$area,
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

top.majors <-  c("LITERATURA DRAMATICA Y TEATRO",
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
p

meds <- ddply(subset(unam, 
             accepted == "A"),
      .(major, area, date),
      summarise,
      score = median(score, na.rm = TRUE))
meds <- ddply(meds, .(major, area), function (df) {
  df$score <- c(NA, diff(df$score))
  df})
ggplot(na.omit(meds), aes(date,score, group = major)) +
  geom_line(alpha = .6) +
  facet_wrap(~area)+
  scale_x_date(breaks = sort(unique(na.omit(meds)$date)),
                     labels = c("Feb 2012", "Jun 2012",
                     "Feb 2013", "Jun 2013")) 

ggplot(subset(unam, area == "Ciencias Físico-Matemáticas y las Ingenierías" &
              accepted == "A"),
       aes(reorder(faculty, score, median), score)) +
  geom_jitter(alpha = .5) +
  geom_boxplot(fill = "transparent", color = "red") +
  ##stat_summary(fun.data = median_cl_boot, color = "red",
  ##             geom = "crossbar")+
  coord_flip() 


shapes <- list(c(15:20,0:10), c(0:1, 15, 2:10),
               c(15:18,0:10), c(15:18,20,0:10))
i <- 1
for(major in levels(as.factor(unam$area))){
  p <- plotMajors(subset(unam, date == "2013-06-01"), major, shapes[[i]])
  addSave(p, str_c(major, "-majors.png"), 14, 7)
  i <- i + 1
}


for(area in levels(as.factor(unam$area))) {
  p <- plotFaculties(unam, area)
  addSave(p, str_c(area, "-faculty.png"))
}

plotFaculties(unam, levels(as.factor(unam$area))[1])

ggplot(unam, aes(score, group = accepted, fill = accepted, color = accepted)) +
  geom_histogram(binwidth = 5, alpha= .6) +
  facet_wrap(~ area)


df <- ddply(unam, .(major, faculty, area), summarise,
            yield = length(accepted[accepted == "A"]) / length(accepted),
            median = median(score[accepted == "A"]))
for(field in levels(as.factor(df$area))) {
  p <- ggplot(subset(df, area == field), aes(yield, reorder(major, yield, max))) +
    geom_point(aes(shape = faculty)) +
    scale_x_continuous(labels = percent)
  addSave(p, str_c(field, "-yield.png"), 14, 7)
}
ggplot(df, aes(yield, median)) +
  geom_point()


max(unam[unam$accepted == "A","score"])
sd(unam[unam$accepted == "A" &
        unam$major == "RELACIONES INTERNACIONALES","score"])
max(unam[unam$accepted == "R","score"], na.rm = TRUE)


nrow(unam[unam$accepted == "R" & unam$score > 80, ])
nrow(unam[unam$accepted == "A" & unam$score > 80, ])


wasted <- subset(unam, (major == "INGENIERIA MECATRONICA" &
              accepted == "R") |
       (major == "INGENIERIA ELECTRICA-ELECTRONICA" &
              accepted == "A") |
       (major == "INGENIERIA MECANICA" &
              accepted == "A"))
minimum <- min(wasted[wasted$accepted == "A" &
                      wasted$major == "INGENIERIA MECANICA","score"],
               na.rm = TRUE)
nrow(wasted[wasted$accepted == "R" & wasted$score > minimum  ,])
nrow(wasted[wasted$accepted == "A" ,])

ggplot(wasted, aes(major, score, color = accepted)) +
  geom_jitter() +
  ##stat_summary(fun.data = median_cl_boot, color = "red",
  ##             geom = "crossbar") +
  coord_flip() +
  scale_color_manual("result", values = c("blue", "red"),
                     labels = c("accepted", "rejected"))


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
               limits = c(as.Date("2011-06-01"), as.Date("2013-08-01")))
direct.label(p, "last.bumpup")

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
ggplot(ss, aes(scores, log.salaries, label = rownames(ss))) +
    geom_point() +
    ylab("Number of correct answers") +
    geom_smooth(method = lm) +
    geom_text(hjust=-0.05, angle = -40, size = 4) +
    coord_cartesian(xlim = c(75.5, 115)) +
    opts(title = "Starting Salary vs Entrance Exam Score") +
    theme_bw()
ggsave(file = "output/score_vs_salary.png",
         dpi=72, width=1.5, height=1.5, scale=4)
