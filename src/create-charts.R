library(ggplot2)
library(zoo)
library(stringr)
library(plyr)
options(stringsAsFactors = FALSE)
library(directlabels)
theme_set(theme_bw())

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

unam <- read.csv("../clean-data/unam-admission.csv")
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
                 "CENTRO PENINSULAR EN HUMANIDADES Y CIENCIAS SOCIALES"))

meds <- ddply(subset(unam, 
             accepted == "A"),
      .(major),
      summarise,
      score = median(score, na.rm = TRUE))
meds[order(-meds$score),]

p <- ggplot(subset(unam, major %in% c("LITERATURA DRAMATICA Y TEATRO",
                                 "MEDICO CIRUJANO",
                                 "INGENIERIA MECATRONICA",
                                 "RELACIONES INTERNACIONALES")&
             accepted == "A"),
       aes(date, score, group = major, color = major)) +
  stat_summary(fun.y = median, geom = "line")


meds <- ddply(subset(unam, 
             accepted == "A"),
      .(major, area, date),
      summarise,
      score = median(score, na.rm = TRUE))
meds <- ddply(meds, .(major, area), function (df) {
  df$score <- c(NA, diff(df$score))
  df})
ggplot(na.omit(meds), aes(date,score, group = major)) +
  geom_line() +
  facet_wrap(~area)

ggplot(subset(unam, accepted == "A"),
       aes(date, score, group = major, color = major)) +
  stat_summary(fun.y = median, geom = "line")
direct.label(p, "last.bumpup")

ggplot(subset(unam, area == "Ciencias Físico-Matemáticas y las Ingenierías" &
              accepted == "A"),
       aes(reorder(faculty, score, median), score)) +
  geom_jitter(alpha = .5) +
  geom_boxplot(fill = "transparent", color = "red") +
  ##stat_summary(fun.data = median_cl_boot, color = "red",
  ##             geom = "crossbar")+
  coord_flip() 


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
    stat_summary(fun.data = median_cl_boot, alpha=.8,
                   color = "red", geom = "linerange") +
    stat_summary(fun.data = median_cl_boot, color = "black",
                 geom = "point", show_guide = FALSE) +
    coord_flip() +
    guides(color = guide_legend("individual scores",
             override.aes = list(alpha = 1)),
           shape = guide_legend("median score (95% CI)",
             override.aes = list(alpha = 1))) +
    scale_shape_manual(values= shapes)
}

plotMajors(unam, levels(as.factor(unam$area))[1], c(15:20,0:10))
plotMajors(unam, levels(as.factor(unam$area))[2], c(15:18,0:10))
plotMajors(unam, levels(as.factor(unam$area))[3], c(15:18,20,0:10))
plotMajors(unam, levels(as.factor(unam$area))[4], c(0:1, 15, 2:10))


ggplot(unam, aes(score, group = accepted, fill = accepted, color = accepted)) +
  geom_histogram(binwidth = 5, alpha= .6) +
  facet_wrap(~ area)

max(unam[unam$accepted == "A","score"])
sd(unam[unam$accepted == "A","score"])
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
  coord_flip()
