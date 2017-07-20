# load libraries

library(ggplot2)
library(catspec)
library(gridExtra)
library(lcmm)

# look at our data

dat <- read.csv("lcmmBlogData.csv")

str(dat)
head(dat)

###################
# start exploring #
###################

# looking at everyones x plot #

p1 <- ggplot(dat, aes(x, y, group=id)) + 
  geom_line() + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", title = "One line per person, all subjects")

p2 <- ggplot(dat[dat$id %in% unique(dat$id)[1:300] & dat$totobs>5,], aes(x, y, group=id)) + 
  geom_line() + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", title = "Just 300 random subjects with >5 total obs")

p3 <- ggplot(dat[dat$id %in% unique(dat$id)[1:300] & dat$totobs>5,], aes(x, y, group=id)) + 
  geom_smooth(aes(group=id), method="lm", se=F) + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x",y = "y", title = "Just 300 random subjects with >5 total obs\n straight line per person")

p4 <- ggplot(dat[dat$id %in% unique(dat$id)[1:300] & dat$totobs>5,], aes(x, y, group=id)) +
  geom_smooth(aes(group=id), method="loess", se=F) + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", title="Just 300 random subjects with >5 total obs\n smoothed line per person")

grid.arrange(p1, p2, p3, p4, nrow=2)

# smoothing out the plots #
# dat$id <- as.character(dat$id)

p1 <- ggplot(dat[dat$totobs==58,], aes(x, y, group=id, colour=id)) +
  geom_line() + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", title = "Raw") + 
  theme(legend.position="none")

p2 <- ggplot(dat[dat$totobs==58,], aes(x, y, group=id, colour=id)) + 
  geom_smooth(aes(group=id), method="loess", se=F) + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x="x" , y="y", title="Smoothed") +
  theme(legend.position="none")

p3 <- ggplot(dat[dat$totobs==58,], aes(x, y, group=id, colour=id)) + 
  geom_smooth(aes(group=id), method="loess", se=F, size=2) +
  geom_line(size=1) + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", title = "Combined") + 
  theme(legend.position="none")

grid.arrange(p1, p2, p3, ncol=3, top = "Three subjects with 58 observations" )

# latent class model

dummy <- dat[dat$x0<(160.8)  & dat$x0>(67)  & dat$rf3 %in% c("95-97","98-00") & dat$rf4 > 9 & dat$totobs>4, ] # & dat$totobs>30 & dat$totobs<36
dummy <- dummy[!dummy$id %in% names(which(table(dummy$id)<4)),]
dummy$id <- as.factor(dummy$id)
length(unique(dummy$id))
summary(as.vector(table(dummy$id)))

###
# 2 Groups
d2 <- lcmm(y ~ x, random = ~x, subject = 'id', mixture = ~x, ng = 2, idiag = TRUE, data = dummy, link="linear")
summary(d2)
postprob(d2) 

# look at the post probs closer
round(summary(as.numeric(d2$pprob[d2$pprob[,"class"] == 1, "prob1"])), 2)
round(summary(as.numeric(d2$pprob[d2$pprob[,"class"] == 2, "prob2"])), 2)

# pull out who is in which class
people2 <- as.data.frame(d2$pprob[,1:2])
dummy$group2 <- factor(people2$class[sapply(as.numeric(dummy$id), function(x) which(people2$id == x))])

# plot check
p1 <- ggplot(dummy, aes(x, y, group = id, colour = group2)) + 
  geom_line() + 
  geom_smooth(aes(group=group2), method = "loess", size = 2, se = F)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", colour = "Latent Class", title = "Raw")

p2 <- ggplot(dummy, aes(x, y, group = id, colour = group2)) + 
  geom_smooth(aes(group = id, colour = group2),size = 0.5, se = F) + 
  geom_smooth(aes(group = group2), method = "loess", size = 2, se = T)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x",y = "y",colour = "Latent Class", title = "Smoothed") +
  theme(legend.position = "none")

grid.arrange(p1, p2, ncol = 2, top = "2 Latent Classes")

###
# 3 Groups
d3 <- lcmm(y ~ x, random = ~x, subject = 'id', mixture = ~x, ng = 3, idiag = TRUE, data = dummy, link = "linear")
summary(d3)
postprob(d3)
summary(as.numeric(d3$pprob[d3$pprob[,"class"] == 1, "prob1"]))
summary(as.numeric(d3$pprob[d3$pprob[,"class"] == 2, "prob2"]))
summary(as.numeric(d3$pprob[d3$pprob[,"class"] == 3, "prob3"]))

people3 <- as.data.frame(d3$pprob[,1:2])
dummy$group3 <- factor(people3$class[sapply(as.numeric(dummy$id), function(x) which(people3$id == x))])

# plot check
p1 <- ggplot(dummy, aes(x, y, group=id, colour=group3)) + 
  geom_line() + 
  geom_smooth(aes(group=group3), method="loess", size=2, se=F)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x="x",y="y",colour="Latent Class", title="Raw")

p2 <- ggplot(dummy, aes(x, y, group=id, colour=group3)) +
  geom_smooth(aes(group=id, colour=group3),size=0.5, se=F) +
  geom_smooth(aes(group=group3), method="loess", size=2, se=T) + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x="x",y="y",colour="Latent Class", title="Smoothed") +
  theme(legend.position="none")

grid.arrange(p1, p2, ncol = 2, top = "3 Latent Classes")

###
# 4 Groups

d4 <- lcmm(y ~ x, random = ~x, subject = 'id', mixture = ~x, ng = 4, idiag = TRUE, data = dummy, link = "linear")
summary(d4)
postprob(d4)
summary(as.numeric(d4$pprob[d4$pprob[,"class"]==1,"prob1"]))
summary(as.numeric(d4$pprob[d4$pprob[,"class"]==2,"prob2"]))
summary(as.numeric(d4$pprob[d4$pprob[,"class"]==3,"prob3"]))
summary(as.numeric(d4$pprob[d4$pprob[,"class"]==4,"prob4"]))

people4 <- as.data.frame(d4$pprob[,1:2])
dummy$group4 <- factor(people4$class[sapply(as.numeric(dummy$id), function(x) which(people4$id == x))])

# plot check
p1 <- ggplot(dummy, aes(x, y, group = id, colour = group4)) + 
  geom_line() + 
  geom_smooth(aes(group=group4), method = "loess", size = 2, se = F) + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", colour = "Latent Class", title = "Raw")
p2 <- ggplot(dummy, aes(x, y, group = id, colour = group4)) + 
  geom_smooth(aes(group = id, colour = group4),size = 0.5, se = F) + 
  geom_smooth(aes(group = group4), method = "loess", size = 2, se = T)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", colour = "Latent Class", title = "Smoothed") +
  theme(legend.position="none")

grid.arrange(p1, p2, ncol = 2, top = "4 Latent Classes")

# trying with 3 groups - higher term #
dummy$x2 <- dummy$x^2
d32 <- lcmm(y ~ x + x2, random = ~x + x2, subject = 'id', mixture = ~x + x2, ng = 3, idiag = TRUE, data = dummy, link = "linear")
summary(d32)
postprob(d32)
summary(as.numeric(d32$pprob[d32$pprob[,"class"]==1,"prob1"]))
summary(as.numeric(d32$pprob[d32$pprob[,"class"]==2,"prob2"]))
summary(as.numeric(d32$pprob[d32$pprob[,"class"]==3,"prob3"]))

people32 <- as.data.frame(d32$pprob[,1:2])
dummy$group32 <- factor(people32$class[sapply(as.numeric(dummy$id), function(x) which(people32$id == x))])

# plot check

p1 <- ggplot(dummy, aes(x, y, group = id, colour = group32)) + 
  geom_line() + 
  geom_smooth(aes(group = group32), method = "loess", size = 2, se = F)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", colour = "Latent Class", title = "Raw")
p2 <- ggplot(dummy, aes(x, y, group = id, colour = group32)) + 
  geom_smooth(aes(group = id, colour = group32), size = 0.5, se = F) + 
  geom_smooth(aes(group=group32), method = "loess", size = 2, se = T)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", colour = "Latent Class")

grid.arrange(p1,p2, ncol = 2, top = "3 Latent Classes - higher order term")
