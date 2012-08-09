dat <- data.frame(
    names = c("FS","JC","MC","NDK","UK","CB","EM","HL","AW","PH","SZ","TJ","ED","GZ"),
    height = c(185, 180, 170, 160, 155, 155, 155, 160, 170, 180, 185, 185, 178, 192),
    weight = c( 95, 101, 101, 101,  95,  80,  68,  62,  62,  62,  68,  74,  74,  74),
    age =    c(18, 15, 10,  5,  2,  2,  2,  2,  5, 10, 15, 18, 18, 18),
    memory = c(50, 60, 60, 60, 50, 40, 30, 20, 10, 10, 10, 20, 30, 50),
    sib = c(0,0,0,0,0,0,0,0,0,1,2,3,3.4,4),
    bfc = c(6,5,4.4,3,2.7,2,1,0,0,0,0,0,0,0),
    inf = c( 6, 4, 2.5, 0, 0, 0, 0, 0, 3.1, 4, 6, 6, 6, 6),
    dcs = c( 4e6, 5e6, 5e6, 5e6, 4e6, 3e6, 2e6, 1e6, 1e6, 1e6, 2e6, 3e6, 3.5e6, 4e6)
)

# blanks ####

g1 <- ggplot(dat, aes(x=height, y=weight)) + geom_blank() +
    labs(x="Height", y="Weight") + opts(title="Figure 1") +
    scale_x_continuous(limits=c(150,195), breaks=seq(145,195,5)) + 
    scale_y_continuous(breaks=seq(60,100,5))

g2 <- ggplot(dat, aes(x=age, y=memory)) + geom_blank() +
    labs(x="Age", y="Memory (Verbal)") + opts(title="Figure 2") +
    scale_x_continuous(limits=c(0,20), breaks=seq(0,20,2)) + 
    scale_y_continuous(breaks=seq(5,65,5))

g3 <- ggplot(dat, aes(x=sib, y=bfc)) + geom_blank() +
    labs(x="No. Siblings", y="No. Bone Fractures") + opts(title="Figure 3") +
    scale_x_continuous(limits=c(-0.5,4.5), breaks=seq(0,4,1)) 
    
g4 <- ggplot(dat, aes(x=inf, y=dcs)) + geom_blank() +
    labs(x="No. Previous Infections", y="No. Dendric Cells") + opts(title="Figure 4") +
    scale_x_continuous( limits=c(-0.5,6.5), breaks=seq(0,6,1)) 

png(paste0(Sys.Date(),"_blank_plain.png"), units="in", width=14, height=4, res=300)
grid.arrange(g1,g2,g3,g4, ncol=4, clip=T)
dev.off()

# with points ####
g1 <- g1 + geom_point()
g2 <- g2 + geom_point()
g3 <- g3 + geom_point()
g4 <- g4 + geom_point()

png(paste0(Sys.Date(),"_points_plain.png"), units="in", width=14, height=4, res=300)
grid.arrange(g1,g2,g3,g4, ncol=4, clip=T)
dev.off()

# with paths ####

g1 <- g1 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)
g2 <- g2 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)
g3 <- g3 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)
g4 <- g4 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)
    
png(paste0(Sys.Date(),"_points_path_plain.png"), units="in", width=14, height=4, res=300)
grid.arrange(g1,g2,g3,g4, ncol=4, clip=T)
dev.off()

dat

# jittering ####
set.seed(1234)
dat[,2:9] <- apply(dat[,2:9],2, function(x) round(jitter(x),1))

# blanks ####

g1 <- ggplot(dat, aes(x=height, y=weight)) + geom_blank() +
    labs(x="Height", y="Weight") + opts(title="Figure 1") +
    scale_x_continuous(limits=c(150,195), breaks=seq(145,195,5)) + 
    scale_y_continuous(breaks=seq(60,100,5))

g2 <- ggplot(dat, aes(x=age, y=memory)) + geom_blank() +
    labs(x="Age", y="Memory (Verbal)") + opts(title="Figure 2") +
    scale_x_continuous(limits=c(0,20), breaks=seq(0,20,2)) + 
    scale_y_continuous(breaks=seq(5,65,5))

g3 <- ggplot(dat, aes(x=sib, y=bfc)) + geom_blank() +
    labs(x="No. Siblings", y="No. Bone Fractures") + opts(title="Figure 3") +
    scale_x_continuous(limits=c(-0.5,4.5), breaks=seq(0,4,1)) 

g4 <- ggplot(dat, aes(x=inf, y=dcs)) + geom_blank() +
    labs(x="No. Previous Infections", y="No. Dendric Cells") + opts(title="Figure 4") +
    scale_x_continuous( limits=c(-0.5,6.5), breaks=seq(0,6,1)) 

png(paste0(Sys.Date(),"_blank_jitter.png"), units="in", width=14, height=4, res=300)
grid.arrange(g1,g2,g3,g4, ncol=4, clip=T)
dev.off()

# with points ####
g1 <- g1 + geom_point()
g2 <- g2 + geom_point()
g3 <- g3 + geom_point()
g4 <- g4 + geom_point()

png(paste0(Sys.Date(),"_points_jitter.png"), units="in", width=14, height=4, res=300)
grid.arrange(g1,g2,g3,g4, ncol=4, clip=T)
dev.off()

# with paths ####

g1 <- g1 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)
g2 <- g2 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)
g3 <- g3 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)
g4 <- g4 + geom_path(size=1.5)  + geom_point(colour="red",size=3.5)

png(paste0(Sys.Date(),"_points_path_jitter.png"), units="in", width=14, height=4, res=300)
grid.arrange(g1,g2,g3,g4, ncol=4, clip=T)
dev.off()