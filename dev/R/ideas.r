###############################################################################
# element difference view (similar to SQM method)
x <- raeithel
element.no <- 12
r <- getRatingLayer(x)
col <- r[ , element.no]
x[ , ] <- r - col
x <- setScale(x, min= x@scale$min - x@scale$max, 
                 max= x@scale$max * 2)

b <- prepareBiplotData(x)
biplotDraw(b)
addVarianceExplainedToBiplot(x, b)

# Result: unclear if this is helpful!


# map projections
library(maps)
library(mapproj)
library(maptools)
library(MHmisc)




###############################################################################
# project random polar coordinates onto 2D
n <- 100
m <- list()
m$x <- sample(-180:180, n,  rep=T)
#m$x <- interleave_vectors(m$x, NA)
m$y <- sample(-90:90, n,  rep=T)
#m$y <- interleave_vectors(m$y, NA)
labels <- randomSentences(n, 5) 
df <- data.frame(x=m$x, y=m$y)
mp <- mapproject(df, proj='vandergrinten', par=NULL, orientation=c(0,0,90))
colors <- mp$x 
colors[colors >= 0] <- "green"
colors[colors < 0] <- "red"

df <- cbind(df, data.frame(xp=mp$x, yp=mp$y, labels))
#df <- na.omit(df)
x.lim <- range(df$xp, na.rm=T)*1.2
y.lim <- range(df$yp, na.rm=T)*1.2
plot(df[3:4], xlim=x.lim, ylim=y.lim, type="n", pch=4, 
      xaxt="n", yaxt="n", xlab="", ylab="", col=colors)
map.grid(font=1, col="grey", cex=.6, lty=2, labels=T)
#s <- pointLabel(df[1:2], labels=df$labels, cex=.7, do=F)
#text(s, labels, cex=.7, col=grey(.2))

# convex hull around points
# library(spatstat)
# index <- df$x > -50 & df$x < 50 & df$y > -50 & df$y < 50
# w <- convexhull.xy(df[index, 3:4])
# cv <- list(x=w$bdry[[1]]$x, y=w$bdry[[1]]$y)
# polygon(cv, col="lightgrey", border=NA)

#points(df[index, 3:4], pch=16, col=colors[index], cex=1.6)

fit <- hclust(dist(df[1:2]))
groups <- cutree(fit, k=10) 

# for (i in 1:10){
#   index <- groups == i
#   w <- convexhull.xy(df[index, 3:4])
#   cv <- list(x=w$bdry[[1]]$x, y=w$bdry[[1]]$y)
#   polygon(cv, col="lightgrey", border=NA)
# }

points(df[3:4], pch=4, col=colors)
dev.new()

###############################################################################
# make animated gif with sliding map projection of random points
n <- 100
m <- list()
m$x <- sample(-180:180, n,  rep=T)
m$y <- sample(-90:90, n,  rep=T)
labels <- randomSentences(n, 5) 
df <- data.frame(x=m$x, y=m$y)
mp <- mapproject(df, proj='vandergrinten', par=NULL, orientation=c(0,0,90))
colors <- mp$x 
colors[colors >= 0] <- "green"
colors[colors < 0] <- "red"
x.lim <- range(df$xp, na.rm=T)*1.2
y.lim <- range(df$yp, na.rm=T)*1.2

pdf("rotation.pdf")
for (i in 0:360){
  mp <- mapproject(df, proj='vandergrinten', par=NULL, orientation=c(90,0,i))
  df <- data.frame(x=m$x, y=m$y, xp=mp$x, yp=mp$y, labels)
  plot(df[3:4], xlim=x.lim, ylim=y.lim, type="n", pch=4, 
        xaxt="n", yaxt="n", xlab="", ylab="", col=colors)
  map.grid(font=1, col="grey", cex=.6, lty=2, labels=T)
  points(df[3:4], pch=4, col=colors)
}
dev.off()
system("convert -delay 3 rotation.pdf rotation.gif")


###############################################################################
# project cartesian coordinates to polar space
# v   vector of length three (x,y,z)
cartesianToPolarCoords <- function(v){
  phi <- atan2(v[2], v[1]) * 180/pi
  theta <- acos(v[3]/sum(v^2)^.5) * 180/pi
  c(phi=phi, theta=90-theta)
}
cartesianToPolarCoords(c(-1,-1,0))

library(OpenRepGrid)
x <- raeithel
#draw3d(x)
esa <- calcEsaCoords(x)
c.coords <- rbind(esa$c, esa$c *-1)
p <- as.data.frame(t(apply(c.coords[,1:3], 1, cartesianToPolarCoords)))
p$labels <- unlist(getConstructNames(x))

# project random polar coordinates onto 2D
lim <- data.frame(phi=c(-90,180, 0, 180), theta=c(85, 0, -90, -90))
limp <- mapproject(lim$phi, lim$theta, proj='azequidistant', par=NULL, orientation=c(90,0,0))
limp

pdf("grid_map.pdf", width=14)
layout(matrix(1:2, ncol=2))
cex <- 1
mp <- mapproject(p$phi, p$theta, proj='azequidistant', par=NULL, orientation=c(90,90,0))
df <- cbind(p, data.frame(x=mp$x, y=mp$y))
df$colors <- df$x 
df$colors[df$colors >= 0] <- "green"
df$colors[df$colors < 0] <- "red"
x.lim <- range(df$x, na.rm=T)*1.3
y.lim <- range(df$y, na.rm=T)*1.3
x.lim <- c(-3.0, 3.0)
y.lim <- c(-3.0, 3.0)
par(mar=c(.5,.5,.5,.5))
plot(df[4:5], xlim=x.lim, ylim=y.lim, type="n", pch=4, 
      xaxt="n", yaxt="n", xlab="", ylab="", col=colors, asp=1/1, bty="n")
map.grid(nx=9, ny=9, font=1, col=grey(.75), cex=.6, lty=2, labels=T)
s <- pointLabel(df[4:5], labels=df$labels, cex=cex, do=F)
text(s, df$labels, cex=cex, col=grey(.2))
points(df[4:5], pch=4, col=df$colors)

mp <- mapproject(p$phi, p$theta, proj='azequidistant', par=NULL, orientation=c(-90,90,0))
df <- cbind(p, data.frame(x=mp$x, y=mp$y))
df$colors <- df$x 
df$colors[df$colors >= 0] <- "green"
df$colors[df$colors < 0] <- "red"
plot(df[4:5], xlim=x.lim, ylim=y.lim, type="n", pch=4, 
      xaxt="n", yaxt="n", xlab="", ylab="", col=colors, asp=1/1, bty="n")
map.grid(nx=9, ny=9, font=1, col=grey(.75), cex=.6, lty=2, labels=T)
s <- pointLabel(df[4:5], labels=df$labels, cex=cex, do=F)
text(s, df$labels, cex=cex, col=grey(.2))
points(df[4:5], pch=4, col=df$colors)
dev.off()

fit <- hclust(getRatingLayer(x))
groups <- cutree(fit, k=10)


###############################################################################
# make animated gif with sliding map projection of a grid

library(OpenRepGrid)
x <- boeker
esa <- calcEsaCoords(x)
c.coords <- rbind(esa$c, esa$c *-1)
p <- as.data.frame(t(apply(c.coords[,1:3], 1, cartesianToPolarCoords)))
p$labels <- unlist(getConstructNames(x))
mp <- mapproject(p$phi, p$theta, proj='vandergrinten', par=NULL, orientation=c(90,0,0))
df <- cbind(p, data.frame(x=mp$x, y=mp$y))
df$colors <- df$x 
df$colors[df$colors >= 0] <- "green"
df$colors[df$colors < 0] <- "red"
x.lim <- c(-1.1, 1.1)
y.lim <- c(-.7, .7)

pdf("grid_rotate.pdf", width=11)
for (i in 0:360){
  mp <- mapproject(p$phi, p$theta, proj='vandergrinten', par=NULL, orientation=c(45,0,i))
  df <- cbind(p, data.frame(x=mp$x, y=mp$y), colors=df$colors)
  par(mar=c(.5,.5,.5,.5))
  plot(df[4:5], xlim=x.lim, ylim=y.lim, type="n", pch=4, 
        xaxt="n", yaxt="n", xlab="", ylab="", col=colors, bty="n")
  map.grid(nx=9, ny=9, font=1, col=grey(.75), cex=.6, lty=2, labels=T)
  text(df[c("x", "y")], df$labels, cex=.7, col=grey(.2), adj=c(.5,1))
  points(df[4:5], pch=4, col=df$colors)
}
dev.off()
system("convert -delay 3 grid_rotate.pdf grid_rotate.gif")

dev.new()



###############################################################################

# decoupling of constructs
#

args <- list( name=c("element_1", "element_2", "element_3", "element_4"),
              l.name=c("left_1", "left_2", "left_3"),
              r.name=c("right_1", "right_2", "right_3"),
              scores=c( 1,0,1,0,
                        1,1,1,0,
                        1,0,1,0 ),
              min=0, max=1, 
              coupled=T)
x <- makeRepgrid(args)

decouple <- function(x){
  if (x@coupled) {
      x <- doubleEntry(x)
      x@coupled <- FALSE
  }
  x
}




