rg <- rg3
dim=1:3
e.radius <- .22 
c.radius <- .1
rg <- esa(rg)
e <- rg@results$esa$e[,dim]
c.i <- rg@results$esa$c[,dim]
c.c <- c.i * -1
max.dim <- max(abs(rbind(e, c.i, c.c))) *1.2
open3d()
par3d(userMatrix = diag(4))
#rgl.bg(color="black")
spheres3d(e[,1], e[,2], e[,3], radius=e.radius, color="blue")
spheres3d(c.i[,1], c.i[,2], c.i[,3], radius=c.radius, alpha=.4, color="grey")
spheres3d(c.c[,1], c.c[,2], c.c[,3], radius=c.radius, alpha=.4, color="red")
e.t <- e - e.radius/2
texts3d(x= e.t[,1], y= e[,2], z= e[,3], text=rg@elements, adj=c(1,1), cex=.7, col="blue") 
texts3d(x= c.i[,1], y= c.i[,2], z= c.i[,3], text=rg@constructs$leftPole, adj=c(1,1), cex=.7, col="grey") 
texts3d(x= c.c[,1], y= c.c[,2], z= c.c[,3], text=rg@constructs$rightPole, adj=c(1,1), cex=.7, col="red") 
spheres3d(0,0,0, radius=max.dim, alpha=.05, color="black")

lines3d(c(0,max.dim), c(0,0), c(0,0), lwd=1)
lines3d(c(0,0), c(0,max.dim), c(0,0), lwd=1)
lines3d(c(0,0), c(0,0), c(0,max.dim), lwd=1)
text3d(max.dim, 0, 0, "X", cex=1.2, adj=c(1,1))
text3d(0, max.dim, 0, "Y", cex=1.2, adj=c(1,1))
text3d(0, 0, max.dim, "Z", cex=1.2, adj=c(1,1))
spheres3d(max.dim, 0, 0, radius=.05)
spheres3d(0, max.dim, 0, radius=.05)
spheres3d(0, 0, max.dim, radius=.05)
#rgl.bringtotop(stay = TRUE)

rgl.setMouseCallbacks(button=1, begin=function(){}, end=function(){})


f <- select3d()


##########################################################################

