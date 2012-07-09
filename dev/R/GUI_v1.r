# WINDOWS ONLY DUE TO USE OF GTK+ DEVICES THAT ARE NOT PROPERLY IMPLEMENTED UNDER MAC OS
#
debug <- TRUE 
library(grid)
library(plotrix)
library(RGtk2)
library(gWidgets)
options("guiToolkit"="RGtk2")


## Top Level
w = gwindow("openRepgrid GUI v0.0")

## Toolbar
defHandler = function(h,...) print("hi")
mbl = list(
  File = list(
    openFile = list(handler=defHandler, icon="open"),
    close = list(handler=defHandler, icon="cancel")
    ),
  Edit = list(
    paste = list(handler=defHandler),
    copy =  list(handler=defHandler)
    )
  )
tbl = list(
  new = list(handler = defHandler, icon="new"),
  quit = list(handler= defHandler, icon="quit"),
  save = list(handler= defHandler, icon="save")
  )

tb = gtoolbar(tbl, cont=w)


## Left panel ##
leftGroup <- ggroup(use.scrollwindow = TRUE, cont=w, horizontal=FALSE)
size(leftGroup) <- c(180, 600)
# Left:Settings
settingsGroupLeft <- gexpandgroup("Settings", horizontal=FALSE, container=leftGroup)
labelConeAngle <- glabel("Some setting", cont=settingsGroupLeft)
spinButtonConeAngle <- gspinbutton(from = 0, to = 50, value=initValueConeAngle, container = settingsGroupLeft)
checkboxOnMouseOverLeft <- gcheckbox("Maus über Box", cont=settingsGroupLeft, value=FALSE) 
# Links:Layers
layersGroupLeft <- gexpandgroup("Layers", horizontal=FALSE, container=leftGroup)
tableLayers <- tblLeft <- gtable(1:7, container=layersGroupLeft)
size(tableLayers) <- c(150, 150)

# left: 3d controls

handler.3dgrid <- function(h, ...) {
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
	rgl.bringtotop(stay = TRUE)
}

group.left.3dcontrols <- gexpandgroup("3D controls", horizontal=FALSE, container=leftGroup)
buttonLeft <- gbutton("Start 3D", handler=handler.3dgrid, cont=group.left.3dcontrols)
handler.b.left.3dsnapshot.save <- function(h, ...){
	saveFile <- function() gfile("Save as .png",type="save")
	snapshot3d(saveFile())
}
b.left.3dsnapshot.save <- gbutton("3D Snapshot", handler=handler.b.left.3dsnapshot.save, cont=group.left.3dcontrols)

# Left: grids	
loadedGridsGroupLeft <- gexpandgroup("grids", horizontal=FALSE, container=leftGroup)			
gl <- glabel("grids", cont=loadedGridsGroupLeft)
df <- data.frame(no=1:20, elements=rep(c(10, 8), each=10), constructs=sample(3:6, 20, rep=T))
tblLeft <- gtable(df, multiple=TRUE, container=loadedGridsGroupLeft)
size(tblLeft) <- c(150,500)
svalue(tblLeft, index = TRUE) <- 1										# Ersten Eintrag in tblLeft markieren


## Mitte ##
scrollGroup <- ggroup(use.scrollwindow = TRUE, cont=w)
size(scrollGroup) <- c(900,680)
nb <- gnotebook(container = scrollGroup, closebuttons=TRUE, expand=TRUE)

# erster Tab
tbl <- glayout(container = nb, spacing = 2, homo=F, label="Data")
tbl[1, 1, anchor = c(-1, 1), expand = FALSE] <- (b = gbutton("X", cont=tbl))
blankDF <- data.frame(variables=character(0), stringsAsFactors=FALSE)
tbl[2, 2, expand = FALSE] <- (tblMiddle <- gtable(blankDF, container=tbl))
size(tblMiddle) <- c(400,400)
svalue(tblMiddle, index = TRUE) <- 1										# Ersten Eintrag in tblLeft markieren

# zweiter Tab
tbl <- glayout(container = nb, spacing = 2, homo=F, label="Biplot")
tbl[1, 1, expand = FALSE] <- (b = gbutton("X", cont=tbl))
tbl[2, 2, expand = FALSE] <- (g3 = ggraphics(cont=tbl, expand=T))
size(g3) <- c(500,500)

# dritter Tab
tbl <- glayout(container = nb, spacing = 2, homo=F, label="Bertin")
tbl[1, 1, expand = FALSE] <- (b = gbutton("X", cont=tbl))
tbl[2, 2, expand = FALSE] <- (g2 = ggraphics(cont=tbl, expand=T))
buttons <- glayout(container = tbl, spacing = 2, homo=F, label="Bertin")
buttons[1, 1, expand = FALSE] <- (b4 = gbutton("save plot", cont=buttons))
buttons[1, 2, expand = FALSE] <- (b5 = gbutton("redraw plot", cont=buttons))
tbl[3, 2 , anchor = c(1, 1), expand = FALSE] <- buttons
size(g2) <- c(700,500)

# vierter tab
res <- data.frame(no=1:100, elements=round(rep(c(10, 8), each=50),0), constructs=sample(3:6, 100, rep=T),
				interviewer= sample(1:3, 100, r=T), complexity=abs(round(rnorm(100)*10,1)),
				PVAFF=abs(round(rnorm(100)*10,1)), SQM= abs(round(rnorm(100)*100,0)))
tbl <- glayout(container = nb, spacing = 2, homo=F, label="Multiple grids")
tbl[1, 1, expand = FALSE] <- (tblMulti <- gtable(res, container=tbl))
size(tblMulti) <- c(700,550)
svalue(tblMulti, index = TRUE) <- 1										# Ersten Eintrag in tblLeft markieren
buttons <- glayout(container = tbl, spacing = 2, homo=F, label="Bertin")
buttons[1, 1, expand = FALSE] <- (b6 = gbutton("export data", cont=buttons))
tbl[2, 1 , anchor = c(1, 1), expand = FALSE] <- buttons

## Rechts ##
# group for bertin display
# group for biplots

group.right.bertin <- ggroup(use.scrollwindow = TRUE, cont=w, horizontal=FALSE)
size(group.right.bertin) <- c(180, 600)
labelNoBoxRight <- glabel("Bertin Controls", cont=group.right.bertin)

group.right.biplot <- ggroup(use.scrollwindow = TRUE, horizontal=FALSE)
size(group.right.biplot) <- c(180, 600)
labelNoBoxRight <- glabel("Biplot Controls", cont=group.right.biplot)

# Unten
statusBar <- gstatusbar("idle", cont=w)

visible(g3) <- TRUE
esaplot(rg3)

visible(g2) <- TRUE
bertin2PlusLegend(rg3, colors=c("darkred", "white"))
tblMiddle[,] <- getRepgridData(rg3)


delete(w, group.right.bertin)								# Gruppe1 im Hauptcontainer löschen 
add(w, group.right.biplot)



# open3d()
# spheres3d(rnorm(10), rnorm(10), rnorm(10), radius=0.3, color="blue")
# spheres3d(0, 0, 0, radius=3, alpha=.4, color="blue")
# grid3d(c("x", "y+", "z"))
# 
# tmp <- getRepgridData(rg3)
# tmp <- tmp[-c(1,6)]
# coords <- svd(tmp)$u %*% diag(svd(tmp)$d)
# x <- coords[,1]
# y <- coords[,2]
# z <- coords[,3]
# open3d()
# bg3d("black")
# spheres3d(x, y, z, radius=.5, alpha=1.0, color="blue")
# f <- select3d()
# keep <- f(x,y,z)
# rgl.pop()
# spheres3d(x[keep],y[keep],z[keep], radius=.5, color='red')
# spheres3d(x[!keep],y[!keep],z[!keep], radius=.5, color="blue")

