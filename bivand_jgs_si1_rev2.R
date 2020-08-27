### R code from vignette source 'bivand_jgs_si1_rev2.Rnw'

zz <- file("output.Rout", open="wb")
sink(zz)
sink(zz, type = "message")

###################################################
### code chunk number 12: bivand_jgs_si1_rev2.Rnw:164-167
###################################################
td <- tempfile()
dir.create(td)
Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)


###################################################
### code chunk number 13: bivand_jgs_si1_rev2.Rnw:212-220 (eval = FALSE)
###################################################
## BCrepos <- BiocManager::repositories()
## bioc <- available.packages(repo = BCrepos[1])
## bioc_ann <- available.packages(repo = BCrepos[2])
## bioc_exp <- available.packages(repo = BCrepos[3])
## cran <- available.packages()
## saveRDS(cran, file="cran_200820.rds")
## pdb <- rbind(cran, bioc, bioc_ann, bioc_exp)
## saveRDS(pdb, file="pdb_200820.rds")


###################################################
### code chunk number 14: bivand_jgs_si1_rev2.Rnw:224-250 (eval = FALSE)
###################################################
pdb <- readRDS("pdb_200820.rds")
suppressPackageStartupMessages(library(miniCRAN))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(magrittr))
pg <- makeDepGraph(pdb[, "Package"], availPkgs = pdb, suggests=TRUE, enhances=TRUE, includeBasePkgs = FALSE)
pr <- pg %>%
  page.rank(directed = FALSE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank")
  cutoff <- quantile(pr[, "page.rank"], probs = 0.2)
popular <- pr[pr[, "page.rank"] >= cutoff, ]
toKeep <- names(popular)
vids <- V(pg)[toKeep]
gs <- induced.subgraph(pg, vids = toKeep)
cl <- walktrap.community(gs, steps = 3)
topClusters <- table(cl$membership) %>%
  sort(decreasing = TRUE) %>%
  head(25)
cluster <- function(i, clusters, pagerank, n=10){
  group <- clusters$names[clusters$membership == i]
  pagerank[group, ] %>% sort(decreasing = TRUE) %>% head(n)
}
z <- lapply(names(topClusters)[1:15], cluster, clusters=cl, pagerank=pr, n=50)
## saveRDS(z, file="all_z_200820.rds")


###################################################
### code chunk number 15: bivand_jgs_si1_rev2.Rnw:255-268
###################################################

pdf("figure1.pdf")
par(mar=c(4,4,2,1)+0.1)
suppressPackageStartupMessages(library(wordcloud))
## z <- readRDS("all_z_200820.rds")
sp_cl <- which(sapply(z, function(x) "sp" %in% names(x)))
sf_cl <- which(sapply(z, function(x) "sf" %in% names(x)))
sp_range <- range(z[[sp_cl]])
sf_range <- range(z[[sf_cl]])
sp_pr <- unname(z[[sp_cl]]["sp"])
sf_pr <- unname(z[[sf_cl]]["sf"])
oopar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
for (i in c(sp_cl, sf_cl)) wordcloud(names(z[[i]]), freq=unname(z[[i]]), scale=c(3, 0.5))
par(oopar)
dev.off()

###################################################
### code chunk number 16: bivand_jgs_si1_rev2.Rnw:292-295
###################################################
df <- data.frame(a=letters[1:3], b=1:3)
df$c <- list(d=1, e="1", f=TRUE)
str(df)


###################################################
### code chunk number 17: bivand_jgs_si1_rev2.Rnw:302-303
###################################################
library(sf)


###################################################
### code chunk number 18: bivand_jgs_si1_rev2.Rnw:310-314
###################################################
pt1 <- st_point(c(1,3))
pt2 <- pt1 + 1
pt3 <- pt2 + 1
str(pt3)


###################################################
### code chunk number 19: bivand_jgs_si1_rev2.Rnw:321-322
###################################################
st_as_text(pt3)


###################################################
### code chunk number 20: bivand_jgs_si1_rev2.Rnw:330-331
###################################################
st_as_binary(pt3)


###################################################
### code chunk number 21: bivand_jgs_si1_rev2.Rnw:338-340
###################################################
pt_sfc <- st_as_sfc(list(pt1, pt2, pt3), crs=3857)
str(pt_sfc)


###################################################
### code chunk number 22: bivand_jgs_si1_rev2.Rnw:347-348
###################################################
st_distance(pt_sfc)


###################################################
### code chunk number 23: bivand_jgs_si1_rev2.Rnw:355-357
###################################################
pt_sfc1 <- st_as_sfc(list(pt1, pt2, pt3), crs=4326)
st_distance(pt_sfc1)


###################################################
### code chunk number 24: bivand_jgs_si1_rev2.Rnw:364-369
###################################################
mat <- matrix(0, 3, 3)
mat[1,2] <- mat[2,1] <- s2::s2_distance(st_as_text(pt1), st_as_text(pt2))
mat[1,3] <- mat[3,1] <- s2::s2_distance(st_as_text(pt1), st_as_text(pt3))
mat[2,3] <- mat[3,2] <- s2::s2_distance(st_as_text(pt2), st_as_text(pt3))
mat


###################################################
### code chunk number 25: bivand_jgs_si1_rev2.Rnw:377-379
###################################################
st_geometry(df) <- pt_sfc
str(df)


###################################################
### code chunk number 26: bivand_jgs_si1_rev2.Rnw:388-390
###################################################
tf <- tempfile(fileext=".gpkg")
st_write(df, dsn=tf, quiet=TRUE)


###################################################
### code chunk number 28: bivand_jgs_si1_rev2.Rnw:400-402
###################################################
df1 <- st_read(dsn=tf, quiet=TRUE)
df1


###################################################
### code chunk number 29: bivand_jgs_si1_rev2.Rnw:409-410
###################################################
read_sf(dsn=tf) %>% dplyr::filter(a == "c")


###################################################
### code chunk number 30: bivand_jgs_si1_rev2.Rnw:417-419
###################################################
buf_df1 <- st_buffer(df1, dist=0.3)
st_area(buf_df1)


###################################################
### code chunk number 32: bivand_jgs_si1_rev2.Rnw:432-436
###################################################
pdf("figure2.pdf")
plot(st_geometry(buf_df1))
plot(st_geometry(df1), add=TRUE, pch=4)
dev.off()

###################################################
### code chunk number 33: bivand_jgs_si1_rev2.Rnw:452-456
###################################################
library(stars)
fn <- system.file("tif/L7_ETMs.tif", package = "stars")
L7 <- read_stars(fn)
L7


###################################################
### code chunk number 34: bivand_jgs_si1_rev2.Rnw:463-465
###################################################
ndvi <- function(x) (x[4] - x[3])/(x[4] + x[3])
(s2.ndvi <- st_apply(L7, c("x", "y"), ndvi))


###################################################
### code chunk number 35: bivand_jgs_si1_rev2.Rnw:472-474
###################################################
L7p <- read_stars(fn, proxy=TRUE)
L7p


###################################################
### code chunk number 36: bivand_jgs_si1_rev2.Rnw:481-482
###################################################
(L7p.ndvi = st_apply(L7p, c("x", "y"), ndvi))


###################################################
### code chunk number 37: bivand_jgs_si1_rev2.Rnw:489-490
###################################################
(x6 <- split(L7, "band"))


###################################################
### code chunk number 38: bivand_jgs_si1_rev2.Rnw:497-500
###################################################
x6$mean <- (x6[[1]] + x6[[2]] + x6[[3]] + x6[[4]] + x6[[5]] + x6[[6]])/6
xm <- st_apply(L7, c("x", "y"), mean)
all.equal(xm[[1]], x6$mean)


###################################################
### code chunk number 39: bivand_jgs_si1_rev2.Rnw:513-514
###################################################
olinda <- st_read("olinda.gpkg", quiet=TRUE)


###################################################
### code chunk number 40: bivand_jgs_si1_rev2.Rnw:521-525
###################################################
library(classInt)
cI_fisher <- classIntervals(olinda$"DEPRIV", n=7, style="fisher")
set.seed(1)
cI_bclust <- classIntervals(olinda$"DEPRIV", n=7, style="bclust")


###################################################
### code chunk number 41: bivand_jgs_si1_rev2.Rnw:533-534
###################################################
pal <- rcartocolor::carto_pal(7, "SunsetDark")


###################################################
### code chunk number 43: bivand_jgs_si1_rev2.Rnw:547-553
###################################################
pdf("figure3.pdf")
oopar <- par(mar=c(4,3,0,1)+0.1, mfrow=c(1,2))
plot(cI_fisher, pal, xlab="fisher", main="", ylab="")
plot(cI_bclust, pal, xlab="bclust", main="", ylab="")
par(oopar)
dev.off()

###################################################
### code chunk number 44: bivand_jgs_si1_rev2.Rnw:562-576 (eval = FALSE)
###################################################
#x11()
pdf("figure4a.pdf")
plot(olinda[,"DEPRIV"], nbreaks=7, breaks="fisher", pal=pal, key.pos=NULL, main="")
#gridGraphics::grid.echo()
#library(grid)
#g1 <- grid.grab()
dev.off()
#x11()
pdf("figure4b.pdf")
library(cartography)
choroLayer(olinda, var="DEPRIV", method="fisher-jenks", nclass=7, col=pal, legend.values.rnd=3)
#gridGraphics::grid.echo()
#library(grid)
#g2 <- grid.grab()
dev.off()
## save(g1, g2, file="g1g2.rda")


###################################################
### code chunk number 45: bivand_jgs_si1_rev2.Rnw:581-586
###################################################
## load("g1g2.rda")
#gridExtra::grid.arrange(g1, g2, ncol=2, respect=TRUE)


###################################################
### code chunk number 47: bivand_jgs_si1_rev2.Rnw:607-610
###################################################
library(tmap)
o1 <- tm_shape(olinda) + tm_fill("DEPRIV", style="fisher", n=7, palette=pal)
o2 <- o1 + tm_borders(lwd=0.8)


###################################################
### code chunk number 48: bivand_jgs_si1_rev2.Rnw:615-618
###################################################
pdf("figure5.pdf")
tmap_arrange(o1, o2, ncol=2)
dev.off()

###################################################
### code chunk number 49: bivand_jgs_si1_rev2.Rnw:627-631
###################################################
library(ggplot2)
g1 <- ggplot(olinda) + geom_sf(aes(fill=DEPRIV))
g2 <- g1 + theme_void() + scale_fill_gradientn(colours=pal,
  breaks=round(cI_fisher$brks, 3))


###################################################
### code chunk number 50: bivand_jgs_si1_rev2.Rnw:638-641
###################################################
pdf("figure6.pdf")
gridExtra::grid.arrange(g1, g2, ncol=2, respect=TRUE)
dev.off()

###################################################
### code chunk number 51: bivand_jgs_si1_rev2.Rnw:650-652
###################################################
g3 <- g1 + theme_void() + scale_fill_gradientn(colours=pal,
  breaks=round(cI_bclust$brks, 3))


###################################################
### code chunk number 52: bivand_jgs_si1_rev2.Rnw:658-661
###################################################
pdf("figure7.pdf")
gridExtra::grid.arrange(g2, g3, ncol=2, respect=TRUE)
dev.off()

###################################################
### code chunk number 53: bivand_jgs_si1_rev2.Rnw:673-683 (eval = FALSE)
###################################################
pdb <- readRDS("pdb_200820.rds")
deps_sp <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports"), recursive = TRUE, reverse = TRUE)
deps_sf <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports"), recursive = TRUE, reverse = TRUE)
deps_sp3 <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports", "Suggests"), recursive = TRUE, reverse = TRUE)
deps_sf3 <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports", "Suggests"), recursive = TRUE, reverse = TRUE)
deps_sp1 <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports"), recursive = FALSE, reverse = TRUE)
deps_sf1 <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports"), recursive = FALSE, reverse = TRUE)
deps_sp2 <- tools::package_dependencies(packages = "sp", pdb, which = c("Depends", "Imports", "Suggests"), recursive = FALSE, reverse = TRUE)
deps_sf2 <- tools::package_dependencies(packages = "sf", pdb, which = c("Depends", "Imports", "Suggests"), recursive = FALSE, reverse = TRUE)
## save(deps_sp, deps_sf, deps_sp1, deps_sf1, deps_sp2, deps_sf2, deps_sp3, deps_sf3, file="deps_sp_sf_200820.rda")


###################################################
### code chunk number 54: bivand_jgs_si1_rev2.Rnw:686-718
###################################################
## load("deps_sp_sf_200820.rda")
mat <- matrix(0, ncol=4, nrow=5)
mat[1,1] <- length(deps_sp[[1]])
mat[2,1] <- length(deps_sf[[1]])
sp_sf <- match(deps_sp[[1]], deps_sf[[1]])
sf_sp <- match(deps_sf[[1]], deps_sp[[1]])
mat[4,1] <- length(deps_sf[[1]][is.na(sf_sp)])
mat[5,1] <- length(deps_sf[[1]][!is.na(sf_sp)])
mat[3,1] <- length(deps_sp[[1]][is.na(sp_sf)])
mat[1,4] <- length(deps_sp2[[1]])
mat[2,4] <- length(deps_sf2[[1]])
sp_sf2 <- match(deps_sp2[[1]], deps_sf2[[1]])
sf_sp2 <- match(deps_sf2[[1]], deps_sp2[[1]])
mat[4,4] <- length(deps_sf2[[1]][is.na(sf_sp2)])
mat[5,4] <- length(deps_sf2[[1]][!is.na(sf_sp2)])
mat[3,4] <- length(deps_sp2[[1]][is.na(sp_sf2)])
mat[1,2] <- length(deps_sp3[[1]])
mat[2,2] <- length(deps_sf3[[1]])
sp_sf3 <- match(deps_sp3[[1]], deps_sf3[[1]])
sf_sp3 <- match(deps_sf3[[1]], deps_sp3[[1]])
mat[4,2] <- length(deps_sf3[[1]][is.na(sf_sp3)])
mat[5,2] <- length(deps_sf3[[1]][!is.na(sf_sp3)])
mat[3,2] <- length(deps_sp3[[1]][is.na(sp_sf3)])
mat[1,3] <- length(deps_sp1[[1]])
mat[2,3] <- length(deps_sf1[[1]])
sp_sf1 <- match(deps_sp1[[1]], deps_sf1[[1]])
sf_sp1 <- match(deps_sf1[[1]], deps_sp1[[1]])
mat[4,3] <- length(deps_sf1[[1]][is.na(sf_sp1)])
mat[5,3] <- length(deps_sf1[[1]][!is.na(sf_sp1)])
mat[3,3] <- length(deps_sp1[[1]][is.na(sp_sf1)])
rownames(mat) <- c("Sum \\pkg{sp}", "Sum \\pkg{sf}", "Only \\pkg{sp}", "Only \\pkg{sf}", "Both")
colnames(mat) <- c("Recursive", "Recursive w/Suggests", "Not recursive", "Not recursive w/Suggests")


###################################################
### code chunk number 55: bivand_jgs_si1_rev2.Rnw:724-725
###################################################
rgdal::rgdal_extSoftVersion()


###################################################
### code chunk number 56: bivand_jgs_si1_rev2.Rnw:732-733
###################################################
rgeos::rgeos_extSoftVersion()


###################################################
### code chunk number 57: bivand_jgs_si1_rev2.Rnw:740-741
###################################################
sf_extSoftVersion()


###################################################
### code chunk number 58: bivand_jgs_si1_rev2.Rnw:751-753
###################################################
suppressMessages(library(xtable))
print(xtable(mat, align=c("l","r","r","r","r"), digits=c(NA, 0, 0, 0, 0), display=c("s", "d", "d", "d", "d")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,2,5))


###################################################
### code chunk number 59: bivand_jgs_si1_rev2.Rnw:771-773
###################################################
bp_file <- system.file("gpkg/b_pump.gpkg", package="sf")
b_pump_sf <- st_read(bp_file, quiet=TRUE)


###################################################
### code chunk number 60: bivand_jgs_si1_rev2.Rnw:780-788
###################################################
proj5 <- paste0("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717",
 " +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs")
legacy <- st_crs(proj5)
proj6 <- legacy$proj4string
proj5_parts <- unlist(strsplit(proj5, " "))
proj6_parts <- unlist(strsplit(proj6, " "))
proj5_parts[!is.element(proj5_parts, proj6_parts)]
proj6_parts[!is.element(proj6_parts, proj5_parts)]


###################################################
### code chunk number 61: bivand_jgs_si1_rev2.Rnw:795-797
###################################################
b_pump_sf1 <- b_pump_sf
st_crs(b_pump_sf1) <- st_crs(st_crs(b_pump_sf1)$proj4string)


###################################################
### code chunk number 62: bivand_jgs_si1_rev2.Rnw:804-806
###################################################
all.equal(st_coordinates(st_geometry(b_pump_sf)), 
          st_coordinates(st_geometry(b_pump_sf1)))


###################################################
### code chunk number 63: bivand_jgs_si1_rev2.Rnw:813-816
###################################################
b_pump_sf_ll <- st_transform(b_pump_sf, 4326)
b_pump_sf1_ll <- st_transform(b_pump_sf1, 4326)
st_distance(b_pump_sf_ll, b_pump_sf1_ll)


###################################################
### code chunk number 64: bivand_jgs_si1_rev2.Rnw:821-828
###################################################
library(mapview)
if (sf:::CPL_gdal_version() >= "3.1.0") mapviewOptions(fgb = FALSE)
pts <- rbind(b_pump_sf_ll, b_pump_sf1_ll)
pts$CRS <- c("original", "degraded")
webmap1 <- mapview(pts, zcol="CRS", map.type="OpenStreetMap", col.regions=c("green", "red"), cex=18)
##webmap1
mapshot(webmap1, file="webmap1.png")
## magick::image_write(magick::image_read("webmap1.png"), path="webmap1.eps", format="eps")


###################################################
### code chunk number 65: bivand_jgs_si1_rev2.Rnw:847-848
###################################################
st_crs("EPSG:4326")


###################################################
### code chunk number 66: bivand_jgs_si1_rev2.Rnw:855-857
###################################################
library(sp)
cat(wkt(CRS(SRS_string="EPSG:4326")), "\n")


###################################################
### code chunk number 67: bivand_jgs_si1_rev2.Rnw:864-867
###################################################
sf_from_sp <- st_crs(CRS(SRS_string="EPSG:4326"))
o <- strsplit(sf_from_sp$wkt, "\n")[[1]]
cat(paste(o[grep("CS|AXIS|ORDER", o)], collapse="\n"))


###################################################
### code chunk number 68: bivand_jgs_si1_rev2.Rnw:872-875
###################################################
sp_from_sf <- as(st_crs("EPSG:4326"), "CRS")
o <- strsplit(wkt(sp_from_sf), "\n")[[1]]
cat(paste(o[grep("CS|AXIS|ORDER", o)], collapse="\n"))


###################################################
### code chunk number 69: bivand_jgs_si1_rev2.Rnw:884-886
###################################################
b_pump_sp <- as(b_pump_sf, "Spatial")
b_pump_sp1 <- as(b_pump_sf1, "Spatial")


###################################################
### code chunk number 71: bivand_jgs_si1_rev2.Rnw:898-899
###################################################
library(rgdal)


###################################################
### code chunk number 72: bivand_jgs_si1_rev2.Rnw:906-913
###################################################
WKT <- wkt(b_pump_sp)
o <- list_coordOps(WKT, "EPSG:4326")
c(nrow(o), nrow(o[o$instantiable,]), sum(o$number_grids))
aoi0 <- project(t(unclass(bbox(b_pump_sp))), WKT, inv=TRUE)
aoi <- c(t(aoi0 + c(-0.1, +0.1)))
o_aoi <- list_coordOps(WKT, "EPSG:4326", area_of_interest=aoi)
c(nrow(o_aoi), nrow(o_aoi[o_aoi$instantiable,]), sum(o_aoi$number_grids))


###################################################
### code chunk number 73: bivand_jgs_si1_rev2.Rnw:922-924
###################################################
b_pump_sp_ll <- spTransform(b_pump_sp, CRS(SRS_string="EPSG:4326"))
cat(strwrap(get_last_coordOp()), sep="\n")


###################################################
### code chunk number 74: bivand_jgs_si1_rev2.Rnw:931-935
###################################################
o <- list_coordOps(wkt(b_pump_sp1), "EPSG:4326", area_of_interest=aoi)
cat(nrow(o), o$ballpark, "\n")
b_pump_sp1_ll <- spTransform(b_pump_sp1, CRS(SRS_string="EPSG:4326"))
cat(strwrap(get_last_coordOp()), sep="\n")


###################################################
### code chunk number 75: bivand_jgs_si1_rev2.Rnw:944-946
###################################################
enable_proj_CDN()
list.files(td)


###################################################
### code chunk number 76: bivand_jgs_si1_rev2.Rnw:953-957
###################################################
o <- list_coordOps(WKT, "EPSG:4326", area_of_interest=aoi)
c(nrow(o), nrow(o[o$instantiable,]), sum(o$number_grids))
b_pump_sp_llg <- spTransform(b_pump_sp, CRS(SRS_string="EPSG:4326"))
cat(strwrap(get_last_coordOp()), sep="\n")


###################################################
### code chunk number 77: bivand_jgs_si1_rev2.Rnw:964-967
###################################################
list.files(td)
file.size(file.path(td, list.files(td)[1]))
disable_proj_CDN()


###################################################
### code chunk number 78: bivand_jgs_si1_rev2.Rnw:974-978
###################################################
c(spDists(b_pump_sp1_ll, b_pump_sp_ll),
  spDists(b_pump_sp_llg, b_pump_sp_ll))*1000
all.equal(unname(coordinates(b_pump_sp_ll)),
          unname(st_coordinates(st_geometry(b_pump_sf_ll))))


sessionInfo()
sink(type = "message")
sink()

