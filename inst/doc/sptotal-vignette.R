## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, message = FALSE-------------------------------------------
########################################################################
########################################################################
########################################################################
#        Introduction
########################################################################
########################################################################
########################################################################

## ---- echo = FALSE, message = FALSE-------------------------------------------
########################################################################
########################################################################
########################################################################
#        Data
########################################################################
########################################################################
########################################################################

## ---- echo = FALSE, message = FALSE, fig.align="center", cache = FALSE--------
old.par = par(mar = c(0,0,0,0))
plot(c(0,1), c(0,1), type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', 
  xlab = '', ylab = '')
rect(0, .7, .08, 1, col = rgb(228/255,26/255,28/255))
rect(0, 0, .08, .68, col = 'white')
rect(.1, .7, .48, 1, col = rgb(55/255,126/255,184/255))
rect(.1, 0, .48, .68, col = rgb(55/255,126/255,184/255, alpha = .3))
rect(.5, .7, .58, 1, col = rgb(77/255,175/255,74/255))
rect(.6, .7, .68, 1, col = rgb(77/255,175/255,74/255))
rect(.5, 0, .58, .68, col = rgb(77/255,175/255,74/255, alpha = .3))
rect(.6, 0, .68, .68, col = rgb(77/255,175/255,74/255, alpha = .3))
rect(.7, .7, .78, 1, col = rgb(152/255,78/255,163/255))
rect(.7, 0, .78, .68, col = rgb(152/255,78/255,163/255, alpha = .3))
rect(.8, .7, 1, 1, col = rgb(255/255,127/255,0/255))
rect(.8, 0, 1, .68, col = rgb(255/255,127/255,0/255, alpha = .3))
par(old.par)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("sptotal")

## -----------------------------------------------------------------------------
library(sptotal)

## -----------------------------------------------------------------------------
data(simdata)

## -----------------------------------------------------------------------------
head(simdata)

## ---- fig.width = 5, fig.height = 5, fig.align = "center", message = FALSE, cache = FALSE----
require(ggplot2)
ggplot(data = simdata, aes(x = x, y = y)) + geom_point(size = 3) +
  geom_point(data = subset(simdata, wts2 == 1), colour = "red",
    size = 3)

## -----------------------------------------------------------------------------
sum(simdata[ ,'Z'])

## -----------------------------------------------------------------------------
sum(simdata[ ,'wts2'] * simdata[ ,'Z'])

## -----------------------------------------------------------------------------
set.seed(1)
# take a random sample of 100
obsID <- sample(1:nrow(simdata), 100)
simobs <- simdata
simobs$Z <- NA
simobs[obsID, 'Z'] <- simdata[obsID, 'Z']

## ---- fig.width = 5, fig.height = 5, fig.align = "center", cache = FALSE------
ggplot(data = simobs, aes(x = x, y = y)) +
  geom_point(shape = 1, size = 2.5, stroke = 1.5) +
  geom_point(data = subset(simobs, !is.na(Z)), shape = 16, size = 3.5) +
  geom_point(data = subset(simobs, !is.na(Z) & wts2 == 1), shape = 16,
    colour = "red", size = 3.5) +
  geom_point(data = subset(simobs, is.na(Z) & wts2 == 1), shape = 1,
    colour = "red", size = 2.5, stroke = 1.5)

## ---- echo = FALSE, message = FALSE-------------------------------------------
########################################################################
########################################################################
########################################################################
#        Using the sptotal package
########################################################################
########################################################################
########################################################################

## -----------------------------------------------------------------------------
slmfit_out1 <- slmfit(formula = Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential")

## -----------------------------------------------------------------------------
summary(slmfit_out1) 

## ---- results = "hide"--------------------------------------------------------
pred_obj <- predict(slmfit_out1)

## ---- results = "hide"--------------------------------------------------------
prediction_df <- pred_obj$Pred_df
prediction_df[ ,c("x", "y", "Z", "Z_pred_density")]

## ---- warning = FALSE---------------------------------------------------------
check.variogram(slmfit_out1)

## ---- fig.height = 3----------------------------------------------------------
qplot(residuals(slmfit_out1), bins = 20) + xlab("Residuals")
qplot(residuals(slmfit_out1, cross.validation = TRUE), bins = 20) +
  xlab("CV Residuals")

## -----------------------------------------------------------------------------
get.predinfo(pred_obj, conf_level = 0.90)

## -----------------------------------------------------------------------------
get.predplot(pred_obj)

## -----------------------------------------------------------------------------
pred_obj2 <- predict(slmfit_out1, wtscol = "wts2")
get.predinfo(pred_obj2)

## ---- message = FALSE---------------------------------------------------------
data(AKmoose)

## ---- message = FALSE---------------------------------------------------------
require(rgeos)
centroids <- data.frame(ID = AKmoose@data,
  x = rgeos::gCentroid(AKmoose, byid=TRUE)@coords[ ,'x'],
  y = rgeos::gCentroid(AKmoose, byid=TRUE)@coords[ ,'y'])

## -----------------------------------------------------------------------------
xy <- LLtoTM(mean(centroids$x), centroids$y, centroids$x)$xy

## -----------------------------------------------------------------------------
moose_df <- AKmoose@data ## name the data set moose_df
head(moose_df) ## look at the first 6 observations

## -----------------------------------------------------------------------------
moose_df$x = xy[ ,'x']
moose_df$y = xy[ ,'y']

## ---- results = "hide"--------------------------------------------------------
cbind(moose_df$x, moose_df$y, centroids$x, centroids$y)

## -----------------------------------------------------------------------------
head(moose_df)
str(moose_df$total)

## -----------------------------------------------------------------------------
moose_df$surveyed <- as.numeric(levels(moose_df$surveyed))[moose_df$surveyed]
moose_df$total <- as.numeric(levels(moose_df$total))[moose_df$total]

## -----------------------------------------------------------------------------
moose_df$total[moose_df$surveyed == 0] <- NA
head(moose_df)

## -----------------------------------------------------------------------------
ggplot(data = moose_df, aes(x = x, y = y)) +
  geom_point(aes(colour = total), size = 4) +
  scale_colour_viridis_c() +
  theme_bw()

## ---- results = "hide", fig.keep = "none"-------------------------------------
slmfit_out_moose <- slmfit(formula = total ~ strat, 
  data = moose_df, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential")
summary(slmfit_out_moose)
check.variogram(slmfit_out_moose)
qplot(residuals(slmfit_out_moose), bins = 20) + xlab("Residuals")
qplot(residuals(slmfit_out_moose, cross.validation = TRUE), bins = 20) +
  xlab("CV Residuals")

pred_moose <- predict(slmfit_out_moose)
get.predinfo(pred_moose)
get.predplot(pred_moose)

## -----------------------------------------------------------------------------
moose_df$fake_area <- c(rep(1, 700), rep(2, 160))

## -----------------------------------------------------------------------------
slmfit_out_moose_area <- slmfit(formula = total ~ strat, 
  data = moose_df, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", areacol = 'fake_area')
summary(slmfit_out_moose_area)

## -----------------------------------------------------------------------------
pred_obj_area <- predict(slmfit_out_moose_area)
head(pred_obj_area$Pred_df[ ,c("total_pred_density", "total_pred_count",
  "fake_area")])
tail(pred_obj_area$Pred_df[ ,c("total_pred_density", "total_pred_count",
  "fake_area")])

## -----------------------------------------------------------------------------
get.predinfo(pred_obj_area)

## -----------------------------------------------------------------------------
data(USlakes)

## -----------------------------------------------------------------------------
ggplot(data = USlakes, aes(x = log(DOC_RESULT))) + geom_histogram(bins = 20)

## -----------------------------------------------------------------------------
lakes <- USlakes[log(USlakes$DOC_RESULT) < 5, ]

## -----------------------------------------------------------------------------
nrow(lakes)

## -----------------------------------------------------------------------------
plot(USlakes$XCOORD, USlakes$YCOORD, pch = 19, 
  cex = 2 * log(lakes$DOC_RESULT) / max(log(lakes$DOC_RESULT)))

## -----------------------------------------------------------------------------
ggplot(data = lakes, aes(x = RVFPUNDWOODY_RIP, y = log(DOC_RESULT))) +
  geom_jitter(width = 0.02)

## -----------------------------------------------------------------------------
set.seed(2)
LakeObsID <- sample(1:nrow(lakes), 500)
lakeobs <- lakes
lakeobs$DOC_RESULT <- NA
lakeobs[LakeObsID, 'DOC_RESULT'] <- lakes[LakeObsID, 'DOC_RESULT']
lakeobs$wts <- 1 / nrow(lakeobs)

## -----------------------------------------------------------------------------
slmfitout_exp_lakes <- slmfit(formula = DOC_RESULT ~ ELEVATION + RVFPUNDWOODY_RIP + 
  FCIBIG_LIT + RVFCGNDBARE_RIP + RVFCGNDWOODY_RIP, data = lakeobs, 
  xcoordcol = 'XCOORD', ycoordcol = 'YCOORD', CorModel = "Exponential")
summary(slmfitout_exp_lakes)

## -----------------------------------------------------------------------------
slmfitout_sph_lakes <- slmfit(formula = DOC_RESULT ~ ELEVATION + RVFPUNDWOODY_RIP + 
  FCIBIG_LIT + RVFCGNDBARE_RIP + RVFCGNDWOODY_RIP, data = lakeobs, 
  xcoordcol = 'XCOORD', ycoordcol = 'YCOORD', CorModel = "Spherical")
summary(slmfitout_sph_lakes)

## -----------------------------------------------------------------------------
AIC(slmfitout_exp_lakes)
AIC(slmfitout_sph_lakes)

## -----------------------------------------------------------------------------
pred_exp_lakes <- predict(slmfitout_exp_lakes,  wtscol = "wts")
get.predinfo(pred_exp_lakes, conf_level = 0.95)
mean(lakes$DOC_RESULT)

## ---- results = "hide"--------------------------------------------------------
citation("sptotal")
