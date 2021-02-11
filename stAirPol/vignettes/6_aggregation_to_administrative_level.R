## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 8,
  fig.height = 6,
  comment = "#>"
)

## ----messages=FALSE,warning=FALSE, echo=FALSE, results='hide'------------
require(stAirPol)
require(spTimer)
require(data.table)

## ------------------------------------------------------------------------
# devtools::install_github("tidyverse/ggplot2")
require(ggplot2)


## ------------------------------------------------------------------------
data("muc_airPol_p2")
data("muc_airPol_p2_grid")
data <- clean_model_data(muc_airPol_p2)

## ------------------------------------------------------------------------
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
                         beta.prior = Norm(0, 10^4))
spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)
scale.transform = "SQRT"
cov.fnc = "exponential"
model.gpp <- fit_sp_model(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 20,
                          knots_method = 'random',
                          knots_plot = TRUE,
                          knots_seed = 220292,
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)

## --------------------------------------------------------------
# pred.gpp <- predict(model.gpp, muc_airPol_p2_grid)
pred.gpp <- predict_split(model.gpp, new_data = muc_airPol_p2_grid,
                          sample_count = 100)

## --------------------------------------------------------------
require(dplyr)
require(osmdata)

## --------------------------------------------------------------
q <- getbb("munich germany") %>%
  opq() %>%
  add_osm_feature("boundary", "administrative")
shape_muc <- osmdata_sf(q)$osm_multipolygons

## --------------------------------------------------------------
shape_muc <- shape_muc[as.character(shape_muc$admin_level) == 10, ]

## --------------------------------------------------------------
shape_muc <- shape_muc[grepl('Bezirksteil', shape_muc$name), ]

## --------------------------------------------------------------
ggplot(data = shape_muc) +
  geom_sf() + theme_void()

## --------------------------------------------------------------
g_admin_level <- aggregate_to_shape_file(prediction = pred.gpp,
                                         shape = shape_muc)
print(g_admin_level)

## --------------------------------------------------------------
pred.gpp.agg2 <- pred.gpp[, .(prediction = mean(prediction)),
                         by = list(lon, lat, sensor_id)]
g_temporal <- ggplot(data = pred.gpp.agg2, aes(x = lon, y = lat,
                                               fill = prediction)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  geom_tile() +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20),
         shape = "colorbar") +
  theme_void() +
  ggsn::scalebar(dist = 3, dd2km = TRUE, model = 'WGS84',
                 location = "bottomleft", st.dist = 0.05,
                 x.min = min(pred.gpp.agg2$lon),
                 x.max = max(pred.gpp.agg2$lon),
                 y.min = min(pred.gpp.agg2$lat),
                 y.max = max(pred.gpp.agg2$lat))
print(g_temporal)

