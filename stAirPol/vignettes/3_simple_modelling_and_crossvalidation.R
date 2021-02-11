## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 10,
  fig.height = 6,
  comment = "#>"
)

## ----messages=FALSE,warning=FALSE, echo=FALSE, results='hide'------------
require(stAirPol)
require(spTimer)
require(ggplot2)
require(data.table)

## ------------------------------------------------------------------------
data("muc_airPol_p2")

## ------------------------------------------------------------------------
data <- clean_model_data(muc_airPol_p2, timesIQR = 1.5)


## ------------------------------------------------------------------------
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)

## ------------------------------------------------------------------------
model.gp.p2 <- fit_sp_model(data = data, formula = formula, model = 'GP')

## ------------------------------------------------------------------------
summary(model.gp.p2)

## ------------------------------------------------------------------------
# plot(model.gp.p2)
# dev.off()

## ------------------------------------------------------------------------
priors <- spT.priors(model = "GP", inv.var.prior = Gamm(a = 2, b = 1),
                     beta.prior = Norm(0, 10^4))

## ------------------------------------------------------------------------
cov.fnc = "exponential"

## ------------------------------------------------------------------------
spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)

## ------------------------------------------------------------------------
report = 5

## ------------------------------------------------------------------------
scale.transform = "SQRT"
model.gp.p2.mod <- fit_sp_model(data = data,
                            formula = formula,
                            model = 'GP',
                            priors = priors,
                            cov.fnc = cov.fnc,
                            report = report,
                            scale.transform = scale.transform,
                            spatial.decay = spatial.decay)
summary(model.gp.p2.mod)

## ------------------------------------------------------------------------
training_set <- get_test_and_training_set(data, sampel_size = 0.75,
                                      random.seed = 220292)

## ------------------------------------------------------------------------
model.gp.p2 <- fit_sp_model(data = data, formula = formula, model = 'GP',
                            training_set = training_set)
model.gp.p2.mod <- fit_sp_model(data = data,
                                formula = formula,
                                model = 'GP',
                                priors = priors,
                                cov.fnc = cov.fnc,
                                report = report,
                                training_set = training_set,
                                scale.transform = scale.transform,
                                spatial.decay = spatial.decay)

## ------------------------------------------------------------------------
pred.gp.p2 <- predict(model.gp.p2, data, training_set)
pred.gp.p2.mod <- predict(model.gp.p2.mod, data, training_set)

## ------------------------------------------------------------------------
evaluate_prediction(pred.gp.p2)
evaluate_prediction(pred.gp.p2.mod)

## ------------------------------------------------------------------------
plot(pred.gp.p2.mod)

## ------------------------------------------------------------------------
plot(pred.gp.p2.mod, time_dimension = TRUE)

## ------------------------------------------------------------------------
priors.ar <- spT.priors(model = "AR",
                        inv.var.prior = Gamm(a = 2, b = 1),
                        beta.prior = Norm(0, 10^4),
                        rho.prior=Norm(0,10^10))
model.ar.p2 <- fit_sp_model(data = data,
                            formula = formula,
                            model = 'AR',
                            priors = priors.ar,
                            cov.fnc = cov.fnc,
                            report = report,
                            training_set = training_set,
                            scale.transform = scale.transform,
                            spatial.decay = spatial.decay)

pred.ar.p2 <- predict(model.ar.p2, data, training_set)
evaluate_prediction(pred.ar.p2)

## ------------------------------------------------------------------------
priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
                        beta.prior = Norm(0, 10^4))
model.gpp.p2 <- fit_sp_model(data = data,
                            formula = formula,
                            model = 'GPP',
                            priors = priors.gpp,
                            cov.fnc = cov.fnc,
                            knots_count = 4,
                            report = report,
                            training_set = training_set,
                            scale.transform = scale.transform,
                            spatial.decay = spatial.decay)

pred.gpp.p2 <- predict(model.gpp.p2, data, training_set)
evaluate_prediction(pred.gpp.p2)

## ------------------------------------------------------------------------
evaluate_prediction_table(list('pred.gp.p2' = pred.gp.p2,
                               'pred.gp.p2.mod' = pred.gp.p2.mod,
                               'pred.ar.p2' = pred.ar.p2,
                               'pred.gpp.p2' = pred.gpp.p2))

gridExtra::grid.arrange(grobs = list(
  plot(pred.gp.p2) + ggtitle('GP'),
  plot(pred.gp.p2.mod) + ggtitle('mod GP'),
  plot(pred.ar.p2) + ggtitle('AR'),
  plot(pred.gpp.p2) + ggtitle('GPP')
))

