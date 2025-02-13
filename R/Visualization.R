#' Plot Main Effect
#' @param evalData Model output as obtained from ONAM::evaluateModel
#' @param effect Effect to be plotted, must be present in the model formula. For interaction terms, use plotInteractionEffect
#' @returns Returns a ggplot2 object of the specified effect
#' @examples
#' \donttest{
#' # Basic example for a simple ONAM-model
#' # Create training data
#' n <- 1000
#' x1 <- runif(n, -2, 2)
#' x2 <- runif(n, -2, 2)
#' y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
#'   pweibull(-x2, shape = 0.5)) +
#'   x1 * x2
#' trainDat <- cbind(x1, x2, y)
#' # Define model
#' model_formula <- y ~ mod1(x1) + mod1(x2) +
#'   mod1(x1, x2)
#' list_of_deep_models <- list(mod1 = ONAM:::getSubModel)
#' # Fit model
#' mod <- fitPHOModel(model_formula, list_of_deep_models,
#'                    trainDat, nEnsemble = 2,
#'                    progresstext = T, verbose = 1)
#' evalData <- evaluateModel(mod)
#' plotMainEffect(evalData, "x1")
#' }
#' @export plotMainEffect
plotMainEffect <- function(evalData, effect)
{
  if(!effect %in% colnames(evalData$finalTotalPredictions))
  {
    stop(paste(effect,
               " is not present in the fitted model effects.",
               sep = ""))
  }
  plotData <-
    data.frame(x = evalData$data[,effect],
               y = evalData$finalTotalPredictions[,effect])
  out_plot <- ggplot2::ggplot(plotData, aes(x = x, y = y)) +
    ggplot2::geom_point() + ggplot2::ylab("Effect") +
    ggplot2::xlab(effect)
  return(out_plot)
}
#' Plot Main Effect
#' @param evalData Model output as obtained from ONAM::evaluateModel
#' @param effect1 First effect to be plotted
#' @param effect2 Second effect to be plotted
#' @param interpolate If TRUE, values will be interpolated for a smooth plot. If FALSE (default), only observations in the data will be plotted.
#' @param customColors color palette object for the interaction plot. Default is "spectral", returning a color palette based on the spectral theme.
#' @param n_interpolate number of values per coordinate axis to interpolate. Ignored if 'interpolate = FALSE'.
#' @returns Returns a 'ggplot2' object of the specified effect interaction
#' @examples
#' \donttest{
#' # Basic example for a simple ONAM-model
#' # Create training data
#' n <- 1000
#' x1 <- runif(n, -2, 2)
#' x2 <- runif(n, -2, 2)
#' y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
#'   pweibull(-x2, shape = 0.5)) +
#'   x1 * x2
#' trainDat <- cbind(x1, x2, y)
#' # Define model
#' model_formula <- y ~ mod1(x1) + mod1(x2) +
#'   mod1(x1, x2)
#' list_of_deep_models <- list(mod1 = ONAM:::getSubModel)
#' # Fit model
#' mod <- fitPHOModel(model_formula, list_of_deep_models,
#'                    trainDat, nEnsemble = 2,
#'                    progresstext = T, verbose = 1)
#' evalData <- evaluateModel(mod)
#' plotInterEffect(evalData, "x1", "x2", interpolate = TRUE)
#' }
#' @export plotInterEffect
plotInterEffect <- function(evalData, effect1, effect2,
                            interpolate = FALSE,
                            customColors = "spectral",
                            n_interpolate = 200)
{
  inter <- paste(effect1, effect2, sep = "_")
  if(!inter %in% colnames(evalData$finalTotalPredictions))
  {
    inter <- paste(effect2, effect1, sep = "_")
    tmp <- effect1
    effect1 <- effect2
    effect2 <- tmp
    if(!inter %in% colnames(evalData$finalTotalPredictions))
    {
      stop(paste("No interaction effect fitted for ",
                 effect1, " and ", effect2,".", sep = ""))
    }
  }
  if(typeof(customColors) != "closure")
  {
    if(customColors == "spectral")
    {
      customColors <-
        grDevices::colorRampPalette(colors = (x = RColorBrewer::brewer.pal(n = 11, name = "Spectral")))
    }
  }
  if(interpolate)
  {
    tmpInterp <-
      akima::interp(x = evalData$data[,effect1],
                    y = evalData$data[,effect2],
                    z =
                      evalData$finalTotalPredictions[,inter],
                    nx = n_interpolate, ny = n_interpolate,
                    duplicate = "mean")
    plotData <-
      data.frame(x = rep(tmpInterp$x, length(tmpInterp$y)),
                 y = rep(tmpInterp$y, each = length(tmpInterp$x)),
                 Prediction = tmpInterp$z %>% c())
    plotData <- plotData[which(!is.na(plotData$Prediction)),]
    aes_gradient <-
      ggplot2::scale_fill_gradientn(colors =
                                      customColors(n = 100),
                                    values =
                                      scales::rescale(c(min(plotData$Prediction),
                                                        mean(plotData$Prediction),
                                                        max(plotData$Prediction))),
                                    guide = "colorbar",
                                    limits = c(min(plotData$Prediction),
                                               max(plotData$Prediction)))
    geom_param <- ggplot2::geom_tile()
    aes_param <- ggplot2::aes(x = x, y = y, fill = Prediction)
  }else
  {
    plotData <-
      data.frame(x = evalData$data[,effect1],
                 y = evalData$data[,effect2],
                 Prediction =
                   evalData$finalTotalPredictions[,inter])
    aes_gradient <-
      ggplot2::scale_color_gradientn(colors =
                              customColors(n = 100),
                            values =
                              scales::rescale(c(min(plotData$Prediction),
                                                mean(plotData$Prediction),
                                                max(plotData$Prediction))),
                            guide = "colorbar",
                            limits = c(min(plotData$Prediction),
                                       max(plotData$Prediction)))
    geom_param <- ggplot2::geom_point()
    aes_param <- ggplot2::aes(x = x, y = y, color = Prediction)
  }
  inter_theme <- ggplot2::theme(plot.title.position = "plot",
                       plot.caption.position =  "plot",
                       # plot.margin = grid::unit(c(0,0,0,0), "mm"),
                       panel.grid = element_blank(),
                       panel.background = element_blank(),
                       legend.position = "right")
  out_plot <-
    ggplot2::ggplot(plotData, aes_param) +
    # geom_point(size = 0.75) +
    geom_param +
    aes_gradient +
    inter_theme +
    ggplot2::ylab(effect1) + ggplot2::xlab(effect2)
  return(out_plot)
}
