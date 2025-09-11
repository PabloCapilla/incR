#' @title Quick visualisation of incubation temperatures, on-bouts and off-bouts
#' @description After \code{\link{incRscan}} has been used, \code{\link{incRplot}}
#' provides a quick visualisation of the incubation temperature trace with coloured
#' on- and off-bouts. Environmental temperatures can also be added to the plot
#' @param data data table with incubation temperature data
#' @param time.var Character string. Name of the variable with time of the day for
#' temperature data. Please, have time in 
#' decimal hours. If \code{\link{incRprep}} has been previously used, "dec_time" can
#' be used.
#' @param day.var Character string. Name of the variable with date for
#' temperature observation. No specific format is needed. 
#' If \code{\link{incRprep}} has been previously used, "date" can
#' be used.
#' @param inc.temperature.var Character string. Name of the variable with incubation 
#' temperatures.
#' @param env.temperature.var Character string. Name of the variable with environmental
#' temperatures. If no value is provided, a plot with no environmental temperatures
#' is produced.
#' @param vector.incubation name of the binary variable storing
#' information about the presence/absence of the incubating individual in the nest.
#' If \code{\link{incRscan}} has been used, "incR_score" can be used.
#' @return Plot of incubation temperature, on-bouts and off-bouts with (optional) 
#' environmental temperatures. The plot is generated using \code{\link[ggplot2]{ggplot2}}.The
#' user can customised the appearance of the plot using tools 
#' within \code{\link[ggplot2]{ggplot2}}(see example) 
#' @examples
#' # loading example data
#' data(incR_procdata)
#' my_plot <- incRplot(data = incR_procdata[complete.cases(incR_procdata$temperature),], 
#'                     time.var = "dec_time", 
#'                     day.var = "date", 
#'                     inc.temperature.var = "temperature", 
#'                     env.temperature.var = "env_temp",
#'                     vector.incubation = "incR_score")
#'                     
#' # see your plot
#' my_plot
#' 
#' # add new labels (ggplot2 required)
#' my_plot + ggplot2::labs(x = "New X label", y = "New Y label")
#' @seealso \code{\link{incRscan}} 
#' @export 

incRplot <- function (data, 
                      time.var, 
                      day.var, 
                      inc.temperature.var, 
                      env.temperature.var = NULL,
                      vector.incubation) {
  # vector.incubation has to be character or factor
  data[[vector.incubation]] <- base::as.factor(data[[vector.incubation]])
  
  new_incRplot <- ggplot2::ggplot(data = data, 
                                  ggplot2::aes_string(x = time.var, 
                                                      y = inc.temperature.var, 
                                                      color = vector.incubation)) +
    ggplot2::geom_line(color = "black", size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(.~base::get(day.var)) +
    ggplot2::scale_color_manual(values = c("#8da0cb", "#fc8d62"),
                                name = "", 
                                labels = c("Absence", "Presence"))  +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Time of the day", y = "Temperature") +
    ggplot2::scale_x_continuous(breaks = seq(1,24,2), labels = seq(1,24,2)) 
  
  
  if(is.null(env.temperature.var)) {
    return (new_incRplot) 
  } else {
    new_env_incRplot <- new_incRplot +
      ggplot2::geom_line(ggplot2::aes_string(x = time.var, y = env.temperature.var),
                         color = "#66c2a5",
                         size = 1.5) 
    return (new_env_incRplot) 
  }
}


