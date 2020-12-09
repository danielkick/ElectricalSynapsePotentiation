# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

return_dVdT <- function(df = d,
                        channel = "IN9",
                        time.channel = "Time",
                        time.step = 100,
                        dVdT.channel.name = "IN9.dVdT"){
  df <- df[, c(time.channel, channel)]
  names(df) <- c("Time", "Voltage")
  df$dVdT <- NA

  #end.steps <- seq(from = min(df[[time.channel]]), to = max(df[[time.channel]]), by = time.step)
  steps <- seq(from = min(df[["Time"]]), to = max(df[["Time"]]), by = time.step)

  for (i in seq_along(steps)){
    if (i != length(steps)){
      start.step <- steps[i]
      stop.step <- steps[i+1]
      #print(i)
      fm <- lm(Voltage ~ Time, data = df[df$Time >= start.step &
                                           df$Time < stop.step, ])

      df[df$Time >= start.step & df$Time < stop.step, "dVdT"] <- fm$coefficients[2]

    }
  }
  #Set to user names
  names(df) <- c(time.channel, channel, dVdT.channel.name)
  return(df)
}
