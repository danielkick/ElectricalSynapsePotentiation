# return_event_cor <- function(df = d,
#                              time.channel = "Time",
#                              ref.channel = "IN4",
#                              contrast.channel = "IN9",
#                              event.channel = "IN4.Bursts",
#                              start.feature = "Start",
#                              event.feature = "Event", # All
#                              cor.channel.name = "IN4.Corr",
#                              correlation.method = "pearson" # c("pearson", "kendall", "spearman")
# ) {
#   df <- as.data.frame(df)
#   df <- df[, c(time.channel, ref.channel, contrast.channel, event.channel)]
#
#
#   starts <- df[[time.channel]][df[[event.channel]] == start.feature]
#
#   df[[cor.channel.name]] <- NA
#
#   if (event.feature == "Event") {
#     for (i in seq(1, (length(starts) - 1))) {
#       df[(df[[time.channel]] >= starts[i] &
#             df[[time.channel]] < starts[i + 1]) &
#            df[[event.channel]] == event.feature, cor.channel.name] <- cor(
#              x = df[(df[[time.channel]] >= starts[i] &
#                        df[[time.channel]] < starts[i + 1]) &
#                       df[[event.channel]] == event.feature, ref.channel],
#              y = df[(df[[time.channel]] >= starts[i] &
#                        df[[time.channel]] < starts[i + 1]) &
#                       df[[event.channel]] == event.feature, contrast.channel],
#              method = correlation.method
#            )
#     }
#   } else if (event.feature == "All") {
#     for (i in seq(1, (length(starts) - 1))) {
#       df[(df[[time.channel]] >= starts[i] &
#             df[[time.channel]] < starts[i + 1]), cor.channel.name] <- cor(
#               x = df[(df[[time.channel]] >= starts[i] &
#                         df[[time.channel]] < starts[i + 1]), ref.channel],
#               y = df[(df[[time.channel]] >= starts[i] &
#                         df[[time.channel]] < starts[i + 1]), contrast.channel],
#               method = correlation.method
#             )
#     }
#   } else {
#     warning("Returning NA -- event.feature not recognized. Supply 'Event' or 'All'.")
#     df <- NA
#   }
#
#   return(df)
# }
