# return_period <- function(df = d,
#                           time.channel = "time",
#                           event.channel.name = "in14.bursts"){
#   df <- as.data.frame(df)
#   starts <- df[df[[event.channel.name]] %in% c("Start"), c(time.channel, event.channel.name)]
#   starts$starts.shift <- c(starts[[time.channel]][seq(from = 2, to = nrow(starts), by = 1)],0)
#   starts$period <- starts[["starts.shift"]]- starts[[time.channel]]
#   #Since we don't have the start of the next cycle, the last observation's period is set to NA
#   starts[nrow(starts), "period"] <- NA
#
#   # fill in df's period col
#   df$period <- NA
#   for (i in seq(from = 1, to = nrow(starts)-1, by = 1)){
#     df[(df[[time.channel]] >= starts[[time.channel]][i]) &
#          (df[[time.channel]] < starts[[time.channel]][i+1]), "period"] <- starts[i, "period"]
#   }
#
#   return(df[,c("period")])
# }
#
#
# ### ####
# return_delay <- function(df = d,
#                          time.channel = "time",
#                          event.channel.1 = "in14.bursts",
#                          event.channel.2 = "in15.bursts",
#                          rename.cols = c("d_in14", "in14_d"),
#                          ...){
#   # df = trace
#   # time.channel = "time"
#   # event.channel.1 = "in14.bursts"
#   # event.channel.2 = "in15.bursts"
#   # rename.cols = c("d_in14", "in14_d")
#
#   df <- as.data.frame(df)
#   starts.1 <- df[df[[event.channel.1]] %in% c("Start"), c(time.channel, event.channel.1)]
#   starts.1$id <- "one"
#   starts.2 <- df[df[[event.channel.2]] %in% c("Start"), c(time.channel, event.channel.2)]
#   starts.2$id <- "two"
#
#   starts <- full_join(starts.1[, c(time.channel, "id")],
#                       starts.2[, c(time.channel, "id")])
#   starts <- starts[order(starts$time),]
#
#
#   # To avoid looping through all (i, i+1) pairs, we're shifting the df up and down so we can slice it up based on which columns match.
#   starts.prev <- rbind(c(0, 0), starts[seq(1, to =nrow(starts)-1), ]) #shift everything down by one
#   starts.next <- rbind(starts[seq(2, to =nrow(starts)), ], c(0, 0)) #shift everything up by one
#   names(starts.prev) <- c("time.prev", "id.prev")
#   names(starts.next) <- c("time.post", "id.post")
#
#   starts <- cbind(starts, starts.prev, starts.next)
#
#   #col for burst in 1n14 -> in15 and another for in15 -> in14. Then if we want the minimum dist, it's easy to slice by the min absolute value.
#   starts$delay.prev <- NA
#   starts$delay.post <- NA
#
#   starts[starts$id.prev != starts$id, "delay.prev"] <-
#     starts[starts$id.prev != starts$id, "time.prev"] - starts[starts$id.prev != starts$id, time.channel]
#
#   starts[starts$id != starts$id.post, "delay.post"] <-
#     starts[starts$id != starts$id.post, time.channel] - starts[starts$id != starts$id.post, "time.post"]
#
#   #first delay.prev and last delay.post are meaningless
#   starts[1, "delay.prev"] <- NA
#   starts[nrow(starts), "delay.post"] <- NA
#
#
#
#
#   # fill in df's period cols
#   df$delay.prev <- NA
#   df$delay.post <- NA
#
#   for (i in seq(from = 1, to = nrow(starts.2)-1, by = 1)){
#
#     df[(df[[time.channel]] >= starts.2[[time.channel]][i]) &
#          (df[[time.channel]] < starts.2[[time.channel]][i+1]), "delay.prev"] <-
#       starts[starts[[time.channel]] == starts.2[i, time.channel], "delay.prev"]
#
#     df[(df[[time.channel]] >= starts.2[[time.channel]][i]) &
#          (df[[time.channel]] < starts.2[[time.channel]][i+1]), "delay.post"] <-
#       starts[starts[[time.channel]] == starts.2[i, time.channel], "delay.post"]
#
#   }
#
#   df <- df[,c("delay.prev","delay.post")]
#
#   if (length(rename.cols) == 2){
#     names(df) <- rename.cols
#   } else {
#     warning("rename.cols is not the same lenght as output! Using default names.")
#   }
#
#   return(df)
# }
#
# ### ####
# return_phase <- function(df = trace,
#                          delay = "in15_d",
#                          period = "in14.period"){
#   df <- as.data.frame(df)
#
#   return(df[[delay]] / df[[period]])
# }
