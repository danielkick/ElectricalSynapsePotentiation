# # Input two event columns, specify one reference column
# # Get period from each ref
# # Return Start.Col2 - Start.Ref / Period.Ref
# solve_delay_period_phase <- function(df = d,
#                                      time.channel = "Time",
#                                      ref.event.channel = "IN9.Bursts",
#                                      contrast.event.channel = "IN4.Bursts",
#                                      relevant.feature = "Start", # or "End"
#                                      output.channel.prefix = "IN9.IN4") {
#   df <- as.data.frame(df)
#
#   # reduce df size
#   df <- df[, c(time.channel, ref.event.channel, contrast.event.channel)]
#
#   ref.start.times <- df[df[[ref.event.channel]] == relevant.feature, ]
#
#
#   shifted.feature.times <- c(ref.start.times[ref.start.times[[ref.event.channel]] == relevant.feature, time.channel][-1], 0)
#
#   ref.start.times[, "Period"] <- shifted.feature.times - ref.start.times[[time.channel]]
#
#   contrast.start.times <- df[df[[contrast.event.channel]] == relevant.feature, ]
#
#   # label and merge event times so we can get delay easily
#   temp1 <- ref.start.times[, c(time.channel, ref.event.channel)]
#   temp2 <- contrast.start.times[, c(time.channel, ref.event.channel)]
#   temp1[, 2] <- "ref"
#   temp2[, 2] <- "contrast"
#   names(temp1) <- names(temp2) <- c(time.channel, "id")
#
#   temp1 <- full_join(temp1, temp2)
#   temp1 <- arrange(temp1, temp1[[time.channel]])
#   temp1$Delay <- NA
#   # For the first event we only have to consider i and i+1,
#   # but for the rest we have to use the minimum delay, looking at
#   # i - (i+1) AND i - (i-1)
#
#   for (i in seq(from = 1, to = (length(temp1$id) - 1))) {
#     if (i == 1) {
#       # this let's us compare all three but really just compare i vs j when i == 1
#       id.h <- temp1[i, "id"]
#     } else {
#       id.h <- temp1[i - 1, "id"]
#     }
#     id.i <- temp1[i, "id"]
#     id.j <- temp1[i + 1, "id"]
#
#
#     diff.hi <- Inf
#     diff.ij <- Inf
#     if (i > 1) {
#       diff.hi <- abs(temp1[i - 1, time.channel] - temp1[i, time.channel])
#     }
#     diff.ij <- abs(temp1[i, time.channel] - temp1[i + 1, time.channel])
#
#
#     if ((id.h == id.i) & (id.i == id.j)) {
#       # do nothing. activity hasn't switched
#     } else if ((id.h == id.i) | (diff.hi > diff.ij)) {
#       # Either there is only one comparison that is valid OR ij is the smaller distance
#       if (id.i == "ref") {
#         temp1[i, "Delay"] <- diff.ij
#       } else {
#         temp1[i, "Delay"] <- diff.ij * -1
#       }
#     } else if ((id.i == id.j) | (diff.hi <= diff.ij)) {
#       # Either there is only one comparison that is valid OR ij is the smaller distance OR they're equal and it doesn't matter which sign is used.
#
#       # This is flipped because we're considering i-1 and i
#       if (id.i == "contrast") {
#         temp1[i, "Delay"] <- diff.hi
#       } else {
#         temp1[i, "Delay"] <- diff.hi * -1
#       }
#     } else {
#       warning("Somehthing unexpected happened.\n Refer to source code")
#     }
#   }
#
#   # for (i in seq(from = 1, to = (length(temp1$id)-1) )){
#   #   # if they are the same then there's no value to return
#   #   if (temp1$id[i] == temp1$id[i+1]){
#   #     # do nothing, leave as NA
#   #   } else {
#   #     # check if the time delay between i and i+1 or i and i-1 is
#   #
#   #   # if contrast is first, make sure delay is negative
#   #     delay <- abs(temp1[i, time.channel] - temp1[i+1, time.channel])
#   #     if (temp1[i, "id"] == "ref"){
#   #       temp1[i, "Delay"] <- delay
#   #     } else {
#   #       temp1[i, "Delay"] <- delay*-1
#   #     }
#   #
#   #   }
#   # }
#
#   # Add delay to original df
#   df$Delay <- NA
#   for (i in seq(from = 1, to = (length(temp1[[time.channel]]) - 1))) {
#     df[df[[time.channel]] >= temp1[i, time.channel] &
#          df[[time.channel]] < temp1[i + 1, time.channel], "Delay"] <- temp1[i, "Delay"]
#   }
#
#   # Add period to original df
#   df$Period <- NA
#   for (i in seq(from = 1, to = (length(ref.start.times[[time.channel]]) - 1))) {
#     df[df[[time.channel]] >= ref.start.times[i, time.channel] &
#          df[[time.channel]] < ref.start.times[i + 1, time.channel], "Period"] <- ref.start.times[i, "Period"]
#   }
#
#   # Add Phase where we have both Delay and Period
#   df$Phase <- NA
#   df[!is.na(df$Delay) & !is.na(df$Period), "Phase"] <- df[!is.na(df$Delay) & !is.na(df$Period), "Delay"] / df[!is.na(df$Delay) & !is.na(df$Period), "Period"]
#
#
#   names(df) <- c(time.channel,
#                  ref.event.channel,
#                  contrast.event.channel,
#                  paste0(output.channel.prefix, ".Delay"),
#                  paste0(output.channel.prefix, ".Period"),
#                  paste0(output.channel.prefix, ".Phase"))
#   return(df)
# }
