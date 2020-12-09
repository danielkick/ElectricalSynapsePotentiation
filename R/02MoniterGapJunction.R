
## Block 1 ====

## Reduce the multiple observations (for passive property data) into a single measurement of tecc/tevc per LC ====
collapse_duplicate_gj_obs <- function(input.df) {
  input.df$keep <- TRUE

  ref.input.df <- input.df %>% group_by(Experiment, Time, Inj_Cell) %>% tally()
  ref.input.df <- ref.input.df[ref.input.df$n > 1, ]

  for (i in seq(from = 1, to = nrow(ref.input.df))) {
    choose <- input.df[input.df$Experiment == as.character(ref.input.df[i, "Experiment"]) &
                         input.df$Time == as.double(ref.input.df[i, "Time"]) &
                         input.df$Inj_Cell == as.character(ref.input.df[i, "Inj_Cell"]), 2] %>% min()

    input.df[input.df$Experiment == as.character(ref.input.df[i, "Experiment"]) &
               input.df$Time == as.double(ref.input.df[i, "Time"]) &
               input.df$Inj_Cell == as.character(ref.input.df[i, "Inj_Cell"]) &
               input.df[, 2] != choose, "keep"] <- FALSE
  }

  input.df <- input.df[input.df$keep == TRUE, ]
  input.df <- input.df[!is.na(input.df$Experiment), !(names(input.df) %in% c("keep"))]
  return(input.df)
}

collapse_duplicate_current_obs <- function(input.df = htk.p,
                                           target.mV = 0,
                                           prefer.pn = "online", # online selects online p/n before manual (Only makes a difference for htk).
                                           prefer.recording = "lowest", # lowest selects the lowerst recording number. Hightest selects the hightest recording number.
                                           deduplicate = "closest" # of remaining values, select the one with mV closest to 0mV

) {
  if (TRUE == FALSE) {
    # This exists for debugging alone
    input.df <- htk.p
    target.mV <- 0
    prefer.pn <- "online"
    prefer.recording <- "lowest"
    deduplicate <- "closest"
  }

  round.bool <- (10 * round(input.df[, 6] / 10)) == target.mV
  # retain values closest to zero
  input.df <- input.df[round.bool, ]
  input.df <- input.df[!is.na(input.df$Experiment), ]
  # create distance from 0mV col to aid in choosing the best guess for 0mV
  input.df <- input.df %>% mutate(dist = abs(input.df[, 6] - target.mV))
  input.df$keep <- TRUE
  # if there are two zeroes, find them
  get_duplicate_obs <- function(input.df) {
    ref.df <- input.df %>% group_by(Experiment, Time, Inj_Cell) %>% tally()
    ref.df <- ref.df[ref.df$n > 1, ]
    return(ref.df)
  }
  ref.df <- get_duplicate_obs(input.df)

  ## Here we use the "keep" col defined previously to choose between duplicate observatoins

  if (prefer.pn == "online") {
    # select the value with the lowest
    print("retaining the online recordings")
    for (i in seq(from = 1, to = nrow(ref.df), by = 1)) {
      if (sum(input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                       input.df$Time == as.double(ref.df[i, "Time"]) &
                       input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]), "subtracted.rin"] == "NA") != 0) {
        #print(i)

        input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                   input.df$Time == as.double(ref.df[i, "Time"]) &
                   input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]) &
                   input.df$subtracted.rin != "NA", "keep"] <- FALSE
      }
    }
    input.df <- input.df[input.df$keep == TRUE, ]
    input.df <- input.df[!is.na(input.df$Experiment), ]
    # refresh ref.df after each winnowing
    ref.df <- get_duplicate_obs(input.df)
  }

  if (prefer.recording == "lowest") {
    # select the value with the lowest
    print("retaining the lowest recording number")
    for (i in seq(from = 1, to = nrow(ref.df), by = 1)) {
      #print(i)
      choose <- input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                           input.df$Time == as.double(ref.df[i, "Time"]) &
                           input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]), 2] %>% min(na.rm = TRUE)

      input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                 input.df$Time == as.double(ref.df[i, "Time"]) &
                 input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]) &
                 input.df[, 2] != choose, "keep"] <- FALSE
    }
    input.df <- input.df[input.df$keep == TRUE, ]
    input.df <- input.df[!is.na(input.df$Experiment), ]
    # refresh ref.df after each winnowing
    ref.df <- get_duplicate_obs(input.df)
  } else if (prefer.recording == "highest") {
    # select the value with the highest
    print("retaining the highest recording number")
    for (i in seq(from = 1, to = nrow(ref.df), by = 1)) {
      #print(i)
      choose <- input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                           input.df$Time == as.double(ref.df[i, "Time"]) &
                           input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]), 2] %>% max(na.rm = TRUE)

      input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                 input.df$Time == as.double(ref.df[i, "Time"]) &
                 input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]) &
                 input.df[, 2] != choose, "keep"] <- FALSE
    }
    input.df <- input.df[input.df$keep == TRUE, ]
    input.df <- input.df[!is.na(input.df$Experiment), ]
    # refresh ref.df after each winnowing
    ref.df <- get_duplicate_obs(input.df)
  }

  ## Here we deduplicate

  if (deduplicate == "closest") {
    # select the value with the smallest difference from zero
    print("retaining the closest to target")
    for (i in seq(from = 1, to = nrow(ref.df), by = 1)) {
      #print(i)
      choose <- input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                           input.df$Time == as.double(ref.df[i, "Time"]) &
                           input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]), "dist"] %>% min(na.rm = TRUE)

      input.df[input.df$Experiment == as.character(ref.df[i, "Experiment"]) &
                 input.df$Time == as.double(ref.df[i, "Time"]) &
                 input.df$Inj_Cell == as.character(ref.df[i, "Inj_Cell"]) &
                 input.df$dist != choose, "keep"] <- FALSE
    }
    input.df <- input.df[input.df$keep == TRUE, ]
    input.df <- input.df[!is.na(input.df$Experiment), ]
  }

  input.df <- input.df[, !(names(input.df) %in% c("dist", "keep"))]

  return(input.df)
}

## Calculate percent change for later use: ====
#not pretty but this'll get the job done
convert_data_to <- function(input.df = all.data,
                            convert.to = c("percent"), #percent, difference
                            data.cols = c(
                              "r11", "r12", "r1", "rc", "cc", "rmp", "gj",
                              "htk.peak.mV", "htk.peak.nA", "htk.end.mV", "htk.end.nA",
                              "a.peak.mV", "a.peak.nA", "a.end.mV", "a.end.nA"
                            )) {
  input.df <- input.df[!duplicated(input.df), ]
  input.df$inter <- interaction(input.df$Experiment, input.df$Inj_Cell)
  inters <- input.df$inter %>% unique()

  if (convert.to == "difference") {
    for (i in seq_along(inters)) {
      for (j in seq_along(data.cols)) {
        input.df[input.df$inter == inters[i], data.cols[j]] <- (input.df[input.df$inter == inters[i], data.cols[j]] - input.df[input.df$inter == inters[i] & input.df$Time == 0, data.cols[j]])
      }
    }
  }
  if (convert.to == "percent") {
    for (i in seq_along(inters)) {
      for (j in seq_along(data.cols)) {
        input.df[input.df$inter == inters[i], data.cols[j]] <- ((input.df[input.df$inter == inters[i], data.cols[j]] - input.df[input.df$inter == inters[i] & input.df$Time == 0, data.cols[j]])
                                                                / input.df[input.df$inter == inters[i] & input.df$Time == 0, data.cols[j]])*100 #This has been changed to percent difference
      }
    }
  }
  return(input.df)
}


## Deduplicate gap junction measurements ====
# Make versions of the dfs that have only one value for the gap junction measurements
deduplicate_gj_obs <- function(input.df = M.d) {
  input.df[, "obs"] <- interaction(input.df$Experiment, input.df$Time)
  uniq.obs <- unique(input.df$obs)
  for (i in 1:length(uniq.obs)) {
    input.df[input.df$obs == uniq.obs[[i]], "rc"] <- mean(input.df[input.df$obs == uniq.obs[[i]], "rc"], na.rm = T)
    input.df[input.df$obs == uniq.obs[[i]], "gj"] <- mean(input.df[input.df$obs == uniq.obs[[i]], "gj"], na.rm = T)
  }
  input.df <- input.df[!duplicated(input.df$obs), ]
  return(input.df)
}




## Block 2 Resampling functions ====

# run_standardized_lme_Phase <- function(response.var = "cc",
#                                  input.df = M[M$Condition %in% c("PS.0", "PS.22", "PS.45", "PS.90"), ],
#                                  mute.time = FALSE) {
#   tic <- Sys.time()
#
#   # Transform the data so that it's ready for the test
#   input.df <- input.df[!(is.na(input.df[[response.var]])), ]
#   input.df$response.var <- input.df[[response.var]]
#   # input.df$Time <- as.factor(input.df$Time)
#   input.df$interact <- interaction(input.df$Experiment, input.df$Inj_Cell)
#
#   input.df$Phase <- "None"
#
#   input.df[input.df$Condition %in% c("PS.0"),  "Phase"] <- "0"
#   input.df[input.df$Condition %in% c("PS.22"), "Phase"] <- "22"
#   input.df[input.df$Condition %in% c("PS.45"), "Phase"] <- "45"
#   input.df[input.df$Condition %in% c("PS.90"), "Phase"] <- "90"
#
#
#   input.df <- input.df[, c("response.var", "interact", "Time", "Phase"
#                            # , "Amp"
#                            )]
#
#   # First model with observed data
#   fm <- lme(response.var ~ Time * Phase, random = ~1 | interact, method = "ML", data = input.df)
#   stats.table <- anova.lme(fm)
#
#   toc <- Sys.time()
#
#   if (mute.time != TRUE){
#     print(toc - tic)
#   }
#   return(stats.table)
# }

run_standardized_lme_resample_Phase <- function(response.var = "cc",
                                                input.df = M[M$Condition %in% c("PS.0", "PS.22", "PS.45", "PS.90"), ],
                                                nreps = 1e4,
                                                use.seed = 432431,
                                                mute.completion = FALSE,
                                                mute.time = FALSE) {
  #for debugging
  if (F == T){
    response.var = "cc"
    input.df = M[M$Condition %in% c("PS.0", "PS.22", "PS.45", "PS.90"), ]
    nreps = 100
    use.seed = 432431
    mute.completion = FALSE
    mute.time = FALSE
  }


  tic <- Sys.time()

  # Transform the data so that it's ready for the test
  input.df <- input.df[!(is.na(input.df[[response.var]])), ]
  input.df$response.var <- input.df[[response.var]]
  # input.df$Time <- as.factor(input.df$Time)
  input.df$interact <- interaction(input.df$Experiment, input.df$Inj_Cell)

  input.df$Phase <- "None"

  input.df[input.df$Condition %in% c("PS.0"),  "Phase"] <- "0"
  input.df[input.df$Condition %in% c("PS.22"), "Phase"] <- "22"
  input.df[input.df$Condition %in% c("PS.45"), "Phase"] <- "45"
  input.df[input.df$Condition %in% c("PS.90"), "Phase"] <- "90"


  input.df <- input.df[, c("response.var", "interact", "Time", "Phase"
                           # , "Amp"
  )]

  # First model with observed data
  fm <- lme(response.var ~ Time * Phase, random = ~1 | interact, method = "ML", data = input.df)
  stats.table <- anova.lme(fm)

  F_stats <- matrix(NA, nrow = nreps, ncol = length(stats.table$`F-value`))
  F_stats[1, ] <- stats.table$`F-value`


  set.seed(use.seed)
  for (ii in 2:nreps) {

    if (mute.completion != TRUE){
      print(paste0(((ii/nreps)*100), "%"))
    }
    rand.lme <- try(lme(sample(response.var) ~ Time * Phase, random = ~1 | interact, method = "ML", data = input.df))
    F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)


    # In case there is a convergence issue we immediately replace that value to get the target number of reps.
    while (is.na(as.numeric(F_stats[ii, 1]))) {
      print("Replacing model with singular convergence")
      rand.lme <- try(lme(sample(response.var) ~ Time * Phase, random = ~1 | interact, method = "ML", data = input.df))
      F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)

    }
  }

  F_stats <- as.data.frame(F_stats)
  names(F_stats) <- rownames(stats.table)

  #Convert from factor/cha to int
  walk(1:4, function(X){
    F_stats[,X] <<- as.numeric(F_stats[,X])
  })
  toc <- Sys.time()

  if (mute.time != TRUE){
    print(toc - tic)
  }
  return(na.omit(F_stats))
}



run_standardized_lme_resample_Phase_Orig <- function(response.var = "cc",
                                                     input.df = M[M$Condition %in% c("PS.0.orig",
                                                                                     "PS.22.orig",
                                                                                     "PS.45.orig",
                                                                                     "PS.90.orig",
                                                                                     "PS.180.orig"), ],
                                                     nreps = 1e4,
                                                     use.seed = 432431,
                                                     mute.completion = FALSE,
                                                     mute.time = FALSE) {
  #for debugging
  if (F == T){
    response.var = "cc"
    input.df = M[M$Condition %in% c("PS.0", "PS.22", "PS.45", "PS.90"), ]
    nreps = 100
    use.seed = 432431
    mute.completion = FALSE
    mute.time = FALSE
  }


  tic <- Sys.time()

  # Transform the data so that it's ready for the test
  input.df <- input.df[!(is.na(input.df[[response.var]])), ]
  input.df$response.var <- input.df[[response.var]]
  # input.df$Time <- as.factor(input.df$Time)
  input.df$interact <- interaction(input.df$Experiment, input.df$Inj_Cell)

  input.df$Phase <- "None"

  input.df[input.df$Condition %in% c("PS.0.orig"),  "Phase"] <- "0"
  input.df[input.df$Condition %in% c("PS.22.orig"), "Phase"] <- "22"
  input.df[input.df$Condition %in% c("PS.45.orig"), "Phase"] <- "45"
  input.df[input.df$Condition %in% c("PS.90.orig"), "Phase"] <- "90"
  input.df[input.df$Condition %in% c("PS.180.orig"), "Phase"] <- "90"

  input.df <- input.df[, c("response.var", "interact", "Time", "Phase"
                           # , "Amp"
  )]

  # First model with observed data
  fm <- lme(response.var ~ Time * Phase, random = ~1 | interact, method = "ML", data = input.df)
  stats.table <- anova.lme(fm)

  F_stats <- matrix(NA, nrow = nreps, ncol = length(stats.table$`F-value`))
  F_stats[1, ] <- stats.table$`F-value`


  set.seed(use.seed)
  for (ii in 2:nreps) {

    if (mute.completion != TRUE){
      print(paste0(((ii/nreps)*100), "%"))
    }
    rand.lme <- try(lme(sample(response.var) ~ Time * Phase, random = ~1 | interact, method = "ML", data = input.df))
    F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)


    # In case there is a convergence issue we immediately replace that value to get the target number of reps.
    while (is.na(as.numeric(F_stats[ii, 1]))) {
      print("Replacing model with singular convergence")
      rand.lme <- try(lme(sample(response.var) ~ Time * Phase, random = ~1 | interact, method = "ML", data = input.df))
      F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)

    }
  }

  F_stats <- as.data.frame(F_stats)
  names(F_stats) <- rownames(stats.table)

  #Convert from factor/cha to int
  walk(1:4, function(X){
    F_stats[,X] <<- as.numeric(F_stats[,X])
  })
  toc <- Sys.time()

  if (mute.time != TRUE){
    print(toc - tic)
  }
  return(na.omit(F_stats))
}


# For 2x2

run_standardized_lme_resample <- function(response.var = "cc",
                                          input.df = M[M$Condition %in% c("PS.0.High.Amp", "PS.0", "PS.22", "PS.22.High.Amp"), ],
                                          nreps = 1e4,
                                          use.seed = 432431,
                                          mute.completion = FALSE,
                                          mute.time = FALSE) {
  #for debugging
  if (F == T){
    response.var = "cc"
    input.df = M[M$Condition %in% c("PS.0.High.Amp", "PS.0", "PS.22", "PS.22.High.Amp"), ]
    nreps = 100
    use.seed = 432431
    mute.completion = FALSE
    mute.time = FALSE
  }

  tic <- Sys.time()

  # Transform the data so that it's ready for the test
  input.df <- input.df[!(is.na(input.df[[response.var]])), ]
  input.df$response.var <- input.df[[response.var]]
  # input.df$Time <- as.factor(input.df$Time)
  input.df$interact <- interaction(input.df$Experiment, input.df$Inj_Cell)

  input.df$Phase <- "None"
  input.df[input.df$Condition %in% c("PS.22", "PS.22.High.Amp"), "Phase"] <- "Offset"
  input.df[input.df$Condition %in% c("PS.0", "PS.0.High.Amp"), "Phase"] <- "Normal"
  input.df$Amp <- "None"
  input.df[input.df$Condition %in% c("PS.0.High.Amp", "PS.22.High.Amp"), "Amp"] <- "High"
  input.df[input.df$Condition %in% c("PS.0", "PS.22"), "Amp"] <- "Normal"

  input.df <- input.df[, c("response.var", "interact", "Time", "Phase", "Amp")]

  # First model with observed data
  fm <- lme(response.var ~ Time * Phase + Time * Amp + Phase * Amp, random = ~1 | interact, method = "ML", data = input.df)
  stats.table <- anova.lme(fm)

  F_stats <- matrix(NA, nrow = nreps, ncol = length(stats.table$`F-value`))
  F_stats[1, ] <- stats.table$`F-value`


  set.seed(use.seed)
  for (ii in 2:nreps) {

    if (mute.completion != TRUE){
      print(paste0(((ii/nreps)*100), "%"))
    }
    rand.lme <- try(lme(sample(response.var) ~ Time * Phase + Time * Amp + Phase * Amp, random = ~1 | interact, method = "ML", data = input.df))
    F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)


    # In case there is a convergence issue we immediately replace that value to get the target number of reps.
    while (is.na(as.numeric(F_stats[ii, 1]))) {
      print("Replacing model with singular convergence")
      rand.lme <- try(lme(sample(response.var) ~ Time * Phase + Time * Amp + Phase * Amp, random = ~1 | interact, method = "ML", data = input.df))
      F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)

    }
  }

  F_stats <- as.data.frame(F_stats)
  names(F_stats) <- rownames(stats.table)

  #Convert from factor/cha to int
  walk(1:7, function(X){
    F_stats[,X] <<- as.numeric(F_stats[,X])
  })
  toc <- Sys.time()

  if (mute.time != TRUE){
    print(toc - tic)
  }
  return(na.omit(F_stats))
}


run_standardized_lme_resample_W_45 <- function(response.var = "cc",
                                               input.df = M[M$Condition %in% c("PS.0.High.Amp", "PS.0", "PS.45", "PS.22.High.Amp"), ],
                                               nreps = 1e4,
                                               use.seed = 432431,
                                               mute.completion = FALSE,
                                               mute.time = FALSE) {
  #for debugging
  if (F == T){
    response.var = "cc"
    input.df = M[M$Condition %in% c("PS.0.High.Amp", "PS.0", "PS.45", "PS.22.High.Amp"), ]
    nreps = 100
    use.seed = 432431
    mute.completion = FALSE
    mute.time = FALSE
  }

  tic <- Sys.time()

  # Transform the data so that it's ready for the test
  input.df <- input.df[!(is.na(input.df[[response.var]])), ]
  input.df$response.var <- input.df[[response.var]]
  # input.df$Time <- as.factor(input.df$Time)
  input.df$interact <- interaction(input.df$Experiment, input.df$Inj_Cell)

  input.df$Phase <- "None"
  input.df[input.df$Condition %in% c("PS.45", "PS.22.High.Amp"), "Phase"] <- "Offset"
  input.df[input.df$Condition %in% c("PS.0", "PS.0.High.Amp"), "Phase"] <- "Normal"
  input.df$Amp <- "None"
  input.df[input.df$Condition %in% c("PS.0.High.Amp", "PS.22.High.Amp"), "Amp"] <- "High"
  input.df[input.df$Condition %in% c("PS.0", "PS.45"), "Amp"] <- "Normal"

  input.df <- input.df[, c("response.var", "interact", "Time", "Phase", "Amp")]

  # First model with observed data
  fm <- lme(response.var ~ Time * Phase + Time * Amp + Phase * Amp, random = ~1 | interact, method = "ML", data = input.df)
  stats.table <- anova.lme(fm)

  F_stats <- matrix(NA, nrow = nreps, ncol = length(stats.table$`F-value`))
  F_stats[1, ] <- stats.table$`F-value`


  set.seed(use.seed)
  for (ii in 2:nreps) {

    if (mute.completion != TRUE){
      print(paste0(((ii/nreps)*100), "%"))
    }
    rand.lme <- try(lme(sample(response.var) ~ Time * Phase + Time * Amp + Phase * Amp, random = ~1 | interact, method = "ML", data = input.df))
    F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)


    # In case there is a convergence issue we immediately replace that value to get the target number of reps.
    while (is.na(as.numeric(F_stats[ii, 1]))) {
      print("Replacing model with singular convergence")
      rand.lme <- try(lme(sample(response.var) ~ Time * Phase + Time * Amp + Phase * Amp, random = ~1 | interact, method = "ML", data = input.df))
      F_stats[ii, ] <- try(anova.lme(rand.lme)$`F-value`)

    }
  }

  F_stats <- as.data.frame(F_stats)
  names(F_stats) <- rownames(stats.table)

  #Convert from factor/cha to int
  walk(1:7, function(X){
    F_stats[,X] <<- as.numeric(F_stats[,X])
  })
  toc <- Sys.time()

  if (mute.time != TRUE){
    print(toc - tic)
  }
  return(na.omit(F_stats))
}


## Block 3 ggplot ====
kickme <- function(base_size = 14,
                   base_family = "sans") {
  # colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
  (
    ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
      theme(
        line = element_line(colour = "azure2"),
        rect = element_rect(
          # fill = colors["Light Gray"],
          fill = "transparent",
          color = NA,
          linetype = 0,
          colour = NA
        ),
        # text = element_text(colour = colors["Dark Gray"]),
        # axis.title = element_blank(),
        # axis.text = element_text(),

        axis.text.x = element_text(
          size = 12,
          colour = "Black",
          angle = 0
        ),
        axis.text.y = element_text(
          size = 12,
          colour = "Black",
          angle = 0
        ),
        axis.title.x = element_text(
          size = 18,
          colour = "Black",
          angle = 0
        ),
        axis.title.y = element_text(
          size = 18,
          colour = "Black",
          angle = 90
        ),

        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.background = element_rect(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        # panel.grid = element_line(colour = NULL),
        # panel.grid.major = element_line(colour = colors["Medium Gray"]),
        # panel.grid.minor = element_blank(),

        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),

        plot.title = element_text(
          hjust = 0,
          size = rel(1.5),
          face = "bold"
        ),
        plot.margin = unit(c(
          1,
          1, 1, 1
        ), "lines"),
        # strip.background = element_rect()
        strip.background = element_rect(fill = "transparent", colour = NA),

        strip.text.x = element_text(
          size = 18,
          colour = "Black",
          angle = 0
        )
      )
  )
}


## Block 4 More resampling functions ====
# Generic function for calculating an epirical p from an array (assumes one tail and observed value is at index 1) intended for f distributions
one_tail_to_epval <- function(input.array = me_all[[4]][,2], tail = "upper"){
  if (tail == "upper"){
    more.extreme <- sum(input.array >= input.array[1], na.rm = T)
    return(more.extreme / (length(input.array) - sum(is.na(input.array))))
  }else if (tail == "lower"){
    more.extreme <- sum(input.array <= input.array[1], na.rm = T)
    return(more.extreme / (length(input.array) - sum(is.na(input.array))))
  }else{
    warning("Please set tail to either \"upper\" or \"lower\" \nReturning nothing.")
  }
}

# Generic function for calculating an epirical p from an array (assumes two tailed, symmetric, and observed value is at index 1)
two_tail_to_epval <- function(input.array = ls_means$OH){
  xbar <- mean(input.array, na.rm = T)
  if (input.array[1] < xbar){
    more.extreme <- sum(input.array <= input.array[1], na.rm = T) +
      sum(input.array >= (xbar+(xbar - input.array[1])), na.rm = T) #reflected across the mean to make this two tailed
  } else if (input.array[1] > xbar){
    more.extreme <- sum(input.array >= input.array[1], na.rm = T) +
      sum(input.array <= (xbar+(xbar - input.array[1])), na.rm = T) #reflected across the mean to make this two tailed
  } else {
    warning("Observed == sample mean!")
    more.extreme <- length(input.array) - sum(is.na(input.array))
  }
  return(more.extreme/(length(input.array) - sum(is.na(input.array))))
}

## Block 1 ====
## Block 1 ====
## Block 1 ====

