#' Add Metadata
#'
#' This function uses a dataframe with the metadata for a set of observations to label the observations themselves.
#'
#'@title Add Metadata
#'@aliases
#'
#'@param input.df This data.frame holds the observations.
#'@param metadata.df This data.frame holds the metadata to be applied to the observations.
#'
#'@return input.df
#'
#'@author Daniel Kick (\email{daniel.r.kick@@protonmail.com})
#'
#'@references
#'
#'@keywords metadata
#'
#'@export
#'
#'@examples
#' add_metadata(input.df = as.data.frame(read_xlsx("S:/Data_Daniel/180117_inverted_wave/data/tecc_gj.xlsx")), metadata.df = as.data.frame(read_xlsx("S:/Data_Daniel/180117_inverted_wave/data/metadata.xlsx")))
#'

add_metadata <- function(input.df, metadata.df) {
  # 1. look at first column. May have ".abf" if it does, remove it.
  input.df[, "File Name"] <- stringr::str_replace(input.df[, "File Name"],
                                                  pattern = ".abf",
                                                  replace = ""
  )
  # 2. split experiment columns by "_" for experiment and recording
  input.df <- tidyr::separate(
    input.df, `File Name`,
    c("Experiment", "Recording"),
    sep = "_"
  )
  input.df[, "Recording"] <- as.numeric(input.df[, "Recording"])
  # 3. for each row look in metafile to find the matching experiment/file pair. Add labels on cell id, timepoint, etc.
  input.df[, "Time Exposed"] <- 0
  input.df[, "Condition"] <- ""
  input.df[, "IN4"] <- ""
  input.df[, "IN9"] <- ""

  exps <- unique(input.df[, "Experiment"])
  for (exp in seq(from = 1, to = length(exps))) {
    recs <- unique(input.df[input.df$Experiment == exps[exp], "Recording"])
    for (rec in seq(from = 1, to = length(recs))) {

      #if file name is not in the metadata file, print warning to stdout
      if (nrow(metadata.df[metadata.df$Experiment == exps[exp] & metadata.df$Recording == recs[rec], ]) == 0){
        print(paste("Missing in metadata.df: ", as.character(paste (exps[exp], recs[rec]))))
      } else{
        #silently add metadata
        #print( paste (exps[exp], recs[rec]))
        #To extend this function, add to this section and by line 23
        input.df[input.df$Experiment == exps[exp] & input.df$Recording == recs[rec], "Time Exposed"] <-
          metadata.df[metadata.df$Experiment == exps[exp] & metadata.df$Recording == recs[rec], "Time Exposed"]

        input.df[input.df$Experiment == exps[exp] & input.df$Recording == recs[rec], "Condition"] <-
          metadata.df[metadata.df$Experiment == exps[exp] & metadata.df$Recording == recs[rec], "Condition"]

        input.df[input.df$Experiment == exps[exp] & input.df$Recording == recs[rec], "IN4"] <-
          metadata.df[metadata.df$Experiment == exps[exp] & metadata.df$Recording == recs[rec], "IN4"]

        input.df[input.df$Experiment == exps[exp] & input.df$Recording == recs[rec], "IN9"] <-
          metadata.df[metadata.df$Experiment == exps[exp] & metadata.df$Recording == recs[rec], "IN9"]
      }
    }
  }
  print("Existing metadata added")
  #this is just to make sure merging is okay.
  input.df$Recording <- as.character(input.df$Recording)
  return(input.df)
}

# now obsolete
#' #' Solve for Resistances
#' #'
#' #' This function uses a dataframe with the two electrode current clamp data and calculates resistances from it.
#' #'
#' #'@title Solve for Resistances
#'
#' solve_for_resistances <- function(input.df = tecc){
#'   input.df = df.tecc_gj
#'
#'   input.df <- na.omit(input.df)
#'   input.df[,"r1"] <- 0
#'   input.df[,"rc"] <- 0
#'   input.df[, "interact"] <- interaction(input.df[,"Experiment"], input.df[,"Recording"])
#'   #input.df[, "interact"] <- interaction(input.df[,"interact"], input.df[,"Inj_Cell"])
#'   interactions <- unique(input.df$interact)
#'
#'
#'   for (interact in seq(length(interactions))){
#'     #print(interactions[interact])
#'     #  interact = 189
#'
#'     #for interact = 6 (170623b.0020) there is only one row of r11 instead of two. This is because it is keyed in as lc.na. This causes premature termination. To get around this we only add to the df if there is more than one row for a given interaction
#'     if (length(input.df[input.df$interact == interactions[interact], "r11"]) == 2){
#'       #here there should be two rows, the medians for both cells at one time point.
#'       #We treat them separately so that this is easy to understand
#'
#'       #for injecting into the cell listed first
#'       temp.r11 <- input.df[input.df$interact == interactions[interact], "r11"][1]
#'       temp.r22 <- input.df[input.df$interact == interactions[interact], "r11"][2]
#'       temp.r12 <- input.df[input.df$interact == interactions[interact], "r12"][1]
#'
#'       input.df[input.df$interact == interactions[interact], "r1"][1] <-
#'         (((temp.r11*temp.r22)-(temp.r12^2)) / (temp.r22-temp.r12))
#'
#'       input.df[input.df$interact == interactions[interact], "rc"][1] <-
#'         (((temp.r11*temp.r22)-(temp.r12^2)) / (temp.r12))
#'
#'       #for injecting into the cell listed second
#'       temp.r11 <- input.df[input.df$interact == interactions[interact], "r11"][2]
#'       temp.r22 <- input.df[input.df$interact == interactions[interact], "r11"][1]
#'       temp.r12 <- input.df[input.df$interact == interactions[interact], "r12"][2]
#'
#'       input.df[input.df$interact == interactions[interact], "r1"][2] <-
#'         (((temp.r11*temp.r22)-(temp.r12^2)) / (temp.r22-temp.r12))
#'
#'       input.df[input.df$interact == interactions[interact], "rc"][2] <-
#'         (((temp.r11*temp.r22)-(temp.r12^2)) / (temp.r12))
#'
#'     } else {
#'       print(paste("Error: interaction #",
#'                   as.character(interact),
#'                   ",",
#'                   as.character(interactions[interact]),
#'                   "contains",
#'                   as.character(length(input.df[input.df$interact == interactions[interact], "r11"])),
#'                   "row(s)"))
#'     }
#'   }
#'   input.df <- input.df[,!(names(input.df) %in% c("interact"))]
#'
#'   return(input.df)
#' }

#' Solve for Resistances 2
#'
#' This function should act the same as the "solve_for_resistances" function, only much, much faster. Uses a dataframe with the two electrode current clamp data and calculates resistances from it.
#'
#'@title Solve for Resistances 2
#'
solve_for_resistances2 <- function(df = df.tecc_gj,
                                  v1 = "IN4_mean",
                                  i1 = "IN7_mean",
                                  v2 = "IN9_mean",
                                  i2 = "IN12_mean",
                                  exp = "Experiment",
                                  rec = "Recording"
){

  # Grouping prevents this from working.
  df <- ungroup(df)

  ## Set up selection vectors for when i1 (e.g. IN7) is being used to inject current
  inj_i1 <- abs(df[[i1]]) > abs(df[[i2]])
  inj_i2 <- abs(df[[i1]]) < abs(df[[i2]])

  ## figure out coupling coef
  df[inj_i1, "cc"] <- df[inj_i1, v2] / df[inj_i1, v1] #when inj IN7
  df[inj_i2, "cc"] <- df[inj_i2, v1] / df[inj_i2, v2] #when inj IN12

  ## figure out input resistance ====
  df[inj_i1, "r11"] <- df[inj_i1, v1] / df[inj_i1, i1] #when inj IN7
  df[inj_i2, "r11"] <- df[inj_i2, v2] / df[inj_i2, i2] #when inj IN12

  ## figure out transfer resistance ====
  df[inj_i1, "r12"] <- df[inj_i1, v2] / df[inj_i1, i1]
  df[inj_i2, "r12"] <- df[inj_i2, v1] / df[inj_i2, i2]

  ## figure out membrane resist and coupling resist for both cells ====
  temp <- df %>%
    dplyr::select(
      exp, rec, r11, r12
    ) %>%
    mutate(cell = ifelse(inj_i1, "cell1", "cell2")) %>%
    pivot_wider(names_from = cell,
                values_from = c("r11", "r12")
    ) %>%
    mutate(r1_cell1 = ((r11_cell1*r11_cell2) - (r12_cell1^2)) / (r11_cell2 - r12_cell1)) %>%
    mutate(r1_cell2 = ((r11_cell2*r11_cell1) - (r12_cell2^2)) / (r11_cell1 - r12_cell2)) %>%
    mutate(rc_cell1 = ((r11_cell1*r11_cell2) - (r12_cell1^2)) / (r12_cell1)) %>%
    mutate(rc_cell2 = ((r11_cell2*r11_cell1) - (r12_cell2^2)) / (r12_cell2)) %>%
    pivot_longer(
      -c(exp, rec),
      names_to = c(".value", "cell"),
      names_sep = "_",
      values_drop_na = TRUE
    )

  df <- full_join(df, temp) %>%
    dplyr::select(-cell)

  return(df)
}































#' Check Start End
#'
#' Used in quality control, this function checks if there are observations for two given timepoints (e.g. t=0 and t=60). It's used in processing two electrode current clamp files.
#'
#'@title Check Start End
#'@aliases
#'
#'@param input.df This data.frame holds the observations.
#'@param start.time The first time to check for.
#'@param end.time The second time to check for.
#'
#'@return input.df
#'
#'@author Daniel Kick (\email{daniel.r.kick@@protonmail.com})
#'
#'@references
#'
#'@keywords tecc
#'
#'@export
#'
#'@examples
#' check_start_end(input.df = tecc, start.time = 0, end.time = 60)
#'

check_start_end <- function(input.df = tecc,
                            start.time = 0,
                            end.time = 60){
  problem.observations.start <- c()
  problem.observations.end <- c()
  input.df$interact <- interaction(input.df$Experiment, input.df$Inj_Cell)
  observations <- unique(input.df$interact)
  for (observation in seq(length(observations))){
    if(  as.character(start.time) %in% unique(input.df[input.df$interact == observations[observation], "Time Exposed"]) == FALSE){
      warning(paste0("start.time not found for ",as.character(observations[observation])))
      problem.observations.start <- append(problem.observations.start,
                                           as.character(observations[observation]))
    } else if(  as.character(end.time) %in% unique(input.df[input.df$interact == observations[observation], "Time Exposed"]) == FALSE){
      warning(paste0("end.time not found for ",as.character(observations[observation])))
      problem.observations.end <- append(problem.observations.end,
                                         as.character(observations[observation]))
    }
  }
  print("Removing Observations:")
  print(paste0("Missing start: ", as.character(problem.observations.start)))
  print(paste0("Missing end: ", as.character(problem.observations.end)))
  drop.obs <- c(problem.observations.start, problem.observations.end)

  input.df <- input.df[!(input.df$interact %in% drop.obs),]
  input.df <- input.df[, !(names(input.df) %in% c("interact"))]
  return(input.df)
}

#' Find and Fix Switched Columns
#'
#'@title Find and Fix Switched Columns
#'@param input.df
#'
#'@return
#'
#'@author Daniel Kick (\email{daniel.r.kick@@protonmail.com})
#'
#'@export


find_and_fix_switched_cols <- function(input.df = tevc){
  #a way to fix the columns where baseline and mean got switched:
  #A setting in clampfit switched the baseline/mean

  #1. Find all experiments with a baseline at -80 (In 9 or In 4) and save as new df
  #input.df <- tevc

  input.df$interact <- interaction(input.df$Experiment, input.df$Recording)
  interactions <- unique(input.df[input.df$IN4_baseline <= -70 | input.df$IN9_baseline <= -70, "interact"])
  unflagged.interactions <- setdiff(unique(input.df$interact), interactions)

  correct.df <- input.df[input.df$interact %in% unflagged.interactions, ]
  input.df <- input.df[input.df$interact %in% interactions, ]

  #2. Sweeps go negative to positive, so the mean in the wrong direction in addition to being in the wrong place
  output.names <- names(input.df)
  first.cols <- input.df[, c("Experiment","Recording","Trace","Trace Start")]
  last.cols <- input.df[, c("path","Time Exposed","Condition","IN4","IN9")]

  #to get the baseline we'll add the mean and baseline.
  adjusted.baselines <-
    (input.df[, c("IN4_mean","IN7_mean","IN9_mean","IN12_mean")] + input.df[, c("IN4_baseline","IN7_baseline","IN9_baseline","IN12_baseline")])
  names(adjusted.baselines) <- c("IN4_baseline","IN7_baseline","IN9_baseline","IN12_baseline")

  #to get the mean we'll need to multiply by -1 since the Clampfit has the `mean` in relation to the actual mean so the relative position is actually opposite of what is shown.
  adjusted.means <-
    (input.df[, c("IN4_mean","IN7_mean","IN9_mean","IN12_mean")] * -1)
  names(adjusted.means) <- c("IN4_mean","IN7_mean","IN9_mean","IN12_mean")

  #3. now the cols are appropriately named so we can cbind them into the right format and rbind them to `correct.df`
  corrected.df <-
    cbind(first.cols, adjusted.means, adjusted.baselines, last.cols)

  #need to drop interact from correct.df for merger
  correct.df <- correct.df[, !(names(correct.df) %in% c("interact"))]
  input.df <- rbind(correct.df, corrected.df)
  return(input.df)
}



# for htk processing ----

#' Simplify Data.frame
#'
#' Needed to merge certain data.frames. This is simply a wrapper that drops columns `IN4_rin` and `IN9_rin`.
#'
#'@title Simplify Data.frame
#'@aliases
#'
#'@param input.df This data.frame contains columns `IN4_rin` and `IN9_rin`.
#'
#'@return This data.frame does not contain columns `IN4_rin` and `IN9_rin`.
#'
#'@author Daniel Kick (\email{daniel.r.kick@@protonmail.com})
#'
#'@references
#'
#'@keywords simplify
#'
#'@export
#'
#'@examples
#'
#'

#need to merge channel recordings
simplify_df <- function(input.df){
  return(input.df[, !(names(input.df) %in% c("IN4_rin", "IN9_rin"))])
}


#' Stack Columns by Channel
#'
#' Adds a column channel (`IN4` or `IN9`) and
#'
#'@title Stack Columns by Channel
#'@aliases
#'
#'@param input.df This data.frame contains columns `IN4_rin` and `IN9_rin`.
#'
#'@return This data.frame does not contain columns `IN4_rin` and `IN9_rin`.
#'
#'@author Daniel Kick (\email{daniel.r.kick@@protonmail.com})
#'
#'@references
#'
#'@keywords
#'
#'@export
#'
#'@examples
#'
#'

stack_channels <- function(input.df){
  #input.df <- a_peak
  IN4 <- input.df[, c(1:4,7:10)]
  IN9 <- input.df[, c(1:2,5:10)]
  names(IN4) <- c("Experiment","Recording","mV","nA","Time Exposed","Condition","IN4","IN9")
  names(IN9) <- c("Experiment","Recording","mV","nA","Time Exposed","Condition","IN4","IN9")
  IN4[,"Channel"] <- "IN4"
  IN9[,"Channel"] <- "IN9"
  IN4[IN4$Channel == "IN4","Cell"] <- IN4[IN4$Channel == "IN4", "IN4"]
  IN9[IN9$Channel == "IN9","Cell"] <- IN9[IN9$Channel == "IN9", "IN9"]

  IN4 <- IN4[!is.na(IN4[,"mV"]),]
  IN4 <- IN4[!is.na(IN4[,"nA"]),]
  IN9 <- IN9[!is.na(IN9[,"mV"]),]
  IN9 <- IN9[!is.na(IN9[,"nA"]),]

  return(bind_rows(mutate_all(IN4, as.character), mutate_all(IN9, as.character)))
}


#' Separate Peak and End Observations
#'
#' Used in processing ionic current amplitude measurements.
#'
#'@title Separate Peak and End Observations
#'@aliases
#'
#'@param input.df This data.frame holds ionic current amplitude observations.
#'@param current.type The type of current input.df contains. Current choices are "a", "htk" or "htk.ls".
#'
#'@return return.list This contains two data.frames, the first has transient amplitudes, the second sustained amplitudes.
#'
#'@author Daniel Kick (\email{daniel.r.kick@@protonmail.com})
#'
#'@references
#'
#'@keywords
#'
#'@export
#'
#'@examples
#' sep_peak_end(input.df = a, current.type = "a")
#'

sep_peak_end <- function(input.df = a, current.type = "a") {
  return.list <- list()
  # repurposed this code from phase_shift_report
  # drop non relevant columns
  if (current.type == "a") {
    a.peak <- input.df[, c(
      "Experiment", "Recording",
      "IN4_mV_peak", "IN7_nA_peak", "IN9_mV_peak", "IN12_nA_peak",

      "Time Exposed", "Condition", "IN4", "IN9"
    )]
    a.end <- input.df[, c(
      "Experiment", "Recording",

      "IN4_mV_end", "IN7_nA_end", "IN9_mV_end", "IN12_nA_end",
      "Time Exposed", "Condition", "IN4", "IN9"
    )]
    return.list[[1]] <- a.peak
    return.list[[2]] <- a.end
  } else if (current.type == "htk") {
    htk.peak <- input.df[, c(
      "Experiment", "Recording",
      "IN4_mV_peak", "IN7_nA_peak", "IN9_mV_peak", "IN12_nA_peak",

      "Time Exposed", "Condition", "IN4",
      "IN9", "IN4_rin", "IN9_rin"
    )]
    htk.end <- input.df[, c(
      "Experiment", "Recording",

      "IN4_mV_end", "IN7_nA_end", "IN9_mV_end", "IN12_nA_end",
      "Time Exposed", "Condition", "IN4",
      "IN9", "IN4_rin", "IN9_rin"
    )]
    return.list[[1]] <- htk.peak
    return.list[[2]] <- htk.end
  } else if (current.type == "htk.ls") {
    htk.ls.peak <- input.df[, c(
      "Experiment", "Recording",
      "IN4_mV_peak", "IN7_nA_peak", "IN9_mV_peak", "IN12_nA_peak",

      "Time Exposed", "Condition", "IN4",
      "IN9", "IN4_rin", "IN9_rin"
    )]
    htk.ls.end <- input.df[, c(
      "Experiment", "Recording",

      "IN4_mV_end", "IN7_nA_end", "IN9_mV_end", "IN12_nA_end",
      "Time Exposed", "Condition", "IN4",
      "IN9", "IN4_rin", "IN9_rin"
    )]
    return.list[[1]] <- htk.ls.peak
    return.list[[2]] <- htk.ls.end
  } else {
    print("Error unknown current. Input: a | htk | htk.ls")
  }
  return(return.list)
}





















