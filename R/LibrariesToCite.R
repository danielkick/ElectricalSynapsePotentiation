libs <- c(
  ## General ====
  "readABF",
  "readxl",
  "tidyverse",
  "zoo",
  "furrr",
  "here",
  "janitor",
  ## Stats ====
  # "agricolae",
  "AICcmodavg",
  # "bayestestR",
  "broom",
  "car",
  "equivalence",
  # "estimate",
  # "multcomp",
  "nlme",
  # "rstanarm",
  # "Hmisc",
  ## Plots ====
  "cowplot",
  "ggrepel",
  # "ggsci", "ggthemes",
  "lemon",
  "patchwork",
  "plotly"
  # "see",
  )

for (lib in libs){
  print(citation(lib))
}





