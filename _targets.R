# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(quarto)

# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "janitor", "here", "lubridate", "sf") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Target list:
list(
  tar_target(fao_files,
             list.files("/capstone/seamissions/data/fao_seafood_production/", pattern="*.csv", full.names = TRUE),
             format = "file"),
  tar_target(fao_catch, prep_fao(fao_files)),
  tar_target(emissions_files, 
             list.files("/capstone/seamissions/data/meds_capstone_project", pattern="*.csv", full.names=TRUE),
             format = "file"),
  tar_target(emissions, merge_emissions(emissions_files)),
  tar_target(emissions_zones, intersection(emissions)),
  tar_target(emissions_partitioned_grouped, partition_emissions(emissions_zones)),
  tar_target(full_emissions_fao_species, merge_catch_fao(emissions_partitioned_grouped, fao_catch)),
  tar_target(sau_files,
             list.files("/capstone/seamissions/data/sau/", pattern="*.csv", full.names = TRUE),
             format = "file"),
  tar_target(sau_catch, prep_sau(sau_files)),
  tar_target(full_emissions_sau_species, merge_catch_sau(emissions_partitioned_grouped, sau_catch)),
  # Make quarto notebook -----
  tar_quarto(
    name = quarto_book,
    path = "qmd",
    quiet = FALSE)
)
