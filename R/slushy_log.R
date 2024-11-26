#' @title Summarized Package Environment Diagnostic
#'
#' @description
#' The `slushy_log` function provides an overview of the package environment, 
#' including package usage, dependencies, and mismatches. It runs 
#' `renv::diagnostics` by default and can generate a custom report comparing 
#' library, lockfile, and package usage details.
#'
#' @param project Character. Path to the project directory. Defaults to the root of the project.
#' @param config List. The configuration object retrieved from `get_config()` that includes the `pkgs` list.
#' @param custom Logical. If `TRUE`, runs additional diagnostics alongside the summarized report.
#'
#' @details
#' This function inspects the following aspects of the package environment:
#' \itemize{
#'   \item Synchronization between the local environment and `renv` lockfile.
#'   \item Installed packages versus available versions in the CRAN snapshot.
#'   \item Top-level package usage, including those in the config file, `DESCRIPTION`, and active code.
#'   \item Miscellaneous packages not documented in the config or `DESCRIPTION`.
#'   \item Packages added or removed compared to the config file.
#' }
#' Additionally, it identifies outliers that are in use but not documented in the 
#' config or their dependencies.
#'
#' @return Invisible `NULL`. Outputs details in the console.
#' 
#' @examples
#' \dontrun{
#' # Generate a summarized package diagnostic report
#' slushy_log(project = "/path/to/project", custom = TRUE)
#' }
#'
#' @export
#'
#' @importFrom renv status diagnostics paths dependencies
#' @importFrom dplyr pull left_join filter mutate rename select setdiff
#' @importFrom tibble as_tibble
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_h3 cli_text cli_ul
#' @importFrom stringr str_detect
#' @importFrom utils installed.packages available.packages
#' 
slushy_log <- function(project = NULL, config = get_config(), custom = FALSE) { 
  if (is.null(project)) {
    project <- proj_root()
  }
  
  #----------------------------- LIBRARY VS LOCK -------------------------------
  
  # Extract list of packages in library and lock file
  renv_status <- quietly(status)(project = project)$result
  
  library_df <- lst_to_df(renv_status$library$Packages)
  lockfile_df <- lst_to_df(renv_status$lockfile$Packages)
  
  library_df_pkgs <- library_df %>% pull(Package)
  lockfile_df_pkgs <- lockfile_df %>% pull(Package)
  
  #------------------------ INSTALLED VS CRAN SNAPSHOT -------------------------
  
  installed_pkgs_df <- installed.packages(paths$library())[, c("Package", "Version", "Priority")] %>% as_tibble()
  available_pkgs_df <- available.packages(repos = getOption("repos"))[, c("Package", "Version", "Priority")] %>% as_tibble()
  
  # List of recommended and optional packages
  rec_op_pkgs <- available_pkgs_df %>% filter(is.na(Priority) != TRUE) %>% pull(Package)
  
  # List of pacakges in library vs lock file (excluding recommended and option packages)
  lib_not_lock <- setdiff(library_df_pkgs, lockfile_df_pkgs) %>% setdiff(rec_op_pkgs)
  lock_not_lib <- setdiff(lockfile_df_pkgs, library_df_pkgs) %>% setdiff(rec_op_pkgs)
  
  # Installed pacakges vs available packages (CRAN snapshot)
  inst_vs_avail_df <- left_join(rename(installed_pkgs_df,"Version_Inst" = "Version") %>% select(-Priority),
                                rename(available_pkgs_df,"Version_Avail" = "Version"),
                                by = "Package") %>%
    mutate(Version_Match = Version_Inst == Version_Avail)
  
  # Subset for mismatches
  inst_vs_avail_subset <- inst_vs_avail_df %>%
    filter(Version_Match == FALSE | is.na(Version_Match) == TRUE)
  
  #------------------------------- CREATE LISTS --------------------------------
  
  # List of packages in the config file
  config_pkgs <- config$pkgs
  
  # List of packages in the DESCRIPTION file
  desc_file <- file.path(project, "DESCRIPTION")
  desc_pkgs <- desc_get_deps(file = desc_file)$package %>% sort()
  
  # List of packages used in code files (excluding ones in Rprofile/DESCRIPTION)
  used_pkgs <- dependencies(root = project, progress = FALSE) %>%
    as_tibble() %>% 
    filter(!str_detect(Source, regex("\\.Rprofile$", ignore_case = TRUE)) &
             !str_detect(Source, regex("DESCRIPTION$", ignore_case = TRUE)) &
             !str_detect(Source, regex("renv.lock", ignore_case = TRUE)) ) %>%
    pull(Package) %>% unique() %>% sort()
  
  # List of packages added and removed
  pkgs_added <- setdiff(desc_pkgs, config_pkgs)
  pkgs_removed <- setdiff(config_pkgs, desc_pkgs)
  
  # List of top level packages  
  top_level_pkgs <- c(used_pkgs, desc_pkgs, config_pkgs, pkgs_added, pkgs_removed) %>% unique()
  
  # List of packages in use but not in config or DESCRIPTION
  misc_pkgs <- setdiff(used_pkgs, unique(c(config_pkgs, desc_pkgs)))
  
  # List of config package dependencies
  config_pkg_deps <- package_dependencies(packages = config_pkgs,
                                          db = installed.packages(),
                                          which = c("Depends", "Imports", "LinkingTo"),
                                          recursive = FALSE) %>% unlist() %>% unique() %>% sort()
  
  # List of DESCRIPTION package dependencies
  desc_pkg_deps <- package_dependencies(packages = desc_pkgs,
                                        db = installed.packages(),
                                        which = c("Depends", "Imports", "LinkingTo"),
                                        recursive = FALSE) %>% unlist() %>% unique() %>% sort()
  
  #------------------------------- CREATE TABLES -------------------------------
  
  # Table display on whether package is config, in DESCRIPTION, or in use
  pkg_usage_df <- data.frame(pkg = top_level_pkgs) %>%
    mutate(
      in_used_pkgs = pkg %in% used_pkgs, # meaning used in code
      in_desc_pkgs = pkg %in% desc_pkgs,
      in_config_pkgs = pkg %in% config_pkgs,
    ) %>%
    mutate(across(starts_with("in_"), ~ ifelse(.x, "y", "n")))
  
  # Table display on whether misc package is a dependency of a config or DESCRIPTION package
  is_misc_pkg_dep <- data.frame(pkg = misc_pkgs) %>%
    mutate(dep_of_desc_pkgs = pkg %in% desc_pkg_deps,
           dep_of_config_pkgs = pkg %in% config_pkg_deps,
           neither = !dep_of_desc_pkgs & !dep_of_config_pkgs) %>%
    mutate(across(-pkg, ~ ifelse(.x, "y", "n")))
  
  # List of outlier packages
  outlier_pkgs <- is_misc_pkg_dep %>% filter(neither == TRUE) %>% pull(pkg)
  
  #------------------------------ CONSOLE OUTPUT -------------------------------
  
  if (custom) {
    # Run diagnostics in addition to custom report
    cli_alert_info("Running `r1env::diagnostics`...")
    renv::diagnostics()
    cli_alert_success("Diagnostics completed.")
  }
  
  # Custom summarized report
  cli_alert_info("Generating summarized report...")
  
  # Check locally installed environment matches lock file
  if (renv_status$synchronized==FALSE){
    cli_alert_warning("Local environment is out of sync with lockfile.")
  } else {
    cli_alert_success("Local environment is in sync with lockfile.")
  }
  
  # Output table of package version mismatches (installed vs CRAN)
  cli_h3("Package Version Mismatches (Installed vs CRAN Snapshot):")
  print(as.data.frame(inst_vs_avail_subset))
  cli_text("")
  cli_text("Note: `slushy` and `renv` are expected")
  
  ###
  
  # Table display of package usage
  cli_h3("Top-Level Package Usage Overview:")
  cli_alert_info("Summary of package usage across different contexts:")
  print(pkg_usage_df)
  
  # Packages in config file
  cli_h3("Original Approved Package Set:")
  cli_alert_info("{length(config_pkgs)} packages specified in the config file.")
  print(config_pkgs)
  
  # Packages in DESCRIPTION file
  cli_h3("Updated Package Set:")
  cli_alert_info("{length(desc_pkgs)} packages listed in DESCRIPTION.")
  print(desc_pkgs)
  
  # Packages used in the code files
  cli_h3("Packages Used in Code Files:")
  cli_alert_info("{length(used_pkgs)} packages are actively used in code.")
  print(used_pkgs)
  
  ###
  
  # Packages added and removed in comparison to config
  cli_h3("Packages Added or Removed:")
  cli_alert_info("Added packages not in config file: {length(pkgs_added)}")
  cli_ul(pkgs_added)
  cli_alert_info("Removed packages not in DESCRIPTION file: {length(pkgs_removed)}")
  cli_ul(pkgs_removed)
  
  # Table display of miscellaneous package dependencies
  cli_h3("Miscellaneous Package Dependency Check:")
  cli_alert_info("Check if non top-level (misc) packages are dependencies.")
  print(is_misc_pkg_dep)
  
  cli_text("")
  cli_alert_info("The following packages are not documented in the config, DESCRIPTION, nor their dependencies:")
  print(outlier_pkgs)
  
  cli_text("")
  cli_alert_success("Summarized report generated successfully.")
  
  invisible(NULL)
}

#' @title Convert a List to a Data Frame
#'
#' @description
#' Converts a structured list of package details into a tidy data frame for easier analysis and processing.
#'
#' @param lst List. A structured list where each element contains package details such as `Package`, `Version`, `Source`, `Repository`, `Requirements`, and `Hash`.
#'
#' @return A `data.frame` with columns:
#' \itemize{
#'   \item `Package`: Package name (character).
#'   \item `Version`: Package version (character).
#'   \item `Source`: Package source (character).
#'   \item `Repository`: Repository information (character).
#'   \item `Requirements`: Collapsed requirements as a single string (character).
#'   \item `Hash`: Package hash (character).
#' }
#'
#' @details
#' The function ensures missing fields in the input list are replaced with `NA_character_`. 
#' If `Requirements` is present, it collapses multiple entries into a single comma-separated string.
#'
#' @examples
#' \dontrun{
#' example_list <- list(
#'   list(Package = "dplyr", Version = "1.0.10", Source = "CRAN", Repository = "https://cran.r-project.org"),
#'   list(Package = "ggplot2", Version = "3.4.0", Source = "CRAN", Repository = "https://cran.r-project.org", Requirements = c("grid", "scales"))
#' )
#' 
#' lst_to_df(example_list)
#' }
#'
#' @importFrom rlang %||%
#' @keywords internal
#' 
lst_to_df <- function(lst) {
  # Check if the input is a list
  if (!is.list(lst)) stop("Input must be a list")
  
  # Extract the necessary data into a dataframe
  df <- do.call(rbind, lapply(lst, function(package) {
    data.frame(
      Package = package$Package %||% NA_character_,
      Version = package$Version %||% NA_character_,
      Source = package$Source %||% NA_character_,
      Repository = package$Repository %||% NA_character_,
      Requirements = if (!is.null(package$Requirements)) {
        paste(package$Requirements, collapse = ", ")
      } else {
        NA_character_
      },
      Hash = package$Hash %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  
  return(df)
}

