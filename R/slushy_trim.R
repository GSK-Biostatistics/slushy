#' Trim installed packages
#'
#' This function will trim down the installed packages to only those that are 
#' used in the code files of the project. Unused packages will be removed from 
#' the DESCRIPTION file, uninstalled from the project library, and removed from 
#' the `renv.lock` file.
#'
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param config List of configuration options. If not provided, defaults to reading from 
#'   default settings in config file included in slushy package.
#'
#' @export
#'
#' @importFrom renv dependencies
#' @importFrom desc desc_get_deps
#' @importFrom dplyr as_tibble filter pull setdiff intersect
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success
#' @importFrom tools package_dependencies
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_detect regex
#' @importFrom utils menu
#' 
#' @examples
#' \dontrun{
#'
#'  # default
#'  slushy_trim()
#'
#'  # pass options
#'  slushy_trim(config = get_config("slushy_config.yml"))
#' }
#' 
slushy_trim <- function(project = NULL, config = get_config(config_file = "slushy_config.yml")) {
  
  if (is.null(project)) {
    project <- proj_root()
  }
  
  # Confirm clean environment
  confirm_clean_env()
  
  # List of packages currently installed in project library
  proj_library_pkgs <- rownames(installed.packages(paths$library())) %>% unique()
  
  # List of packages used in code files (excluding ones in Rprofile/DESCRIPTION)
  used_pkgs <- dependencies(root = project, progress = FALSE) %>%
    as_tibble() %>% 
    filter(!str_detect(Source, regex("\\.Rprofile$", ignore_case = TRUE)) &
             !str_detect(Source, regex("DESCRIPTION$", ignore_case = TRUE)) &
             !str_detect(Source, regex("renv.lock", ignore_case = TRUE)) ) %>%
    pull(Package) %>%
    unique()
  
  # List of dependencies of packages used
  used_pkgs_deps <- package_dependencies(packages = used_pkgs,
                                         db = installed.packages(),
                                         which = c("Depends", "Imports", "LinkingTo"),
                                         recursive = FALSE) %>%
    unlist() %>%
    unique()
  
  # List of packages in the DESCRIPTION file
  desc_file <- file.path(project, "DESCRIPTION")
  desc_pkgs <- desc_get_deps(file = desc_file)$package
  
  # Keep packages that are used and their dependencies 
  pkgs_to_keep <- append(used_pkgs, used_pkgs_deps) %>% unique()
  
  # Drop packages not in the keep list
  pkgs_to_drop <- setdiff(proj_library_pkgs, pkgs_to_keep)
  
  # List of drop packages that are top-level only (in DESCRIPTION file)
  pkgs_to_drop_top_level <- intersect(desc_pkgs, pkgs_to_drop)
  
  # Notify user about packages to be trimmed
  if (length(pkgs_to_drop) > 0) {
    cli_alert_info(paste("The following packages are not used and will be removed with their dependencies:\n\n",
                         paste(pkgs_to_drop_top_level, collapse = ", "), "\n\n"))
    
    # Ask user if they want to proceed
    choice <- menu(c("Proceed", "Cancel"), title = "Would you like to proceed?")
    
    ## Choice 1
    if (choice == 1) {
      # Drop unused packages
      slushy_drop(pkgs_to_drop, restart = FALSE)
      
      # Notify user of successful trimming
      cli_alert_success("Trimming complete!")
      
      ## Choice 2
    } else if (choice == 2) {
      # Notify user of operation cancel
      stop("Trimming operation cancelled.")
    }
    
  } else {
    cli_alert_info("No packages need to be trimmed.")
  }
  
  invisible(NULL)
}
