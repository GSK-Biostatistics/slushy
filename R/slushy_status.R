#' Check for differences between local environment and the project lock file.
#'
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param pkg_deps_ok Include package dependencies as part of the agreed upon set? If FALSE, any use of these dependencies found in the project will be flagged in a message.
#'
#' @return Sync status (TRUE = synced, FALSE = not synced), invisibly. This function is called for its side effects.
#' @export
#'
#' @details
#' The following will be checked for:
#'
#' - Packages used in files (via \code{library("package")} or \code{package::}) that are not documented in the lockfile.
#'
#' - Installed package versions differ from lockfile.
#'
#' - Packages in lockfile are not installed in local environment.
#'
#' @importFrom renv dependencies status
#' @importFrom jsonlite fromJSON
#' @importFrom purrr quietly
#' @importFrom utils installed.packages
#' @importFrom dplyr summarise group_by filter pull select
#' @importFrom cli cli_alert_success cli_alert_warning cli_h2
#' @importFrom rlang .data
#' @importFrom desc desc_get_deps
#'
#' @examples
#' \dontrun{
#'   slushy_status()
#' }
slushy_status <- function(project = NULL, pkg_deps_ok = get_config()$pkg_deps_ok){

  if (is.null(project)){
    project <- proj_root()
  }

  pkgs_base <- installed.packages(lib.loc = file.path(R.home(), "library"),
                                  priority = c("base", "recommended")) %>% row.names()
  pkgs_deps <- dependencies(root = project, progress = FALSE)
  pkgs_used <- unique(pkgs_deps)$Package
  
  if (pkg_deps_ok){
    pkgs_ok <- names(fromJSON(file.path(project, "renv.lock"))$Packages)
  } else {
    pkgs_ok <- unique(c(desc_get_deps()$package, "renv"))
  }
   
  pkgs_ok_plus_base <- unique(c(pkgs_ok, pkgs_base))
  pkgs_extra <- setdiff(pkgs_used, pkgs_ok_plus_base)


  cli_h2("Checking slushy sync status")

  if (length(pkgs_extra)>0){
    pkgs_print_txt <- pkgs_deps %>% filter(.data$Package %in% pkgs_extra) %>% select("Package", "Source") %>%
      group_by(.data$Package) %>%
      summarise(print_txt = paste0(.data$Package[1], ":\n", paste0(paste0(" ", .data$Source), collapse = "\n")) ) %>%
      pull(.data$print_txt)

    cli_alert_warning(
      paste0("The following packages are being used in the project but are not part of the agreed-upon set:\n",
             paste0(pkgs_print_txt, collapse = "\n\n"))
    )
  } else {
    cli_alert_success("All packages used in the project are part of the agreed-upon set.")
  }

  renv_status <- quietly(status)(project = project)$result

  if (renv_status$synchronized==FALSE){
    cli_alert_warning(
      "Local environment is out of sync with lockfile. Use slushy::slushy_sync() to sync."
    )
  } else {
    cli_alert_success("Local environment is in sync with lockfile.")
  }


  is_synced <- !( length(pkgs_extra)>0 | renv_status$synchronized==FALSE )

  invisible(is_synced)
}
