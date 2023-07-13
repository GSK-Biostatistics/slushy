#' Update a slushy environment
#'
#' Update all packages in an existing slushy environment.
#'
#' @param date CRAN snapshot date to use for packages. If NULL, today's date will be used.
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param config List of configuration options. If not provided, defaults to reading from default settings in config file included in slushy package.
#' @param restart Restart session after environment is updated? Defaults to TRUE.
#'
#' @return This function is called for its side effects.
#' @export
#'
#' @details
#' This function will update all packages according to the closest CRAN snapshot date to the one supplied. Updates can occur forward or backward, depending on whether the project is transitioning to a newer or older snapshot. The lockfile will be updated following any updates.
#'
#' @importFrom renv dependencies
#' @importFrom cli cli_alert_info
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#'
#'  # default
#'  slushy_update()
#'
#'  # pass options
#'  slushy_update(date = "2022-09-01")
#' }
slushy_update <- function(date = NULL,
                          project = NULL,
                          config = get_config(),
                          restart = TRUE){

  if (is.null(project)){
    project <- proj_root()
  }

  # Confirm Clean Environment
  confirm_clean_env()

  # define pkgs to exclude
  exclude <- c("slushy","renv")

  # Update CRAN repo snapshot date ------------------------------------------
  repos <- get_repos(date,
                     repo_url = config$rspm_url)

  options("repos" = repos)

  # updates the lock file w/ new CRAN repo url
  update_snapshot(quiet = TRUE,
                  project = project)

  # get original set of pkgs from DESCRIPTION
  pkgs <- dependencies("DESCRIPTION", root = project, progress = FALSE)$Package

  # install/remove pkgs using updated snapshot
  all_pkgs <- check_for_updates(pkgs, project, repos, diffs_only = FALSE)

  pkgs_not_found <- setdiff(
    filter(all_pkgs, is.na(.data$new_version))$Package,
    exclude)

  if (length(pkgs_not_found)>0){
    cli_alert_info(paste0("The following packages are not available in the new snapshot and will be dropped from the lock file:\n",
                          paste0(pkgs_not_found, collapse = ", ")))

    lapply(pkgs_not_found, quiet_remove)
  }

  all_pkgs_diffs <- all_pkgs %>%
    filter(!is.na(.data$new_version)) %>%
    filter(is.na(.data$lock_version) |
             (!is.na(.data$lock_version) & !.data$lock_version == .data$new_version)
    )

  if (length(exclude)>0){
    exclude_desc <- exclude[which(exclude %in% pkgs)] # any in the DESCRIPTION file

    if (length(exclude_desc)>0){
      exclude_notify <- exclude_desc[which(exclude_desc %in% all_pkgs_diffs$Package)] # any in the list to be updated
      if (length(exclude_notify)>0){
        cli_alert_info(paste0("Newer versions have been found for the following packages, but they will be excluded from updates:\n",
                              paste0(exclude_notify, collapse = ", ")))
      }
    }
  }

  pkgs_to_install <- setdiff(all_pkgs_diffs$Package, exclude)
  lapply(pkgs_to_install, try_install)


  # updates the lock file w/ new pkg versions
  update_snapshot(project = project)

  # restart R session
  if (restart){
    return(invisible(getOption("restart")()))
  } else {
    return(invisible(NULL))
  }
}


#' Preview the update to a slushy environment
#'
#'
#' @param date CRAN snapshot date to use for packages. If NULL, today's date will be used.
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param config List of configuration options. If not provided, defaults to reading from default settings in config file included in slushy package.
#'
#' @return Console message describing updates to be made.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  slushy_update_preview()
#' }
#'
#' @importFrom rlang `!!` sym `:=` .data
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter arrange rename pull select
slushy_update_preview <- function(date = NULL,
                                  project = NULL,
                                  config = get_config()){

  if (is.null(project)){
    project <- proj_root()
  }

  # Confirm Clean Environment
  confirm_clean_env()

  # define pkgs to exclude
  exclude <- c("slushy","renv")

  # Update CRAN repo snapshot date ------------------------------------------
  repos <- get_repos(date, repo_url = config$rspm_url)

  # get original set of pkgs from DESCRIPTION
  pkgs <- dependencies("DESCRIPTION", root = project, progress = FALSE)$Package

  # get dates for col names
  date_new <- basename(repos[["RSPM"]])
  date_old <- fromJSON(file.path(project, "renv.lock"))$R$Repositories %>%
    filter(.data$Name=="RSPM") %>%
    pull(.data$URL) %>%
    basename()

  # get comparison of old vs new
  pkgs_compare <- check_for_updates(pkgs, project, repos, diffs_only = TRUE) %>%
    arrange(.data$dependency, .data$Package) %>%
    rename(!!sym(date_old) := "lock_version",
           !!sym(date_new) := "new_version")

  # print out

  cat("* Installed packages to be updated:\n\n",
      paste(capture.output(
        print(filter(pkgs_compare, !.data$dependency)%>%
                filter(!.data$Package %in% exclude) %>%
                select(-"dependency") %>%
                filter(!is.na(.data[[date_new]])) %>%
                as.data.frame(), row.names = FALSE)), collapse = "\n"),
      "\n\n",
      "* Dependencies to be updated:\n\n",
      paste(capture.output(
        print(filter(pkgs_compare, .data$dependency)%>%
                filter(!.data$Package %in% exclude) %>%
                select(-"dependency") %>%
                filter(!is.na(.data[[date_new]])) %>%
                as.data.frame(), row.names = FALSE)), collapse = "\n"),
      "\n\n",
      "* Packages unavailable in new snapshot:\n\n",
      paste(capture.output(
        print(filter(pkgs_compare, is.na(.data[[date_new]])) %>%
                filter(!.data$Package %in% exclude) %>%
                select(-"dependency") %>%
                as.data.frame(), row.names = FALSE)), collapse = "\n"),
      "\n\n",
      "* Excluded packages that will not be updated:\n\n",
      paste(capture.output(
        print(filter(pkgs_compare, .data$Package %in% exclude) %>%
                filter(.data$Package %in% pkgs) %>%
                select(-"dependency") %>%
                filter(!is.na(.data[[date_new]])) %>%
                as.data.frame(), row.names = FALSE)), collapse = "\n"))

}



# check for packages that have updated, including deps, before installing
#' @noRd
#'
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom dplyr arrange bind_rows rename filter left_join mutate tibble
#' @importFrom stats na.omit
#' @importFrom utils available.packages
#' @importFrom cli cli_alert_info
#' @importFrom rlang .data
check_for_updates <- function(pkgs, project, repos, diffs_only = TRUE){

  # get pkgs and versions in lock file (current)
  lock_file <- file.path(project, "renv.lock")

  lock_file_pkgs <- fromJSON(lock_file)$Packages %>%
    map(~.x[c("Package", "Version")]) %>%
    bind_rows() %>%
    arrange()  %>%
    rename("lock_version" = "Version")

  # try to add any pkgs that are in DESCRIPTION but not in lock file (failed previously)
  pkgs_to_add <- setdiff(pkgs, lock_file_pkgs$Package)

  if (length(pkgs_to_add)>0){
    lock_file_pkgs <- bind_rows(
      lock_file_pkgs,
      tibble(Package = pkgs_to_add)
    )
  }

  # get pkgs and versions for snapshot (new)
  rspm_pkgs <- available.packages(repos = repos)[,c("Package","Version")] %>%
    as.data.frame(row.names = FALSE)

  # subset & see what needs updating
  all_pkgs <- left_join(lock_file_pkgs, rspm_pkgs, by = "Package") %>%
    rename("new_version" = "Version") %>%
    mutate(dependency = ! .data$Package %in% pkgs)

  if (diffs_only){
    pkgs_to_update <- all_pkgs %>%
      filter(is.na(.data$lock_version) |
               is.na(.data$new_version) |
               !.data$lock_version==.data$new_version)

    return(pkgs_to_update)

  } else {
    return(all_pkgs)
  }

}



