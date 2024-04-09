
#' Add packages to an existing slushy environment.
#'
#' Install and document additional packages in a slushy environment.
#'
#' @param pkgs Character vector of packages to install.
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param config List of configuration options. If not provided, defaults to reading from default settings in config file included in slushy package.
#' @param restart Restart session after package is added? Defaults to TRUE.
#'
#' @return This function is called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#'   slushy_add("newpkg")
#' }
slushy_add <- function(pkgs,
                       project = NULL,
                       config = get_config(),
                       restart = TRUE){


  if (is.null(project)){
    project <- proj_root()
  }

  # Confirm Clean Environment
  confirm_clean_env()

  # Install
  installed_pkgs <- c()
  for(pkg in pkgs){
    installed_pkgs <- c(
      installed_pkgs,
      try_install(pkg, check_agreed = TRUE, config = config)
    )
  }

  # update description file
  add_desc_deps(installed_pkgs, file = file.path(project, "DESCRIPTION"))

  # update snapshot
  update_snapshot(project = project)

  # restart R session
  if (restart){
    return(invisible(getOption("restart")()))
  } else {
    return(invisible(NULL))
  }
}



#' Drop packages from an existing slushy environment.
#'
#' @param pkgs Character vector of packages to remove
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param restart Restart session after package is added? Defaults to TRUE.
#'
#' @return This function is called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#'   slushy_drop("oldpkg")
#' }
#' @importFrom renv remove
#' @importFrom desc desc_get_deps
slushy_drop <- function(pkgs,
                        project = NULL,
                        restart = TRUE){


  if (is.null(project)){
    project <- proj_root()
  }

  # Confirm Clean Environment
  confirm_clean_env()

  # Uninstall
  removed_pkgs <- c()
  for(pkg in pkgs){
    removed_pkgs <- c(
      removed_pkgs,
      try_drop(pkg)
    )
  }

  # update description file
  existing_deps <- desc_get_deps(file = file.path(project, "DESCRIPTION"))
  add_desc_deps(setdiff(existing_deps$package, removed_pkgs), replace = TRUE)

  # update snapshot
  update_snapshot(project = project)

  # restart R session
  if (restart){
    return(invisible(getOption("restart")()))
  } else {
    return(invisible(NULL))
  }
}

#' Drop package from project
#' @param pkg Name of package as string
#' @param library The R library to be used. If NULL, the active project library will be used.
#'
#' @noRd
#'
#' @importFrom renv remove
#' @importFrom utils capture.output
#' @importFrom cli cli_progress_done
try_drop <- function(pkg,
                       library = NULL){

  res <- ""
  id <- cli_step_notime(paste0("Removing `", pkg, "`...{res}"))

  success <- tryCatch({
    p <- capture.output({remove(pkg, library = library)})
    if (p[2]==paste0("* Package '", pkg, "' is not installed -- nothing to do.")){
      res <- "package is not installed -- nothing to do."
    } else {
      res <- "done"
    }
    cli_progress_done(id = id, result = "done")
    pkg
  }, error = function(e){
    res <- "failed"
    cli_progress_done(id = id, result = "failed")
    return(NULL)
  }
  )

}

