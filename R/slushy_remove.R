
#' Remove slushy infrastructure from a project.
#'
#' Deletes the files and folders needed for the slushy environment before
#' restarting.
#'
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param restart Restart session after slushy is removed? Defaults to TRUE.
#'
#' @return This function is called for its side effects.
#' @export
#'
#' @importFrom rstudioapi executeCommand
#' @importFrom cli cli_alert_success
#'
#' @examples
#' \dontrun{
#'  slushy_remove()
#' }
slushy_remove <- function(project = NULL, restart = TRUE, keep = "slushy_config.yml"){
  
  if (is.null(project)){
    project <- proj_root()
  }
  
  # remove standalone files
  files <- c(".renvignore", "renv.lock", "DESCRIPTION", "slushy_config.yml") |>
    setdiff(keep)

  for (f in files){
    f <- file.path(project, f)
    if (file.exists(f)) file.remove(f)
  }

  # clean .Rprofile  and remove if empty
  clean_rprofile(project = project,
                 remove_empty = TRUE)

  # remove renv directory
  dirs <- file.path(project, "renv")
  if (dir.exists(dirs)){
    unlink(dirs, recursive=TRUE, force = TRUE)
  }
  cli_alert_success("Removed slushy infrastructure.")

  # restart R session
  if (restart){
    return(invisible(executeCommand("restartR")))
  } else {
    return(invisible(NULL))
  }

}
