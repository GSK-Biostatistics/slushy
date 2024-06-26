#' Sync local environment to match the project's lockfile.
#'
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param config List of configuration options. If not provided, defaults to reading from default settings in config file included in slushy package.
#' 
#' Install/update packages as needed to match the lockfile.
#'
#' @return This function is called for its side effects.
#' @export
#'
#' @importFrom renv restore
#' @importFrom cli cli_alert_success cli_h2
#' @importFrom rlang `%||%`
#'
#' @examples
#' \dontrun{
#'   slushy_sync()
#' }
slushy_sync <- function(project = NULL, config = get_config()){
  
  # Confirm Clean Environment
  confirm_clean_env()
  
  res <- suppressMessages(slushy_status(project = project))
  
  pkg_deps_ok <- config$pkg_deps_ok %||% TRUE
  
  if(res){
    out <- slushy_status(project = project, pkg_deps_ok)
    return(invisible(out))
  }
  
  
  # restore
  restore(project = project, 
          repos = getOption("repos"), # loaded from renv.lock by renv::load() in renv/activate.R 
          clean = TRUE, 
          prompt = FALSE) 

  
  # rerun status check
  slushy_status(project = project, pkg_deps_ok)
  
  return(invisible(TRUE))
}
