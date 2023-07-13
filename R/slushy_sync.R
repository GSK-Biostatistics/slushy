#' Sync local environment to match the project's lockfile.
#'
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#'
#' Install/update packages as needed to match the lockfile.
#'
#' @return This function is called for its side effects.
#' @export
#'
#' @importFrom renv restore
#' @importFrom purrr quietly
#' @importFrom cli cli_alert_success cli_h2
#'
#' @examples
#' \dontrun{
#'   slushy_sync()
#' }
slushy_sync <- function(project = NULL){

  # Confirm Clean Environment
  confirm_clean_env()

  res <- suppressMessages(slushy_status(project = project))

  if(res){
    out <- slushy_status(project = project)
    return(invisible(out))
  }


  # restore
  restore_results <- quietly(restore)(project = project, clean = TRUE, prompt = FALSE)$result

  for (i in seq_along(restore_results)) {
    cli_alert_success("Updated {names(restore_results)[i]} to {restore_results[[i]]$Version}")
  }

  # rerun status check
  slushy_status(project = project)

  return(invisible(TRUE))
}
