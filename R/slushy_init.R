
#' Initialize a slushy environment
#'
#' Set up a slushy environment in an existing project.
#'
#' @param date CRAN snapshot date (as string with format yyyy-mm-dd) to use for
#'   packages. If NULL, today's date will be used.
#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param config List of configuration options. If not provided, defaults to reading from default settings in config file included in slushy package.
#' @param restart Restart session after slushy is initialized? Defaults to TRUE.
#'
#' @return This function is called for its side effects.
#' @export
#'
#' @details The following steps will be performed:
#'
#' 1. {renv} is initialized using the closest CRAN snapshot date to the one
#' supplied. 
#' 2. Agreed upon packages (supplied in config) are installed and
#' documented in a DESCRIPTION file. 
#' 3. Explicit snapshotting via {renv} is
#' performed to capture the state of the project's library. 
#' 4. If restart=TRUE,
#' session restarts and {renv} is activated for the project.
#'
#' @importFrom renv init paths restore
#' @importFrom cli cli_alert_info cli_h3
#' @importFrom tools package_dependencies
#' @importFrom utils installed.packages
#' @importFrom rlang `%||%`
#'
#' @examples
#' \dontrun{
#'
#'  # default
#'  slushy_init()
#'
#'  # pass options
#'  slushy_init(date = "2022-08-01", config = get_config("slushy_config.yml"))
#' }
slushy_init <- function(date = NULL,
                        project = NULL,
                        config = get_config(),
                        restart = TRUE){


  if (is.null(project)){
    project <- proj_root()
  }
  
  # Check config required params are present -----------------------------------
  check_config_params(config)

  # Force slushy ---------------------------------------------------------------
  pkgs <- c(config$pkgs, "slushy") %>% unique()

  # Define slushy lib path -----------------------------------------------------
  slushy_loc <- .getNamespaceInfo(asNamespace("slushy"), "path")

  # Confirm env is clean -------------------------------------------------------
  confirm_clean_env()

  # Get the env settings from config -------------------------------------------
  env_settings <- config$environment

  # Evaluate env settings ------------------------------------------------------
  if (!is.null(env_settings)){
    env_settings <- lapply(env_settings, function(x) eval(parse(text = x)))
    do.call("Sys.setenv", env_settings)
  }

  # Clean out slushy things from the Rprofile, if applicable -------------------
  clean_rprofile(project = project,
                 remove_empty = TRUE)
  
  # Update ignores -------------------------------------------------------------
  update_ignores(project = project)

  # Initialize new renv project w/ snapshot date -------------------------------
  config_date <- config$date
  date <- date %||% config_date 
  repos <- get_repos(date,
                     repo_url = config$rspm_url)

  options("repos" = repos)
  
  # Create empty DESCRIPTION just listing {slushy} as an Import ----------------
  document_pkgs(pkgs = "slushy",
                project = project)
  
  # Initialize 
  settings <- c(list(snapshot.type = "explicit"), config$renv_settings)
  settings <- settings[!duplicated(names(settings))] 
  init(
    project = project,
    repos = repos,
    bare = TRUE,
    settings = settings,
    restart = FALSE
  )

  ## add necessary configurations to the .Rprofile to be run on startup
  add_slushy_rprofile_code(project = project,
                           config)


  # Copy slushy pkg to project directory if not there --------------------------
  try_install_slushy(slushy_loc)

  # Create lock file from DESCRIPTION ------------------------------------------
  update_snapshot(repos = repos,
                  project = project,
                  force = TRUE)


  # Clean the project library to ensure we are starting fresh ------------------
  quiet_restore <- quietly(restore)(project = project,
                                    clean = TRUE,
                                    prompt = FALSE)

  # Document needed pkgs in DESCRIPTION ----------------------------------------
  document_pkgs(pkgs,
                project = project)

  # Install necessary packages -------------------------------------------------
  for(pkg in setdiff(pkgs, "slushy")){
    try_install(pkg, repos = repos, check_agreed = TRUE, config = config)
  }

  # Look for dependencies of slushy that may be missing ------------------------
  slushy_deps_missing <- setdiff(
    package_dependencies("slushy",
                         db = installed.packages(),
                         which = c("Depends","Imports","LinkingTo"))$slushy,
    rownames(installed.packages()))

  if (length(slushy_deps_missing)>0){
    cli_h3("Installing missing slushy dependencies:")
    for(pkg in slushy_deps_missing){
      try_install(pkg, repos = repos, check_agreed = FALSE, config = config)
    }
  }

  # Create updated lock file ---------------------------------------------------
  update_snapshot(repos = repos,
                  project = project)

  # Restart R session ----------------------------------------------------------
  if (restart){
    return(invisible(getOption("restart")()))
  } else {
    return(invisible(NULL))
  }

}



#' Re-Initialize a slushy environment
#'
#' Following an update of the {slushy} package, re-initialize the slushy
#' environment using the current list of packages, snapshot date, and
#' environment settings.
#'

#' @param project The project directory. If NULL, defaults to the nearest parent
#'   directory that contains an `.Rproj` file relative to the current working directory.
#' @param restart Restart session after slushy is initialized? Defaults to TRUE.
#' @param config List of configuration options. If not provided, defaults to reading from default settings in config file included in slushy package.
#'
#' @export
#'
#' @return This function is called for its side effects.
#'
#' @importFrom dplyr filter pull
#' @importFrom desc desc_get_deps
#' @importFrom rlang .data
slushy_reinit <- function(project = NULL,
                          restart = TRUE,
                          config = get_config()){

  if (is.null(project)){
    project <- proj_root()
  }

  # get pkg list from DESCRIPTION
  pkgs <- desc_get_deps()$package

  # get snapshot date from lockfile
  date <- fromJSON(file.path(project, "renv.lock"))$R$Repositories %>%
    filter(.data$Name=="RSPM") %>%
    pull(.data$URL) %>%
    basename()

  # init slushy
  # current gitignore will be kept
  slushy_init(date = date,
              project = project,
              config = config,
              restart = restart)
}
