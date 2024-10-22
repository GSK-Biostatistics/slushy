# slushy development 0.5.2.9001
- Add user-supplied configuration file to be 'unignored' in the `gitignore` file

# slushy development 0.5.2.9000
- Add a `keep` parameter to the `slushy_remove()` function, allowing users to specify files that should not be removed during the cleanup process. The default value is `"slushy_config.yml"`.

# slushy 0.5.2
- Explicitly pass repos to `renv::install` for `slushy_add`

# slushy 0.5.1
- Fix issue where selected version of {renv} may be unable to be retrieved on start up if it does not align with {renv} version from selected CRAN snapshot.

# slushy 0.5.0

- Upversion {renv} dependency
- Set config `ppm.enabled` to `FALSE` and settings `ppm.enabled` to `TRUE` in the included config to disable use of P3M in addition to specified PPM repo.
- Allow `slushy_sync()` to print messages from `renv::restore()` in real time
- Modify `.gitignore` and `.renvignore` during `slushy_init()` to ensure slushy works as expected in a git project. Specifically, `slushy_init()` now updates the `.gitignore` so critical files related to slushy are not accidentally ignored by git, and adds a `.renvignore` file to ensure R packages being used in the project are not accidentally overlooked.
 
# slushy 0.4.0

First release. 
