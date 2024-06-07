# slushy 0.5.0

- Upversion {renv} dependency
- Set config `ppm.enabled` to `FALSE` and settings `ppm.enabled` to `TRUE` in the included config to disable use of P3M in addition to specified PPM repo.
- Allow `slushy_sync()` to print messages from `renv::restore()` in real time
- Modify `.gitignore` and `.renvignore` during `slushy_init()` to ensure slushy works as expected in a git project. Specifically, `slushy_init()` now updates the `.gitignore` so critical files related to slushy are not accidentally ignored by git, and adds a `.renvignore` file to ensure R packages being used in the project are not accidentally overlooked.
 
# slushy 0.4.0

First release. 