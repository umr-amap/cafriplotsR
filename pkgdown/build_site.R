# Build the pkgdown site into docs/ (GitHub Pages serves master:/docs).
#
# Run from the package root:
#   source("pkgdown/build_site.R")
#
# Why not plain pkgdown::build_site():
#
# 1. pkgdown renders every *.md file at the package root and in .github/ as a
#    page of the site (README.md and NEWS.md excepted), and has no option to
#    switch this off. Working notes (CLAUDE.md, README.fr.md, any *_PLAN.md
#    left in the root...) were therefore published. Only README.md (home
#    page), NEWS.md (changelog) and the vignettes/ articles belong on the site,
#    so every other root .md is hidden for the duration of the build.
#
# 2. pkgdown never deletes a page whose source has gone, so docs/ kept the
#    html of notes removed from the root long ago. The site is rebuilt from an
#    empty docs/ (clean = TRUE). Nothing is lost: everything in docs/ is
#    generated, and public-access.json is copied back from pkgdown/assets/.

local({
  site_md <- c("README.md", "NEWS.md", "LICENSE.md", "LICENCE.md")
  suffix <- ".pkgdown-hidden"

  if (!file.exists("_pkgdown.yml")) {
    stop("Run this script from the package root.")
  }

  # A build killed before on.exit() ran leaves files hidden: put them back
  leftovers <- c(
    list.files(".", paste0("\\.md", suffix, "$"), all.files = TRUE),
    list.files(".github", paste0("\\.md", suffix, "$"), full.names = TRUE)
  )
  file.rename(leftovers, sub(paste0(suffix, "$"), "", leftovers))

  stray <- c(
    list.files(".", "\\.md$"),
    list.files(".github", "\\.md$", full.names = TRUE)
  )
  stray <- stray[!basename(stray) %in% site_md]

  if (length(stray) > 0) {
    message("Kept out of the site: ", paste(stray, collapse = ", "))
    file.rename(stray, paste0(stray, suffix))
    on.exit(file.rename(paste0(stray, suffix), stray), add = TRUE)
  }

  pkgdown::build_site_github_pages(clean = TRUE, install = TRUE, new_process = TRUE)
})
