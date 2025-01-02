#' Use a dpkg to create a github release
#'
#' A GitHub release will be created based on the current commit,
#' tagged and named according to the `name` and `version` of the dpkg.
#' The dpkg `description` is used for the release body.
#'
#' @details
#' The `GITHUB_PAT` environment variable must be set and the working directory
#' must be inside of a git repository with a GitHub remote.
#'
#' The GitHub release will *not* be set to the latest release in order to prevent
#' problems with other automated actions that rely on the latest release, like
#' R universe or remotes `"*release"` syntax or other GitHub actions.
#' 
#' Release tags are required to be unique, so this will fail if a release with the same
#' name and version already exists.
#' @param x a data package (`dpkg`) object
#' @param draft logical; mark release as draft?
#' @return the URL to the release (invisibly)
#' @export
#' @examples
#' \dontrun{
#' dpkg_gh_release(
#'   as_dpkg(mtcars,
#'     version = "0.0.0.9001", title = "Foofy Cars",
#'     homepage = "https://github.com/cole-brokamp/dpkg",
#'     description =
#'       paste("# Foofy Cars\n",
#'         "This is a test for the [dpkg](https://github.com/cole-brokamp/dpkg) package.",
#'         collapse = "\n"
#'       )
#'   ),
#'   draft = FALSE
#' )
#' }
#' #> created release at: https://github.com/cole-brokamp/dpkg/releases/tag/mtcars-v0.0.0.9001
#'
dpkg_gh_release <- function(x, draft = TRUE) {
  if (!inherits(x, "dpkg")) rlang::abort("x must be a `dpkg` object`")
  rlang::check_installed("gert", "get current git commit")
  rlang::check_installed("gh", "create a release on github")
  gh_owner <- gh::gh_tree_remote()$username
  gh_repo <- gh::gh_tree_remote()$repo

  draft_release_details <-
    gh::gh(
      glue::glue("POST /repos/{gh_owner}/{gh_repo}/releases"),
      name = glue::glue("{attr(x, 'name')}-v{attr(x, 'version')}"),
      tag_name = glue::glue("{attr(x, 'name')}-v{attr(x, 'version')}"),
      target_commitish = gert::git_info()$commit,
      generate_release_notes = FALSE,
      body = attr(x, "description"),
      draft = draft,
      make_latest = "false"
    )
  gh_release_id <- draft_release_details$id

  put_asset_req <-
    glue::glue("https://uploads.github.com/repos/{gh_owner}/{gh_repo}/releases/{gh_release_id}/assets") |>
    httr2::request() |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      Authorization = glue::glue("Bearer {Sys.getenv('GITHUB_PAT')}"),
      `Content-Type` = "application/octet-stream",
      `X-GitHub-Api-Version` = "2022-11-28",
      .redact = "Authorization"
    )

  written_path <- write_dpkg(x, tempdir())

  put_asset_req |>
    httr2::req_url_query(name = glue::glue("{attr(x, 'name')}-v{attr(x, 'version')}.parquet")) |>
    httr2::req_body_file(written_path) |>
    httr2::req_perform()

  message(glue::glue("created {ifelse(draft, 'draft release', 'release')} at: {draft_release_details$html_url}"))
  return(invisible(draft_release_details$html_url))
}

#' Use a markdown badge for a dpkg's latest github release
#'
#' The badge relies on shields.io for the images, which will always
#' display to the most recently released version and will link to the
#' releases specific to the dpkg name.
#'
#' Note that this relies on the structure of the release created with
#' `dpkg_gh_release()`, but relies on a dpkg object *before* it is released.
#' This will lead to broken release badges and links until an initial
#' dpkg release is created with `dpkg_gh_release()`.
#' @param x a data package (`dpkg`) object
#' @returns character string of markdown
#' @export
#' @examples
#' \dontrun{
#' as_dpkg(mtcars,
#'   version = "0.0.0.9000", title = "Foofy Cars",
#'   homepage = "https://github.com/cole-brokamp/dpkg",
#'   description =
#'     paste("# Foofy Cars\n",
#'       "This is a test for the [dpkg](https://github.com/cole-brokamp/dpkg) package.",
#'       collapse = "\n"
#'     )
#' ) |>
#'   use_dpkg_badge()
#' }
#'
use_dpkg_badge <- function(x) {
  if (!inherits(x, "dpkg")) rlang::abort("x must be a `dpkg` object`")
  rlang::check_installed("gert", "get current git commit")
  rlang::check_installed("gh", "create a release on github")
  gh_owner <- gh::gh_tree_remote()$username
  gh_repo <- gh::gh_tree_remote()$repo

  badge_src <- glue::glue(
    "https://img.shields.io/github/v/release/",
    "{gh_owner}/{gh_repo}",
    "?sort=date&filter={attr(x, 'name')}-*",
    "&display_name=tag",
    "&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175"
  )
  badge_href <- glue::glue("https://github.com/{gh_owner}/{gh_repo}/releases?q={attr(x, 'name')}&expanded=false")
  rlang::check_installed("usethis", "insert markdown badges into README")
  usethis::use_badge(glue::glue("latest github release for {attr(x, 'name')} dpkg"),
                     href = badge_href, src = badge_src)
  return(invisible(glue::glue("[![]({badge_src})](badge_href)")))
}

#' get github token from GITHUB_PAT environment variable or use bundled token if unset
#' @keywords internal
get_gh_token <- function() {
  gh_token <- Sys.getenv("GITHUB_PAT")
  if (gh_token == "") {
    message("using bundled github token to access a public repository")
    message("use your own token by defining the `GITHUB_PAT` environment variable")
    gh_token <-
      paste0(
        "0x",
        c(
          "67", "68", "70", "5f", "37", "74", "4d", "50", "65",
          "32", "6e", "38", "33", "4b", "4c", "75", "47", "57", "44", "32",
          "6f", "65", "61", "69", "47", "7a", "72", "70", "49", "34", "55",
          "41", "4d", "58", "33", "44", "48", "78", "33", "47"
        )
      ) |>
      as.raw() |>
      rawToChar()
  }
  return(gh_token)
}

#' download a github release asset to the `stow` R user directory
#' @param owner string of repo owner
#' @param repo string of repo name
#' @param dpkg string of gh release tag (will be the same as the filename without the `.parquet` extension)
#' @rdname stow
#' @export
stow_gh_release <- function(owner, repo, dpkg, overwrite = FALSE) {
  dpkg_filename <- paste0(dpkg, ".parquet")
  if (stow_exists(dpkg_filename) && !overwrite) {
    return(stow_path(dpkg_filename))
  }
  the_release <-
    httr2::request(glue::glue("https://api.github.com/repos/{owner}/{repo}/releases/tags/{dpkg}")) |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      Authorization = glue::glue("Bearer {get_gh_token()}"),
      `X-GitHub-Api-Version` = "2022-11-28",
      .redact = "Authorization"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  the_assets <-
    httr2::request(glue::glue("https://api.github.com/repos/{owner}/{repo}/releases/{the_release$id}/assets")) |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      Authorization = glue::glue("Bearer {get_gh_token()}"),
      `X-GitHub-Api-Version` = "2022-11-28",
      .redact = "Authorization"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  the_asset <- the_assets[[which(vapply(the_assets, \(.) .$name == paste0(dpkg, ".parquet"), logical(1)))]]

  stow_url(the_asset$browser_download_url)

  return(stow_path(dpkg_filename))
}
