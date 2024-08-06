#' Use a dpkg to create a github release
#'
#' The release will be tagged at the current commit and
#' named according to the `name` and `version` of the dpkg.
#' The `GITHUB_PAT` environment variable must be set and the working directory
#' must be inside of a git repository with a GitHub remote.
#' Trying to create more than one release from the current commit will result in an error.
#' @param x a data package (`dpkg`) object
#' @param draft logical; mark release as draft?
#' @return the URL to the release (invisibly)
#' @export
#' @examples
#' \dontrun{
#' dpkg_gh_release(as_dpkg(mtcars))
#' }
dpkg_gh_release <- function(x, draft = TRUE) {
  rlang::check_installed("gert", "get current git commit")
  rlang::check_installed("gh", "create a release on github")
  gh_owner <- gh::gh_tree_remote()$username
  gh_repo <- gh::gh_tree_remote()$repo

  draft_release_details <-
    gh::gh(
      glue::glue("POST /repos/{gh_owner}/{gh_repo}/releases"),
      name = glue::glue("{x@name} {x@version}"),
      tag_name = glue::glue("{x@name}-v{x@version}"),
      target_commitish = gert::git_info()$commit,
      draft = draft
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
    httr2::req_url_query(name = glue::glue("{x@name}-v{x@version}.parquet")) |>
    httr2::req_body_file(written_path) |>
    httr2::req_perform()

  message("created draft release at: ", draft_release_details$html_url)
  return(invisible(draft_release_details$html_url))
}

#' get github token from GITHUB_PAT environment variable or use bundled token if unset
#' @examples
#' # withr::with_envvar(c(GITHUB_PAT = ""), get_gh_token())
#' # get_gh_token()
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

# stow a github release asset created with dpkg_github_release()
# @examples
# stow_gh_release("cole-brokamp", "dpkg", "mtcars-v0.0.0.9000")
# stow_gh_release("cole-brokamp", "dpkg", "mtcars-v0.0.0.9000", force = TRUE)
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

  httr2::request(glue::glue("https://api.github.com/repos/{owner}/{repo}/releases/assets/{the_asset$id}")) |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      Authorization = glue::glue("Bearer {get_gh_token()}"),
      `X-GitHub-Api-Version` = "2022-11-28",
      .redact = "Authorization"
    ) |>
    httr2::req_perform(path = stow_path(dpkg_filename))
  return(stow_path(dpkg_filename))
}
