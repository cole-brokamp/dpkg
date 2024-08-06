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
