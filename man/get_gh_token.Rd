% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dpkg_release_gh.R
\name{get_gh_token}
\alias{get_gh_token}
\title{get github token from GITHUB_PAT environment variable or use bundled token if unset}
\usage{
get_gh_token()
}
\description{
get github token from GITHUB_PAT environment variable or use bundled token if unset
}
\examples{
# withr::with_envvar(c(GITHUB_PAT = ""), get_gh_token())
# get_gh_token()
}
\keyword{internal}
