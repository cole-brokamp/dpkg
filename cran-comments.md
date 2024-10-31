## R CMD check results

Resubmission of dpkg 0.6.0:

- Used only undirected quotation marks in description text
- Removed example in unexported function
- Removed commented out examples
- I believe \dontrun is needed in my case because running the example would error without a user-provided github repository and github API key.
- I've confirmed that the DESCRIPTION file contains all of the authors, contributors, and copyright holders in the Authors@R section.

0 errors | 0 warnings | 0 notes

* This is a new release.
* R CMD checks passed with above results on:
  - R v4.4.1 (macos, windows, and ubuntu)
  - R v4.3.3 (ubuntu)
  - A development version of R (ubuntu and windows)
