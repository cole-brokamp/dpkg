#' example labeled table (`lbl_tbl`)
#' 
#' @export
#' @examples
#' example_lbl_tbl()
example_lbl_tbl <- function() {
  tibble::tibble(
    id = substr(vapply(1:5, rlang::hash, character(1)), 1, 6),
    letters = as_lbl_vec(letters[1:5], "The Alphabet"),
    numbers = as_lbl_vec(1:5, "Those Numbers"),
    logicals = as_lbl_vec(c(TRUE, TRUE, FALSE, TRUE, FALSE), "Is foofy")
  ) |>
    as_lbl_tbl(
      name = "my_labeled_table",
      title = "My Labeled Table",
      version = "0.2.1",
      description = paste0(
        "## My Labeled Table",
        "### About  ",
        "This is example was created using the code included in the example.",
        "Suggested to use for testing and learning the `lbl` package for R.",
        "### Sources  ",
        "(e.g.) Created using `make_my_labeled_data.R` to scrape the example website between July 1-3, 2024."
      ),
      created = Sys.time()
    )
}
