library(lbl)
library(dplyr, warn.conflicts = FALSE)
library(sf)

# download CAGIS OpenData Auditor geodatabase
the_gdb <- fs::path(tools::R_user_dir("lbl_example", "cache"), "Parcels2024.gdb")
if (!fs::file_exists(the_gdb)) {
  tmp <- tempfile(fileext = ".zip")
  download.file("https://www.cagis.org/Opendata/Auditor/Parcels2024.gdb.zip", tmp)
  unzip(tmp, exdir = fs::path_dir(the_gdb))
}

# read in parcels merged with condos layer
rd <-
  sf::st_read(dsn = the_gdb, layer = "HAM_PARCELS_MERGED_W_CONDOS") |>
  st_zm() |>
  st_drop_geometry() |>
  tibble::as_tibble()

d <-
  rd |>
  transmute(
    parcel_id = AUDPTYID,
    parcel_addr_number = ADDRNO,
    parcel_addr_street = ADDRST,
    parcel_addr_suffix = ADDRSF,
    land_use = CLASS,
    condo_id = CONDOMTCH,
    condo_unit = UNIT,
    market_total_value = MKT_TOTAL_VAL,
    acreage = ACREDEED,
    homestead = HMSD_FLAG == "Y",
    rental_registration = RENT_REG_FLAG == "Y"
  )

lu_keepers <-
  c(
    "single family dwelling" = "510",
    "two family dwelling" = "520",
    "three family dwelling" = "530",
    "condominium unit" = "550",
    "apartment, 4-19 units" = "401",
    "apartment, 20-39 units" = "402",
    "apartment, 40+ units" = "403",
    "mobile home / trailer park" = "415",
    "other commercial housing" = "419",
    "office / apartment over" = "431",
    "boataminium" = "551",
    "landominium" = "555",
    "manufactured home" = "560",
    "other residential structure" = "599",
    "condo or pud garage" = "552",
    "metropolitan housing authority" = "645",
    "lihtc res" = "569"
  )

d <-
  d |>
  filter(
    !is.na(parcel_id),
    (!is.na(parcel_addr_number)) & (!is.na(parcel_addr_street)),
    land_use %in% lu_keepers
  ) |>
  mutate(land_use = as.factor(land_use),
         land_use = forcats::fct_recode(land_use, !!!lu_keepers)) |>
  tidyr::unite(
    col = "parcel_address",
    any_of(starts_with("parcel_addr_")),
    sep = " ", na.rm = TRUE, remove = TRUE
  )

out <-
  d |>
  as_lbl_tbl(
    name = "cagis_parcels",
    title = "CAGIS Parcels",
    version = "0.1.0",
    description =
      glue::glue(
        "## About\n",
        "Data are derived from the Hamilton County, OH Auditor data distributed through [CAGIS Open Data](https://cagismaps.hamilton-co.org/cagisportal/mapdata/download).",
        "The tabular data resource has {nrow(d)} rows and {ncol(d)} columns.",
        "Each row is a *unique* parcel identifier (`parcel_id`).",
        "Columns include characteristics derived from the data.",
        "\n## Notes\n",
        "Because 'second line' address components (e.g., 'Unit 2B') are not captured, a single address can refer to multiple parcels in the case of condos or otherwise shared building ownership.",
        "Large apartment complexes often use multiple mailing addresses that are not the same as the parcel address(es).",
        "\n## Scripts\n",
        "`parcel_example.R` was used to access the source on {Sys.Date()} and save `cagis_parcels.rds`.",
        "Briefly, parcels were excluded if they were missing an identifier, missing an address number, missing an address street name, or were not considered to be residential",
        .sep = "\n"
      )
  )

out

out |>
  mutate(across(everything(), as_lbl_vec))



get_labels(out)

u

out <- out |>
  update_field("parcel_id",
    description = "uniquely identifies properties; the auditor Parcel Number"
  ) |>
  update_field("condo_id",
    description = "used to match two parcels to the same building of condos"
  ) |>
  update_field("parcel_address",
    description = "derived by pasting parcel_address_{number, street, suffix}` together"
    ) |>
  update_field("centroid_lat",
               description = "calculated as centroid of casted multipolygon geometry and projected from Ohio South to WGS84") |>
  update_field("centroid_lon",
               description = "calculated as centroid of casted multipolygon geometry and projected from Ohio South to WGS84")

write_fr_tdr(out, dir = fs::path_package("parcel"))
