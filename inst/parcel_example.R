library(lbl)
library(dplyr, warn.conflicts = FALSE)
library(sf)

md <-
  list(
    name = "cagis_parcels",
    title = "CAGIS Open Data Parcels",
    version = "0.1.0",
    description = paste(
        "A tabular data resource derived from the Hamilton County, OH Auditor data distributed through [CAGIS Open Data](https://cagismaps.hamilton-co.org/cagisportal/mapdata/download) of parcel-level characteristics collected by Hamilton County.",
        "Briefly, parcels were excluded if they were missing an identifier, missing an address number, missing an address street name, or were not considered to be residential.",
        "Because 'second line' address components (e.g., 'Unit 2B') are not captured, a single address can refer to multiple parcels in the case of condos or otherwise shared building ownership.",
        "Large apartment complexes often use multiple mailing addresses that are not the same as the parcel address(es).",
        .sep = "\n"
      )
  )

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
  mutate(
    land_use = as.factor(land_use),
    land_use = forcats::fct_recode(land_use, !!!lu_keepers)
  ) |>
  tidyr::unite(
    col = "parcel_address",
    any_of(starts_with("parcel_addr_")),
    sep = " ", na.rm = TRUE, remove = TRUE
  )

# add metadata and updated labels
out <-
  d |>
  as_lbl_tbl(!!!md) |>
  update_labels(
    parcel_address = "Derived by pasting `parcel_addr_{number, street, suffix}` together",
    parcel_id = "Uniquely identifies parcels of land (i.e., auditor parcel number)",
    land_use = "Parcel land usage code; distinct from city land use codes"
  )

## login using profile sso account
system2("aws", c("sso", "login", "--profile", "geomarker-io"))
Sys.setenv("AWS_PROFILE" = "geomarker-io")

codec_board <-
  pins::board_s3(
    bucket = "io.geomarker.codec",
    versioned = FALSE,
    prefix = "data/",
    profile = "geomarker-io",
    cache = tools::R_user_dir("io.geomarker.codec.data", "cache")
  )

pins::pin_write(codec_board, out, name = glue::glue("{attr(out, 'name')}_v{attr(out, 'version')}"), type = "rds")
