# CAGIS Open Data Parcels

## About

A tabular data resource derived from the Hamilton County, OH Auditor data distributed through [CAGIS Open Data](https://cagismaps.hamilton-co.org/cagisportal/mapdata/download) of parcel-level characteristics collected by Hamilton County.

## Sources

- `parcel_example.R`
- <https://cagismaps.hamilton-co.org/cagisportal/mapdata/download>

## Details

Parcels were excluded if they were missing an identifier, missing an address number, missing an address street name, or were not considered to be residential.

Because 'second line' address components (e.g., 'Unit 2B') are not captured, a single address can refer to multiple parcels in the case of condos or otherwise shared building ownership.
Large apartment complexes often use multiple mailing addresses that are not the same as the parcel address(es).
