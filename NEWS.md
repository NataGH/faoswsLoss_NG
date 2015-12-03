# CHANGES IN faosws VERSION 0.6.2

## MINOR CHANGES

- `SetTableData` has been deprecated in favor of new functions specified in `?SaveDatatable`

## BUG FIXES

- Fixed bug where `AddDeletions` would remove columns from table passed to it
- Fixed Mac incompatibility with `ReadDatatable`


# CHANGES IN faosws VERSION 0.6.1

## BUG FIXES

- Fixed incompatibility with old version of libcurl


# CHANGES IN faosws VERSION 0.6.0

## NEW FEATURES

- Added `AddInsertions`, `AddModifications`, `AddDeletions` to write rows to a Datatable with companions
`Changeset` and `Finalize` which track lines already sent.

## MINOR CHANGES

- Changed name of `FetchDatatable` to `BoundDatatable` 
- Changed name of `ReadDatatableList` to `FetchDatatableConfig`


# CHANGES IN faosws VERSION 0.5.0

## NEW FEATURES

- Added `ReadDatatable` to read Datatables, the new implementation of ad-hoc 
tables. `GetTableData` has been deprecated. - Added `ReadDatatableList` to get
metadata for all Datatables

## MAJOR CHANGES

- Added `DtDimension` class for Datatable columns specification

## MINOR CHANGES

- Minor documentation fixes

## BUG FIXES

- Fixed export of `MappingTableKey`


# CHANGES IN faosws VERSION 0.4.2

## MINOR CHANGES

- Duplicated keys in `DatasetKey` objects are now removed with a warning -
`GetTestEnvironment` no longer checks to see if it is on the server (This should
be handled entirely by `if(CheckDebug())`) - Added README.md for non-R users

## BUG FIXES

- Temporary fix for Rdatatable/data.table#1352 until v1.9.8 comes out - All keys
in an empty data.frame are now chars. Fixes #SWS-797


# CHANGES IN faosws VERSION 0.4.1

## MINOR CHANGES

- `CheckDebug` made more robust - `GetTableData` now sanitises table names -
Minor documentation fixes


# CHANGES IN faosws VERSION 0.4.0

## NEW FEATURES

- Added `CheckDebug`, a function which returns TRUE when not on the server -
Added `SetClientFiles`, a function to change the directory where the client 
certificates are stored

## MAJOR CHANGES

- NAMESPACE is now generated automatically - faosws objects regarding
authentication are now stored in an environment called .swsenv which is declared
in SetClientFiles.R

## MINOR CHANGES

- `?faosws` and `?faosws-package` now give a help page with basic instructions -
Slots are now properly documented as slots