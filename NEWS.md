## BUG FIXES

- `FetchDatatableConfig` now correctly handles missing tables when there is more than one


# CHANGES IN faosws VERSION 0.8.0

## NEW FEATURES

- New function `ClearSessionOnly` which allows clearing of keys marked 
  session-only

## BUG FIXES

- All data read in from the system now uses proper UTF-8 encoding
- Prevented error when POST response is empty


# CHANGES IN faosws VERSION 0.7.3

## MINOR CHANGES

- Added documentation for setup on Macs

## BUG FIXES

- Fixed error when there are no keys in `GetTestEnvironment`


# CHANGES IN faosws VERSION 0.7.2

## MINOR CHANGES

- Additions to documentation

## BUG FIXES

- Prevented error in `GetHistory` when setting column order
- Fixed 'appended/updated' issue in SWS API (not user-visible)


# CHANGES IN faosws VERSION 0.7.1

## MINOR CHANGES

- Added vignette R_API_2014 to package
- Added tests
- Improved documentation for `GetCodeList`, `SaveData`, `GetMapping`

## BUG FIXES

- `AddViolation` now allows full table of NA values


# CHANGES IN faosws VERSION 0.7.0

## NEW FEATURES

- `ReadDatatable` now takes a `validationOptions` argument to read data from the
  validation API
- New function `AddViolation` which allows updating of violation data

## MINOR CHANGES

- `Changeset` now has a new `type` option which differentiates between 
  validation and writing to a dataset

  
# CHANGES IN faosws VERSION 0.6.3

## MINOR CHANGES

- `FetchDatatableConfig` now throws an informative error if a table doesn't 
  exist
- Reading from an empty Datatable now results in a table with the correct 
  columns

## BUG FIXES

- Fixed header format on `ReadDatatable` and `SaveDatatable` POST requests


# CHANGES IN faosws VERSION 0.6.2

## MINOR CHANGES

- `SetTableData` has been deprecated in favor of new functions specified in
  `?SaveDatatable`

## BUG FIXES

- Fixed bug where `AddDeletions` would remove columns from table passed to it
- Fixed Mac incompatibility with `ReadDatatable`


# CHANGES IN faosws VERSION 0.6.1

## BUG FIXES

- Fixed incompatibility with old version of libcurl


# CHANGES IN faosws VERSION 0.6.0

## NEW FEATURES

- Added `AddInsertions`, `AddModifications`, `AddDeletions` to write rows to a
  Datatable with companions `Changeset` and `Finalize` which track lines already
  sent

## MINOR CHANGES

- Changed name of `FetchDatatable` to `BoundDatatable` 
- Changed name of `ReadDatatableList` to `FetchDatatableConfig`


# CHANGES IN faosws VERSION 0.5.0

## NEW FEATURES

- Added `ReadDatatable` to read Datatables, the new implementation of ad-hoc 
  tables. `GetTableData` has been deprecated. 
- Added `ReadDatatableList` to get metadata for all Datatables

## MAJOR CHANGES

- Added `DtDimension` class for Datatable columns specification

## MINOR CHANGES

- Minor documentation fixes

## BUG FIXES

- Fixed export of `MappingTableKey`


# CHANGES IN faosws VERSION 0.4.2

## MINOR CHANGES

- Duplicated keys in `DatasetKey` objects are now removed with a warning 
- `GetTestEnvironment` no longer checks to see if it is on the server (This
  should be handled entirely by `if(CheckDebug())`)
- Added README.md for non-R users

## BUG FIXES

- Temporary fix for Rdatatable/data.table#1352 until v1.9.8 comes out 
- All keys in an empty data.frame are now chars. Fixes #SWS-797


# CHANGES IN faosws VERSION 0.4.1

## MINOR CHANGES

- `CheckDebug` made more robust - `GetTableData` now sanitises table names 
- Minor documentation fixes


# CHANGES IN faosws VERSION 0.4.0

## NEW FEATURES

- Added `CheckDebug`, a function which returns TRUE when not on the server 
- Added `SetClientFiles`, a function to change the directory where the client 
  certificates are stored

## MAJOR CHANGES

- NAMESPACE is now generated automatically - faosws objects regarding
  authentication are now stored in an environment called .swsenv which is 
  declared in SetClientFiles.R

## MINOR CHANGES

- `?faosws` and `?faosws-package` now give a help page with basic instructions 
- Slots are now properly documented as slots