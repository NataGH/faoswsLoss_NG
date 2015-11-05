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
- `CheckDebug` made more robust
- `GetTableData` now sanitises table names
- Minor documentation fixes


# CHANGES IN faosws VERSION 0.4.0

## NEW FEATURES

- Added `CheckDebug`, a function which returns TRUE when not on the server 
- Added `SetClientFiles`, a function to change the directory where the client
certificates are stored

## MAJOR CHANGES

- NAMESPACE is now generated automatically
- faosws objects regarding authentication are now stored in an environment 
called .swsenv which is declared in SetClientFiles.R

## MINOR CHANGES

- `?faosws` and `?faosws-package` now give a help page with basic instructions
- Slots are now properly documented as slots