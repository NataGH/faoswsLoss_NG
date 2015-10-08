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