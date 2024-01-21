# Revision history for phladiprelio-general-datatype

## 0.1.0.0 -- 2023-09-22

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2023-09-22

* First version revised A. Fixed issue with some non-typical though possible strings.

## 0.2.0.0 -- 2023-09-23

* Second version. Added new functions, some generalization provided. Improved .cabal file. 
Some minor documentation improvements. Fixed inaccuracies in the documentation.

## 0.2.1.0 -- 2023-09-23

* Second version revised A. Fixed issue with readU2 function.

## 0.2.2.0 -- 2023-09-23

* Second version revised B. Fixed issue with readBasic0G function and related ones.

## 0.3.0.0 -- 2023-10-01

* Third version. Added new datatype and functions to the library. Some documentation improvements.

## 0.3.0.1 -- 2023-10-01

* Third version revised A. Fixed issue with README.md file.

## 0.3.1.0 -- 2023-10-01

* Third version revised B. Added new functions to the library.

## 0.4.0.0 -- 2023-11-11

* Fourth version. Added a new module Phladiprelio.General.Distance with functionality to calculate
  similarity of the two non-negative 'Real' numbers lists.

## 0.4.1.0 -- 2023-11-11

* Fourth version revised A. Added a new function Phladiprelio.General.Distance.distanceSqrG2.

## 0.5.0.0 -- 2023-11-17

* Fifth version. Added a new module Phladiprelio.General.Datatype3 with extended functionality for
  PhLADiPreLiO.

 ## 0.5.1.0 -- 2023-11-17

* Fifth version revised A. Fixed two issues that led to incorrect behaviour of readU2 and readU3
  functions and also fixed issue with splitL0 function.

## 0.5.2.0 -- 2023-12-01

* Fifth version revised B. Fixed issue with Phladiprelio.General.Distance.distanceSqrG2 function
  that led earlier to incorrect results.

## 0.6.0.0 -- 2024-01-19

* Sixth version. Removed the module Phladiprelio.General.Datatype except readU2 function that moved to the Phladiprelio.General.Datatype3 module because the removed functionality in extended version is provided by the Phladiprelio.General.Datatype3 module. To find the removed functionality, please, refer to the versions up to 0.5.2.0. Fixed issue with the readSimple3 function for '=' after just one syllable. 

## 0.7.0.0 -- 2024-01-21

* Seventh version. Switched to more time efficient imlementation using Data.Sequence functionality from the containers package. Added it as a new (widely used in many cases) dependency. Some code and documentation improvements. Added readU2 to the export list of the Phladiprelio.General.Datatype3 module.

