# dxr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

# dxr 0.0.0.9001

* Added tests for many functions

* Replaced deprecated call to scales::percent() with a call to scales::number().

* Fixed installation instructions

* Ensured that intermediate calculations used in proportion_difference() are 
  performed at full precision. 
  
# dxr 0.0.9006

We're getting there: 

* Can round-trip data to excel and back

* Can calculate line data

* Can calculate "2x2" style table

* Can calculate PPA and NPA by analyte and output it to a human readable table
