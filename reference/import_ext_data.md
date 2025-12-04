# Get path to external data for package examples

Access sample files in `inst/extdata` directory.

## Usage

``` r
import_ext_data(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, the example files will be listed.

## Examples

``` r
import_ext_data()
#>  [1] "Control-trace.png"               "DMH-brain-slice.jpg"            
#>  [3] "empty_raw_datasheet.csv"         "rat-methods.jpg"                
#>  [5] "sample-ap-count-data.csv"        "sample_abf.abf"                 
#>  [7] "sample_ap_data.csv"              "sample_cell_characteristics.csv"
#>  [9] "sample_eEPSC_data.csv"           "sample_new_eEPSC_data.csv"      
#> [11] "sample_theme_options.csv"       
import_ext_data("sample_cell_characteristics.csv")
#> [1] "/home/runner/work/_temp/Library/patchclampplotteR/extdata/sample_cell_characteristics.csv"
```
