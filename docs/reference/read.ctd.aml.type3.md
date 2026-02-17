# Read AML ctd format 3 (not exported)

This is an ad-hoc attempt to read files provided by a user in late
September, 2024. See “Details” for some provisos.

## Usage

``` r
read.ctd.aml.type3(file, encoding, debug = 0)
```

## Arguments

- file:

  character value naming a file.

- encoding:

  ignored.

- debug:

  ignored.

## Details

This function was based on 4 sample files, evidently created with AML
Sailfish 1.4.8.0 software. No documentation was made available, so the
code was written by inspection of the files and some guessing on the
format. This means that the code is likely to be brittle against file
variations.

It is not envisioned that much support will be provided for this file
format, given the lack of documentation. This is the third format seen
for AML files, and it seems likely that there are other formats in
existence. Another factor mitigating against oce adding high support for
this format is the fact that the files made available to the author
contain startling errors in the stated units of for density and sound
speed, which raises questions about the development state of the AML
software.

## Author

Dan Kelley
