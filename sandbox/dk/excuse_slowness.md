Posting to R-pkg-devels dated 4:49AM 2022-05-13

>Actually, oce is one of the very few packages where you cannot do much
>to reduce this:
>
>* checking R code for possible problems ... [177s] OK
>* checking examples ... [128s] OK
>* checking tests ... [70s] OK
>* checking re-building of vignette outputs ... [52s] OK
>
>This shows the runtime checks only consume 4 minutes, but the package is
>rather complex and needs a lot of time in the other parts of the checks.
>
>Therefore reverse dependency checks have been triggered on CRAN.
>
>Best,
>Uwe Ligges
