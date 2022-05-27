Two things are being done in this directory (see
https://github.com/dankelley/oce/discussions/1756 for discussion).

1. Create a high-resolution dataset for the Nova Scotia region.
2. Solve a problem with plotting topo with GEBCO colours.  Originally, before
   2020-12-14, shallow waters were being plotted with a reddish (land-like)
   hue.  This was basically because the palette is constructed by blending
   colours, and the water colours are bluish, while the land ones are reddish,
   but mixing blue and red does not yield white.  I solved this by adding a
   white band, although I found that band to be too wide in tests, so I first
   expanded the 9-element water and land bands to be longer.  See the above-named website for details.

NOTE:  the new datafile, called `topoNS.rda` for now, is 87K (was 163K before
compression).

