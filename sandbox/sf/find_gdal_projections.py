#!/usr/bin/env python

# This python code is based on code provided at
# https://stackoverflow.com/questions/47019616/gdal-doesnt-recognize-bipolar-conic-western-hemisphere-projection
# with the only difference being in formatting.

from osgeo import osr
from subprocess import Popen, PIPE

osr.UseExceptions()

# Get the list of PROJ.4 projections
proj = {}
p = Popen(['proj', '-lp'], stdout=PIPE)
for line in p.communicate()[0].split('\n'):
    if ':' in line:
        a, b = line.split(':')
        proj[a.strip()] = b.strip()

# Brute force method of testing GDAL's OSR module
supported = set()
not_supported = set()
for k in proj.keys():
    sr = osr.SpatialReference()
    try:
        _ = sr.ImportFromProj4('+proj=' + k)
        supported.add(k)
    except RuntimeError as e:
        not_supported.add(k)

print('This information comes from a python script based on code provided at https://stackoverflow.com/questions/47019616/gdal-doesnt-recognize-bipolar-conic-western-hemisphere-projection\n')
print('{0} total projections, {1} supported, {2} not supported'
      .format(len(proj), len(supported), len(not_supported)))
print('\nSupported: ' + ', '.join(sorted(supported)))
print('\nNot supported: ' + ', '.join(sorted(not_supported)))
