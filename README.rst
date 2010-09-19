``Oce`` is an R package for processing oceanographic data.  Stable
versions of the package are normally installed with R itself.  This
repository is for developers and users who wish to report bugs or
suggest features.  There are two branches of interest: a relatively
stable branch named ``master`` and the development branch named
``develop``.  These may be thought of as beta and alpha pre-release
versions of Oce.  To use the develop branch, you must first set up to
"track" it, by doing as follows

    git checkout --track origin/develop

This only has to be done *one time*.  After this is done, do

     git checkout develop

to switch to the ``develop`` branch and then do

     git pull

to pull the updates.  (If you stay on the develop branch, you will
just need to do this final step, to get updates.)

Please note that the ``develop`` branch is merged into the ``master``
branch about once every few weeks, or whenever the developer convinces
himself that there is a useful (and stable) change that needs broader
dissemination.




