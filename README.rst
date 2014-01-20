``Oce`` is an R package for processing oceanographic data.  Stable versions of
the package are normally installed from within R, with the code presented here
being intended for advanced users.  

The git branching model is used to organize the code. The ``master`` branch
holds the code used in the official releases, and is mainly provided to
document the history of those releases.  It is updated only when new versions
of Oce are updated to the Comprehensive R Archive Network (CRAN), perhaps once
a season.  The ``develop`` branch may be updated several times per day, as the
author fixes bugs or adds features that are motivated by day-to-day usage.
This is the branch favoured by users who need new features or would like to
help the author in the development of Oce.  The easy way to install the
``develop`` branch is to execute the following commands in R::

{% highlight r %}
library(devtools)
install_github('ocedata', 'dankelley', 'master')
install_github('oce', 'dankelley', 'develop')
{% endhighlight %}

This uses the ``devtools`` package, which is one of many powerful tools written
by Hadley Wickham (whose github site is https://github.com/hadley).

In addition to the ``master`` and ``develop`` branches, there is a ``testing``
branch that is sometimes used to try out variants of new features, and many
temporary branches that are used to address bugs or requests for new features.

Bug reports and feature requests should be made through the GitHub issue
system, and often the code branches that address such issues are named after
the issues, e.g. branch ``issue100`` addressing issue number 100.

Oce is emphatically an open-source system, and so the participation of users is
very important.  This is why Git is used for version control of the Oce source
code, and why GitHub is the host for that code.  All users are invited to take
part in the development process, by suggesting features, by reporting bugs, or
just by watching as others do such things.  Oceanography is a collaborative
discipline, so it makes sense that the evolution of the Oce package be
similarly collaborative.

