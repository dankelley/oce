---
permalink: ocedemo1c.html
title: ocedemo1c
---

# Test some ruby from the docs.

{% highlight ruby %}
def show
  @widget = Widget(params[:id])
  respond_to do |format|
    format.html # show.html.erb
    format.json { render json: @widget }
  end
end
{% endhighlight %}

# Test some R

{% highlight r %}
library(oce)
data(ctd)
plot(ctd, which=c(1,2,3,5))
{% endhighlight %}


