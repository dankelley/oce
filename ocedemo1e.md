---
permalink: ocedemo1e.html
title: ocedemo1e
layout: default
---

# Test 1235

Things I have tried

- add the usual link href line to 
- ``css: syntax`` in preamble
- ``css: syntax.css`` in preamble
- ``layout: default`` in preamble

## ruby (from the docs)

{% highlight ruby %}
def show
  @widget = Widget(params[:id])
  respond_to do |format|
    format.html # show.html.erb
    format.json { render json: @widget }
  end
end
{% endhighlight %}

## R

{% highlight r %}
library(oce)
data(ctd)
plot(ctd, which=c(1,2,3,5))
{% endhighlight %}


