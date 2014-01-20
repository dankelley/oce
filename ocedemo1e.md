---
permalink: ocedemo1e.html
title: ocedemo1e
layout: default
---


# Test 1215

Things I have tried

- add the usual link href line to 
- set ``css: syntax`` in YAML preamble
- set ``css: syntax.css`` in YAML preamble
- remove ``layout: default`` from preamble

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


