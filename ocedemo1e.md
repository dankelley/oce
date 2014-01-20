<link href="syntax.css" media="screen" rel="stylesheet" type="text/css" />

---
css: syntax.css
pygments: true
brokenlayout: default
permalink: ocedemo1e.html
title: ocedemo1e
---

# Test 0958

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


