---
permalink: http://dankelley.github.io/oce/web/ocedemo1.html
title: oce demo 1
---

{% highlight r %}
library(oce)
data(ctd)
plot(ctd, which=c(1,2,3,5))
{% endhighlight %}


{% highlight ruby %}
def show
  @widget = Widget(params[:id])
  respond_to do |format|
    format.html # show.html.erb
    format.json { render json: @widget }
  end
end
{% endhighlight %}

