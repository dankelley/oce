---
permalink: /ocedemo1b.html
title: oce demo 1b
---

TEST some ruby from the docs.

{% highlight ruby %}
def show
  @widget = Widget(params[:id])
  respond_to do |format|
    format.html # show.html.erb
    format.json { render json: @widget }
  end
end
{% endhighlight %}

