---
layout: post
title:  "Welcome to Jekyll!"
date:   2016-06-05 17:15:06 +0800
categories: jekyll update
---
## 函数式编程中的错误处理

## 什么是lifting?

`var zoltar = compose(map(console.log), map(fortune), getAge(moment()));`

At the time of calling, a function can be surrounded by map, which transforms it from a non-functory function to a functory one, in informal terms. We call this process lifting.


## Side Effect 包括但不限于以下：

* changing the file system
* inserting a record into a database
* making an http call
* mutations
* printing to the screen / logging
* obtaining user input
* querying the DOM
* accessing system state

Any interaction with the world outside of a function is a side effect
You’ll find this post in your `_posts` directory. Go ahead and edit it and re-build the site to see your changes. You can rebuild the site in many different ways, but the most common way is to run `jekyll serve`, which launches a web server and auto-regenerates your site when a file is updated.

To add new posts, simply add a file in the `_posts` directory that follows the convention `YYYY-MM-DD-name-of-post.ext` and includes the necessary front matter. Take a look at the source for this post to get an idea about how it works.

Jekyll also offers powerful support for code snippets:

{% highlight ruby %}
def print_hi(name)
  puts "Hi, #{name}"
end
print_hi('Tom')
#=> prints 'Hi, Tom' to STDOUT.
{% endhighlight %}

Check out the [Jekyll docs][jekyll-docs] for more info on how to get the most out of Jekyll. File all bugs/feature requests at [Jekyll’s GitHub repo][jekyll-gh]. If you have questions, you can ask them on [Jekyll Talk][jekyll-talk].

[jekyll-docs]: http://jekyllrb.com/docs/home
[jekyll-gh]:   https://github.com/jekyll/jekyll
[jekyll-talk]: https://talk.jekyllrb.com/
