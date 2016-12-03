---
layout: post
title:  "google performaance"
date:   2016-11-28
categories: js
---

### devtool

Timeline, Main的那行，点击出问题的地方，下边可以看到Call Stack, 也就可以查出问题代码是什么


### 哪些会出问题

***大量layout操作***

```js
function colorizeAndScaleStories() {

  var storyElements = document.querySelectorAll('.story');

  // It does seem awfully broad to change all the
  // colors every time!
  for (var s = 0; s < storyElements.length; s++) {

    var story = storyElements[s];
    var score = story.querySelector('.story__score');
    var title = story.querySelector('.story__title');

    // Base the scale on the y position of the score.
    var height = main.offsetHeight;
    var mainPosition = main.getBoundingClientRect();
    var scoreLocation = score.getBoundingClientRect().top -
        document.body.getBoundingClientRect().top;
    var scale = Math.min(1, 1 - (0.05 * ((scoreLocation - 170) / height)));
    var opacity = Math.min(1, 1 - (0.5 * ((scoreLocation - 170) / height)));

    score.style.width = (scale * 40) + 'px';
    score.style.height = (scale * 40) + 'px';
    score.style.lineHeight = (scale * 40) + 'px';

    // Now figure out how wide it is and use that to saturate it.
    scoreLocation = score.getBoundingClientRect();
    var saturation = (100 * ((scoreLocation.width - 38) / 2));

    score.style.backgroundColor = 'hsl(42, ' + saturation + '%, 50%)';
    title.style.opacity = opacity;
  }
}
```
### 用requestAnimationFrame

Ideally, anything that makes a visible change to the page should happen inside a `requestAnimationFrame` call. 

```js
main.addEventListener('scroll', function() {

  ...

  // Check if we need to load the next batch of stories.
  var loadThreshold = (main.scrollHeight - main.offsetHeight -
      LAZY_LOAD_THRESHOLD);
  if (main.scrollTop > loadThreshold)
    loadStoryBatch();
});

// 变成下边这样

main.addEventListener('scroll', function() {

  ...

  // Check if we need to load the next batch of stories.
  var loadThreshold = (main.scrollHeight - main.offsetHeight -
      LAZY_LOAD_THRESHOLD);
  if (main.scrollTop > loadThreshold)
    requestAnimationFrame(loadStoryBatch);
});
```


### 过多内存使用

不要总是创建新的element,这样多次后会使内存增多。创建一个，每次修改即可

```js
storyDetails = document.createElement('section');
storyDetails.setAttribute('id', 'sd-' + details.id);
storyDetails.classList.add('story-details');
storyDetails.innerHTML = storyDetailsHtml;

document.body.appendChild(storyDetails);

// to this

storyDetails.setAttribute('id', 'sd-' + details.id);
storyDetails.innerHTML = storyDetailsHtml;
```
