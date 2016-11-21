---
layout: post
title:  "window history"
date:   2016-11-21
categories: js
---

### 历史相关的,不能去新的页面

```js
window.history.back();

window.history.forward();

// Go back two entries.
window.history.go(-2);

// Go forward 3 entries.
window.history.go(3);

// length
window.history.length
```

### 对历史的修改等

***pushState***

```js
window.history.pushState(stateObj, title, URL);
```

* stateObj – The state object is used to store data that is associated the new history entry. This could include the page title, a URL to load via AJAX or even the page content itself.
* title – The title parameter should act as a description for the history entry.
* URL – (optional) This is the URL that will be associated with the history entry. The browser won’t load this URL when pushState() is called, but will display it in the address bar. It’s worth noting that this URL may be loaded if the user decides to refresh the page or restarts the browser.

***replaceState()***

This can be useful if you want to add some data to your state object after pushState() has been called.

```js
// Updates the current history entry.
window.history.replaceState(stateObj, title, URL);
```

Note: The `pushState()` and `replaceState()` methods will not cause a hashchange event to be fired.

### The popstate Event

Most commonly when the browsers back or forward buttons are clicked (or a call to back(), forward() or go() is executed).

```js
window.addEventListener('popstate', function(event) {
  var state = event.state;
});
```

It’s worth noting that calls to pushState() and replaceState() will not trigger a popstate event.

### window.history.state

This is useful if you need to read the state object when a popstate event has not been fired.

```js
window.history.state;
```


