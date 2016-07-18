---
layout: post
title:  "Observer Pattern and Context This"
date:   2016-07-06
categories: js
---

[http://robdodson.me/javascript-design-patterns-observer/](http://robdodson.me/javascript-design-patterns-observer/)

```js
var publisher = {  
    subscribers: {
        any: [] // event type: subscribers
    },
    on: function(type, fn, context) {
        type = type || 'any';
        fn = typeof fn === 'function' ? fn : context[fn];
        if (typeof this.subscribers[type] === "undefined") {
            this.subscribers[type] = [];
        }
        this.subscribers[type].push({ fn: fn, context: context || this });
    },
    remove: function(type, fn, context) {
        this.visitSubscribers('unsubscribe', type, fn, context);
    },
    fire: function(type, publication) {
        this.visitSubscribers('publish', type, publication);
    },
    visitSubscribers: function(action, type, arg, context) {
        var pubtype = type || 'any',
            subscribers = this.subscribers[pubtype],
            i,
            max = subscribers ? subscribers.length : 0;

        for (i = 0; i < max; i += 1) {
            if (action === 'publish') {
                // Call our observers, passing along arguments
                 subscribers[i].fn.call(subscribers[i].context, arg);
            } else {
                if (subscribers[i].fn === arg && subscribers[i].context === context) {
                    subscribers.splice(i, 1);
                }
            }
        }
    }
};
```