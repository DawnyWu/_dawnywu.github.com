---
layout: post
title:  "progressive web app"
date:   2016-11-28
categories: js
---

### app shell

需要有几样东西

* HTML and CSS for the "skeleton" of your user interface complete with navigation and content placeholders.

* An external JavaScript file (app.js) for handling navigation and UI logic as well as the code to display posts retrieved from the server and store them locally using a storage mechanism like IndexedDB.

* A web app manifest and service worker loader to enable off-line capabilities.

```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>App Shell</title>
  <link rel="manifest" href="/manifest.json">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>App Shell</title>
  <link rel="stylesheet" type="text/css" href="styles/inline.css">
</head>

<body>
  <header class="header">
    <h1 class="header__title">App Shell</h1>
  </header>

  <nav class="nav">
  ...
  </nav>

  <main class="main">
  ...
  </main>

  <div class="dialog-container">
  ...
  </div>

  <div class="loader">
    <!-- Show a spinner or placeholders for content -->
  </div>

  <script src="app.js" async></script>
  <script>
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('/sw.js').then(function(registration) {
      // Registration was successful
      console.log('ServiceWorker registration successful with scope: ', registration.scope);
    }).catch(function(err) {
      // registration failed :(
      console.log('ServiceWorker registration failed: ', err);
    });
  }
  </script>
</body>
</html>
```

### Caching the app shell manually

```js
var cacheName = 'shell-content';
var filesToCache = [
  '/css/styles.css',
  '/js/scripts.js',
  '/images/logo.svg',

  '/offline.html’,

  '/’,
];

self.addEventListener('install', function(e) {
  console.log('[ServiceWorker] Install');
  e.waitUntil(
    caches.open(cacheName).then(function(cache) {
      console.log('[ServiceWorker] Caching app shell');
      return cache.addAll(filesToCache);
    })
  );
});
```

### 注册 service worker

`chrome://serviceworker-internals/`这个可以设置debug serviceWorker

1 创建一个 JavaScript 文件作为 `service worker`

2 告诉浏览器注册这个 JavaScript 文件为 `service worker`

```js
if('serviceWorker' in navigator) {  
    navigator.serviceWorker  
        .register('/service-worker.js')  
        .then(function() { console.log('Service Worker Registered'); });  
}
```

###  监听install event

```js
var cacheName = 'weatherPWA-step-6-1';
var filesToCache = [];

self.addEventListener('install', function(e) {
  console.log('[ServiceWorker] Install');
  e.waitUntil(
    caches.open(cacheName).then(function(cache) {
      console.log('[ServiceWorker] Caching app shell');
      return cache.addAll(filesToCache);
    })
  );
});
```

### 缓存内容

```js
ar cacheName = 'weatherPWA-step-6-1';
var filesToCache = [];

self.addEventListener('install', function(e) {
  console.log('[ServiceWorker] Install');
  e.waitUntil(
    caches.open(cacheName).then(function(cache) {
      console.log('[ServiceWorker] Caching app shell');
      return cache.addAll(filesToCache);
    })
  );
});
```

###更新缓存 

```js
self.addEventListener('activate', function(e) {  
  console.log('[ServiceWorker] Activate');  
  e.waitUntil(  
    caches.keys().then(function(keyList) {  
      return Promise.all(keyList.map(function(key) {  
        console.log('[ServiceWorker] Removing old cache', key);  
        if (key !== cacheName) {  
          return caches.delete(key);  
        }  
      }));  
    })  
  );  
});
```

### 从缓存中加载 app shell

self.addEventListener('fetch', function(e) {  
  console.log('[ServiceWorker] Fetch', e.request.url);  
  e.respondWith(  
    caches.match(e.request).then(function(response) {  
      return response || fetch(e.request);  
    })  
  );  
});

###拦截请求

```js
self.addEventListener('fetch', function(e) {  
  console.log('[ServiceWorker] Fetch', e.request.url);  
  var dataUrl = 'https://publicdata-weather.firebaseio.com/';  
  if (e.request.url.indexOf(dataUrl) === 0) {  
    // Put data handler code here  
  } else {  
    e.respondWith(  
      caches.match(e.request).then(function(response) {  
        return response || fetch(e.request);  
      })  
    );  
  }  
});

// Put data handler code here  
e.respondWith(  
  fetch(e.request)  
    .then(function(response) {  
      return caches.open(dataCacheName).then(function(cache) {  
        cache.put(e.request.url, response.clone());  
        console.log('[ServiceWorker] Fetched&Cached Data');  
        return response;  
      });  
    })  
);
```

###从缓存中获取资料

```js
if ('caches' in window) {
  /*
   * Check if the service worker has already cached this city's weather
   * data. If the service worker has the data, then display the cached
   * data while the app fetches the latest data.
   */
  caches.match(url).then(function(response) {
    if (response) {
      response.json().then(function updateFromCache(json) {
        var results = json.query.results;
        results.key = key;
        results.label = label;
        results.created = json.query.created;
        app.updateForecastCard(results);
      });
    }
  });
}
```

### XHR响应比缓存快怎么办？

var cardLastUpdatedElem = card.querySelector('.card-last-updated');
var cardLastUpdated = cardLastUpdatedElem.textContent;
if (cardLastUpdated) {
  cardLastUpdated = new Date(cardLastUpdated);
  // Bail if the card has more recent data then the data
  if (dataLastUpdated.getTime() < cardLastUpdated.getTime()) {
    return;
  }
}

### make offline

https://developers.google.com/web/fundamentals/getting-started/codelabs/offline/

在`index.html`中

```html
<script>
if('serviceWorker' in navigator) {
  navigator.serviceWorker
    .register('/sw.js')
    .then(function() { console.log("Service Worker Registered"); });
}
</script>
```

in `sw.js`

```js
importScripts('/cache-polyfill.js');

self.addEventListener('install', function(e) {
 console.log("shit")
 e.waitUntil(
   caches.open('airhorner').then(function(cache) {
     return cache.addAll([
       '/',
       '/index.html',
       '/index.html?homescreen=1',
       '/?homescreen=1',
       '/styles/main.css',
       '/scripts/main.min.js',
       '/sounds/airhorn.mp3'
     ]);
   })
 );
});

self.addEventListener('fetch', function(event) {
  console.log(event.request.url);
  event.respondWith(
  caches.match(event.request).then(function(response) { 
    return response || fetch(event.request);
  })
  );
});
```