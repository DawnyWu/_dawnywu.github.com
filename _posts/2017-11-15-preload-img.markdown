---
layout: post
title:  "preload img"
date:   2016-11-15
categories: js
---

```js
<script type="text/javascript">
var my_image2 = new Image();

// notify the user that the image has been preloaded, and reveal the
// button to use the preloaded image
function notify()
{
    document.getElementById('preloadbutton2').style.display = 'none';

    document.getElementById('after_preload').style.display = 'block';
}

function preload()
{
    my_image2.onload = notify;

    my_image2.src = 'bigsaturn.jpg';
}

// using only the file name, we can take advantage of the preloaded image
function use_preloaded_image()
{
    document.getElementById('saturnplaceholder').src = 'bigsaturn.jpg';
}
</script>
<input type="button" 
    id="preloadbutton2" 
    value="Preload Image" 
    onclick="preload();this.value='Loading. Please wait...'" />

<div id="after_preload" style="display: none">
  <input type="button" value="Use Preloaded Image"
  onclick="use_preloaded_image()" /><br />

  <img src="blank.jpg" id="saturnplaceholder" width="500" />
</div>
```