---
layout: post
title:  "Webpack Advanced"
date:   2016-07-05
categories: webpack
---

***CommonsChunkPlugin***

You can specify the `minChunk` option in here as well. This option says, if any module is used X or more times, then take it out and pull it into the common chunk. The name must match with the key in the entry object.

Now the next time you run WebPack, you'll have another outputed chunk that contains jQuery as well as any module that you have used 3 or more times.

```js
var CommonsPlugin = new require("webpack/lib/optimize/CommonsChunkPlugin")

// ...

module.exports = {  
  entry: {
    common: ["jquery"]
  },
  plugins: [
    new CommonsPlugin({
      minChunks: 3,
      name: "common"
    });
  ]
};
```


***require.ensure***

```js
require.ensure(["./below_the_fold"],
  function(require) {
  // Now require it "sync"
  require("./below_the_fold");
  },
  "below_the_fold");
  // 3rd optional argument to name the "chunk" that get's output...
```

load on demand

```js
let map;  
$mapButton.on("click", function() {
  if (!mapLoaded) {
    require.ensure([
      "../map/index"
    ], (require) => {
      if (map) {
        return map.open();
      }

      let MapComponent = require("../map/index");

      map = new MapComponent({
        el: ".map_holder"
      });

      map.open();
    }, "map");
  }
});
```
***require.context***

[https://webpack.github.io/docs/context.html](https://webpack.github.io/docs/context.html)

A context is created if your request contains expressions

`require("./template/" + name + ".jade");`

webpack parses the require statement and extracts some information:

* Directory: `./template`
* Regular expression: `/^.*\.jade$/`

It contains references to all modules in that directory that can be required with a request matching the regular expression. 

```js
{
    "./table.jade": 22,
    "./table-row.jade": 23,
    "./directory/folder.jade": 24
}
```

You can create your own context with the require.context function.

```js
require.context(
  directory, 
  useSubdirectories = false, 
  regExp = /^\.\//
)
```

examples:

```js
require.context("./test", false, /Test$/)
// a context with all files from the test directory that can be
// required with a request endings with "Test"

require.context("..", true, /^grunt-[^\/]+\/tasks/[^\/]+$/)
// all grunt task that are in a modules directory of the parent folder
```

A context module exports a (require) function that takes one argument: the request.

* property `resolve` which is a `function` and returns the module id of the parsed request.
* property `keys` which is a `function` that returns all possible requests that the context module can handle.
* property `id` which is the module id of the context module.

```js
var req = require.context("./templates", true, /^\.\/.*\.jade$/);

var tableTemplate = req("./table.jade");
// tableTemplate === require("./templates/table.jade");

var tableTemplateId = req.resolve("./table.jade");
// tableTemplateId === require.resolve("./templates/table.jade");

req.keys();
// is ["./table.jade", "./table-row.jade", "./directory/folder.jade"]

req.id;
// is i. e. 42
```

```js
function requireAll(requireContext) {
  return requireContext.keys().map(requireContext);
}
// requires and returns all modules that match

var modules = requireAll(require.context("./spec", true, /^\.\/.*\.js$/));
// is an array containing all the matching modules
```

If the module source contains a require that cannot be statically analyzed, the context is the current directory.