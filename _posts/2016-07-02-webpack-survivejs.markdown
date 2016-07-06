---
layout: post
title:  "Webpack Survivejs"
date:   2016-07-02
categories: webpack
---
***split webpack.config.js file***

不同环境使用不同的webpack配置，大致有如下三种方法

* Maintain configuration in multiple files and point Webpack to each through --config parameter. Share configuration through module imports. You can see this approach in action at webpack/react-starter.
* Push configuration to a library which you then consume. Example: HenrikJoreteg/hjs-webpack.
* Maintain configuration within a single file and branch there. If we trigger a script through npm (i.e., npm run test), npm sets this information in an environment variable. We can match against it and return the configuration we want.

这里使用第三种

```js
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

const merge = require('webpack-merge');


const PATHS = {
  app: path.join(__dirname, 'app'),
  build: path.join(__dirname, 'build')
};


module.exports = {


const common = {

  // Entry accepts a path or an object of entries.
  // We'll be using the latter form given it's
  // convenient with more complex configurations.
  entry: {
    app: PATHS.app
  },
  output: {
    path: PATHS.build,
    filename: '[name].js'
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: 'Webpack demo'
    })
  ]
};


var config;

// Detect how npm is run and branch based on that
switch(process.env.npm_lifecycle_event) {
  case 'build':
    config = merge(common, {});
    break;
  default:
    config = merge(common, {});
}

module.exports = config;
```

***webpack validator***

```js
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const merge = require('webpack-merge');

const validate = require('webpack-validator');


...


module.exports = config;


module.exports = validate(config);
```

***Hot Module Replacement***

```js
const webpack = require('webpack');

exports.devServer = function(options) {
  return {
    devServer: {
      // Enable history API fallback so HTML5 History API based
      // routing works. This is a good default that will come
      // in handy in more complicated setups.
      historyApiFallback: true,

      // Unlike the cli flag, this doesn't set
      // HotModuleReplacementPlugin!
      hot: true,
      inline: true,

      // Display only errors to reduce the amount of output.
      stats: 'errors-only',

      // Parse host and port from env to allow customization.
      //
      // If you use Vagrant or Cloud9, set
      // host: options.host || '0.0.0.0';
      //
      // 0.0.0.0 is available to all network devices
      // unlike default `localhost`.
      host: options.host, // Defaults to `localhost`
      port: options.port // Defaults to 8080
    },
    plugins: [
      // Enable multi-pass compilation for enhanced performance
      // in larger projects. Good default.
      new webpack.HotModuleReplacementPlugin({
        multiStep: true
      })
    ]
  };
}
```

***webpack dev server***

`localhost:8080/webpack-dev-server/` instead of the root. It will provide status information within the browser itself at the top of the application.

The quickest way to enable automatic browser refresh for our project is to run `webpack-dev-server --inline`. --inline, runs the server in so called inline mode that writes the webpack-dev-server client to the resulting code.

Hot Module Replacement builds on top the webpack-dev-server

`webpack-dev-server --inline --hot`

***source map***

Webpack can generate both inline sourcemaps included within bundles or separate sourcemap files. 

The former is useful during development due to better performance while the latter is handy for production usage as it will keep the bundle size small.

```js
switch(process.env.npm_lifecycle_event) {
  case 'build':
    config = merge(
      common,

      {
        devtool: 'source-map'
      },

      parts.setupCSS(PATHS.app)
    );
  default:
    config = merge(
      common,

      {
        devtool: 'eval-source-map'
      },

      parts.setupCSS(PATHS.app),
      ...
    );
}

module.exports = validate(config);
```

`eval-source-map(default)` builds slowly initially, but it provides fast rebuild speed and yields real files. 

Faster development specific options, such as `cheap-module-eval-source-map` and `eval`, produce lower quality sourcemaps. All eval options will emit sourcemaps as a part of your JavaScript code.

***Minifying the Code***

The easiest way to enable minification is to call `webpack -p`

`-p` is a shortcut for `--optimize-minimize`, you can think it as `-p` for "production". 

或者用插件

```js

exports.minify = function() {
  return {
    plugins: [
      new webpack.optimize.UglifyJsPlugin({
        compress: {
          warnings: false
        }
      })
    ]
  };
}
```
***UglifyJs***

有时候需要控制Uglifyjs

An UglifyJS feature known as `mangling` will be enabled by default. The feature will reduce local function and variable names to a minimum, usually to a single character.

Given these transformations can break your code, you have to be a little careful. A good example of this is Angular 1 and its dependency injection system. As it relies on strings, you must be careful not to mangle those or else it will fail to work.

```js
new webpack.optimize.UglifyJsPlugin({
  // Don't beautify output (enable for neater output)
  beautify: false,

  // Eliminate comments
  comments: false,

  // Compression specific options
  compress: {
    warnings: false,

    // Drop `console` statements
    drop_console: true
  },

  // Mangling specific options
  mangle: {
    // Don't mangle $
    except: ['$'],

    // Don't care about IE8
    screw_ie8 : true,

    // Don't mangle function names
    keep_fnames: true
  }
})
```

If you enable mangling, it is a good idea to set except: ['webpackJsonp'] to avoid mangling the Webpack runtime.

***DefinePlugin***

It is able to rewrite matching free variables.

```js
var foo;

// Not free, not ok to replace
if(foo === 'bar') {
  console.log('bar');
}

// Free, ok to replace
if(bar === 'bar') {
  console.log('bar');
}
```

```js
var foo;

// Not free, not ok to replace
if(foo === 'bar') {
  console.log('bar');
}

// Free, ok to replace
if('bar' === 'bar') {
  console.log('bar');
}
```

so UglifyJS gives us

```js
var foo;

// Not free, not ok to replace
if(foo === 'bar') {
  console.log('bar');
}

// Free, ok to replace
if(true) {
  console.log('bar');
}
```

```js
var foo;

// Not free, not ok to replace
if(foo === 'bar') {
  console.log('bar');
}

// Free, ok to replace
console.log('bar');
```

use in webpack

```js

exports.setFreeVariable = function(key, value) {
  const env = {};
  env[key] = JSON.stringify(value);

  return {
    plugins: [
      new webpack.DefinePlugin(env)
    ]
  };
}
```

```js
// Detect how npm is run and branch based on that
switch(process.env.npm_lifecycle_event) {
  case 'build':
    config = merge(
      common,
      {
        devtool: 'source-map'
      },

      parts.setFreeVariable(
        'process.env.NODE_ENV',
        'production'
      ),

      parts.minify(),
      parts.setupCSS(PATHS.style)
    );
    break;
  default:
    ...
}

module.exports = validate(config);
```

***bundle splitting***

[http://survivejs.com/webpack/building-with-webpack/splitting-bundles/#setting-up-a-vendor-bundle](http://survivejs.com/webpack/building-with-webpack/splitting-bundles/#setting-up-a-vendor-bundle)

To improve the situation, we can define a vendor entry containing React. This is done by matching the dependency name. 

```js
const common = {
  // Entry accepts a path or an object of entries.
  // We'll be using the latter form given it's
  // convenient with more complex configurations.
  entry: {

    app: PATHS.app,
    vendor: ['react']

  },
  output: {
    path: PATHS.build,
    filename: '[name].js'
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: 'Webpack demo'
    })
  ]
};
```
这样就得到了两个bundle文件，然后有时候我们会发现，这两个文件加起来的大小大于原本的bundle文件。这是因为这两个文件共同包含了某些模块。

这时候我们可以用`CommonsChunkPlugin`来帮助我们

```js

exports.extractBundle = function(options) {
  const entry = {};
  entry[options.name] = options.entries;

  return {
    // Define an entry point needed for splitting.
    entry: entry,
    plugins: [
      // Extract bundle and manifest files. Manifest is
      // needed for reliable caching.
      new webpack.optimize.CommonsChunkPlugin({
        names: [options.name, 'manifest']
      })
    ]
  };
}

parts.extractBundle({
  name: 'vendor',
  entries: ['react']
}),

```




***placeholder***

- [path] - Returns entry path.
- [name] - Returns entry name.
- [hash] - Returns build hash.
- [chunkhash] - Returns a chunk specific hash.

```js
// Detect how npm is run and branch based on that
switch(process.env.npm_lifecycle_event) {
  case 'build':
    config = merge(
      common,
      {

        devtool: 'source-map',
        output: {
          path: PATHS.build,
          filename: '[name].[chunkhash].js',
          // This is used for require.ensure. The setup
          // will work without but this is useful to set.
          chunkFilename: '[chunkhash].js'
        }

      },
      ...
    );
    break;
  default:
    ...
}

module.exports = validate(config);
```

***separating-css***

We can achieve this using the `ExtractTextPlugin`. 

It comes with overhead during the compilation phase, and it won't work with Hot Module Replacement (HMR) by design. Given we are using it only for production, that won't be a problem.

The plugin operates in two parts.

* There's a loader, `ExtractTextPlugin.extract`, that marks the assets to be extracted. 
* The plugin itself will then use that information to write the file. In a function form the idea looks like this:

```js
const ExtractTextPlugin = require('extract-text-webpack-plugin');

exports.extractCSS = function(paths) {
  return {
    module: {
      loaders: [
        // Extract CSS during build
        {
          test: /\.css$/,
          loader: ExtractTextPlugin.extract('style', 'css'),
          include: paths
        }
      ]
    },
    plugins: [
      // Output extracted CSS to a file
      new ExtractTextPlugin('[name].[chunkhash].css')
    ]
  };
}
```

原来的代码如下，虽然用ExtractTextPlugin提取出了css,浏览器可以缓存，但是 If you try to modify either index.js or main.css, the hash of both files (app.js and app.css) will change! 

```js

require('react');

require('./main.css');

```

所以要在index.js中删除`require('./main.css');`，然后在webpack中load相关css

***Eliminating Unused CSS***

It is important the plugin is used after the `ExtractTextPlugin` as otherwise it won't work!

```js

const PurifyCSSPlugin = require('purifycss-webpack-plugin');

exports.purifyCSS = function(paths) {
  return {
    plugins: [
      new PurifyCSSPlugin({
        basePath: process.cwd(),
        // `paths` is used to point PurifyCSS to files not
        // visible to Webpack. You can pass glob patterns
        // to it.
        paths: paths
      }),
    ]
  }
}
```

***analyzing build statistics***

```js
// package.json
{
  "scripts": {
    "stats": "webpack --profile --json > stats.json",
  },
}
```

```js
//webpack.config.js
module.exports = validate(config, {
  quiet: true
});
```

If you execute npm run stats now, you should find stats.json at your project root after it has finished processing. 

***Profile Tools***

[http://survivejs.com/webpack/building-with-webpack/analyzing-build-statistics/](http://survivejs.com/webpack/building-with-webpack/analyzing-build-statistics/)


*******

### Loading Assets

***load css***

```js
module.exports = {
  ...
  module: {
    loaders: [
      {
        // Match files against RegExp
        test: /\.css$/,

        // Apply loaders against it. These need to
        // be installed separately. In this case our
        // project would need *style-loader* and *css-loader*.
        loaders: ['style', 'css'],

        // Restrict matching to a directory. This also accepts an array of paths.
        // Although optional, I prefer to set this (better performance,
        // clearer configuration).
        include: path.join(__dirname, 'app')
      }
    ]
  }
};
```

#### Modules in javascript

***Commonjs***

```js
var MyModule = require('./MyModule');

// export at module root
module.exports = function() { ... };

// alternatively, export individual functions
exports.hello = function() {...};
```

***ES6***

```js
import MyModule from './MyModule.js';

// export at module root
export default function () { ... };

// or export as module function,
// you can have multiple of these per module
export function hello() {...};
```

***AMD***

```js
define(['./MyModule.js'], function (MyModule) {
  // export at module root
  return function() {};
});

// or
define(['./MyModule.js'], function (MyModule) {
  // export as module function
  return {
    hello: function() {...}
  };
});
```

***UMD***

***loader***

Generally you either use `loader (accepts string)` or `loaders field (accepts array of strings)` and then pass possible query parameters using one of the available methods.

It is good to keep in mind that Webpack's loaders are always evaluated from right to left and from bottom to top (separate definitions). 

```js
{
  test: /\.css$/,
  loaders: ['style', 'css'],
  include: PATHS.app
}
```

等于

```js
{
  test: /\.css$/,
  loaders: ['style'],
  include: PATHS.app
},
{
  test: /\.css$/,
  loaders: ['css'],
  include: PATHS.app
}
```

***Passing Parameters to a Loader***

```js
{
  test: /\.jsx?$/,
  loaders: [
    'babel?cacheDirectory,presets[]=react,presets[]=es2015'
  ],
  include: PATHS.app
}
```

写成如下格式更加清晰


```js
{
  test: /\.jsx?$/,
  loader: 'babel',
  query: {
    cacheDirectory: true,
    presets: ['react', 'es2015']
  },
  include: PATHS.app
}
```

***loading css***

```js
const common = {
  ...
  module: {
    loaders: [
      {
        test: /\.css$/,
        loaders: ['style', 'css'],
        include: PATHS.style
      }
    ]
  },
  ...
};
```

 `css-loader` goes through possible `@import` and `url()` statements within the matched files and treats them as regular `require`. This allows us to rely on various other loaders, such as `file-loader` or `url-loader`.

`style-loader` picks up the output and injects the CSS into the resulting bundle.

***sass***

```js
{
  test: /\.scss$/,
  loaders: ['style', 'css', 'sass'],
  include: PATHS.style
}
```

***Loading Stylus and YETICSS***

```js
const common = {
  ...
  module: {
    loaders: [
      {
        test: /\.styl$/,
        loaders: ['style', 'css', 'stylus'],
        include: PATHS.style
      }
    ]
  },
  // yeticss
  stylus: {
    use: [require('yeticss')]
  }
};
```

```scss
@import 'yeticss'

//or
@import 'yeticss/components/type'
```

***PostCSS***

`PostCSS` allows you to perform transformations over CSS through JavaScript plugins. You can even find plugins that provide you Sass-like features. 

```js
const autoprefixer = require('autoprefixer');
const precss = require('precss');

module.exports = {
  module: {
    loaders: [
      {
        test: /\.css$/,
        loaders: ['style', 'css', 'postcss'],
        include: PATHS.style
      }
    ]
  },
  // PostCSS plugins go here
  postcss: function () {
      return [autoprefixer, precss];
  }
};
```

***load cssnext***

```js
{
  test: /\.css$/,
  loaders: ['style', 'css', 'cssnext'],
  include: PATHS.style
}
```

******

#### Loading images

Webpack allows you to inline assets by using `url-loader`. It will output your images as BASE64 strings within your JavaScript bundles. 

This will decrease the amount of requests needed while growing the bundle size. 

Webpack allows you to control the inlining process and defer loading to `file-loader` that outputs image files and returns paths to them.

***Setting Up url-loader***

Assuming you have configured your styles correctly, Webpack will resolve any `url()` statements your styling might have. You can of course point to the image assets through your JavaScript code as well.

```js
// load .jpg and .png files while inlining files below 25kB
{
  test: /\.(jpg|png)$/,
  loader: 'url?limit=25000',
  include: PATHS.images
}
```

***Setting Up file-loader***

```js
{
  test: /\.(jpg|png)$/,
  // By default file-loader returns the MD5 hash
  // of the file's contents with the original extension
  loader: 'file?name=[path][name].[hash].[ext]',
  include: PATHS.images
}
```

***Loading SVGs***

```js
{
  test: /\.svg$/,
  loader: 'file',
  include: PATHS.images
}
```

The example SVG path below is relative to the CSS file:

```css
.icon {
   background-image: url('../assets/icon.svg');
}
```

If you want the raw SVG content, you can use the `raw-loader` for this purpose. This can be useful if you want to inject the SVG content to directly to JavaScript or HTML markup.

***Compressing Images***

In case you want to compress your images, use `image-webpack-loader` or `svgo-loader` (SVG specific). This type of loader should be applied first to the data so remember to place it as the last within loaders listing.

****

### Loading Fonts

```js
{
  test: /\.woff$/,
  loader: 'url?limit=50000',
  include: PATHS.fonts
}
```

or

```js
{
  // Match woff2 in addition to patterns like .woff?v=1.1.1.
  test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
  loader: 'url',
  query: {
    limit: 50000,
    mimetype: 'application/font-woff',
    name: './fonts/[hash].[ext]'
  }
}
```

***Supporting Multiple Formats***

```js
{
  test: /\.woff$/,
  // Inline small woff files and output them below font/.
  // Set mimetype just in case.
  loader: 'url',
  query: {
    name: 'font/[hash].[ext]',
    limit: 5000,
    mimetype: 'application/font-woff'
  },
  include: PATHS.fonts
},
{
  test: /\.ttf$|\.eot$/,
  loader: 'file',
  query: {
    name: 'font/[hash].[ext]'
  },
  include: PATHS.fonts
}
```


