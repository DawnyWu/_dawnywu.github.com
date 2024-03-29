---
layout: post
title:  "Webpack Egghead"
date:   2016-07-02
categories: webpack
---

***webpack context 属性***


***webapck validator***

validate webpack文件在不同环境下是否有问题

```json
"validate": "npm-run-all --parallel validate-webpack:* lint test --serial check-coverage",
"validate-webpack:dev": "webpack-validator webpack.config.js --env.dev",
"validate-webpack:prod": "webpack-validator webpack.config.js --env.prod",
```

***webpack tree shaking***

删除babel-preset-es2015，使用babel-preset-es2015-webpack，会使得在最终uglify的时候，不bundle没有用到的函数，从而减小bundle文件的大小

比如一个文件export多个函数，外部没有用到的函数最终不会bundle进去

```json
"presets": ["es2015-webpack", "stage-2"],
```
***webpack code splitting***

动态加载js文件

```
System.import('./facts/XXX').then()
```
***webpack chunkhash cache***

不再是bundle.js,而是bundle.8903890243890.js文件

html中也不能再单纯指向bundle.js,而是应当指向生成的bundle文件，故用HtmlWebpackPlugin帮忙

```js
output: {
  filename: 'bundle.[chunkhash].js',
  path: resolve(__dirname, 'dist'),
  pathinfo: !env.prod,
},

new HtmlWebpackPlugin({
  template: './index.html'
})
```
***CommonsChunkPlugin***

有的时候项目中有一些常用的，很少改动的依赖，我们可以用这个插件把他bundle成单独的bundle file,这样可以和其他代码分开加载，并且可以充分利用浏览器缓存

```js
entry: {
  app: './js/app.js',
  vendor: ['lodash', 'jquery'],
},
// 会生成`bundle.app.js`和`bundle.vendor.js`
output: {
  filename: 'bundle.[name].js',
  path: resolve(__dirname, 'dist'),
  pathinfo: true,
},

// cache vender bundle file
plugins: [
  new webpack.optimize.CommonsChunkPlugin({
    name: 'vendor',
  }),
]

```

```html
<script src="/bundle.vendor.js"></script>
<script src="/bundle.app.js"></script>
```

有时候两个项目用到了相同的module,我们想要把module单独打包。

***import a non-ES6 module***

`npm i -D exports-loader`

```js
// ?（要引入的变量名）
import leftPad from 'exports?leftPad!./non_node_modules/leftPad'
```
原理是在文件最后加了一句
`module.exports = leftPad`

leftPad文件将leftPad变量定义在了window上,我们可以用imports-loader去掉它

`npm i -D imports-loader`

```js
import leftPad from 'imports?window=>{}exports?leftPad!./non_node_modules/leftPad'
```

原理是在文件开头加了一句
`var window = {}`

每次加载的时候写这么一大串太麻烦，我们把它写到webpack.config里

```js
{
  test: require.resolve('./src/js/non_node_modules/left-pad'),
  // loader: 'imports?window=>{}exports?leftPad',
  loaders: [
    'imports?window=>{}',
    'exports?leftPad',
  ],
}
```


