---
layout: post
title:  "Set Up project"
date:   2016-10-25
categories: project
---

### Jest 

想要用jest，可是遇到不知道怎么解决的问题

测试文件中引入想要测试的函数

import isEqual from "../../../common/src/libs/util"

而util.js文件中有这样一句，`import zhilinUpload from 'pajk-zhilin/libs/upload';`

然后就报了zhilin相关的错误

```shell
/Users/wu/dev/ping_an/overwatch/node_modules/pajk-zhilin/libs/upload.js:3
export default function(fileSelect, fileElem, success, url) {
^^^^^^
SyntaxError: Unexpected token export
```




测试文件中引入想要测试的函数

```js
import { posto, get, jsonp, call } from "../../../common/src/libs/api"
```

而api.js文件中有这样一句，`import { Message } from 'antd';`引入了antd

然后就报了antd相关的错误

```shell
/Users/wu/dev/ping_an/overwatch/node_modules/antd/lib/style/index.less:1
    ({"Object.<anonymous>":function(module,exports,require,__dirname,__filename,global,jest){@import "./themes/default";
```    

不认识less中的@符号

import 



### yarn

```shell
yarn add babel-jest --dev

yarn remove jest
```

### webpack-dashboard

webpack.js

```json
var DashboardPlugin = require('webpack-dashboard/plugin');

  plugins: [
    new DashboardPlugin(),
  ]
```

```json
"start": "webpack-dashboard -- ./node_modules/webpack/bin/webpack.js -w --progress --e dev",
"start:serv": "webpack-dashboard -- webpack-dev-server --hot --progress",
```