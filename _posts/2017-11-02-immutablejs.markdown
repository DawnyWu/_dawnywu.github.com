---
layout: post
title:  "immutablejs"
date:   2016-11-02
categories: js
---

### fromJS 和 Map() 的区别

`fromJS()` 会把所有层的内容都变为immutable

`Map()`只一层

```js
var a = {address: {postcode: 5085}}
var d = Immutable.Map(a);
```

这里用`Map()`, `d.get('address')` is immutable, 想要改变`key address`的值，只能使用Immutable.Map.set()方法。

可是key postcode是可以改变的，`d.get('address').postcode=6000`就可以改变了

```js
var a = {address: {postcode: 5085}}
var b = Immutable.fromJS(a);
b.get('address').postcode=6000;
console.log(JSON.stringify(b));   //Outputs {"address":{"postcode":5085}}
```

而用`fromJS`创建的`obj`是改不了的

### 



var grid = {
    editable: false,
    widgets: [{
        name: 'Some widget',
        type: 'List',
        defaultDataSource: 'daily',
        dataSources: {}
    }, {
        name: 'Some widget1',
        type: 'List',
        defaultDataSource: 'daily',
        dataSources: {}
    }]
};

var state = Immutable.fromJS(grid);

var newState = state.updateIn(['widgets'], function (list) {
    return list.push(Immutable.Map({
        name: 'Some widget2',
        type: 'List',
        defaultDataSource: 'daily',
        dataSources: {}
    }));
});

var newState1 = state.updateIn(['widgets'], function (list) {
    return list.push(Immutable.Map({
        name: 'Some widget2',
        type: 'List',
        defaultDataSource: 'daily',
        dataSources: {}
    }));
});

console.log(state.toJS(), newState.toJS(), newState1.toJS());

console.log(newState.equals(newState1)); //false



### 普通obj可以嵌套immutable obj么？




