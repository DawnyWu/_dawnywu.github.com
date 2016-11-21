---
layout: post
title:  "Iterable"
date:   2016-11-08
categories: js
---


### what iterable object

Any object that contains a `[Symbol.iterator]` method is an `iterable object`.


### for of

```js
let iterable = [1, 2, 3];
for (let item of iterable) {
    console.log(item); // 1, 2, 3
}

let iterable2 = new Set([4, 5, 6]);
for (let item of iterable2) {
    console.log(item); // 4, 5, 6
}

let iterable3 = '789';
for (let item of iterable3) {
    console.log(item); // '7', '8', '9'
}
```

### iterator

```js
let iterable = ['a', 'b', 'c'];

iterable[Symbol.iterator] // => function values() { [native code] }

iterator = iterable[Symbol.iterator]()  // => ArrayIterator

iterator.next() // => Object {value: "a", done: false}

// for-of abstracts away iterator instantiation and consumption:
for (let item of iterable) {
    console.log(item); // 'a', 'b', 'c'
}

// A crude illustration of for-of's inner workings:
for (let _iterator = iterable[Symbol.iterator](), _result, item; 
         _result = _iterator.next(), 
         item = _result.value, 
         !_result.done;) {
    console.log(item); // 'a', 'b', 'c'
}

```



### 自己创建Symbol.iterator

```js
let iterable = {
    0: 'a',
    1: 'b',
    2: 'c',
    length: 3,
    [Symbol.iterator]() {
        let index = 0;
        return {
            next: () => {
                let value = this[index];
                let done = index >= this.length;
                index++;
                return { value, done };
            }
        };
    }
};
for (let item of iterable) {
    console.log(item); // 'a', 'b', 'c'
}
```

`for-of`的内在流程

 The `for-of` will call the `[Symbol.iterator]` method on the iterable passed in and iterate over the returned iterator, by calling its next method on every iteration and passing the result's `value` property to the loop body until the next method's result's `done` property is true.