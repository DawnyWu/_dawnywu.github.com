---
layout: post
title:  "js-metaprogramming"
date:   2016-11-08
categories: js
---

### reflective metaprogramming

Introspection: you have read-only access to the structure of a program.
Self-modification: you can change that structure.
Intercession: you can redefine the semantics of some language operations.

如果不截断target的方法的话，对proxy的操作会直接作 用于target

***target可以是object***

```js
const target = {};
const handler = {
    /** Intercepts: getting properties */
    get(target, propKey, receiver) {
        console.log(`GET ${propKey}`);
        return 123;
    },

    /** Intercepts: checking whether properties exist */
    has(target, propKey) {
        console.log(`HAS ${propKey}`);
        return true;
    }
};
const proxy = new Proxy(target, handler);
```

proxy截断has, get方法，但不会截断set

```js
proxy.bar = 'abc';
target.bar
// 'abc'
```

***target可以是function***

apply: Making a function call, triggered via
  proxy(···)
  proxy.call(···)
  proxy.apply(···)
construct: Making a constructor call, triggered via
  new proxy(···)

***Intercepting method calls***

method calls are viewed as two separate operations: First a get to retrieve a function, then an apply to call that function.

一个截断方法的例子，实际上截断的是get方法。。。

```js
const obj = {
    multiply(x, y) {
        return x * y;
    },
    squared(x) {
        return this.multiply(x, x);
    },
};

// traceMethodCalls(obj)会创建一个obj的proxy
function traceMethodCalls(obj) {
    const handler = {
        get(target, propKey, receiver) {
            // 获得target method
            const origMethod = target[propKey];
            return function (...args) {
                const result = origMethod.apply(this, args);
                console.log(propKey + JSON.stringify(args)
                    + ' -> ' + JSON.stringify(result));
                return result;
            };
        }
    };
    return new Proxy(obj, handler);
}

const tracedObj = traceMethodCalls(obj);

tracedObj.multiply(2,7)
// multiply[2,7] -> 14
// 14
tracedObj.squared(9)
// multiply[9,9] -> 81
// squared[9] -> 81
// 81
```

***Revocable proxies***

const {proxy, revoke} = Proxy.revocable(target, handler);

After you call the function revoke for the first time, any operation you apply to proxy causes a TypeError. Subsequent calls of revoke have no further effect.

```js
const target = {}; // Start with an empty object
const handler = {}; // Don’t intercept anything
const {proxy, revoke} = Proxy.revocable(target, handler);

proxy.foo = 123;
console.log(proxy.foo); // 123

revoke();

console.log(proxy.foo); // TypeError: Revoked
```

***Proxies as prototypes***

```js
const proto = new Proxy({}, {
    get(target, propertyKey, receiver) {
        console.log('GET '+propertyKey);
        return target[propertyKey];
    }
});

const obj = Object.create(proto);
obj.bla;

// Output:
// GET bla
```

***Forwarding intercepted operations***

感觉相当于middleware，在中间做一些事情，最后还是操作target

```js
const handler = {
    deleteProperty(target, propKey) {
        console.log('DELETE ' + propKey);
        return delete target[propKey];
        // return Reflect.deleteProperty(target, propKey);
    },
    has(target, propKey) {
        console.log('HAS ' + propKey);
        return propKey in target;
        // return Reflect.has(target, propKey);
    },
    // Other traps: similar
}
```

```js
const handler = new Proxy({}, {
    get(target, trapName, receiver) {
        // Return the handler method named trapName
        return function (...args) {
            // Don’t log args[0]
            console.log(trapName.toUpperCase()+' '+args.slice(1));
            // Forward the operation
            return Reflect[trapName](...args);
        }
    }
});
```

***Tracing property accesses (get, set)***

我们要写一个方法tracePropAccess(),logs whenever a property of obj, whose key is in the Array propKeys, is set or got.

```js
class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
    toString() {
        return `Point(${this.x}, ${this.y})`;
    }
}
// Trace accesses to properties `x` and `y`
const p = new Point(5, 7);
p = tracePropAccess(p, ['x', 'y']);

p.x
// GET x
// 5
p.x = 21
// SET x=21
// 21
```

```js
function tracePropAccess(obj, propKeys) {
    // Store the property data here
    const propData = Object.create(null);
    // Replace each property with a getter and a setter
    propKeys.forEach(function (propKey) {
        propData[propKey] = obj[propKey];
        Object.defineProperty(obj, propKey, {
            get: function () {
                console.log('GET '+propKey);
                return propData[propKey];
            },
            set: function (value) {
                console.log('SET '+propKey+'='+value);
                propData[propKey] = value;
            },
        });
    });
    return obj;
}
```

```js
function tracePropAccess(obj, propKeys) {
    const propKeySet = new Set(propKeys);
    return new Proxy(obj, {
        get(target, propKey, receiver) {
            if (propKeySet.has(propKey)) {
                console.log('GET '+propKey);
            }
            return Reflect.get(target, propKey, receiver);
        },
        set(target, propKey, value, receiver) {
            if (propKeySet.has(propKey)) {
                console.log('SET '+propKey+'='+value);
            }
            return Reflect.set(target, propKey, value, receiver);
        },
    });
}
```

***Warning about unknown properties (get, set)***

We make the proxy a prototype of an object.

If a property isn’t found in the object, the get trap of the proxy is triggered. If the property doesn’t even exist in the prototype chain after the proxy, it really is missing and we throw an exception

```js
const PropertyChecker = new Proxy({}, {
    get(target, propKey, receiver) {
        if (!(propKey in target)) {
            throw new ReferenceError('Unknown property: '+propKey);
        }
        return Reflect.get(target, propKey, receiver);
    }
});


const obj = { __proto__: PropertyChecker, foo: 123 };
obj.foo  // own
// 123
obj.fo
// ReferenceError: Unknown property: fo
obj.toString()  // inherited
// '[object Object]'
```

#### in检测obj中有没有相应的key, 但是不是会去__proto__中去找呢？

会的

感觉好像如果要做middleware，直接proxy object

如果想要在obj之后做兜底，要做obj的proto

```js
function PropertyChecker() { }
PropertyChecker.prototype = new Proxy(···);

class Point extends PropertyChecker {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

const p = new Point(5, 7);
console.log(p.x); // 5
console.log(p.z); // ReferenceError
```

#### 想要避免obj生成属性，有两种办法

* You can either wrap a proxy around objects that traps set
*  via Object.preventExtensions(obj)

****Negative Array indices (get)****

JavaScript 不支持这种

```js
let a =[1, 2, 3]
a[-1] //=> undefined
```

```js
function createArray(...elements) {
    const handler = {
        get(target, propKey, receiver) {
            // Sloppy way of checking for negative indices
            const index = Number(propKey);
            if (index < 0) {
                propKey = String(target.length + index);
            }
            return Reflect.get(target, propKey, receiver);
        }
    };
    // Wrap a proxy around an Array
    const target = [];
    target.push(...elements);
    return new Proxy(target, handler);
}
const arr = createArray('a', 'b', 'c');
console.log(arr[-1]); // c
```

***check type safe***

```js
function createTypeSafeObject(object) {
    return new Proxy(object, {
          set: function(target, property, value) {
              var currentType = typeof target[property],
                  newType = typeof value;

              if (property in target && currentType !== newType) {
                  throw new Error("Property " + property + " must be a " + currentType + ".");
              } else {
                  target[property] = value;
              }
          }
    });
}
```

写到constructor中

```js
function Person(name) {
    this.name = name;
    return createTypeSafeObject(this);
}

var person = new Person("Nicholas");

console.log(person instanceof Person);    // true
console.log(person.name);                 // "Nicholas"
```

#### Data binding (set)

用proxy在set之前添加callback

```js
function createObservedArray(callback) {
    const array = [];
    return new Proxy(array, {
        set(target, propertyKey, value, receiver) {
            callback(propertyKey, value);
            return Reflect.set(target, propertyKey, value, receiver);
        }
    });    
}
const observedArray = createObservedArray(
    (key, value) => console.log(`${key}=${value}`));

observedArray.push('a');

// "0=a"
// "length=1"
```

#### Accessing a restful web service (method calls)

返回一个proxy, 可以执行任何方法，因为截断了get方法

```js
function createWebService(baseUrl) {
    return new Proxy({}, {
        get(target, propKey, receiver) {
            // Return the method to be called
            return () => httpGet(baseUrl+'/'+propKey);
        }
    });
}
```

#### Revocable references

```js
const resource = { x: 11, y: 8 };
const {reference, revoke} = createRevocableReference(resource);

// Access granted
console.log(reference.x); // 11

revoke();

// Access denied
console.log(reference.x); // TypeError: Revoked
```

```js
function createRevocableReference(target) {
    let enabled = true;
    return {
        reference: new Proxy(target, {
            get(target, propKey, receiver) {
                if (!enabled) {
                    throw new TypeError('Revoked');
                }
                return Reflect.get(target, propKey, receiver);
            },
            has(target, propKey) {
                if (!enabled) {
                    throw new TypeError('Revoked');
                }
                return Reflect.has(target, propKey);
            },
            ···
        }),
        revoke() {
            enabled = false;
        },
    };
}
```

```js
function createRevocableReference(target) {
    const handler = {}; // forward everything
    const { proxy, revoke } = Proxy.revocable(target, handler);
    return { reference: proxy, revoke };
}
```

###  The design of the proxy API

`obj.hasOwnProperty(propKey)`一旦`obj`或`obj`的`proto`覆盖掉了`hasOwnProperty`方法会产生问题

正确的使用方法是`Object.prototype.hasOwnProperty.call(obj, propKey)
`
###  Transparent virtualization and handler encapsulation

Proxies are shielded in two ways:

* It is impossible to determine whether an object is a proxy or not (transparent virtualization).

* You can’t access a handler via its proxy (handler encapsulation).

查看一个obj是否是proxy

```js
// lib.js
const proxies = new WeakSet();

export function createProxy(obj) {
    const handler = {};
    const proxy = new Proxy(obj, handler);
    proxies.add(proxy);
    return proxy;
}

export function isProxy(obj) {
    return proxies.has(obj);
}
```

#### The meta object protocol and proxy traps

the meta object protocol (MOP)

The JavaScript MOP consists of own internal methods that all objects have. 

??? 一堆dynamic dispatch 没看懂

### protect object

There are two ways of protecting objects:

* Non-extensibility protects objects
* Non-configurability protects properties (or rather, their attributes)

Non-extensibility. If an object is non-extensible, you `can’t add properties` and you `can’t change its prototype`:

```js
const obj = Object.preventExtensions({});
console.log(Object.isExtensible(obj)); // false
obj.foo = 123; // TypeError: object is not extensible
Object.setPrototypeOf(obj, null); // TypeError: object is not extensible
```

Non-configurability

```js
const obj = {};
Object.defineProperty(obj, 'foo', {
    value: 123,
    writable: false,
    configurable: false
});
console.log(obj.foo); // 123
obj.foo = 'a'; // TypeError: Cannot assign to read only property

Object.defineProperty(obj, 'foo', {
    configurable: true
}); // TypeError: Cannot redefine property
```

他们的两个特点

* Universal: they work for all objects.
* Monotonic: once switched on, they can’t be switched off again.


