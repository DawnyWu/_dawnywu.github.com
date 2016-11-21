---
layout: post
title:  "generator"
date:   2016-11-10
categories: js
---

### what generator

Generators are functions that can be paused and resumed 

```js
function* genFunc() {
    // (A)
    console.log('First');
    yield;
    console.log('Second');
}
```

const g = genFunc(); 返回一个`generator object`,可以用来控制流程

但是并没有开始执行，g.next()运行到第一个yield

```js
const g = genFunc()
console.log('11111111111' + JSON.stringify(g.next()))
console.log('22222222222' + JSON.stringfy(g.next()))

// "First"
// "1111111111111111.:{\"done\":false}"
// "Second"
// JSON.stringfy is not a function
```

### 创建generator的几种方式

Generator function declarations:

```js
 function* genFunc() { ··· }
 const genObj = genFunc();
```

Generator function expressions:

```js
 const genFunc = function* () { ··· };
 const genObj = genFunc();
```

Generator method definitions in object literals:

```js
 const obj = {
     * generatorMethod() {
         ···
     }
 };
 const genObj = obj.generatorMethod();
 ```

Generator method definitions in class definitions (class declarations or class expressions):

```js
 class MyClass {
     * generatorMethod() {
         ···
     }
 }
 const myInst = new MyClass();
 const genObj = myInst.generatorMethod();
```

### use case: implementing iterables

```js
function* objectEntries(obj) {
    const propKeys = Reflect.ownKeys(obj);

    for (const propKey of propKeys) {
        // `yield` returns a value and then pauses
        // the generator. Later, execution continues
        // where it was previously paused.
        yield [propKey, obj[propKey]];
    }
}
```

generator返回的对象是iterable的，所以可以用for...of

```js
const jane = { first: 'Jane', last: 'Doe' };
for (const [key,value] of objectEntries(jane)) {
    console.log(`${key}: ${value}`);
}
```

### use case: simpler asynchronous code

```js
function fetchJson(url) {
    return fetch(url)
    .then(request => request.text())
    .then(text => {
        return JSON.parse(text);
    })
    .catch(error => {
        console.log(`ERROR: ${error.stack}`);
    });
}
```

```js
const fetchJson = co.wrap(function* (url) {
    try {
        let request = yield fetch(url);
        let text = yield request.text();
        return JSON.parse(text);
    }
    catch (error) {
        console.log(`ERROR: ${error.stack}`);
    }
});
```

????
用高端的 async 写起来是这样的

```js
async function fetchJson(url) {
    try {
        let request = await fetch(url);
        let text = await request.text();
        return JSON.parse(text);
    }
    catch (error) {
        console.log(`ERROR: ${error.stack}`);
    }
}
```

上边两个用起来都是这样的

```js
fetchJson('http://example.com/some_file.json')
.then(obj => console.log(obj));
```


```js
fetchJson('http://example.com/some_file.json')
.then(obj => console.log(obj));
```

### generator可以作为三种角色

1. Iterator

这个道理很简单，generator objects implementing the interface Iterable，for...of和`...`可以操作generator object


2. Observer(data consumer)

yield的时候可以从next方法接收参数的，

```js


#### 1. Generators as iterators 

用generator实现

```js
function* objectEntries(obj) {
    // In ES6, you can use strings or symbols as property keys,
    // Reflect.ownKeys() retrieves both
    const propKeys = Reflect.ownKeys(obj);

    for (const propKey of propKeys) {
        yield [propKey, obj[propKey]];
    }
}
```

```js
const jane = { first: 'Jane', last: 'Doe' };
for (const [key,value] of objectEntries(jane)) {
    console.log(`${key}: ${value}`);
}
// Output:
// first: Jane
// last: Doe
```

如果不使用generator的话 

```js
function objectEntries(obj) {
    let index = 0;
    let propKeys = Reflect.ownKeys(obj);

    return {
        [Symbol.iterator]() {
            return this;
        },
        next() {
            if (index < propKeys.length) {
                let key = propKeys[index];
                index++;
                return { value: [key, obj[key]] };
            } else {
                return { done: true };
            }
        }
    };
}
```

#### 2. Generators as observers(data consumption) 

```js
function* dataConsumer() {
    console.log('Started');
    console.log(`1. ${yield}`); // (A)
    console.log(`2. ${yield}`);
    return 'result';
}
```

```js
// 获得generator obj
const g = dataConsumer()

// 第一次调next
g.next() 
// => Started
// => { value: undefined, done: false }

// 第二次调next
g.next('a')
// => 1. a
// => { value: undefined, done: false }

// 第三次调next
g.next('b')
// => 2. b
// => { value: 'result', done: true }
// 注意value有值了，是return回来的 value
```

 It always sends a value to the currently suspended `yield`, but returns the operand of the following `yield`.

 ***第一个next()很特殊，很讨厌的地方***

 When using a generator as an observer, it is important to note that the only purpose of the first invocation of `next()` is to start the observer. 

 It is only ready for input afterwards, because this first invocation advances execution to the first yield. Therefore, any input you send via the first next() is ignored

 ```js
 function* gen() {
    // (A)
    while (true) {
        const input = yield; // (B)
        console.log(input);
    }
}
const obj = gen();
obj.next('a');
obj.next('b');

// Output:
// b
// 第一次穿入a是没有yield接收的
```

总结一下就是: 使用next的时候，传入传出的时机不一样，A传入值，B传出值


#### generator obj的另外两个方法

return(x) executes return x at the location of yield.

throw(x) executes throw x at the location of yield.

### 3. coroutine

```js
function* tokenize(chars) {
    const iterator = chars[Symbol.iterator]();
    let ch;
    do {
        ch = getNextItem(iterator); // (A)
        if (isWordChar(ch)) {
            let word = '';
            do {
                word += ch;
                ch = getNextItem(iterator); // (B)
            } while (isWordChar(ch));
            yield word; // (C)
        }
        // Ignore all other characters
    } while (ch !== END_OF_SEQUENCE);
}

const END_OF_SEQUENCE = Symbol();

function getNextItem(iterator) {
    const {value,done} = iterator.next();
    return done ? END_OF_SEQUENCE : value;
}

function isWordChar(ch) {
    return typeof ch === 'string' && /^[A-Za-z0-9]$/.test(ch);
}
```

### examples

***process a stream of characters***

addNumbers(extractNumbers(tokenize(CHARS)))

```js
/**
 * Returns an iterable that transforms the input sequence
 * of characters into an output sequence of words.
 */
function* tokenize(chars) {
    const iterator = chars[Symbol.iterator]();
    let ch;
    do {
        ch = getNextItem(iterator); // (A)
        if (isWordChar(ch)) {
            let word = '';
            do {
                word += ch;
                ch = getNextItem(iterator); // (B)
            } while (isWordChar(ch));
            yield word; // (C)
        }
        // Ignore all other characters
    } while (ch !== END_OF_SEQUENCE);
}

const END_OF_SEQUENCE = Symbol();

function getNextItem(iterator) {
    const {value,done} = iterator.next();
    return done ? END_OF_SEQUENCE : value;
}

function isWordChar(ch) {
    return typeof ch === 'string' && /^[A-Za-z0-9]$/.test(ch);
}
```

***counter***

```html
<body>
    Counter: <span id="counter"></span>
</body>
```

```js
function countUp(start = 0) {
    const counterSpan = document.querySelector('#counter');
    while (true) {
        counterSpan.textContent = String(start);
        start++;
    }
}
```

上边这种while(true)会让浏览器卡主

我们用generator改写

```js
function* countUp(start = 0) {
    const counterSpan = document.querySelector('#counter');
    while (true) {
        counterSpan.textContent = String(start);
        start++;
        yield; // pause
    }
}
```

我们可以再改进一下

```js
function* countUp(start = 0) {
    while (true) {
        start++;
        yield* displayCounter(start);
    }
}

function* displayCounter(counter) {
    const counterSpan = document.querySelector('#counter');
    counterSpan.textContent = String(counter);
    yield; // pause
}
```

```js
function run(generatorObject) {
    if (!generatorObject.next().done) {
        // Add a new task to the event queue
        setTimeout(function () {
            run(generatorObject);
        }, 1000);
    }
}
```

### coroutine

因为generator作为observer第一次next()的时候输入值无效，所以在函数中我们先运行一次next()...

```js
/**
 * Returns a function that, when called,
 * returns a generator object that is immediately
 * ready for input via `next()`
 */
function coroutine(generatorFunction) {
    return function (...args) {
        const generatorObject = generatorFunction(...args);
        generatorObject.next();
        return generatorObject;
    };
}
```

这样每一次输入都会有对应的输出了

```js
const wrapped = coroutine(function* () {
    console.log(`First input: ${yield}`);
    return 'DONE';
});
const normal = function* () {
    console.log(`First input: ${yield}`);
    return 'DONE';
};
```

```shell
> wrapped().next('hello!')
First input: hello!
```

```shell
> const genObj = normal();
> genObj.next()
{ value: undefined, done: false }
> genObj.next('hello!')
First input: hello!
{ value: 'DONE', done: true }
```

###  yield*: the full story

`yield*` performs (the equivalent of) a function call from one generator (the caller) to another generator (the callee).

`yield` it propagates yielded values from the callee to the caller.

```js
function* callee() {
    console.log('callee: ' + (yield));
}

function* caller() {
    while (true) {
        yield* callee();
    }
}
```

```shell
> const callerObj = caller();

> callerObj.next() // start
{ value: undefined, done: false }

> callerObj.next('a')
callee: a
{ value: undefined, done: false }

> callerObj.next('b')
callee: b
{ value: undefined, done: false }
```

yield第一次next(),输入什么都没有用，输出value只是看yield后边有什么，有什么输出什么

yield所在的语句不会运行

#### Example: processing asynchronously pushed data

* Each member of the chain of generators (except the last one) has a parameter `target`. It receives data via yield and sends data via `target.next()`.

* The last member of the chain of generators has no parameter target and only receives data.

* The whole chain is prefixed by a non-generator function that makes an asynchronous request and pushes the results into the chain of generators via `next()`.

读文件的例子，3个`generator`,splitLines, numberLines and printLines, Data is pushed into the chain via the non-generator function `readFile`.

`readFile(fileName, splitLines(numberLines(printLines())));`

```js
import {createReadStream} from 'fs';

/**
 * Creates an asynchronous ReadStream for the file whose name
 * is `fileName` and feeds it to the generator object `target`.
 *
 * @see ReadStream https://nodejs.org/api/fs.html#fs_class_fs_readstream
 */
function readFile(fileName, target) {
    const readStream = createReadStream(fileName,
        { encoding: 'utf8', bufferSize: 1024 });
    readStream.on('data', buffer => {
        const str = buffer.toString('utf8');
        target.next(str);
    });
    readStream.on('end', () => {
        // Signal end of output sequence
        target.return();
    });
}
```

```js
/**
 * Turns a sequence of text chunks into a sequence of lines
 * (where lines are separated by newlines)
 */
const splitLines = coroutine(function* (target) {
    let previous = '';
    try {
        while (true) {
            previous += yield;
            let eolIndex;
            while ((eolIndex = previous.indexOf('\n')) >= 0) {
                const line = previous.slice(0, eolIndex);
                target.next(line);
                previous = previous.slice(eolIndex+1);
            }
        }
    } finally {
        // Handle the end of the input sequence
        // (signaled via `return()`)
        if (previous.length > 0) {
            target.next(previous);
        }
        // Signal end of output sequence
        target.return();
    }
});
```

```js
//**
 * Prefixes numbers to a sequence of lines
 */
const numberLines = coroutine(function* (target) {
    try {
        for (const lineNo = 0; ; lineNo++) {
            const line = yield;
            target.next(`${lineNo}: ${line}`);
        }
    } finally {
        // Signal end of output sequence
        target.return();
    }
});
```

```js
const printLines = coroutine(function* () {
    while (true) {
        const line = yield;
        console.log(line);
    }
});
```

###  Lazy pull (generators as iterators)

addNumbers(extractNumbers(tokenize(CHARS)))

Each of the chain members pulls data from a source and yields a sequence of items. Processing starts with tokenize whose source is the string CHARS.

```js
/**
 * Returns an iterable that transforms the input sequence
 * of characters into an output sequence of words.
 */
function* tokenize(chars) {
    const iterator = chars[Symbol.iterator]();
    let ch;
    do {
        ch = getNextItem(iterator); // (A)
        if (isWordChar(ch)) {
            let word = '';
            do {
                word += ch;
                ch = getNextItem(iterator); // (B)
            } while (isWordChar(ch));
            yield word; // (C)
        }
        // Ignore all other characters
    } while (ch !== END_OF_SEQUENCE);
}
const END_OF_SEQUENCE = Symbol();
function getNextItem(iterator) {
    const {value,done} = iterator.next();
    return done ? END_OF_SEQUENCE : value;
}
function isWordChar(ch) {
    return typeof ch === 'string' && /^[A-Za-z0-9]$/.test(ch);
}
```

他是lazy的，以为运行它 tokenize()[Symbol.iterator]()返回iterator, .next()返回一个结果，然后暂停

想一口气查看所有结果可以for of 或者 ...

```js
function* extractNumbers(words) {
    for (const word of words) {
        if (/^[0-9]+$/.test(word)) {
            yield Number(word);
        }
    }
}
```

```js
function* addNumbers(numbers) {
    let result = 0;
    for (const n of numbers) {
        result += n;
        yield result;
    }
}
```

```js
const CHARS = '2 apples and 5 oranges.';
const CHAIN = addNumbers(extractNumbers(tokenize(CHARS)));
console.log([...CHAIN]);
```

### Lazy push (generators as observables)

一开始要把数据push进generator里

```js
/**
 * Pushes the items of `iterable` into `sink`, a generator.
 * It uses the generator method `next()` to do so.
 */
function send(iterable, sink) {
    for (const x of iterable) {
        sink.next(x);
    }
    sink.return(); // signal end of stream
}
```
写个log函数试一下

```js
const logItems = coroutine(function* () {
    try {
        while (true) {
            const item = yield; // receive item via `next()`
            console.log(item);
        }
    } finally {
        console.log('DONE');
    }
});
```

下边我们要写push版的tokenize,上边pull版的数据感觉像是现成的，想要就要。。。可是push版的数据来自next(data)传进来,传给下一个也要sink.next(data)

把char攒够成一个word才push给下一个

```js
const tokenize = coroutine(function* (sink) {
    try {
        while (true) { // (A)
            let ch = yield; // (B)
            if (isWordChar(ch)) {
                // A word has started
                let word = '';
                try {
                    do {
                        word += ch;
                        ch = yield; // (C)
                    } while (isWordChar(ch));
                } finally {
                    // The word is finished.
                    // We get here if
                    // - the loop terminates normally
                    // - the loop is terminated via `return()` in line C
                    sink.next(word); // (D)
                }
            }
            // Ignore all other characters
        }
    } finally {
        // We only get here if the infinite loop is terminated
        // via `return()` (in line B or C).
        // Forward `return()` to `sink` so that it is also
        // aware of the end of stream.
        sink.return();
    }
});

function isWordChar(ch) {
    return /^[A-Za-z0-9]$/.test(ch);
}
```

```js
/**
 * Receives a sequence of strings (via the generator object
 * method `next()`) and pushes only those strings to the generator
 * `sink` that are “numbers” (consist only of decimal digits).
 */
const extractNumbers = coroutine(function* (sink) {
    try {
        while (true) {
            const word = yield;
            if (/^[0-9]+$/.test(word)) {
                sink.next(Number(word));
            }
        }
    } finally {
        // Only reached via `return()`, forward.
        sink.return();
    }
});
```

```js
/**
 * Receives a sequence of numbers (via the generator object
 * method `next()`). For each number, it pushes the total sum
 * so far to the generator `sink`.
 */
const addNumbers = coroutine(function* (sink) {
    let sum = 0;
    try {
        while (true) {
            sum += yield;
            sink.next(sum);
        }
    } finally {
        // We received an end-of-stream
        sink.return(); // signal end of stream
    }
});
```

```shell
> send([5, -2, 12], addNumbers(logItems()));
5
3
15
DONE
```




