---
layout: post
title:  "java wrapper class"
date:   2016-12-12
categories: java
---

### The Wrapper Constructors

All of the wrapper classes except Character provide two constructors: 

* one that takes a primitive of the type being constructed
* one that takes a String representation of the type being constructed—for example

```java
Integer i1 = new Integer(42);
Integer i2 = new Integer("42");

Float f1 = new Float(3.14f);
Float f2 = new Float("3.14f");

// character只有这一种
Character c1 = new Character('c');
```

### The valueOf() Methods

Integer i2 = Integer.valueOf("101011", 2);

### boxing and unboxing


```java
// pre–Java 5 days
Integer y = new Integer(567);
int x = y.intValue();
x++;
y = new Integer(x);
System.out.println("y = " + y);  

// java5 autoboxing
Integer y = new Integer(567);    // make it
y++;                             // unwrap it, increment it,
                                 // rewrap it
System.out.println("y = " + y);  // print it
```

这里用了`y++`,Earlier we mentioned that wrapper objects are immutable...,这是怎么回事？

```java
int x2 = y.intValue();
x2++;
y = new Integer(x2);
```

`y++`的时候指向了另一个新的`obj`


### equals ==


`equals` For now all we have to know is that the intention of the `equals()` method is to determine whether two instances of a given class are "meaningfully equivalent." 

`==`是严格的，比对所指向对象是否为一个

```java
Integer i1 = 1000;
Integer i2 = 1000;

if(i1 != i2) System.out.println("different objects"); // different objects

if(i1.equals(i2)) System.out.println("meaningfully equal"); // meaningfully equal
```

再看一个

```java
Integer i3 = 10;
Integer i4 = 10;
if(i3 == i4) System.out.println("same object");
if(i3.equals(i4)) System.out.println("meaningfully equal");
```

In order to save memory, two instances of the following wrapper objects (created through boxing), will always be == when their primitive values are the same:

* Boolean
* Byte
* Character from \u0000 to \u007f (7f is 127 in decimal) 
* Short and Integer from -128 to 127


Note: When `==` is used to compare a primitive to a wrapper, the wrapper will be unwrapped and the comparison will be primitive to primitive.
