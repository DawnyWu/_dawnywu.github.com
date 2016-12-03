---
layout: post
title:  "java string"
date:   2016-12-01
categories: java
---

### string

`string is immutable`, `java`里对字符串进行操作，如果不赋值的话，原变量不变的

```java
String s1 = "spring ";
String s2 = s1 + "summer "; // spring summer

// 不变
s1.concat("fall ");
// 不变
s2.concat(s1);
s1 += "winter "; // spring winter
System.out.println(s1 + " " + s2);
```

### Stack and Heap

* Instance variables and objects live on the heap.

* Local variables live on the stack.

```java
class Collar { }

class Dog {
  Collar c;           // instance variable
  String name;        // instance variable

  public static void main(String [] args) {
    Dog d;           // local variable: d
    d = new Dog();
    d.go(d)
  }

  void go(Dog dog) { // local variable: dog
    c = new Collar();
    dog.setName("Aiko");
  }

  void setName(String dogName) { // local var: dogName
    name = dogName;
    // do more stuff
  }
}

### static variable & instance variable

`java`的`instance variable`不是放在constructor里边的

```java
class Frog {
  int frogCount = 0;  // Declare and initialize
                       // instance variable
  public Frog() {
    frogCount += 1;  // Modify the value in the constructor
  }

  public static void main (String [] args) {
    new Frog();
    new Frog();
    new Frog();
    System.out.println("Frog count is now " + frogCount);
  }
}
```

you can’t access a nonstatic (instance) variable from a static method. 

```java
class Foo {
  int x = 3;
  public static void main (String [] args) {
    System.out.println("x is " + x);
  }
}
```

static method cannot access an instance (non-static) variable

```java
int size = 42;
static void doMore(){
  int x = size; 
}
```

static method cannot access a non-static method

```java
void go ( );
  static void doMore( ){
    go( ); 
}
````

static methods can't be overridden! This doesn't mean they can't be redefined in a subclass, but redefining and overriding aren't the same thing.

instance 是可以调用class method的，因为可以从instance推断出class是什么


### array

`int[5] scores`这种是错的，不需要写数字

Remember, the JVM doesn't allocate space until you actually instantiate the array object. That's when size matters.

```java
int[] testScores;        // Declares the array of ints
testScores = new int[4];

int[] testScores = new int[4];
```




