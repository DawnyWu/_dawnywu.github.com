---
layout: post
title:  "java"
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

### " " 和 '' 区别

"" 字符串
'' character


### string

`StringBuilder` is not thread safe.

Sun recommends that you use `StringBuilder` instead of `StringBuffer` whenever possible because `StringBuilder` will run faster (and perhaps jump higher)

### File

`new File("foo")`并不创建file文件,下边的可以

```java
File file = new File("foo"); // no file yet 
file.createNewFile(); // make a file, "foo" which is assigned to 'file'
```

```java
File file = new File("foo"); // no file yet 
PrintWriter pw = newPrintWriter(file);
```

***directory***

```java
File myDir = new File("mydir");   // create an object
myDir.mkdir();                    // create an actual directory
```


### Thread

两种方式，集成或者implement interface

***run()***

你可以随意overload run() method,可是Thread Class不会为你调用这个方法。

哪怕是你自己自己调用了run(String s)方法，你只是调用了一个普通的方法而已，并没有新开一个线程，拥有一个新的call stack。

***怎么让线程跑起来***

如果是集成的，比较简单

```java
MyThread t = new MyThread()
```

如果是实现runnable接口，稍微麻烦一点点

```java
MyRunnable r = new MyRunnable();
Thread t = new Thread(r);  // Pass your Runnable to the Thread
```

***顺序***

你可以按顺序启动各个线程，可是运行的时候，执行的先后顺序就不一定了

thread can not be restarted,只能运行一次就完了

### thread states

***Waiting/blocked/sleeping***

The important point is that one thread does not tell another thread to block.

Note also that a thread in a blocked state is still considered to be alive.

***Dead***

Once a thread is dead, it can never be brought back to life!


`sleep()`是static method,一个线程没有办法让另一个线程sleep


###priority

main thread生成another thread,他们的优先级一样

###Thread.yield()

What `yield()` is supposed to do is make the currently running thread head back to runnable to allow other threads of the same priority to get their turn. So the intention is to use `yield()` to promote graceful turn-taking among equal-priority threads

In reality, though, the yield() method isn't guaranteed to do what it claims

A `yield()` won't ever cause a thread to go to the `waiting/sleeping/ blocking` state. At most, a `yield()` will cause a thread to go from running to runnable, but again, it might have no effect at all.

### join()

这个方法不是`static`的

t.join() t先挤进来运行

`t.join()` means "Join me (the current thread) to the end of t, so that t must finish before I (the current thread) can run again."

### Inner Class
From inside the outer class instance code, use the inner class name in the normal way:

```java
MyInner mi = new MyInner();
```

From outside the outer class instance code (including static method code within the outer class), the inner class name must now include the outer class's name:

```java
MyOuter.MyInner
```
To instantiate it, you must use a reference to the outer class:

```java
new MyOuter().new MyInner();
outerObjRef.new MyInner();
```

if you already have an instance of the outer class.

***this的问题***

* To reference the inner class instance itself, from within the inner class code,
use this.

* To reference the "outer this" (the outer class instance) from within the inner class code, use NameOfOuterClass.this (example, MyOuter.this).

***Method-Local Inner Classes***

```java
class MyOuter2 {
  private String x = "Outer2";
  void doStuff() {
    // 创建了一个class
    class MyInner {
      public void seeOuter() {
        System.out.println("Outer x is " + x);
      } // close inner class method
    } // close inner class definition
    MyInner mi = new MyInner();
    // 创建了类，实例化后调用方法
    mi.seeOuter();
  } // close outer class method doStuff()
} // close outer class
```
***the inner class object cannot use the local variables of the method the inner class is in.***

```java
class MyOuter2 {
  private String x = "Outer2";
  void doStuff() {
    String z = "local variable";
    class MyInner { 
      public void seeOuter() {
        System.out.println("Outer x is " + x); 
        // 调用z报错
        System.out.println("Localvariablezis"+z); //Won'tCompile!
      } // close inner class method
    } // close inner class definition
  } // close outer class method doStuff()
} // close outer class
```

因为method local variable在method结束后，从栈中灰飞湮灭。而instance很有可能还在堆中存活

在method local variable前面用final可以解决这个问题

### anonymous inner class

***flavor one***

```java
class Popcorn {
  public void pop() {
  System.out.println("popcorn");
  }
}

class Food {
  // 这里subclass
  Popcorn p = new Popcorn() {
    public void pop() {
      System.out.println("anonymous popcorn");
    }
  };
}
```

```java
class Popcorn {
   public void pop() {
      System.out.println("popcorn");
   }
}
class Food {
   Popcorn p = new Popcorn() {
      public void sizzle() {
        System.out.println("anonymous sizzling popcorn");
      }
      public void pop() {
        System.out.println("anonymous popcorn");
      }
    };
    public void popIt() {
      p.pop();     // OK, Popcorn has a pop() method
      p.sizzle();  // Not Legal! Popcorn does not have sizzle()
    } 
  }
```

上边的会报错，因为polymorphism

***flavor two***

this time the new just-in-time class is an implementer of the Cookable interface. 

```java
interface Cookable {
  public void cook();
}

class Food {
  Cookable c = new Cookable() {
    public void cook() {
      System.out.println("anonymous cookable implementer");
    }
  }; 
}
```

anonymouse class写在参数的位置

```java
class MyWonderfulClass {
  void go() {
    Bar b = new Bar();
    b.doStuff(new Foo() {
      public void foof() {
        System.out.println("foofy");
      } // end foof method
    }); // end inner class def, arg, and b.doStuff stmt.
  } // end go()
} // end class

interface Foo {
  void foof();
}

class Bar {
  void doStuff(Foo f) { }
}
```

### static nested classes

It is simply a non-inner (also called "top-level") class scoped within another. 

A static nested class is simply a class that's a static member of the enclosing class:

```java
class BigOuter {
  static class Nested { }
}
```

***instantiate***

```java
class BigOuter {
  static class Nest {void go() { System.out.println("hi"); } }
}

class Broom {
  static class B2 {void goB2() { System.out.println("hi 2"); } }
  public static void main(String[] args) {
    BigOuter.Nest n = new BigOuter.Nest();   // both class names
    n.go();
    B2 b2 = new B2();
    b2.goB2(); 
  }
}
```

### polymorphism

```java
PlayerPiece player = new PlayerPiece();
Object o = player;
GameShape shape = player;
Animatable mover = player;
```

```java
SuperClass a = new ChildClass()
```

都有`shit()` method，`child` 覆盖了 `super` 的

尽管`a`的`type`是`SuperClass`，调用的时候还是调用`ChildClass`覆盖后的版本

### override

static methods can't be overridden

感觉exception方面还不太清楚


To summarize, `which overridden version of the method to call` (in other words, from which class in the inheritance tree) is decided at runtime based on object type, 
but `which overloaded version of the method to call` is based on the reference type of the argument passed at compile time. 

```java
public class Animal {
   public void eat() {
      System.out.println("Generic Animal Eating Generically");
   }
}
public class Horse extends Animal {
   public void eat() {
       System.out.println("Horse eating hay ");
   }
   public void eat(String s) {
      System.out.println("Horse eating " + s);
   }
}
```

Animal a2 = new Animal();
a2.eat("treats");

报错，animal没有eat(String s)方法

Animal ah2 = new Horse(); 
ah2.eat("Carrots");

报错，还是一样，animal class没有eat(String s)方法

### cast downcast

`Animal animal = new Dog();`的时候，我么不能调用dog class的方法，可以这样`Dog d = (Dog) animal;`

其实upcast也是存在的，只是没有什么特别需要注意的

```java
Dog d = new Dog();
Animal a1 = d;           // upcast ok with no explicit cast
Animal a2 = (Animal) d;  // upcast ok with an explicit cast
```

### equals == ===

java并没有`===`

The `equals()` method in class Object uses only the `==` operator for comparisons, so unless you override `equals()`, two objects are considered equal only if the two references refer to the same object.

override `equals`使其更有意义

### generics

Why can't you pass an ArrayList<Dog> into a method with an argument of ArrayList<Animal>?

In other words, at runtime the JVM KNOWS the type of arrays, but does NOT know the type of a collection.

`<? extends Animal>`
can be EITHER a subclass of a class (abstract or concrete) OR a type that implements the interface after the word extends. 

```java
void foo(List<? extends Serializable> list) // odd, but correct
                                            // to use "extends"
```


List<? extends Object> and List<?> are absolutely identical! But neither of
them are the same as List<Object>


But List<? extends Dog> could mean List<Beagle>, List<Poodle>, and so on. Of course List<?> could be... anything at all.


wildcards can be used only for reference declarations (including arguments, variables, return types, and so on).






****

###ItelliJ IDEA 


