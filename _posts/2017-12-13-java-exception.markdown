---
layout: post
title:  "java exception and error"
date:   2016-12-12
categories: java
---

### exception

The term "exception" means "exceptional condition" and is an occurrence that alters the normal program flow.

`rule` handle or declare

JVM Thrown Exceptions
Programmatically Thrown Exceptions: means Created by an application and/or API developer.



### error

Both `Exception` and `Error` share a common superclass, `Throwable`, thus both can be thrown using the throw keyword.

You are not required to catch Error objects or Error subtypes. you can catch one, but again, you probably won't.

### class

一个文件只有一个public class，并且名字要和文件名一样

Access modifiers: public, protected, private.

Non-access modifiers (including strictfp, final, and abstract).

Class Access

* `Default Access` a class with default access can be seen only by classes within the same package.

* Public Access

* Final Classes  the `final` keyword means the class can't be subclassed. You should make a final class only if you need an absolute guarantee that none of the methods in that class will ever be overridden. Marking a class final means, in essence, your class can't ever be improved upon, or even specialized, by another programmer.

* `Abstract Classes` An abstract class can never be instantiated. Its sole purpose, mission in life, raison d'être, is to be extended (subclassed).
 if even a single method is abstract, the whole class must be declared abstract. One abstract method spoils the whole bunch. 

 