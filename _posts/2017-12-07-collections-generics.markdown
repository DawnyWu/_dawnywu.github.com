---
layout: post
title:  "collections generics"
date:   2016-12-07
categories: java
---

***ordered***

When a collection is ordered, it means you can iterate through the collection in a specific (not-random) order. 

***sorted***

A sorted collection means that the order in the collection is determined according to some rule or rules, known as the sort order.

### List interface

***ArrayList***

Think of this as a growable array. It gives you fast iteration and fast random access. 

***Vector***

A Vector is basically the same as an ArrayList, but Vector methods are syn- chronized for thread safety.

***LinkedList***

LinkedList may iterate more slowly than an ArrayList, but it's a good choice when you need fast insertion and deletion.

### Set interface

***Hashset***

A HashSet is an unsorted, unordered Set.

***LinkedHashSet***

A LinkedHashSet is an ordered version of HashSet. Use this class instead of HashSet when you care about the iteration order.

***TreeSet***

TreeSet The TreeSet is one of two sorted collections 

### Map interface

***HashMap***

The HashMap gives you an unsorted, unordered Map. 

***Hashtable***

Hashtable is the synchronized counterpart to HashMap. 

***LinkedHashMap***

maintains insertion order
Hashtable is the synchronized counterpart to HashMap.

***TreeMap***

 a TreeMap is a sorted Map.

### Autoboxing with Collections

```java
// pre java5的时候要这么写
List myInts = new ArrayList();
myInts.add(new Integer(42));

// 之后这么写, 因为autoboxing
myInts.add(42);
```



