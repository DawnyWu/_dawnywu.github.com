---
layout: post
title:  "parser state chart"
date:   2016-12-26
categories: all
---

### parsing state

parsint state就是语法中的一条，右边加上红点而已。。。

### chart

The nth entry in the chart will contain all of the parse states we could be in after seeing the first n tokens of the input.

### parse state starting position

parse chart是当接收了n个token后，对应的grammar状态（带红点的），可是并不能看出在这个状态的时候经历了什么，经历了几个token，需要配合starting position来表明