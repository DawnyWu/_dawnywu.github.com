---
layout: post
title:  "Build my own angular: Expression & Filter"
date:   2016-07-14
categories: js
---

#### Parse Number

```js
// number 42 to token
{
  text: '42',
  value: 42
}
```

Lex的基本结构，遍历每个character，创建token

```js
Lexer.prototype.lex = function(text) {
  this.text = text;
  this.index = 0;
  this.ch = undefined;
  this.tokens = [];
  while (this.index < this.text.length) {
    this.ch = this.text.charAt(this.index);
  }
  return this.tokens; 
};

// 数字的情况
if (this.isNumber(this.ch)) {
  this.readNumber(); 
} else {
  throw 'Unexpected next character: ' + this.ch; 
}

Lexer.prototype.isNumber = function(ch) {
  return '0' <= ch && ch <= '9';
};


Lexer.prototype.readNumber = function() {
  var number = '';
  while (this.index < this.text.length) {
    var ch = this.text.charAt(this.index);
    if (this.isNumber(ch)) {
      number += ch; 
    } else {
      break; 
    }
    this.index++;
  }

  this.tokens.push({ 
    text: number,
    value: Number(number)
  });
};
```

创建AST `{type: AST.Literal, value: 42}`

Every AST has a root node of type `AST.Program`. That root node has an attribute called `body` that holds the contents of the expression. 

所以最终应当是这样的

```js
{
  type: AST.Program,
  body: {
    type: AST.Literal,
    value: 42
  }
}
```

```js
AST.prototype.ast = function(text) {
  this.tokens = this.lexer.lex(text);
  return this.program();
};

AST.prototype.program = function() {
  return {type: AST.Program, body: this.constant()};
};
AST.prototype.constant = function() {
  return {type: AST.Literal, value: this.tokens[0].value};
};
```



ASTCompiler根据AST来创建函数

```js
ASTCompiler.prototype.compile = function(text) {
  var ast = this.astBuilder.ast(text); 
  // recurse生成一些东西存在state中
  this.state = {body: []};
  this.recurse(ast);
  return new Function(this.state.body.join(''));
};
```
`recurse` is a `recursive method` that we will invoke for each node in the tree.Since each node has a type, and different types of nodes require different kind of processing

```js
ASTCompiler.prototype.recurse = function(ast) {
  switch (ast.type) {
    case AST.Program:
    // Literal类型只需要返回值就可以了
    case AST.Literal: 
      return ast.value;
  }
};
```

```js
// Program类型返回 return 语句 和 ast.body 中的内容
case AST.Program:
  this.state.body.push('return ', this.recurse(ast.body), ';'); 
  break;
```

#### Parsing Floating Point Numbers

现在要允许浮点数`4.2`

```js
Lexer.prototype.readNumber = function() {
  var number = '';
  while (this.index < this.text.length) {
    var ch = this.text.charAt(this.index);
    // 只需要加这一个条件
    if (ch === '.' || this.isNumber(ch)) {
      number += ch; 
    } else {
      break; 
    }
    this.index++; 
  }
```

#### String

```js
// 单引号和双引号
} else if (this.ch === '\'' || this.ch === '"') {
  this.readString();



Lexer.prototype.readString = function() { 
  this.index++;
  var string = '';
  while (this.index < this.text.length) {
    var ch = this.text.charAt(this.index);
    if (ch === '\'' || ch === '"') {
      this.index++;
      this.tokens.push({
              text: string,
              value: string
            });
      return; 
    } else {
      string += ch;
    }
    this.index++;
  }
  throw 'Unmatched quote'; 
};
```

