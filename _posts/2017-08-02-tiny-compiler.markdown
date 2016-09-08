---
layout: post
title:  "Super tiny compiler"
date:   2016-08-02
categories: js
---

```js
(add 2 (subtract 4 2))
```

```js
// Tokens
[
  { type: 'paren',  value: '('        },
  { type: 'name',   value: 'add'      },
  { type: 'number', value: '2'        },
  { type: 'paren',  value: '('        },
  { type: 'name',   value: 'subtract' },
  { type: 'number', value: '4'        },
  { type: 'number', value: '2'        },
  { type: 'paren',  value: ')'        },
  { type: 'paren',  value: ')'        }
]
```

```js
// AST
{
  type: 'Program',
  body: [{
    type: 'CallExpression',
    name: 'add',
    params: [
      {
        type: 'NumberLiteral',
        value: '2'
      }, 
      {
        type: 'CallExpression',
        name: 'subtract',
        params: [
          {
           type: 'NumberLiteral',
           value: '4'
          }, 
          {
           type: 'NumberLiteral',
           value: '2'
          }
        ]
      }
    ]
 }]
}
```