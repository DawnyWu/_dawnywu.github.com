---
layout: post
title:  "normalize state"
date:   2016-09-20
categories: React Redux
---

[https://github.com/reactjs/redux/issues/994](https://github.com/reactjs/redux/issues/994)

### 问题

```js
state = {
   plans: [
    {title: 'A', exercises: [{title: 'exe1'}, {title: 'exe2'},{title: 'exe3'}]},
    {title: 'B', exercises: [{title: 'exe5'}, {title: 'exe1'},{title: 'exe2'}]}
   ]
}
``` 

想要update这个state,比如加一个空的exercise

`state.plans[planIdx].exercises.push({})`

想要修改一个exercise

`state.plans[planIdx].exercises[exerciseIdx] = exercise`

感觉有点麻烦

### 使用Normalize

```js
{
  entities: {
    plans: {
      1: {title: 'A', exercises: [1, 2, 3]},
      2: {title: 'B', exercises: [5, 1, 2]}
     },
    exercises: {
      1: {title: 'exe1'},
      2: {title: 'exe2'},
      3: {title: 'exe3'}
    }
  },
  currentPlans: [1, 2]
}
```

reducer就变成这样

```js
import merge from 'lodash/object/merge';

const exercises = (state = {}, action) => {
  switch (action.type) {
  case 'CREATE_EXERCISE':
    return {
      ...state,
      [action.id]: {
        ...action.exercise
      }
    };
  case 'UPDATE_EXERCISE':
    return {
      ...state,
      [action.id]: {
        ...state[action.id],
        ...action.exercise
      }
    };
  default:
    if (action.entities && action.entities.exercises) {
      return merge({}, state, action.entities.exercises);
    }
    return state;
  }
}

const plans = (state = {}, action) => {
  switch (action.type) {
  case 'CREATE_PLAN':
    return {
      ...state,
      [action.id]: {
        ...action.plan
      }
    };
  case 'UPDATE_PLAN':
    return {
      ...state,
      [action.id]: {
        ...state[action.id],
        ...action.plan
      }
    };
  default:
    if (action.entities && action.entities.plans) {
      return merge({}, state, action.entities.plans);
    }
    return state;
  }
}

const entities = combineReducers({
  plans,
  exercises
});

const currentPlans = (state = [], action) {
  switch (action.type) {
  case 'CREATE_PLAN':
    return [...state, action.id];
  default:
    return state;
  }
}

const reducer = combineReducers({
  entities,
  currentPlans
});
```



