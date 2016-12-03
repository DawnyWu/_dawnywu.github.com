---
layout: post
title:  "react authorization"
date:   2016-11-29
categories: js
---

```html
<Router history={history}>
  <Route component={MainContainer} path="/">
    <IndexRoute authorize={['employee', 'admin']} component={Home} />
    <Route authorize={['employee']} component={Profile}>
      <Route component={PhotosList} path="/profile/photos" />
    </Route>
    <Route authorize={['admin']} component={EmployeesManagement}>
      <Route component={EmployeesList} path="/employees/list" />
      <Route component={EmployeeAdd} path="/employees/add" />
    </Route>
  </Route>
</Router>
```

```js
import React, { PropTypes } from 'react';
import _ from 'lodash';
 
class AuthorizedComponent extends React.Component {
  static propTypes = {
    routes: PropTypes.array.isRequired
  };
 
  static contextTypes = {
    router: PropTypes.object.isRequired
  };
 
  componentWillMount() {
    const { routes } = this.props; // array of routes
    const { router } = this.context;
 
    // check if user data available
    const user = JSON.parse(localStorage.getItem('user'));
    if (!user) {
      // redirect to login if not
      router.push('/login');
    }
 
    // get all roles available for this route
    const routeRoles = _.chain(routes)
      .filter(item => item.authorize) // access to custom attribute
      .map(item => item.authorize)
      .flattenDeep()
      .value();
 
    // compare routes with user data
    if (_.intersection(routeRoles, user.roles).length === 0) {
      router.push('/not-authorized');
    }
  }
}
 
export default AuthorizedComponent;
```

### 思考

目前是基于人的，可以获得每个人的能或不能

{"result":{"success":true,"errorCode":null,"errorMsg":null},"data":{"model":[]},"extra":null}

调用的权限的接口是这个：

http://srv.test.pajkdc.com/diving/resource/findResourcesByUser?relegationName=overwatch

a = response

<!-- 如果成功 -->
a.data.model[0] 是健康圈菜单

a.data.model[1] 是夺宝菜单

a.data.model[1].subResources 是夺宝菜单

### react router 的几个hook 

onChange(prevState, nextState, replace, callback?)

onLeave(prevState)

onEnter(nextState, replace, callback?)

setRouteLeaveHook(route, hook)

### 小布的代码

```js
  function checkRights(route, replace) {
    const state = store.getState();
    const rights = state.common.getIn([ 'userInfo', 'rights' ]);
    const routeRights = state.common.get('routeRights');
    const paths = route.location.pathname.split('/').filter(a => a);
    for (const path of paths) {
      if (path in routeRights) {
        if (!(routeRights[path] in rights)) {
          replace('/withoutAuth');
        }
      }
    }
  }
  return {
    path: '/',
    component: App,
    onChange: (prev, next, replace/* , callback */) => {
      // [TODO] initial route?
      checkRights(next, replace);
    },
```    

后天配置path要加`/`



export function getAuthorization(params) {
  return (dispatch) => 
    Api.getAuth('diving/resource/findResourcesByUser?relegationName=overwatch')
      .then(data => {
        if (data.content.result.success) {
          const resources_codes = data.content.data.model.map((menu) => {
            return menu.subResources.map((item) => {return item.code})
          })
          const code_array = flatten(resources_codes)
          // console.log('code_array:', code_array)
          // co   nsole.log(code_array.indexOf('overwatch.global') !== -1)
          if (code_array.indexOf('duobao-config') !== -1) {
            return true
          }
        }
        return false;
      });
}

to copy
```js
export function getAuthInfo() {
  return (dispatch) =>
    getAuthorization()
      .then( res => { if (res.result.success) { return res.data } else { alert('error') } })
      .then( data => ('data:', flatten( data.model.map( (item) => {return item.subResources} ))))
      .then( items => {items.map((i) => {return i.resourceUrl})})
      .then( path_array => {dispatch({
        type: 'getUserAuth',
        content: path_array || []
      })})
}

const flatten = list => list.reduce(
  (a, b) => a.concat(Array.isArray(b) ? flatten(b) : b), []
);
```