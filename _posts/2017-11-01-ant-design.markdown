---
layout: post
title:  "Ant Design"
date:   2016-10-31
categories: js
---

### Form

```
<FormItem labelCol={{ span: 3 }} wrapperCol={{ span: 8 }} label="联系电话" >
  {getFieldDecorator('contactMobile',
                      { initialValue: officialAccInfo.contactMobile,
                        rules: [{ required: true,
                                  min: 2,
                                  message: '请输入电话号码' },
                                { validator: this.checkTel }
                               ]
                      })(
   <Input type="tel" />
  )}
</FormItem>
```

*** 动态为一个field赋值

```js
this.props.form.setFieldsValue({ skuId: '', skuPieceCount: '', promotionTemplateId: '' });
```

### Table

columns的dataIndex对应数据{key1: "data", key2: "data"}

title就是column头

```js
this.columns = [{
  title: '投放ID',
  dataIndex: 'key1'
}, {
  title: '业务模块',
  dataIndex: 'key2'
}, {
  title: '操作',
  render: (record) => (
    <span>
      <a href="javascript:void(0)" onClick={() => this.handleEdit(record)}>编辑</a>
      <span className="ant-divider"></span>
      <a href="javascript:void(0)" onClick={() => this.handleStop(record)}>停止</a>
      <a href="javascript:void(0)" onClick={() => this.handleStart(record)}>启用</a>
      <span className="ant-divider"></span>
      <a href="javascript:void(0)" onClick={() => this.handleDelete(record)}>删除</a>
    </span> )
}]
}

<Table columns={columns} dataSource={dataSource}/>
```
