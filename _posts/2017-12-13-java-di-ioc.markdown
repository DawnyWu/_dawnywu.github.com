---
layout: post
title:  "java di"
date:   2016-12-13
categories: java
---

```java
public class Customer
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public DateTime CustomerSince { get; set; }
    public string Status { get; set; }
}
```

```java
// 实现接口
public class UglyCustomer : INotifyPropertyChanged
{
    private string _firstName;
    public string FirstName
    {
        get { return _firstName; }
        set
        {
            string oldValue = _firstName;
            _firstName = value;
            if(oldValue != value)
                OnPropertyChanged("FirstName");
        }
    }
    private string _lastName;
    public string LastName
    {
        get { return _lastName; }
        set
        {
           string oldValue = _lastName;
            _lastName = value;
            if(oldValue != value)
                OnPropertyChanged("LastName");
        }
    }

    private DateTime _customerSince;
    public DateTime CustomerSince
    {
        get { return _customerSince; }
    set
        {
            DateTime oldValue = _customerSince;
            _customerSince = value;
            if(oldValue != value)
                OnPropertyChanged("CustomerSince");
        }
    }

    private string _status;
    public string Status
    {
        get { return _status; }
        set
        {
            string oldValue = _status;
            _status = value;
            if(oldValue != value)
                OnPropertyChanged("Status");
        }
    }

    protected virtual void OnPropertyChanged(string property)
    {
        var propertyChanged = PropertyChanged;

        if(propertyChanged != null)
            propertyChanged(this, new PropertyChangedEventArgs(property));
    }

    public event PropertyChangedEventHandler PropertyChanged;
}
```