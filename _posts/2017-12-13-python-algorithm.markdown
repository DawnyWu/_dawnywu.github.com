---
layout: post
title:  "python algorithm"
date:   2016-12-12
categories: python algorithm
---
Order of magnitude is often called Big-O notation (for “order”) and written as 𝑂(𝑓(𝑛)).
It provides a useful approximation to the actual number of steps in the computation.
```python
def sum_of_n(n):
  the_sum = 0
  for i in range(1,n+1):
   the_sum = the_sum + i
  return the_sum


print(sum_of_n(10))



def foo(tom):
   fred = 0
   for bill in range(1, tom+1):
     barney = bill
     fred = fred + barney
   return fred

print(foo(10))
```


a=5
b=6
c = 10

for i in range(n):
  for j in range(n): 
    x=i*i 
    y=j*j
    z=i*j
for k in range(n): 
  w = a * k + 45
  v=b*b 

d = 33

## list

### array

长度是5，中间的是(5-1)/2

### linked list

```python
class Element(object):
    def __init__(self, value):
        self.value = value
        self.next = None

class LinkedList(object):
    def __init__(self, head=None):
        self.head = head

def append(self, new_element):
    current = self.head
    if self.head:
        while current.next:
            current = current.next
        current.next = new_element
    else:
        self.head = new_element
```

```python
"""The LinkedList code from before is provided below.
Add three functions to the LinkedList.
"get_position" returns the element at a certain position.
The "insert" function will add an element to a particular
spot in the list.
"delete" will delete the first element with that
particular value.
Then, use "Test Run" and "Submit" to run the test cases
at the bottom."""

class Element(object):
    def __init__(self, value):
        self.value = value
        self.next = None
        
class LinkedList(object):
    def __init__(self, head=None):
        self.head = head
        
    def append(self, new_element):
        current = self.head
        if self.head:
            while current.next:
                current = current.next
            current.next = new_element
        else:
            self.head = new_element
            
    def get_position(self, position):
        current = self.head
        """Get an element from a particular position.
        Assume the first position is "1".
        Return "None" if position is not in the list."""
        return None
    
    def insert(self, new_element, position):
        """Insert a new node at the given position.
        Assume the first position is "1".
        Inserting at position 3 means between
        the 2nd and 3rd elements."""
        pass
    
    
    def delete(self, value):
        """Delete the first node with a given value."""
        pass

# Test cases
# Set up some Elements
e1 = Element(1)
e2 = Element(2)
e3 = Element(3)
e4 = Element(4)

# Start setting up a LinkedList
ll = LinkedList(e1)
ll.append(e2)
ll.append(e3)

# Test get_position
# Should print 3
print ll.head.next.next.value
# Should also print 3
print ll.get_position(3).value

# Test insert
ll.insert(e4,3)
# Should print 4 now
print ll.get_position(3).value

# Test delete
ll.delete(1)
# Should print 2 now
print ll.get_position(1).value
# Should print 4 now
print ll.get_position(2).value
# Should print 3 now
print ll.get_position(3).value

```python
def insert(self, new_element, position):
    counter = 1
    current = self.head
    if position > 1:
        while current and counter < position:
            if counter == position - 1:
                new_element.next = current.next
                current.next = new_element
            current = current.next
            counter += 1
    elif position == 1:
        new_element.next = self.head
        self.head = new_element
```

"""The LinkedList code from before is provided below.
Add three functions to the LinkedList.
"get_position" returns the element at a certain position.
The "insert" function will add an element to a particular
spot in the list.
"delete" will delete the first element with that
particular value.
Then, use "Test Run" and "Submit" to run the test cases
at the bottom."""

class Element(object):
    def __init__(self, value):
        self.value = value
        self.next = None
        
class LinkedList(object):
    def __init__(self, head=None):
        self.head = head
        
    def append(self, new_element):
        current = self.head
        if self.head:
            while current.next:
                current = current.next
            current.next = new_element
        else:
            self.head = new_element
            
    def get_position(self, position):
        print('self.head.value:',self.head.value)
        current = self.head
        
        for i in range(1, position):
            if(current.value == None):
                return None
            else:
                current = current.next
        
        return current.value
        
        
        """Get an element from a particular position.
        Assume the first position is "1".
        Return "None" if position is not in the list."""
        # return None
    
    def insert(self, new_element, position):
        if(position == 1):
           old_next = self.head
           new_element.next = old_next
           self.head = new_element  
           return
       
        current = self.head
        for i in range(1,position):
            old_next = current.next
            new_element.next = old_next
            current.next = new_element            
       
        
        
        # if(position == 2):
        #     old_next = self.head.next
        #     new_element.next = old_next
        #     self.head.next = new_element
        
        # if(position == 3):
        #     old_next = self.head.next.next
        #     new_element.next = old_next
        #     self.head.next.next = new_element
          
        
    
    
    def delete(self, value):
        """Delete the first node with a given value."""
        if(self.head.value == value):
            further_next = self.head.next
            self.head = further_next
            return
        
        current = self.head
        while(current.next):
            if(current.next.value == value):
                further_next = current.next.next
                current.next = further_next
                return
            else:
                current = current.next
        
        # if(self.head.value ==value):
        #     further_next = self.head.next
        #     self.head = further_next

        # if(self.head.next.value ==value):
        #     further_next = self.head.next.next
        #     self.head.next = further_next        

            
        
            
        
        
        

# Test cases
# Set up some Elements
e1 = Element(1)
e2 = Element(2)
e3 = Element(3)
e4 = Element(4)
e5 = Element(5)
# Start setting up a LinkedList
ll = LinkedList(e1)
ll.append(e2)
ll.append(e3)
ll.append(e4)
ll.append(e5)

# Test get_position
# Should print 3
# print ll.head.next.next.value
# Should also print 3
# print ll.get_position(3)

# Test insert
# ll.insert(e4,3)
# Should print 4 now
# print ll.get_position(3)

# Test delete
ll.delete(3)
# Should print 2 now
print ll.get_position(3)
# Should print 4 now
# print ll.get_position(2)
# Should print 3 now
# print ll.get_position(3)

****

"""Add a couple methods to our LinkedList class,
and use that to implement a Stack.
You have 4 functions below to fill in:
insert_first, delete_first, push, and pop.
Think about this while you're implementing:
why is it easier to add an "insert_first"
function than just use "append"?"""

class Element(object):
    def __init__(self, value):
        self.value = value
        self.next = None
        
class LinkedList(object):
    def __init__(self, head=None):
        self.head = head
        
    def append(self, new_element):
        current = self.head
        if self.head:
            while current.next:
                current = current.next
            current.next = new_element
        else:
            self.head = new_element

    def insert_first(self, new_element):
        if(self.head):
            old_next = self.head
            new_element.next = self.head
            self.head = new_element
        else:
            self.head = new_element
        "Insert new element as the head of the LinkedList"

    def delete_first(self):
        "Delete the first (head) element in the LinkedList as return it"
        if(self.head):
            cache = self.head
            old_next = self.head.next
            self.head = old_next
            return cache

class Stack(object):
    def __init__(self,top=None):
        self.ll = LinkedList(top)

    def push(self, new_element):
        self.ll.insert_first(new_element)
        "Push (add) a new element onto the top of the stack"

    def pop(self):
        "Pop (remove) the first element off the top of the stack and return it"
        return self.ll.delete_first()
    
# Test cases
# Set up some Elements
e1 = Element(1)
e2 = Element(2)
e3 = Element(3)
e4 = Element(4)

# Start setting up a Stack
stack = Stack(e1)

# Test stack functionality
stack.push(e2)
stack.push(e3)
print stack.pop().value
print stack.pop().value
print stack.pop().value
print stack.pop()
stack.push(e4)
print stack.pop().value

*****

"""Make a Queue class using a list!
Hint: You can use any Python list method
you'd like! Try to write each one in as 
few lines as possible.
Make sure you pass the test cases too!"""

class Queue:
    def __init__(self, head=None):
        self.storage = [head]

    def enqueue(self, new_element):
        self.storage.append(new_element)

    def peek(self):
        return self.storage[0] 

    def dequeue(self):
        result = self.storage[0]
        self.storage = self.storage[1:len(self.storage)]
        return result
    
# Setup
q = Queue(1)
q.enqueue(2)
q.enqueue(3)

# Test peek
# Should be 1
print q.peek()

# Test dequeue
# Should be 1
print q.dequeue()

# Test enqueue
q.enqueue(4)
# Should be 2
print q.dequeue()
# Should be 3
print q.dequeue()
# Should be 4
print q.dequeue()
q.enqueue(5)
# Should be 5
print q.peek()

***
"""You're going to write a binary search function.
You should use an iterative approach - meaning
using loops.
Your function should take two inputs:
a Python list to search through, and the value
you're searching for.
Assume the list only has distinct elements,
meaning there are no repeated values, and 
elements are in a strictly increasing order.
Return the index of value, or -1 if the value
doesn't exist in the list."""

# def binary_search(input_array, value):
#     """Your code goes here."""
#     mid_index = len(input_array)/2
#     while(value != input_array[mid_index]):
#         print('mid index:',mid_index)
#         if value > input_array[mid_index]:
#             input_array = input_array[mid_index+1 : len(input_array)]
#         else:
#             input_array = input_array[0: mid_index]
        
#         mid_index = len(input_array)/2
#         print('input_array:', input_array)
#         if(len(input_array)==0):
#             return -1
    
#     return 

def binary_search(input_array, value):
    begin_index = input_array[0] 
    end_index= len(input_array)
    mid_index = len(input_array)/2
    
    while(value != input_array[mid_index]):
        if(value > input_array[mid_index] ):
            begin_index = mid_index +1
        else:
            end_index = mid_index
        
        mid_index = (begin_index + end_index)/2
        if(begin_index == end_index):
            return -1
        # print(begin_index, mid_index, end_index)
    
    return mid_index
    
        


test_list = [1,3,9,11,15,19,29]
test_val1 = 25
test_val2 = 15
print binary_search(test_list, test_val1)
print binary_search(test_list, test_val2)

****

"""Implement a function recursivly to get the desired
Fibonacci sequence value.
Your code should have the same input/output as the 
iterative code in the instructions."""
fib_seq = []
fib_seq[0] = 0
fib_seq[1] = 1
fib_seq[2] = 1
fib_seq[3] = 2
fib_seq[4] = 3
fib_seq[5] = 5
fib_seq[6] = 8
fib_seq[7] = 13
fib_seq[8] = 21
fib_seq[9] = 34

def get_fib(position):
    if(position == 0):
        return 0
        
    if(position == 1):
        return 1
     
    return get_fib(position-1) + get_fib(position-2)


# Test cases
print get_fib(9)
print get_fib(11)
print get_fib(0)

### buble sort

n个item, 两两比较，一轮要比较n-1次，需要n-1轮，在n-1轮后，最后一个元素自然是排好的

### merge sort

9475620138

9 4 7 5 6 2 0 1 3 8

49 57 26 01 38     1+1+1+1+1

4579 0126 38      3+3+1

01245679 38     

0123456789           

要几层呢？ logN层

每层要几次运算？ 最多n-1次运算，因为完成最后一层要n-1次，其他层都不需要那么多

### quick sort

worst case和bubble一样， O(n^2)

*****

## Tree

The correct answer was: D, F, E, B, C, A

Here are what some of the other traversals would look like on that tree:

BFS: A, B, C, D, E, F

In-order: D, B, E, F, A, C

Pre-order: A, B, D, E, F, C

Post-order: D, F, E, B, C, A

Pre-order: 先parent再孩子
In-order: 标记了children才可以标记parent
Post-order: children标记完才能标记parent




class Node:
    def __init__(self, value):
        self.value = value
        self.children = None
Now we can begin to build our tree:

class Tree:
    def __init__(self, root=None):
        self.root = root

class Node(object):
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None
Your task will be to write functions that traverse the tree in a specific order.
START QUIZ















### quick sort

```python
list = [0 2 9 7 3 6 8 4 5 1]

pivot_index = len(list)
test_index = 0

pivot = list[pivot_indx]
test = list[tetst_index]

if test > pivot:
  pre_pivot = list[pivot_index-1]
  list[pivot_index] = test
  list[pivot_index - 1] = pivot
  list[test_index] = pre_pivot
else:

```

*****

### Traverse BinaryTree

class Node(object):
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

class BinaryTree(object):
    def __init__(self, root):
        self.root = Node(root)

    def search(self, find_val):
        if(self.root.value == find_val):
            return true
        if(self.root.left.value == find_val):
            return true
        if(self.root.right.value == find_val):
            return true
        """Return True if the value
        is in the tree, return
        False otherwise."""
        return False

    def print_tree(self):
        # self.my_print_out(self.root)
        """Print out all tree nodes
        as they are visited in
        a pre-order traversal."""
        print(self.value)
        if(self.left):
            self.left.print_tree()
        if(self.right):
            self.right.print_tree()
        return

    def preorder_search(self, start, find_val):
        """Helper method - use this to create a 
        recursive search solution."""
        return False

    def preorder_print(self, start, traversal):
        pass
    
    def my_print_out(self, node):
        print(node.value)
        if(node.left):
            self.my_print_out(node.left)
        if(node.right):
            self.my_print_out(node.right)
        return
    
    def my_search_val(self, node, find_val):
        if(node.value == find_val):
            print('yes')
        if(node.left):
            self.my_search_val( node.left, find_val)
        if(node.right):
            self.my_search_val( node.right, find_val)
        return
        

        


# Set up tree
tree = BinaryTree(1)
tree.root.left = Node(2)
tree.root.right = Node(3)
tree.root.left.left = Node(4)
tree.root.left.right = Node(5)

# tree.my_print_out(tree.root)
# tree.root.print_tree()
tree.my_search_val(tree.root,5)
# Test search
# Should be True
# print tree.search(4)
# Should be False
# print tree.search(6)

# Test print_tree
# Should be 1-2-4-5-3
# print tree.print_tree()


[1,0,0,0,0,1]
[1,1,1,0,1,0]
[1,0,0,0,0,1]
[1,0,0,1,0,1]

*****

## Graph

```python
class Node(object):
    def __init__(self, value):
        self.value = value
        self.edges = []

class Edge(object):
    def __init__(self, value, node_from, node_to):
        self.value = value
        self.node_from = node_from
        self.node_to = node_to

class Graph(object):
    def __init__(self, nodes=[], edges=[]):
        self.nodes = nodes
        self.edges = edges

    def insert_node(self, new_node_val):
        new_node = Node(new_node_val)
        self.nodes.append(new_node)
        
    def insert_edge(self, new_edge_val, node_from_val, node_to_val):
        from_found = None
        to_found = None
        for node in self.nodes:
            if node_from_val == node.value:
                from_found = node
            if node_to_val == node.value:
                to_found = node
        if from_found == None:
            from_found = Node(node_from_val)
            self.nodes.append(from_found)
        if to_found == None:
            to_found = Node(node_to_val)
            self.nodes.append(to_found)
        new_edge = Edge(new_edge_val, from_found, to_found)
        from_found.edges.append(new_edge)
        to_found.edges.append(new_edge)
        self.edges.append(new_edge)

    def get_edge_list(self):
        """Don't return a list of edge objects!
        Return a list of triples that looks like this:
        (Edge Value, From Node Value, To Node Value)"""
        return []

    def get_adjacency_list(self):
        """Don't return any Node or Edge objects!
        You'll return a list of lists.
        The indecies of the outer list represent
        "from" nodes.
        Each section in the list will store a list
        of tuples that looks like this:
        (To Node, Edge Value)"""
        return []
    
    def get_adjacency_matrix(self):
        """Return a matrix, or 2D list.
        Row numbers represent from nodes,
        column numbers represent to nodes.
        Store the edge values in each spot,
        and a 0 if no edge exists."""
        return []

graph = Graph()
graph.insert_edge(100, 1, 2)
graph.insert_edge(101, 1, 3)
graph.insert_edge(102, 1, 4)
graph.insert_edge(103, 3, 4)
# Should be [(100, 1, 2), (101, 1, 3), (102, 1, 4), (103, 3, 4)]
print graph.get_edge_list()

# Should be [None, [(2, 100), (3, 101), (4, 102)], None, [(4, 103)], None]
print graph.get_adjacency_list()

# Should be [[0, 0, 0, 0, 0], [0, 0, 100, 101, 102], [0, 0, 0, 0, 0], [0, 0, 0, 0, 103], [0, 0, 0, 0, 0]]
print graph.get_adjacency_matrix()

****

### hashtable

"""Write a HashTable class that stores strings
in a hash table, where keys are calculated
using the first two letters of the string."""

class HashTable(object):
    def __init__(self):
        self.table = [None]*10000

    def store(self, string):
        """Input a string that's stored in 
        the table."""
        index = self.calculate_hash_value(string)
        self.table[index] = string
        # pass

    def lookup(self, string):
        index = self.calculate_hash_value(string)
        return self.table[index]
        """Return the hash value if the
        string is already in the table.
        Return -1 otherwise."""
        # return -1

    def calculate_hash_value(self, string):
        # Hash Value = (ASCII Value of First Letter * 100) + ASCII Value of Second Letter
        return ord(string[0])*100 + ord(string[1])
        """Helper function to calulate a
        hash value from a string."""
        # return -1
    
# Setup
hash_table = HashTable()

# Test calculate_hash_value
# Should be 8568
print hash_table.calculate_hash_value('UDACITY')

# Test lookup edge case
# Should be -1
print hash_table.lookup('UDACITY')

# Test store
hash_table.store('UDACITY')
# Should be 8568
print hash_table.lookup('UDACITY')

# Test store edge case
hash_table.store('UDACIOUS')
# Should be 8568
print hash_table.lookup('UDACIOUS')


