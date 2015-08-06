#!/usr/bin/env python
import numpy as np
# utils for use with numpy and multidimensional arrays
#def make_1d_view(a):
#    a = np.ascontiguousarray(a)
#    dt = np.dtype((np.void, a.dtype.itemsize * a.shape[1]))
#    return a.view(dt).ravel()

#def f(a, b):
#    return len(np.intersect1d(make_1d_view(a), make_1d_view(b))) != 0

#A=np.array([[2,3,4],[5,6,7],[8,9,10]])
#B=np.array([[5,6,7],[1,3,4]])
#C=np.array([[1,2,3],[6,6,7],[10,8,9]])


#a = np.array([[1, 1, 1, 0, 0, 0],
#              [0, 1, 1, 1, 0, 0],
#              [0, 1, 1, 1, 0, 0],
#              [1, 1, 1, 0, 0, 0],
#              [1, 1, 1, 1, 1, 0]])

#b = np.ascontiguousarray(a).view(np.dtype((np.void, a.dtype.itemsize * a.shape[1])))

#unique_a = np.unique(b).view(a.dtype).reshape(-1, a.shape[1])

def foo(arr1, arr2, func):
    a = np.vstack((arr1, arr2))
    b = np.ascontiguousarray(a).view(np.dtype((np.void, a.dtype.itemsize * a.shape[1])))
    return func(b).view(a.dtype).reshape(-1, a.shape[1])

def foo2(arr1, arr2, func):
    b1 = np.ascontiguousarray(arr1).view(np.dtype((np.void, arr1.dtype.itemsize * arr1.shape[1])))
    b2 = np.ascontiguousarray(arr2).view(np.dtype((np.void, arr2.dtype.itemsize * arr2.shape[1])))
    return func(b1, b2).view(arr1.dtype).reshape(-1, arr1.shape[1])

def union(a, b):
    if a == [] or len(a.shape)<2: return b
    if b == [] or len(b.shape)<2: return a
    return foo2(a, b, np.union1d)
def intersect(a, b):
    if a == [] or len(a.shape) < 2: return []
    if b == [] or len(b.shape) < 2: return []
    return foo2(a, b, np.intersect1d)
def diff(a, b):
    if a == [] or len(a.shape) < 2: return []
    if b == [] or len(b.shape) < 2: return a
    return foo2(a, b, np.setdiff1d)
#print unique_a
#print union(A, B)
#print intersect(A, B)
#print diff(A, B)
