/*
Copyright 2006, 2007 Kirk McDonald

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
module pyd.ctor_wrap;

import std.traits;
import util.typelist: Join;
import python;
import pyd.class_wrap;
import pyd.exception;
import pyd.func_wrap;
import pyd.make_object;
import pyd.lib_abstract;

template call_ctor(T, init) {
    enum params = getparams!(init.Inner!T.FN);
    alias ParameterIdentifierTuple!(init.Inner!T.FN) paramids;
    mixin(Replace!(q{
    T func($params) {
        return new T($ids);
    }
    },"$params",params, "$ids", Join!(",",paramids)));
}

// The default __init__ method calls the class's zero-argument constructor.
template wrapped_init(T) {
    extern(C)
    int init(PyObject* self, PyObject* args, PyObject* kwds) {
        return exception_catcher({
            WrapPyObject_SetObj(self, new T);
            return 0;
        });
    }
}

// The __init__ slot for wrapped structs. T is of the type of a pointer to the
// struct.
template wrapped_struct_init(T) {
    extern(C)
    int init(PyObject* self, PyObject* args, PyObject* kwds) {
        return exception_catcher({
            static if (is(T S : S*)) {
                pragma(msg, "wrapped_struct_init, S is '" ~ prettynameof!(S) ~ "'");
                T t = new S;
                WrapPyObject_SetObj(self, t);
            }
            return 0;
        });
    }
}

//import std.stdio;
// This template accepts a tuple of function pointer types, which each describe
// a ctor of T, and  uses them to wrap a Python tp_init function.
template wrapped_ctors(T,Shim, C ...) {
    //alias shim_class T;
    alias wrapped_class_object!(T) wrap_object;

    extern(C)
    static int func(PyObject* self, PyObject* args, PyObject* kwargs) {
        Py_ssize_t arglen = PyObject_Length(args);
        Py_ssize_t kwlen = kwargs is null?-1:PyObject_Length(kwargs);
        enforce(arglen != -1);
        Py_ssize_t len = arglen + ((kwlen == -1) ? 0:kwlen);

        return exception_catcher({
            // Default ctor
            static if (is(typeof(new T))) {
                if (len == 0) {
                    WrapPyObject_SetObj(self, new T);
                    return 0;
                }
            }
            // find another Ctor
            foreach(i, init; C) {
                if (supportsNArgs!(init.Inner!T.FN)(len) && 
                    (kwlen <= 0 || hasAllNamedArgs!(init.Inner!T.FN)(arglen,kwargs))) {
                    alias call_ctor!(T, init).func fn;
                    T t = applyPyTupleToAlias!(fn, typeof(&fn))(args, kwargs);
                    if (t is null) {
                        PyErr_SetString(PyExc_RuntimeError, "Class ctor redirect didn't return a class instance!");
                        return -1;
                    }
                    WrapPyObject_SetObj(self, t);
                    return 0;
                }
            }
            // No ctor found
            PyErr_SetString(PyExc_TypeError, "Unsupported number of constructor arguments.");
            return -1;
        });
    }
}

