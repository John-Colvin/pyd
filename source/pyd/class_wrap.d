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

/**
  Contains utilities for wrapping D classes.
*/
module pyd.class_wrap;

import deimos.python.Python;

import std.algorithm: countUntil, startsWith;
import std.traits;
import std.conv;
import std.exception: enforce;
import std.functional;
import std.typetuple;
import std.string: format;
import std.typecons: Tuple;
import std.typetuple;
import util.typelist;
import util.replace: Replace;
import util.typeinfo;
import pyd.references;
import pyd.ctor_wrap;
import pyd.def;
import pyd.exception;
import pyd.func_wrap;
import pyd.make_object;
import pyd.make_wrapper;
import pyd.op_wrap;
import pyd.struct_wrap;

PyTypeObject*[ClassInfo] wrapped_classes;
template shim_class(T) {
    PyTypeObject* shim_class;
}

// kill
alias wrapped_class_object(T) = PyObject;

void init_PyTypeObject(T)(ref PyTypeObject tipo) {
    Py_SET_REFCNT(&tipo, 1);
    tipo.tp_dealloc = &wrapped_methods!(T).wrapped_dealloc;
    tipo.tp_new = &wrapped_methods!(T).wrapped_new;
}



// The list of wrapped methods for this class.
template wrapped_method_list(T) {
    PyMethodDef[] wrapped_method_list = [
        { null, null, 0, null }
    ];
}

// The list of wrapped properties for this class.
template wrapped_prop_list(T) {
    static PyGetSetDef[] wrapped_prop_list = [
        { null, null, null, null, null }
    ];
}

//-///////////////////
// STANDARD METHODS //
//-///////////////////

// Various wrapped methods
template wrapped_methods(T) {
    /// The generic "__new__" method
    extern(C)
    PyObject* wrapped_new(PyTypeObject* type, PyObject* args, PyObject* kwds) {
        return type.tp_alloc(type, 0);
    }

    // The generic dealloc method.
    extern(C)
    void wrapped_dealloc(PyObject* self) {
        // EMN: the *&%^^%! generic dealloc method is triggering a call to
        //  *&^%*%(! malloc for that delegate during a @(*$76*&!
        //  garbage collection
        //  Solution: don't use a *&%%^^! delegate in a destructor!
        static struct StackDelegate{
            PyObject* x;
            void dg() {
                remove_pyd_mapping!T(x);
                x.ob_type.tp_free(x);
            }
        }
        StackDelegate x;
        x.x = self;
        exception_catcher_nogc(&x.dg);
    }
}

// why we no use method_wrap ?
template wrapped_repr(T, alias fn) {
    static assert(constCompatible(constness!T, constness!(typeof(fn))),
            format("constness mismatch instance: %s function: %s",
                T.stringof, typeof(fn).stringof));
    alias get_dg = dg_wrapper!(T, typeof(&fn));
    /// The default repr method calls the class's toString.
    extern(C)
    PyObject* repr(PyObject* self) {
        return exception_catcher(delegate PyObject*() {
            auto dg = get_dg(get_d_reference!T(self), &fn);
            return d_to_python(dg());
        });
    }
}

private alias Id(A) = A;

private struct Pack(A...) {
    alias expand = A;
}

enum bool isProperty(alias T) =
        !!(functionAttributes!(T) & FunctionAttribute.property);

enum bool isGetter(alias T) = ParameterTypeTuple!T.length == 0
     && !is(ReturnType!T == void);

// Should this check for implicit conversions? A setter doesn't have to
// take a specific type
template isSetter(RT) {
        enum bool isSetter(alias T) = ParameterTypeTuple!T.length == 1
            && is(ParameterTypeTuple!(T)[0] == RT);
}

enum bool isAnySetter(alias T) = ParameterTypeTuple!T.length == 1;

// This template gets an alias to a property and derives the types of the
// getter form and the setter form. It requires that the getter form return the
// same type that the setter form accepts.
struct property_parts(alias p, string _mode) {
    alias Parent = Id!(__traits(parent, p));
    enum nom = __traits(identifier, p);
    alias Overloads = TypeTuple!(__traits(getOverloads, Parent, nom));
    static if(_mode == "" || countUntil(_mode, "r") != -1) {
        alias Getters = Filter!(isGetter, Overloads);
        static if(_mode == "" && Getters.length == 0) {
            enum isgproperty = false;
            enum rmode = "";
        }else {
            import std.string;
            static assert(Getters.length != 0,
                    format!("can't find property %s.%s getter",
                        Parent.stringof, nom));
            static assert(Getters.length == 1,
                    format!("can't handle property overloads of %s.%s getter (types %s)",
                        Parent.stringof, nom, staticMap!(ReturnType,Getters).stringof));
            alias GetterFn = Getters[0];
            alias getter_type = typeof(&GetterFn) ;
            enum isgproperty = isProperty!GetterFn;
            enum rmode = "r";
        }
    }else {
        enum isgproperty = false;
        enum rmode = "";
    }
    //enum bool pred1 = _mode == "" || countUntil(_mode, "w") != -1;
    static if(_mode == "" || countUntil(_mode, "w") != -1) {
        static if(rmode == "r") {
            alias Setters = Filter!(isSetter!(ReturnType!getter_type), Overloads);
        }else {
            alias Setters = Filter!(isAnySetter, Overloads);
        }

        //enum bool pred2 = _mode == "" && Setters.length == 0;
        static if(_mode == "" && Setters.length == 0) {
            enum bool issproperty = false;
            enum string wmode = "";
        }else{
            static assert(Setters.length != 0, format("can't find property %s.%s setter", Parent.stringof, nom));
            static assert(Setters.length == 1,
                format("can't handle property overloads of %s.%s setter %s",
                    Parent.stringof, nom, Setters.stringof));
            alias SetterFn = Setters[0];
            alias setter_type = typeof(&SetterFn) ;
            static if(rmode == "r") {
                static assert(!(isProperty!GetterFn ^ isProperty!(Setters[0])),
                        format("%s.%s: getter and setter must both be @property or not @property",
                            Parent.stringof, nom));
            }
            enum issproperty = isProperty!SetterFn;
            enum wmode = "w";
        }
    }else{
        enum issproperty = false;
        enum wmode = "";
    }

    static if(rmode != "") {
        alias Type = ReturnType!(GetterFn);
    }else static if(wmode != "") {
        alias Type = ParameterTypeTuple!(SetterFn)[0];
    }

    enum mode = rmode ~ wmode;
    enum bool isproperty = isgproperty || issproperty;
}

//
template wrapped_get(string fname, T, Parts) {
    // A generic wrapper around a "getter" property.
    extern(C)
    PyObject* func(PyObject* self, void* closure) {
        // method_wrap already catches exceptions
        return method_wrap!(T, Parts.GetterFn, fname).func(self, null, null);
    }
}

//
template wrapped_set(string fname, T, Parts) {
    // A generic wrapper around a "setter" property.
    extern(C)
    int func(PyObject* self, PyObject* value, void* closure) {
        PyObject* temp_tuple = PyTuple_New(1);
        if (temp_tuple is null) return -1;
        scope(exit) Py_DECREF(temp_tuple);
        Py_INCREF(value);
        PyTuple_SetItem(temp_tuple, 0, value);
        PyObject* res = method_wrap!(T, Parts.SetterFn, fname).func(self, temp_tuple, null);
        // If we get something back, we need to DECREF it.
        if (res) Py_DECREF(res);
        // If we don't, propagate the exception
        else return -1;
        // Otherwise, all is well.
        return 0;
    }
}

//-///////////////////////////
// CLASS WRAPPING INTERFACE //
//-///////////////////////////

//enum ParamType { Def, StaticDef, Property, Init, Parent, Hide, Iter, AltIter }
struct DoNothing {
    static void call(string classname, T) () {}
}

/**
Wraps a member function of the class.

Supports default arguments, typesafe variadic arguments, and python's
keyword arguments.

Params:
fn = The member function to wrap.
Options = Optional parameters. Takes Docstring!(docstring), PyName!(pyname),
and fn_t.
fn_t = The type of the function. It is only useful to specify this
       if more than one function has the same name as this one.
pyname = The name of the function as it will appear in Python. Defaults to
fn's name in D
docstring = The function's docstring. Defaults to "".
*/
struct Def(alias fn, Options...) {
    alias args = Args!("","", __traits(identifier,fn), "",Options);
    static if(args.rem.length) {
        alias fn_t = args.rem[0];
    }else {
        alias fn_t = typeof(&fn) ;
    }
    mixin _Def!(fn, args.pyname, fn_t, args.docstring);
}

template _Def(alias _fn, string name, fn_t, string docstring) {
    alias func = def_selector!(_fn,fn_t).FN;
    static assert(!__traits(isStaticFunction, func)); // TODO
    static assert((functionAttributes!fn_t & (
                    FunctionAttribute.nothrow_|
                    FunctionAttribute.pure_|
                    FunctionAttribute.trusted|
                    FunctionAttribute.safe)) == 0,
            "pyd currently does not support pure, nothrow, @trusted, or @safe member functions");
    alias /*StripSafeTrusted!*/func_t = fn_t;
    enum realname = __traits(identifier,func);
    enum funcname = name;
    enum min_args = minArgs!(func);
    enum bool needs_shim = false;

    static void call(string classname, T) () {
        alias cT = ApplyConstness!(T, constness!(typeof(func)));
        static PyMethodDef empty = { null, null, 0, null };
        alias list = wrapped_method_list!(T);
        list[$-1].ml_name = (name ~ "\0").ptr;
        list[$-1].ml_meth = cast(PyCFunction) &method_wrap!(cT, func, classname ~ "." ~ name).func;
        list[$-1].ml_flags = METH_VARARGS | METH_KEYWORDS;
        list[$-1].ml_doc = (docstring~"\0").ptr;
        list ~= empty;
        // It's possible that appending the empty item invalidated the
        // pointer in the type struct, so we renew it here.
        PydTypeObject!(T).tp_methods = list.ptr;
    }
    template shim(size_t i, T) {
        enum shim = Replace!(q{
            alias Params[$i] __pyd_p$i;
            $override ReturnType!(__pyd_p$i.func_t) $realname(ParameterTypeTuple!(__pyd_p$i.func_t) t) $attrs {
                return __pyd_get_overload!("$realname", __pyd_p$i.func_t).func!(ParameterTypeTuple!(__pyd_p$i.func_t))("$name", t);
            }
            alias T.$realname $realname;
        }, "$i",i,"$realname",realname, "$name", name,
        "$attrs", attrs_to_string(functionAttributes!func_t) ~ " " ~ tattrs_to_string!(func_t)(),
        "$override",
        // todo: figure out what's going on here
        (variadicFunctionStyle!func == Variadic.no ? "override":""));
    }
}

/**
Wraps a static member function of the class. Similar to pyd.def.def

Supports default arguments, typesafe variadic arguments, and python's
keyword arguments.

Params:
fn = The member function to wrap.
Options = Optional parameters. Takes Docstring!(docstring), PyName!(pyname),
and fn_t
fn_t = The type of the function. It is only useful to specify this
       if more than one function has the same name as this one.
pyname = The name of the function as it will appear in Python. Defaults to fn's
name in D.
docstring = The function's docstring. Defaults to "".
*/
struct StaticDef(alias fn, Options...) {
    alias args = Args!("","", __traits(identifier,fn), "",Options);
    static if(args.rem.length) {
        alias fn_t = args.rem[0];
    }else {
        alias fn_t = typeof(&fn) ;
    }
    mixin _StaticDef!(fn, args.pyname, fn_t, args.docstring);
}

mixin template _StaticDef(alias fn, string name, fn_t, string docstring) {
    alias func = def_selector!(fn,fn_t).FN;
    static assert(__traits(isStaticFunction, func)); // TODO
    alias /*StripSafeTrusted!*/func_t = fn_t;
    enum funcname = name;
    enum bool needs_shim = false;
    static void call(string classname, T) () {
        //pragma(msg, "class.static_def: " ~ name);
        static PyMethodDef empty = { null, null, 0, null };
        alias list = wrapped_method_list!(T);
        list[$-1].ml_name = (name ~ "\0").ptr;
        list[$-1].ml_meth = cast(PyCFunction) &function_wrap!(func, classname ~ "." ~ name).func;
        list[$-1].ml_flags = METH_VARARGS | METH_STATIC | METH_KEYWORDS;
        list[$-1].ml_doc = (docstring~"\0").ptr;
        list ~= empty;
        PydTypeObject!(T).tp_methods = list.ptr;
    }
    template shim(size_t i,T) {
        enum shim = "";
    }
}

/**
Wraps a property of the class.

Params:
fn = The property to wrap.
Options = Optional parameters. Takes Docstring!(docstring), PyName!(pyname),
and Mode!(mode)
pyname = The name of the property as it will appear in Python. Defaults to
fn's name in D.
mode = specifies whether this property is readable, writable. possible values
are "r", "w", "rw", and "" (in the latter case, automatically determine which
mode to use based on availability of getter and setter forms of fn). Defaults
to "".
docstring = The function's docstring. Defaults to "".
*/
struct Property(alias fn, Options...) {
    alias args = Args!("","", __traits(identifier,fn), "",Options);
    static assert(args.rem.length == 0, "Propery takes no other parameter");
    mixin _Property!(fn, args.pyname, args.mode, args.docstring);
}

template _Property(alias fn, string pyname, string _mode, string docstring) {
    alias parts = property_parts!(fn, _mode);
    //pragma(msg, "property: ", parts.nom);
    static if(parts.isproperty) {
        mixin _Member!(parts.nom, pyname, parts.mode, docstring, parts);

        template shim(size_t i, T) {
            enum shim = "";
        }
    }else {
        static if(countUntil(parts.mode,"r") != -1) {
            alias get_t = parts.getter_type;
        }
        static if(countUntil(parts.mode,"w") != -1) {
            alias set_t = parts.setter_type;
        }
        enum realname = __traits(identifier, fn);
        enum funcname = pyname;
        enum bool needs_shim = false;
        static void call(string classname, T) () {
            static PyGetSetDef empty = { null, null, null, null, null };
            wrapped_prop_list!(T)[$-1].name = (pyname ~ "\0").dup.ptr;
            static if (countUntil(parts.mode, "r") != -1) {
                alias cT_g = ApplyConstness!(T, constness!(typeof(parts.GetterFn)));
                wrapped_prop_list!(T)[$-1].get =
                    &wrapped_get!(classname ~ "." ~ pyname, cT_g, parts).func;
            }
            static if (countUntil(parts.mode, "w") != -1) {
                alias cT_s = ApplyConstness!(T, constness!(typeof(parts.SetterFn)));
                wrapped_prop_list!(T)[$-1].set =
                    &wrapped_set!(classname ~ "." ~ pyname,cT_s, parts).func;
            }
            wrapped_prop_list!(T)[$-1].doc = (docstring~"\0").dup.ptr;
            wrapped_prop_list!(T)[$-1].closure = null;
            wrapped_prop_list!(T) ~= empty;
            // It's possible that appending the empty item invalidated the
            // pointer in the type struct, so we renew it here.
            PydTypeObject!(T).tp_getset =
                wrapped_prop_list!(T).ptr;
        }
        template shim(size_t i, T) {
            static if(countUntil(parts.mode, "r") != -1) {
                enum getter = Replace!(q{
                override ReturnType!(__pyd_p$i.get_t) $realname() {
                    return __pyd_get_overload!("$realname", __pyd_p$i.get_t).func("$name");
                }
                } , "$i",i,"$realname",realname, "$name", pyname);
            }else{
                enum getter = "";
            }
            static if(countUntil(parts.mode, "w") != -1) {
                enum setter = Replace!(q{
                override ReturnType!(__pyd_p$i.set_t) $realname(ParameterTypeTuple!(__pyd_p$i.set_t) t) {
                    return __pyd_get_overload!("$realname", __pyd_p$i.set_t).func("$name", t);
                }
                }, "$i", i, "$realname",realname, "$name", pyname);
            }else {
                enum setter = "";
            }
            enum shim = Replace!(q{
                alias Params[$i] __pyd_p$i;
                $getter
                $setter;
            }, "$i",i, "$getter", getter, "$setter",setter);
        }
    }
}

/**
Wraps a method as the class's ___repr__ in Python.

Params:
fn = The property to wrap. Must have the signature string function().
*/
struct Repr(alias _fn) {
    alias fn = def_selector!(_fn, string function()).FN;
    enum bool needs_shim = false;
    static void call(string classname, T)() {
        alias cT = ApplyConstness!(T, constness!(typeof(fn)));
        alias type = PydTypeObject!(T);
        type.tp_repr = &wrapped_repr!(cT, fn).repr;
    }
    template shim(size_t i,T) {
        enum shim = "";
    }
}

/**
Wraps the constructors of the class.

This template takes a single specialization of the ctor template
(see ctor_wrap.d), which describes a constructor that the class
supports. The default constructor need not be
specified, and will always be available if the class supports it.

Supports default arguments, typesafe variadic arguments, and python's
keyword arguments.

Params:
    cps = Parameter list of the constructor to be wrapped.

Bugs:
This currently does not support having multiple constructors with
the same number of arguments.
*/
struct Init(cps ...) {
    alias CtorParams = cps;
    enum bool needs_shim = false;
    template Inner(T) {
        alias BaseT = NewParamT!T;
        alias Overloads = TypeTuple!(__traits(getOverloads, BaseT, "__ctor"));
        template isDesired(alias ctor) {
            alias ps = ParameterTypeTuple!ctor;
            enum bool isDesired = is(ps == CtorParams);
        }
        alias VOverloads = Filter!(isDesired, Overloads);
        static assert(VOverloads.length != 0,
                format("%s: Cannot find constructor with params %s",
                    T.stringof, CtorParams.stringof));
        alias FN = VOverloads[0];

        alias Pt = ParameterTypeTuple!FN;
        alias Pd = ParameterDefaultValueTuple!FN;
    }
    static void call(string classname, T)() {
    }
    template shim(size_t i, T) {
        enum params = getparams!(Inner!T.FN,
                format("__pyd_p%s.Inner!T.Pt",i),
                format("__pyd_p%s.Inner!T.Pd",i));
        alias paramids = ParameterIdentifierTuple!(Inner!T.FN);
        enum shim = Replace!(q{
            alias Params[$i] __pyd_p$i;
            this($params) {
                super($ids);
            }
        }, "$i", i, "$params", params, "$ids", Join!(",", paramids));
    }
}

enum bool isInit(T) = __traits(hasMember, T, "CtorParams");

enum binaryslots = [
    "+": "type.tp_as_number.nb_add",
    "+=": "type.tp_as_number.nb_inplace_add",
    "-": "type.tp_as_number.nb_subtract",
    "-=": "type.tp_as_number.nb_inplace_subtract",
    "*": "type.tp_as_number.nb_multiply",
    "*=": "type.tp_as_number.nb_inplace_multiply",
    "/": "type.tp_as_number.nb_divide",
    "/=": "type.tp_as_number.nb_inplace_divide",
    "%": "type.tp_as_number.nb_remainder",
    "%=": "type.tp_as_number.nb_inplace_remainder",
    "^^": "type.tp_as_number.nb_power",
    "^^=": "type.tp_as_number.nb_inplace_power",
    "<<": "type.tp_as_number.nb_lshift",
    "<<=": "type.tp_as_number.nb_inplace_lshift",
    ">>": "type.tp_as_number.nb_rshift",
    ">>=": "type.tp_as_number.nb_inplace_rshift",
    "&": "type.tp_as_number.nb_and",
    "&=": "type.tp_as_number.nb_inplace_and",
    "^": "type.tp_as_number.nb_xor",
    "^=": "type.tp_as_number.nb_inplace_xor",
    "|": "type.tp_as_number.nb_or",
    "|=": "type.tp_as_number.nb_inplace_or",
    "~": "type.tp_as_sequence.sq_concat",
    "~=": "type.tp_as_sequence.sq_inplace_concat",
    "in": "type.tp_as_sequence.sq_contains",
];

bool isPyBinary(string op) {
    foreach(_op, slot; binaryslots) {
        if (op[$-1] != '=' && op == _op) return true;
    }
    return false;
}
bool isPyAsg(string op0) {
    auto op = op0~"=";
    foreach(_op, slot; binaryslots) {
        if (op == _op) return true;
    }
    return false;
}

enum unaryslots = [
    "+": "type.tp_as_number.nb_positive",
    "-": "type.tp_as_number.nb_negative",
    "~": "type.tp_as_number.nb_invert",
];

bool isPyUnary(string op) {
    foreach(_op, slot; unaryslots) {
        if(op == _op) return true;
    }
    return false;
}

// string mixin to initialize tp_as_number or tp_as_sequence or tp_as_mapping
// if necessary. Scope mixed in must have these variables:
//  slot: a value from binaryslots or unaryslots
//  type: a PyObjectType.
string autoInitializeMethods() {
    return q{
        static if(countUntil(slot, "tp_as_number") != -1) {
            if(type.tp_as_number is null)
                type.tp_as_number = new PyNumberMethods;
        }else static if(countUntil(slot, "tp_as_sequence") != -1) {
            if(type.tp_as_sequence is null)
                type.tp_as_sequence = new PySequenceMethods;
        }else static if(countUntil(slot, "tp_as_mapping") != -1) {
            if(type.tp_as_mapping is null)
                type.tp_as_mapping = new PyMappingMethods;
        }
    };
}

private struct Guess{}

struct BinaryOperatorX(string _op, bool isR, rhs_t) {
    enum op = _op;
    enum isRight = isR;

    static if(isR) enum nom = "opBinaryRight";
    else enum nom = "opBinary";

    enum bool needs_shim = false;

    template Inner(C) {
        enum fn_str1 = "Alias!(C."~nom~"!(op))";
        enum fn_str2 = "C."~nom~"!(op,rhs_t)";
        enum string OP = op;
        static if(!__traits(hasMember, C, nom)) {
            static assert(0, C.stringof ~ " has no "~(isR ?"reflected ":"")~
                    "binary operator overloads");
        }
        template Alias(alias fn) {
            alias Alias = fn;
        }
        static if(is(typeof(mixin(fn_str1)) == function)) {
            alias RHS_T = ParameterTypeTuple!(typeof(mixin(fn_str1)))[0];
            alias RET_T = ReturnType!(typeof(mixin(fn_str1)));
            mixin("alias " ~ fn_str1 ~ " FN;");
            static if(!is(rhs_t == Guess))
                static assert(is(RHS_T == rhs_t),
                        format("expected typeof(rhs) = %s, found %s",
                            rhs.stringof, RHS_T.stringof));
        }else static if(is(rhs_t == Guess)) {
            static assert(false,
                    format("Operator %s: Cannot determine type of rhs", op));
        } else static if(is(typeof(mixin(fn_str2)) == function)) {
            alias RHS_T = rhs_t;
            alias RET_T = ReturnType!(typeof(mixin(fn_str2)));
            mixin("alias "~fn_str2~" FN;");
        } else static assert(false, "Cannot get operator overload");
    }

    static void call(string classname, T)() {
        // can't handle __op__ __rop__ pairs here
    }

    template shim(size_t i, T) {
        // bah
        enum shim = "";
    }
}

/**
Wrap a binary operator overload.

Example:
---
class Foo{
    int _j;
    int opBinary(string op)(int i) if(op == "+"){
        return i+_j;
    }
    int opBinaryRight(string op)(int i) if(op == "+"){
        return i+_j;
    }
}

class_wrap!(Foo,
    OpBinary!("+"),
    OpBinaryRight!("+"));
---

Params:
    op = Operator to wrap
    rhs_t = (optional) Type of opBinary's parameter for disambiguation if
    there are multiple overloads.
Bugs:
    Issue 8602 prevents disambiguation for case X opBinary(string op, T)(T t);
  */
template OpBinary(string op, rhs_t = Guess) if(isPyBinary(op) && op != "in"){
    alias OpBinary = BinaryOperatorX!(op, false, rhs_t);
}

/// ditto
template OpBinaryRight(string op, lhs_t = Guess) if(isPyBinary(op)) {
    alias OpBinaryRight = BinaryOperatorX!(op, true, lhs_t);
}

/**
  Wrap a unary operator overload.
*/
struct OpUnary(string _op) if(isPyUnary(_op)) {
    enum op = _op;
    enum bool needs_shim = false;

    template Inner(C) {
        enum string OP = op;
        static if(!__traits(hasMember, C, "opUnary")) {
            static assert(0, C.stringof ~ " has no unary operator overloads");
        }
        static if(is(typeof(C.init.opUnary!(op)) == function)) {
            alias RET_T = ReturnType!(C.opUnary!(op));
            alias FN = C.opUnary!(op);
        } else static assert(false, "Cannot get operator overload");
    }
    static void call(string classname, T)() {
        alias type = PydTypeObject!T;
        enum slot = unaryslots[op];
        mixin(autoInitializeMethods());
        mixin(slot ~ " = &opfunc_unary_wrap!(T, Inner!T .FN).func;");
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

/**
  Wrap an operator assignment overload.

Example:
---
class Foo{
    int _j;
    void opOpAssign(string op)(int i) if(op == "+"){
        _j = i;
    }
}

class_wrap!(Foo,
    OpAssign!("+"));
---
Params:
    op = Base operator to wrap
    rhs_t = (optional) Type of opOpAssign's parameter for disambiguation if
    there are multiple overloads.
*/
struct OpAssign(string _op, rhs_t = Guess) if(isPyAsg(_op)) {
    enum op = _op~"=";

    enum bool needs_shim = false;

    template Inner(C) {
        enum string OP = op;
        static if(!__traits(hasMember, C, "opOpAssign")) {
            static assert(0, C.stringof ~ " has no operator assignment overloads");
        }
        static if(is(typeof(C.init.opOpAssign!(_op)) == function)) {
            alias RHS_T = ParameterTypeTuple!(typeof(C.opOpAssign!(_op)))[0];
            alias RET_T = ReturnType!(typeof(C.opOpAssign!(_op)));
            alias FN = C.opOpAssign!(_op);
            static if(!is(rhs_t == Guess))
                static assert(is(RHS_T == rhs_t),
                        format("expected typeof(rhs) = %s, found %s",
                            rhs.stringof, RHS_T.stringof));
        }else static if(is(rhs_t == Guess)) {
            static assert(false, "Cannot determine type of rhs");
        } else static if(is(typeof(C.opOpAssign!(_op,rhs_t)) == function)) {
            alias RHS_T = rhs_t;
            alias RET_T = ReturnType!(typeof(C.opOpAssign!(_op,rhs_t)));
            alias FN = C.opOpAssign!(_op,rhs_t);
        } else static assert(false, "Cannot get operator assignment overload");
    }
    static void call(string classname, T)() {
        alias type = PydTypeObject!T;
        enum slot = binaryslots[op];
        mixin(autoInitializeMethods());
        alias OpAsg = Pack!(TypeTuple!(OpAssign));
        alias Nop = Pack!(TypeTuple!());
        static if(op == "^^=")
            mixin(slot ~ " = &powopasg_wrap!(T, Inner!T.FN).func;");
        else
            mixin(slot ~ " = &binopasg_wrap!(T, Inner!T.FN).func;");
    }

    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

// struct types could probably take any parameter type
// class types must take Object
/**
  Wrap opCmp.

Params:
    rhs_t = (optional) Type of opCmp's parameter for disambiguation if there
    are multiple overloads (for classes it will always be Object).
  */
struct OpCompare(_rhs_t = Guess) {
    enum bool needs_shim = false;


    template Inner(C) {
        static if(is(_rhs_t == Guess) && is(C == class)) {
            alias rhs_t = Object;
        }else {
            alias rhs_t = _rhs_t;
        }
        static if(!__traits(hasMember, C, "opCmp")) {
            static assert(0, C.stringof ~ " has no comparison operator overloads");
        }
        static if(!is(typeof(C.init.opCmp) == function)) {
            static assert(0, format("why is %s.opCmp not a function?",C));
        }
        alias Overloads = TypeTuple!(__traits(getOverloads, C, "opCmp"));
        static if(is(rhs_t == Guess) && Overloads.length > 1) {
            static assert(0, format("Cannot choose between %s", Overloads));
        }else static if(Overloads.length == 1) {
            static if(!is(rhs_t == Guess) &&
                !is(ParameterTypeTuple!(Overloads[0])[0] == rhs_t)) {
                static assert(0, format("%s.opCmp: expected param %s, got %s",
                            C, rhs_t, ParameterTypeTuple!(Overloads[0])));
            }else{
                alias FN = Overloads[0];
            }
        }else{
            template isDesiredOverload(alias fn) {
                enum bool isDesiredOverload = is(ParameterTypeTuple!(fn)[0] == rhs_t);
            }
            alias Overloads1 = Filter!(isDesiredOverload, Overloads);
            static assert(Overloads1.length == 1,
                    format("Cannot choose between %s", Overloads1));
            alias FN = Overloads1[0];
        }
    }
    static void call(string classname, T)() {
        alias type = PydTypeObject!T;
        alias cT = ApplyConstness!(T, constness!(typeof(Inner!T.FN)));
        type.tp_richcompare = &rich_opcmp_wrap!(cT, Inner!T.FN).func;
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

/**
  Wrap opIndex, opIndexAssign.

Params:
    index_t = (optional) Types of opIndex's parameters for disambiguation if
    there are multiple overloads.
*/
struct OpIndex(index_t...) {
    enum bool needs_shim = false;
    template Inner(C) {
        static if(!__traits(hasMember, C, "opIndex")) {
            static assert(0, C.stringof ~ " has no index operator overloads");
        }
        static if(is(typeof(C.init.opIndex) == function)) {
            alias Overloads = TypeTuple!(__traits(getOverloads, C, "opIndex"));
            static if(index_t.length == 0 && Overloads.length > 1) {
                static assert(0,
                        format("%s.opIndex: Cannot choose between %s",
                            C.stringof,Overloads.stringof));
            }else static if(index_t.length == 0) {
                alias FN = Overloads[0];
            }else{
                template isDesiredOverload(alias fn) {
                    enum bool isDesiredOverload = is(ParameterTypeTuple!fn == index_t);
                }
                alias Overloads1 = Filter!(isDesiredOverload, Overloads);
                static assert(Overloads1.length == 1,
                        format("%s.opIndex: Cannot choose between %s",
                            C.stringof,Overloads1.stringof));
                alias FN = Overloads1[0];
            }
        }else static if(is(typeof(C.init.opIndex!(index_t)) == function)) {
            alias FN = C.opIndex!(index_t);
        }else{
            static assert(0,
                    format("cannot get a handle on %s.opIndex", C.stringof));
        }
    }
    static void call(string classname, T)() {
        /*
        alias PydTypeObject!T type;
        enum slot = "type.tp_as_mapping.mp_subscript";
        mixin(autoInitializeMethods());
        mixin(slot ~ " = &opindex_wrap!(T, Inner!T.FN).func;");
        */
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

/// ditto
struct OpIndexAssign(index_t...) {
    static assert(index_t.length != 1,
            "opIndexAssign must have at least 2 parameters");
    enum bool needs_shim = false;
    template Inner(C) {
        static if(!__traits(hasMember, C, "opIndexAssign")) {
            static assert(0, C.stringof ~ " has no index operator overloads");
        }
        static if(is(typeof(C.init.opIndex) == function)) {
            alias Overloads = TypeTuple!(__traits(getOverloads, C, "opIndexAssign"));
            template isValidOverload(alias fn) {
                enum bool isValidOverload = ParameterTypeTuple!fn.length >= 2;
            }
            alias VOverloads = Filter!(isValidOverload, Overloads);
            static if(VOverloads.length == 0 && Overloads.length != 0)
                static assert(0,
                        "opIndexAssign must have at least 2 parameters");
            static if(index_t.length == 0 && VOverloads.length > 1) {
                static assert(0,
                        format("%s.opIndexAssign: Cannot choose between %s",
                            C.stringof,VOverloads.stringof));
            }else static if(index_t.length == 0) {
                alias FN = VOverloads[0];
            }else{
                template isDesiredOverload(alias fn) {
                    enum bool isDesiredOverload = is(ParameterTypeTuple!fn == index_t);
                }
                alias Overloads1 = Filter!(isDesiredOverload, VOverloads);
                static assert(Overloads1.length == 1,
                        format("%s.opIndex: Cannot choose between %s",
                            C.stringof,Overloads1.stringof));
                alias FN = Overloads1[0];
            }
        }else static if(is(typeof(C.init.opIndexAssign!(index_t)) == function)) {
            alias FN = C.opIndexAssign!(index_t);
        }else{
            static assert(0,
                    format("cannot get a handle on %s.opIndexAssign", C.stringof));
        }
    }
    static void call(string classname, T)() {
        /*
        alias PydTypeObject!T type;
        enum slot = "type.tp_as_mapping.mp_ass_subscript";
        mixin(autoInitializeMethods());
        mixin(slot ~ " = &opindexassign_wrap!(T, Inner!T.FN).func;");
        */
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

/**
  Wrap opSlice.

  Requires signature
---
Foo.opSlice(Py_ssize_t, Py_ssize_t);
---
 This is a limitation of the C/Python API.
  */
struct OpSlice() {
    enum bool needs_shim = false;
    template Inner(C) {
        static if(!__traits(hasMember, C, "opSlice")) {
            static assert(0, C.stringof ~ " has no slice operator overloads");
        }
        static if(is(typeof(C.init.opSlice) == function)) {
            alias Overloads = TypeTuple!(__traits(getOverloads, C, "opSlice"));
            template isDesiredOverload(alias fn) {
                enum bool isDesiredOverload = is(ParameterTypeTuple!fn ==
                        TypeTuple!(Py_ssize_t,Py_ssize_t));
            }
            alias Overloads1 = Filter!(isDesiredOverload, Overloads);
            static assert(Overloads1.length != 0,
                    format("%s.opSlice: must have overload %s",
                        C.stringof,TypeTuple!(Py_ssize_t,Py_ssize_t).stringof));
            static assert(Overloads1.length == 1,
                    format("%s.opSlice: cannot choose between %s",
                        C.stringof,Overloads1.stringof));
            alias FN = Overloads1[0];
        }else{
            static assert(0, format("cannot get a handle on %s.opSlice",
                        C.stringof));
        }
    }
    static void call(string classname, T)() {
        /*
        alias PydTypeObject!T type;
        enum slot = "type.tp_as_sequence.sq_slice";
        mixin(autoInitializeMethods());
        mixin(slot ~ " = &opslice_wrap!(T, Inner!T.FN).func;");
        */
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

/**
  Wrap opSliceAssign.

  Requires signature
---
Foo.opSliceAssign(Value,Py_ssize_t, Py_ssize_t);
---
 This is a limitation of the C/Python API.
  */
struct OpSliceAssign(rhs_t = Guess) {
    enum bool needs_shim = false;
    template Inner(C) {
        static if(!__traits(hasMember, C, "opSliceAssign")) {
            static assert(0, C.stringof ~ " has no slice assignment operator overloads");
        }
        static if(is(typeof(C.init.opSliceAssign) == function)) {
            alias Overloads = TypeTuple!(__traits(getOverloads, C, "opSliceAssign"));
            template isDesiredOverload(alias fn) {
                alias ps = ParameterTypeTuple!fn;
                enum bool isDesiredOverload =
                    is(ps[1..3] == TypeTuple!(Py_ssize_t,Py_ssize_t));
            }
            alias Overloads1 = Filter!(isDesiredOverload, Overloads);
            static assert(Overloads1.length != 0,
                    format("%s.opSliceAssign: must have overload %s",
                        C.stringof,TypeTuple!(Guess,Py_ssize_t,Py_ssize_t).stringof));
            static if(is(rhs_t == Guess)) {
                static assert(Overloads1.length == 1,
                        format("%s.opSliceAssign: cannot choose between %s",
                            C.stringof,Overloads1.stringof));
                alias FN = Overloads1[0];
            }else{
                template isDesiredOverload2(alias fn) {
                    alias ps = ParameterTypeTuple!fn;
                    enum bool isDesiredOverload2 = is(ps[0] == rhs_t);
                }
                alias Overloads2 = Filter!(isDesiredOverload2, Overloads1);
                static assert(Overloads2.length == 1,
                        format("%s.opSliceAssign: cannot choose between %s",
                            C.stringof,Overloads2.stringof));
                alias FN = Overloads2[0];
            }
        }else{
            static assert(0, format("cannot get a handle on %s.opSlice",
                        C.stringof));
        }
    }
    static void call(string classname, T)() {
        /*
        alias PydTypeObject!T type;
        enum slot = "type.tp_as_sequence.sq_ass_slice";
        mixin(autoInitializeMethods());
        mixin(slot ~ " = &opsliceassign_wrap!(T, Inner!T.FN).func;");
        */
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

/**
  wrap opCall. The parameter types of opCall must be specified.
*/
struct OpCall(Args_t...) {
    enum bool needs_shim = false;

    template Inner(T) {
        alias Overloads = TypeTuple!(__traits(getOverloads, T, "opCall"));
        template isDesiredOverload(alias fn) {
            alias ps = ParameterTypeTuple!fn;
            enum bool isDesiredOverload = is(ps == Args_t);
        }
        alias VOverloads = Filter!(isDesiredOverload, Overloads);
        static if(VOverloads.length == 0) {
            static assert(0,
                    format("%s.opCall: cannot find signature %s", T.stringof,
                        Args_t.stringof));
        }else static if(VOverloads.length == 1){
            alias FN = VOverloads[0];
        }else static assert(0,
                format("%s.%s: cannot choose between %s", T.stringof, nom,
                    VOverloads.stringof));
    }
    static void call(string classname, T)() {
        alias type = PydTypeObject!T;
        alias fn = Inner!T.FN;
        alias cT = ApplyConstness!(T, constness!(typeof(fn)));
        type.tp_call = &opcall_wrap!(cT, fn).func;
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}

/**
  Wraps Foo.length or another function as python's ___len__ function.

  Requires signature
---
Py_ssize_t length();
---
  This is a limitation of the C/Python API.
  */
template Len() {
    alias Len = _Len!();
}

/// ditto
template Len(alias fn) {
    alias Len = _Len!(fn);
}

struct _Len(fnt...) {
    enum bool needs_shim = false;
    template Inner(T) {
        static if(fnt.length == 0) {
            enum nom = "length";
        }else{
            enum nom = __traits(identifier, fnt[0]);
        }
        alias Overloads = TypeTuple!(__traits(getOverloads, T, nom));
        template isDesiredOverload(alias fn) {
            alias ps = ParameterTypeTuple!fn;
            alias rt = ReturnType!fn;
            enum bool isDesiredOverload = isImplicitlyConvertible!(rt,Py_ssize_t) && ps.length == 0;
        }
        alias VOverloads = Filter!(isDesiredOverload, Overloads);
        static if(VOverloads.length == 0 && Overloads.length != 0) {
            static assert(0,
                    format("%s.%s must have signature %s", T.stringof, nom,
                        (Py_ssize_t function()).stringof));
        }else static if(VOverloads.length == 1){
            alias FN = VOverloads[0];
        }else static assert(0,
                format("%s.%s: cannot choose between %s", T.stringof, nom,
                    VOverloads.stringof));
    }
    static void call(string classname, T)() {
        alias type = PydTypeObject!T;
        enum slot = "type.tp_as_sequence.sq_length";
        mixin(autoInitializeMethods());
        mixin(slot ~ " = &length_wrap!(T, Inner!T.FN).func;");
    }
    template shim(size_t i,T) {
        // bah
        enum shim = "";
    }
}


template param1(C) {
    template param1(T) {alias param1 = ParameterTypeTuple!(T.Inner!C .FN)[0]; }
}

enum isOp(A) = __traits(hasMember, A, "op");
enum isUn(A) = A.stringof.startsWith("OpUnary!");
template isBin(T...) {
    static if(T[0].stringof.startsWith("BinaryOperatorX!"))
        enum bool isBin = !T[0].isRight;
    else
        enum bool isBin = false;
}
template isBinR(T...) {
    static if(T[0].stringof.startsWith("BinaryOperatorX!"))
        enum isBinR = T[0].isRight;
    else
        enum isBinR = false;
}

// handle all operator overloads. Ops must only contain operator overloads.
struct Operators(Ops...) {
    enum bool needs_shim = false;

    template BinOp(string op, T) {
        enum isThisOp(A) = A.op == op;
        alias Ops0 = Filter!(isThisOp, Ops);
        alias OpsL = Filter!(isBin, Ops0);
        alias OpsLparams = staticMap!(param1!T, OpsL);
        static assert(OpsL.length <= 1,
                Replace!("Cannot overload $T1 $OP x with types $T2",
                    "$OP", op, "$T1", T.stringof, "$T2",  OpsLparams.stringof));
        alias OpsR = Filter!(isBinR, Ops0);
        alias OpsRparams = staticMap!(param1, OpsR);
        static assert(OpsR.length <= 1,
                Replace!("Cannot overload x $OP $T1 with types $T2",
                    "$OP", op, "$T1", T.stringof, "$T2",  OpsRparams.stringof));
        static assert(op[$-1] != '=' || OpsR.length == 0,
                "Cannot reflect assignment operator");

        static void call() {
            static if(OpsL.length + OpsR.length != 0) {
                alias type = PydTypeObject!T;
                enum slot = binaryslots[op];
                mixin(autoInitializeMethods());
                static if(op == "in") {
                    mixin(slot ~ " = &inop_wrap!(T, Pack!OpsL, Pack!OpsR).func;");
                }else static if(op == "^^" || op == "^^=") {
                    mixin(slot ~ " = &powop_wrap!(T, Pack!OpsL, Pack!OpsR).func;");
                }else {
                    mixin(slot ~ " = &binop_wrap!(T, Pack!OpsL, Pack!OpsR).func;");
                }
            }
        }

    }
    struct UnOp(string op, T) {
        enum isThisOp(A) = A.op == op;
        alias Ops1 = Filter!(isUn, Filter!(isThisOp, Ops));
        static assert(Ops1.length <= 1,
                Replace!("Cannot have overloads of $OP$T1",
                    "$OP", op, "$T1", T.stringof));
        static void call() {
            static if(Ops1.length != 0) {
                alias type = PydTypeObject!T;
                alias Ops1_0 = Ops1[0];
                alias fn = Ops1_0.Inner!T.FN;
                enum slot = unaryslots[op];
                mixin(autoInitializeMethods());
                mixin(slot ~ " = &opfunc_unary_wrap!(T, fn).func;");
            }
        }
    }

    static void call(T)() {
        enum GetOp(A) = A.op;
        alias str_op_tuple = NoDuplicates!(staticMap!(GetOp, Ops));
        enum binops = binaryslots.keys();
        foreach(_op; str_op_tuple) {
            BinOp!(_op, T).call(); // noop if op is unary
            UnOp!(_op, T).call(); // noop if op is binary
        }
    }
}

struct Constructors(string classname, Ctors...) {
    enum bool needs_shim = true;

    static void call(T, Shim)() {
        alias type = PydTypeObject!T;
        alias U = NewParamT!T;
        static if(Ctors.length) {
            type.tp_init = &wrapped_ctors!(classname, T, Shim, Ctors).func;
        }else {
            // If a ctor wasn't supplied, try the default.
            // If the default ctor isn't available, and no ctors were supplied,
            // then this class cannot be instantiated from Python.
            // (Structs always use the default ctor.)
            static if (is(typeof(new U))) {
                static if (is(U == class)) {
                    type.tp_init = &wrapped_init!(Shim).init;
                } else {
                    type.tp_init = &wrapped_struct_init!(U).init;
                }
            }
        }
    }
}

template isDef(string pyname) {
    template isDef(Params...) {
        static if(Params[0].stringof.startsWith("Def!") &&
                __traits(hasMember,Params[0], "funcname")) {
            enum bool isDef = (Params[0].funcname == pyname);
        }else{
            enum bool isDef = false;
        }
    }
}
struct Iterator(Params...) {
    alias Iters = Filter!(isDef!"__iter__", Params);
    alias Nexts = Filter!(isDef!"next", Params);
    enum bool needs_shim = false;
    static void call(T)() {
        alias type = PydTypeObject!T;
        import std.range;
        static if(Iters.length == 1 && (Nexts.length == 1 || isInputRange!(ReturnType!(Iters[0].func)))) {
            version(Python_3_0_Or_Later) {
            }else{
                type.tp_flags |= Py_TPFLAGS_HAVE_ITER;
            }
            type.tp_iter = &opiter_wrap!(T, Iters[0].func).func;
            static if(Nexts.length == 1)
                type.tp_iternext = &opiter_wrap!(T, Nexts[0].func).func;
        }
    }
}

template isOpIndex(P...) {
    enum bool isOpIndex = P[0].stringof.startsWith("OpIndex!");
}
template isOpIndexAssign(P...) {
    enum bool isOpIndexAssign = P[0].stringof.startsWith("OpIndexAssign!");
}
template isOpSlice(P...) {
    enum bool isOpSlice = P[0].stringof.startsWith("OpSlice!");
}
template isOpSliceAssign(P...) {
    enum bool isOpSliceAssign = P[0].stringof.startsWith("OpSliceAssign!");
}
template isLen(P...) {
    enum bool isLen = P[0].stringof.startsWith("Len!");
}
/*
   Extended slice syntax goes through mp_subscript, mp_ass_subscript,
   not sq_slice, sq_ass_slice.

TODO: Python's extended slicing is more powerful than D's. We should expose
this.
*/
struct IndexSliceMerge(Params...) {
    alias OpIndexs = Filter!(isOpIndex, Params);
    alias OpIndexAssigns = Filter!(isOpIndexAssign, Params);
    alias OpSlices = Filter!(isOpSlice, Params);
    alias OpSliceAssigns = Filter!(isOpSliceAssign, Params);
    alias Lens = Filter!(isLen, Params);

    static assert(OpIndexs.length <= 1);
    static assert(OpIndexAssigns.length <= 1);
    static assert(OpSlices.length <= 1);
    static assert(OpSliceAssigns.length <= 1);

    static void call(T)() {
        alias type = PydTypeObject!T;
        static if(OpIndexs.length + OpSlices.length) {
            {
                enum slot = "type.tp_as_mapping.mp_subscript";
                mixin(autoInitializeMethods());
                mixin(slot ~ " = &op_func!(T);");
            }
        }
        static if(OpIndexAssigns.length + OpSliceAssigns.length) {
            {
                enum slot = "type.tp_as_mapping.mp_ass_subscript";
                mixin(autoInitializeMethods());
                mixin(slot ~ " = &ass_func!(T);");
            }
        }
    }


    static extern(C) PyObject* op_func(T)(PyObject* self, PyObject* key) {
        static if(OpIndexs.length) {
            version(Python_2_5_Or_Later) {
                Py_ssize_t i;
                if(!PyIndex_Check(key)) goto slice;
                i = PyNumber_AsSsize_t(key, PyExc_IndexError);
            }else{
                C_long i;
                if(!PyInt_Check(key)) goto slice;
                i = PyLong_AsLong(key);
            }
            if(i == -1 && PyErr_Occurred()) {
                return null;
            }
            alias OpIndex0 = OpIndexs[0];
            return opindex_wrap!(T, OpIndex0.Inner!T.FN).func(self, key);
        }
slice:
        static if(OpSlices.length) {
            if(PySlice_Check(key)) {
                Py_ssize_t len = PyObject_Length(self);
                Py_ssize_t start, stop, step, slicelength;
                if(PySlice_GetIndicesEx(key, len,
                            &start, &stop, &step, &slicelength) < 0) {
                    return null;
                }
                if(step != 1) {
                    PyErr_SetString(PyExc_TypeError,
                            "slice steps not supported in D");
                    return null;
                }
                alias OpSlice0 = OpSlices[0];
                return opslice_wrap!(T, OpSlice0.Inner!T.FN).func(
                        self, start, stop);
            }
        }
        PyErr_SetString(PyExc_TypeError, format(
                    "index type '%s' not supported\0", to!string(key.ob_type.tp_name)).ptr);
        return null;
    }

    static extern(C) int ass_func(T)(PyObject* self, PyObject* key,
            PyObject* val) {
        static if(OpIndexAssigns.length) {
            version(Python_2_5_Or_Later) {
                Py_ssize_t i;
                if(!PyIndex_Check(key)) goto slice;
                i = PyNumber_AsSsize_t(key, PyExc_IndexError);
            }else{
                C_long i;
                if(!PyInt_Check(key)) goto slice;
                i = PyLong_AsLong(key);
            }
            if(i == -1 && PyErr_Occurred()) {
                return -1;
            }
            alias OpIndexAssign0 = OpIndexAssigns[0];
            return opindexassign_wrap!(T, OpIndexAssign0.Inner!T.FN).func(
                    self, key, val);
        }
slice:
        static if(OpSliceAssigns.length) {
            if(PySlice_Check(key)) {
                Py_ssize_t len = PyObject_Length(self);
                Py_ssize_t start, stop, step, slicelength;
                if(PySlice_GetIndicesEx(key, len,
                            &start, &stop, &step, &slicelength) < 0) {
                    return -1;
                }
                if(step != 1) {
                    PyErr_SetString(PyExc_TypeError,
                            "slice steps not supported in D");
                    return -1;
                }
                alias OpSliceAssign0 = OpSliceAssigns[0];
                return opsliceassign_wrap!(T, OpSliceAssign0.Inner!T.FN).func(
                        self, start, stop, val);
            }
        }
        PyErr_SetString(PyExc_TypeError, format(
                    "assign index type '%s' not supported\0", to!string(key.ob_type.tp_name)).ptr);
        return -1;
    }
}

/*
Params: each param is a Type which supports the interface

Param.needs_shim == false => Param.call!(pyclassname, T)
or
Param.needs_shim == true => Param.call!(pyclassname,T, Shim)

    performs appropriate mutations to the PyTypeObject

Param.shim!(i,T) for i : Params[i] == Param

    generates a string to be mixed in to Shim type

where T is the type being wrapped, Shim is the wrapped type

*/

/**
  Wrap a class.

Params:
    T = The class being wrapped.
    Params = Mixture of definitions of members of T to be wrapped and
    optional arguments.
    Concerning optional arguments, accepts PyName!(pyname), ModuleName!(modulename), and Docstring!(docstring).
    pyname = The name of the class as it will appear in Python. Defaults to
    T's name in D
    modulename = The name of the python module in which the wrapped class
            resides. Defaults to "".
    docstring = The class's docstring. Defaults to "".
  */
void wrap_class(T, Params...)() {
    alias args = Args!("","", __traits(identifier,T), "",Params);
    _wrap_class!(T, args.pyname, args.docstring, args.modulename, args.rem).wrap_class();
}
template _wrap_class(_T, string name, string docstring, string modulename, Params...) {
    import std.conv;
    import util.typelist;
    static if (is(_T == class)) {
        //pragma(msg, "wrap_class: " ~ name);
        alias shim_class = pyd.make_wrapper.make_wrapper!(_T, Params).wrapper;
        //alias W.wrapper shim_class;
        alias T = _T;
    } else {
        //pragma(msg, "wrap_struct: '" ~ name ~ "'");
        alias shim_class = void;
        alias T = _T*;
    }
    void wrap_class() {
        if(!Pyd_Module_p(modulename)) {
            if(should_defer_class_wrap(modulename, name)) {
                defer_class_wrap(modulename, name,  toDelegate(&wrap_class));
                return;
            }
        }
        alias type = PydTypeObject!(T);
        init_PyTypeObject!T(type);

        foreach (param; Params) {
            static if (param.needs_shim) {
                param.call!(name, T, shim_class)();
            } else {
                param.call!(name,T)();
            }
        }

        assert(Pyd_Module_p(modulename) !is null, "Must initialize module '" ~ modulename ~ "' before wrapping classes.");
        string module_name = to!string(PyModule_GetName(Pyd_Module_p(modulename)));

        //////////////////
        // Basic values //
        //////////////////
        Py_SET_TYPE(&type, &PyType_Type);
        type.tp_basicsize = PyObject.sizeof;
        type.tp_doc       = (docstring ~ "\0").ptr;
        version(Python_3_0_Or_Later) {
            type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
        }else{
            type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_CHECKTYPES;
        }
        //type.tp_repr      = &wrapped_repr!(T).repr;
        type.tp_methods   = wrapped_method_list!(T).ptr;
        type.tp_name      = (module_name ~ "." ~ name ~ "\0").ptr;

        /////////////////
        // Inheritance //
        /////////////////
        // Inherit classes from their wrapped superclass.
        static if (is(T B == super)) {
            foreach (C; B) {
                static if (is(C == class) && !is(C == Object)) {
                    if (is_wrapped!(C)) {
                        type.tp_base = &PydTypeObject!(C);
                    }
                }
            }
        }

        ////////////////////////
        // Operator overloads //
        ////////////////////////

        Operators!(Filter!(isOp, Params)).call!T();
        // its just that simple.

        IndexSliceMerge!(Params).call!T();
        // indexing and slicing aren't exactly simple.

        //////////////////////////
        // Constructor wrapping //
        //////////////////////////
        Constructors!(name, Filter!(isInit, Params)).call!(T, shim_class)();

        //////////////////////////
        // Iterator wrapping    //
        //////////////////////////
        Iterator!(Params).call!(T)();


        //////////////////
        // Finalization //
        //////////////////
        if (PyType_Ready(&type) < 0) {
            throw new Exception("Couldn't ready wrapped type!");
        }
        Py_INCREF(cast(PyObject*)&type);
        PyModule_AddObject(Pyd_Module_p(modulename), (name~"\0").ptr, cast(PyObject*)&type);

        is_wrapped!(T) = true;
        static if (is(T == class)) {
            is_wrapped!(shim_class) = true;
            wrapped_classes[T.classinfo] = &type;
            wrapped_classes[shim_class.classinfo] = &type;
        }
    }
}

