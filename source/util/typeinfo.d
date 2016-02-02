/*
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
module util.typeinfo;

import std.traits;
import std.compiler;

enum Constness {
    Mutable,
    Const,
    Immutable,
    Wildcard
}

string constness_ToString(Constness c) {
    switch(c){
        case Constness.Mutable:
            return "mutable";
        case Constness.Const:
            return "const";
        case Constness.Immutable:
            return "immutable";
        case Constness.Wildcard:
            return "inout";
        default:
            assert(0);
    }
}

template constness(T) {
    static if(is(T == immutable)) {
        enum constness = Constness.Immutable;
    }else static if(is(T == const)) {
        enum constness = Constness.Const;
    }else static if(is(T == inout)) {
        enum constness = Constness.Wildcard;
    }else {
        enum constness = Constness.Mutable;
    }
}

bool constCompatible(Constness c1, Constness c2) {
    return c1 == c2 || 
        c1 == Constness.Const && c2 != Constness.Wildcard ||
        c2 == Constness.Const && c1 != Constness.Wildcard;
}

template ApplyConstness(T, Constness constness) {
    alias Tu = Unqual!T;
    static if(constness == Constness.Mutable) {
        alias ApplyConstness = Tu;
    }else static if(constness == Constness.Const) {
        alias ApplyConstness = const(Tu);
    }else static if(constness == Constness.Wildcard) {
        alias ApplyConstness = inout(Tu);
    }else static if(constness == Constness.Immutable) {
        alias ApplyConstness = immutable(Tu);
    }else {
        static assert(0);
    }
}

template ApplyConstness2(T, Constness constness) {
    alias Tu = Unqual!T;
    static if(constness == Constness.Mutable) {
        alias ApplyConstness2 = Tu;
    }else static if(constness == Constness.Const) {
        alias ApplyConstness2 = const(Tu);
    }else static if(constness == Constness.Wildcard) {
        alias ApplyConstness2 = Tu;
    }else static if(constness == Constness.Immutable) {
        alias ApplyConstness2 = immutable(Tu);
    }else {
        static assert(0);
    }
}

string attrs_to_string(uint attrs) {
    string s = "";
    with(FunctionAttribute) {
        if(attrs & pure_) s ~= " pure";
        if(attrs & nothrow_) s ~= " nothrow";
        if(attrs & ref_) s ~= " ref";
        if(attrs & property) s ~= " @property";
        if(attrs & trusted) s ~= " @trusted";
        if(attrs & safe) s ~= " @safe";
        if(attrs & nogc) s ~= " @nogc";
        static if(version_major == 2 && version_minor >= 67) {
            if(attrs & return_) s ~= " return";
        }
    }
    return s;
}

// what U should be so 'new U' returns a T
template NewParamT(T) {
    static if(isPointer!T && is(PointerTarget!T == struct)) 
        alias NewParamT = PointerTarget!T;
    else alias NewParamT = T;
}

template StripSafeTrusted(F) {
    enum attrs = functionAttributes!F ; 
    enum desired_attrs = attrs & ~FunctionAttribute.safe & ~FunctionAttribute.trusted;
    enum linkage = functionLinkage!F;
    alias unqual_F = SetFunctionAttributes!(F, linkage, desired_attrs);
    static if(isFunctionPointer!F) {
        enum constn = constness!(pointerTarget!F);
        alias StripSafeTrusted = ApplyConstness!(pointerTarget!unqual_F, constn)*;
    }else static if(isDelegate!F) {
        enum constn = constness!(F);
        alias StripSafeTrusted = ApplyConstness!(unqual_F, constn);
    }else{
        enum constn = constness!(F);
        alias StripSafeTrusted = ApplyConstness!(unqual_F, constn);
    }


}

class Z {
    void a() immutable
    {
    }
}
//static assert(is(StripSafeTrusted!(typeof(&Z.a)) == typeof(&Z.a) ));
//static assert(is(StripSafeTrusted!(typeof(&Z.init.a)) == typeof(&Z.init.a) ));
