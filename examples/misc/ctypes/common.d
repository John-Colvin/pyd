
struct PyObject{ }

extern(C) alias PyFun1 = PyObject* function(PyObject*);
extern(C) alias PyFun2 = PyObject* function(PyObject*, PyObject*);
extern(C) alias PyLFun1 = PyObject* function(long);
extern(C) alias PyLUFun1 = PyObject* function(ulong);
extern(C) alias PyStrFun1 = PyObject* function(const(char)* buf, size_t len);
extern(C) alias PyStrFun2 = PyObject* function(const(char)* str);

struct Funs{
    static __gshared:
    PyStrFun2 raise;
    PyLFun1 long_to_python;
    PyLUFun1 ulong_to_python;
    PyStrFun1 utf8_to_python;

    PyFun2 get_item;
}

extern(C) int pyd_reg_fun(char* _fnom, PyFun1 somefun) {
    import std.conv;
    import std.exception;
    import std.traits;
    import std.typetuple;
    import std.stdio;
    string fnom = to!string(_fnom);
    alias Fields = TypeTuple!(__traits(allMembers, Funs));
    foreach(i,_; Fields) {
        enum string nom = _;
        if(nom == fnom) {
            writefln("Funs[%s]: %s", i, nom);
            mixin("Funs."~nom ~ " = cast(typeof(Funs."~nom~")) somefun;");
            return 0;
        }
    }
    enforce(0);
    return 0;
}
