module pyd.pydmain;

import std.traits : isCallable, moduleName;

mixin template PydInit(alias PydMain)
if (__traits(compiles, PydMain()))
{
    struct _PydInitTestStruct_ {}
    mixin .PydInit!(moduleName!_PydInitTestStruct_, PydMain);
}

mixin template PydInit(string moduleName, alias PydMain)
if (__traits(compiles, PydMain()))
{
    import pyd.def;
    import pyd.exception;
    import pyd.thread;
    import core.runtime;

    version(Python_3_0_Or_Later)
    {
        import deimos.python.Python;

        pragma(mangle, "PyInit_" ~ moduleName)
        extern(C) export PyObject* _PyInit()
        {
            rt_init();
            return pyd.exception.exception_catcher(
                delegate PyObject*()
                {
                    pyd.thread.ensureAttached();
                    pyd.def.pyd_module_name = moduleName;
                    PydMain();
                    return pyd.def.pyd_modules[""];
                });
        }
    }
    else version(Python_2_4_Or_Later)
    {
        pragma(mangle, "init" ~ moduleName)
        extern(C) export void _init()
        {
            rt_init();
            pyd.exception.exception_catcher(
                delegate void()
                {
                    pyd.thread.ensureAttached();
                    pyd.def.pyd_module_name = "%(modulename)s";
                    PydMain();
                });
        }
    }
    else static assert(false);
}
