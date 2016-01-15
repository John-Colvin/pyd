// A minimal "hello world" Pyd module.
module hello;

import pyd.pyd;
import std.stdio;

void hello() {
    writefln("Hello, world!");
}

mixin PydInit!"hello";
extern(C) void PydMain() {
    def!(hello)();
    module_init();
}
