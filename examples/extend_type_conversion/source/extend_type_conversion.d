module extend_type_conversion;

import std.stdio;
import pyd.pyd;

struct S {
    int i;
}

S foo() {
    S s;
    s.i = 12;
    return s;
}

void bar(S s) {
    writeln(s);
}

mixin PydInit!({
    ex_d_to_python((S s) => s.i);
    ex_python_to_d((int i) => S(i));

    def!foo();
    def!bar();
    module_init();
});

