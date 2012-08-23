module util.typelist;


import std.algorithm;
import std.typetuple;

// from std.typecons:

// this would be deprecated by std.typelist.Filter
// emn: std.typelist DOESNT EXIST
template Filter(alias pred, lst...)
{
    static if (lst.length > 0)
    {
        alias Filter!(pred, lst[1 .. $]) tail;
        //
        static if (pred!(lst[0]))
            alias TypeTuple!(lst[0], tail) Filter;
        else
            alias tail Filter;
    }
    else
        alias TypeTuple!() Filter;
}

template Join(string delimit, T...) {
        static if(T.length == 0) {
            enum Join = "";
        }else static if(T.length == 1) {
            enum Join =  T[0];
        }else {
            enum Join = T[0] ~ "," ~ Join!(delimit,T[1..$]);
        }
}


// I rather like the whole algorithm("isThing(a)") shtick.
// useful for templates too?
template Pred(string pred) {
    template Pred(Stuff...) {
        alias Stuff[0] A;
        enum Pred = mixin(pred);
    }
}
