extern(C) shared bool _D2rt6dmain212_d_isHaltingOb;
alias _d_isHalting = _D2rt6dmain212_d_isHaltingOb;
extern(C) {

    void rt_init();
    void rt_term();

    void my_init() {
        rt_init();
    }

    void my_fini() {
        if(!_d_isHalting){
            rt_term();
        }
    }
    void _Dmain(){}

}

