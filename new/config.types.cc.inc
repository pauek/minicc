
struct strings : Option<1, 0> {
    struct size : Option<2, 0> {};
    struct char_access : Option<2, 1> {};
    struct substr : Option<2, 2> {
        struct two_params : Option<3, 0> {};
    };
};
struct bit_ops : Option<1, 1> {
    struct bit_and : Option<2, 3> {};
    struct bit_or : Option<2, 4> {};
    struct bit_xor : Option<2, 5> {};
    struct bit_not : Option<2, 6> {};
};
struct functions : Option<1, 2> {
    struct pass_by_value : Option<2, 7> {};
    struct pass_by_reference : Option<2, 8> {};
};
struct loops : Option<1, 3> {
    struct for_loop : Option<2, 9> {};
    struct while_loop : Option<2, 10> {};
};
