
struct instructions : Option<1, 0> {
    struct assignment : Option<2, 0> {};
    struct cout : Option<2, 1> {};
    struct cin : Option<2, 2> {};
};
struct strings : Option<1, 1> {
    struct size : Option<2, 3> {};
    struct char_access : Option<2, 4> {};
    struct substr : Option<2, 5> {};
};
struct library : Option<1, 2> {
    struct algorithm : Option<2, 6> {
        struct swap : Option<3, 0> {};
        struct min : Option<3, 1> {};
        struct max : Option<3, 2> {};
    };
    struct cctype : Option<2, 7> {
        struct islower : Option<3, 3> {};
        struct isupper : Option<3, 4> {};
        struct isdigit : Option<3, 5> {};
        struct isalpha : Option<3, 6> {};
        struct isspace : Option<3, 7> {};
        struct isalnum : Option<3, 8> {};
        struct toupper : Option<3, 9> {};
        struct tolower : Option<3, 10> {};
    };
};
struct bit_ops : Option<1, 3> {
    struct bit_and : Option<2, 8> {};
    struct bit_or : Option<2, 9> {};
    struct bit_xor : Option<2, 10> {};
    struct bit_not : Option<2, 11> {};
};
struct functions : Option<1, 4> {
    struct pass_by_value : Option<2, 12> {};
    struct pass_by_reference : Option<2, 13> {};
};
struct loops : Option<1, 5> {
    struct for_loop : Option<2, 14> {};
    struct while_loop : Option<2, 15> {};
};
