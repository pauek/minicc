
/*
strings
        substr
        char_access
bit_ops
        bit_and
        bit_or
        bit_xor
*/


std::vector<Level> levels = {
    {{"all"},                                                   {{0, 2}},         {-1},   {No}    },
    {{"strings", "bit_ops"},                                    {{0, 2}, {2, 5}}, {0, 0}, {No, No}},
    {{"substr", "char_access", "bit_and", "bit_or", "bit_xor"},
     {},
     {0, 0, 1, 1},
     {Yes, No, Yes, No, Yes}                                                                      }
};
