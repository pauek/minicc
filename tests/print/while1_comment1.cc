void xxx(string s ) {
    while/* comment 1 */(a  =  true) {
        a = a  +  2    ;
        i    = a;
    }
}
[[out]]--------------------------------------------------
void xxx(string s) {
    while /* comment 1 */ (a = true) {
        a = a + 2;
        i = a;
    }
}
