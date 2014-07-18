void xxx(string s ) {
   while(a  =  true) /* comment 2 */{
      a = a  +  2    ;
      i    = a;
   }
}
[[out]]--------------------------------------------------
void xxx(string s) {
   while (a = true) /* comment 2 */ {
      a = a + 2;
      i = a;
   }
}
[[err]]--------------------------------------------------
