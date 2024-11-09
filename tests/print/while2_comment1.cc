void xxx(string s) {
   while (/*101*/a = true)/*103*/ a = a + 2;
}
[[out]]--------------------------------------------------
void xxx(string s) {
   while (/*101*/ a = true) /*103*/ a = a + 2;
}
