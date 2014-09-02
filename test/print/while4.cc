void xxx(string s) {
   while (a = true)
   while (a = true) a = a + 2;
}
[[out]]--------------------------------------------------
void xxx(string s) {
   while (a = true) while (a = true) a = a + 2;
}
