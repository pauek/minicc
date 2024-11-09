double fn(char c1, char c2) {
   &a->b  /* ble   */   ;
}
[[out]]------------------------------------
double fn(char c1, char c2) {
   &a->b /* ble   */;
}
