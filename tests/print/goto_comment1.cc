int main() { if (a) { goto /* hi hi */ blah; } }
[[out]]------------------------------------------
int main() {
   if (a) {
      goto /* hi hi */ blah;
   }
}
