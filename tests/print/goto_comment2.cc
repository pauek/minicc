int main() { if (a) { goto blah/* hi hi */ ; } }
[[out]]------------------------------------------
int main() {
   if (a) {
      goto blah /* hi hi */;
   }
}
