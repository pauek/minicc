int main() { if (a) goto xxx; }
[[out]]------------------------------------------
int main() {
   if (a) goto xxx;
}
