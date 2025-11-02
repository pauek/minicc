int main() { if (a) goto blah; }
[[out]]------------------------------------------
int main() {
    if (a) goto blah;
}
