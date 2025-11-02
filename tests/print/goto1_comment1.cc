int main() { if (a) goto/* A */xxx/* B */;/* C */ }
[[out]]------------------------------------------
int main() {
    if (a) goto /* A */ xxx /* B */; /* C */
}
