int f() {
    if(a = b)/* after condition: blah */{
        cout +x;
    }
}
[[out]]--------------------------------------------------
int f() {
    if (a = b) /* after condition: blah */ {
        cout + x;
    }
}
