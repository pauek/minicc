void f() {
   /*1*/extern/*2*/volatile/*3*/mutable/*4*/register/*5*/const/*6*/int/*7*/x/*8*/=/*9*/1/*10*/;/*11*/
}
[[out]]--------------------------------------------------
void f() {
   /*1*/const /*2*/ volatile /*3*/ mutable /*4*/ register /*5*/ extern /*6*/ int /*7*/ x /*8*/ = /*9*/ 1 /*10*/; /*11*/
}
