#include <iostream>

using namespace std;

int main() {
    
    char searching='\0';
    string sentence="";
    int counter =0;
    
    getline (cin, sentence);
    searching = 'a';
    for(int i=0; i<sentence.length(); i++) {
        if(sentence[i]==searching) {
                counter++;
            }
        }
    if (counter > 0) cout << "si" << endl;
    else cout << "no" << endl;
    return 0;
}
[[out]]------------------------------------------------------
Program{
   Include(<iostream>)
   Using(std)
   FuncDecl(id:'main', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'char'), Vars = {"searching" = Char<\0>})
         DeclStmt(Type(id:'string'), Vars = {"sentence" = String<>})
         DeclStmt(Type(id:'int'), Vars = {"counter" = Int<0>})
         ExprStmt(CallExpr(id:'getline', Args = {id:'cin', id:'sentence'}))
         ExprStmt(=(id:'searching', Char<a>))
         ForStmt(DeclStmt(Type(id:'int'), Vars = {"i" = Int<0>}), <(id:'i', CallExpr(FieldExpr(id:'sentence', 'length'), Args = {})), IncrExpr<++, post>(id:'i'), {
            Block({
               IfStmt(==(IndexExpr(id:'sentence', id:'i'), id:'searching'), Block({
                  ExprStmt(IncrExpr<++, post>(id:'counter'))
               }))
            })
         })
         IfStmt(>(id:'counter', Int<0>), ExprStmt(<<(<<(id:'cout', String<si>), id:'endl')), ExprStmt(<<(<<(id:'cout', String<no>), id:'endl')))
         ExprStmt<return>(Int<0>)
      })
   })
}