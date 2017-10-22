
#include <algorithm>
#include <sstream>
#include "ast.hh"
#include "prettypr2.hh"
#include "cast.h"
using namespace std;

struct OutputWriter {
   OutputWriter(ostream& out = std::cout) : indent_(0), out_(out) {}

   void Indent() { indent_ += 3; }
   void Dedent() { indent_ -= 3; }

   template<typename T>
   void Write(const T& t) { out_ << t; }

   template<typename T1, typename T2>
   void Write(const T1& t1, const T2& t2) { out_ << t1 << t2; }

   template<typename T1, typename T2, typename T3>
   void Write(const T1& t1, const T2& t2, const T3& t3) { 
      out_ << t1 << t2 << t3; 
   }

   void EndLine() { out_ << endl; }
   void BeginLine() { out_ << string(indent_, ' '); }

   void Line(string s) { BeginLine(); Write(s); EndLine(); }

   std::string Indentation() const { 
      return std::string(indent_, ' ');
   }

private:
   int indent_;
   ostream& out_;
};

class CommentPrinter2 {
   Ast *x;
   int i;
   bool was_empty, had_endl;
   OutputWriter& writer;

   CommentSeq *commseq() { 
      CommentSeq *c = (i < x->comments.size() ? x->comments[i] : 0);
      was_empty = (c == 0);
      had_endl = (c != 0 ? c->has_endl() : false);
      i++;
      return c;
   }
   string CMT(bool pre, bool post, bool _endl, bool missing);
public:
   CommentPrinter2(Ast *_x, OutputWriter& wr) 
      : x(_x), writer(wr), i(0), had_endl(false), was_empty(true) {}

   CommentSeq *next()    const { return (i < x->comments.size() ? x->comments[i] : 0); }
   bool last_had_endl()  const { return had_endl; }
   bool last_was_empty() const { return was_empty; }

   string _cmt ()  { return CMT(1, 0, 0, 0); }
   string _cmt_()  { return CMT(1, 1, 0, 0); }
   string  cmt_()  { return CMT(0, 1, 0, 0); }
   string  cmt ()  { return CMT(0, 0, 0, 0); }
   string _cmtl()  { return CMT(1, 0, 1, 0); }
};

static ostream& print_comment_seq(ostream& o, CommentSeq* C, string indentation) {
   if (C == 0) {
      return o;
   }
   for (int i = 0; i < C->items.size(); i++) {
      const Comment& c = C->items[i];
      switch (c.kind) {
      case Comment::none:
         break;
      case Comment::multiline:
         if (i > 0 and C->items[i-1].kind != Comment::endline) {
            o << ' ';
         }
         o << "/*" << c.text << "*/";
         break;
      case Comment::singleline:
         o << "//" << c.text;
         break;
      case Comment::endline:
         o << endl << indentation;
      }
   }
   return o;
}

string CommentPrinter2::CMT(bool pre, bool post, bool _endl, bool missing) {
   CommentSeq *cn = commseq();
   ostringstream out;
   if (cn != 0 and !cn->items.empty()) {
      if (pre and !cn->starts_with_endl()) {
         out << " ";
      }
      print_comment_seq(out, cn, writer.Indentation());
      if (_endl and !cn->has_endl()) {
         out << endl << writer.Indentation();
      } else if (!_endl) {
         out << (post ? " " : "");
      }
   } else {
      if (_endl) {
         out << endl << writer.Indentation();
      } else if (missing) {
         out << ' ';
      }
   }
   return out.str();
}

struct PrettyPrinter2 {
   OutputWriter out;


   PrettyPrinter2(ostream& o) : out(o) {}
   void Print(Ast *ast);
};

void PrettyPrinter2::Print(Ast* ast) {
   assert(ast != nullptr);
   switch (ast->type()) {
   case AstType::Program: {
      Program *X = cast<Program>(ast);
      CommentPrinter2 cp(X, out);
      int i;
      for (i = 0; i < X->nodes.size(); i++) {
         out.Write(cp.cmt());
         Ast *n = X->nodes[i];
         if ((!cp.last_was_empty() and !cp.last_had_endl()) or
             (i > 0 and n->is<FuncDecl>() and 
              (X->comments[i] and !X->comments[i]->ends_with_empty_line()))) {
            out.EndLine();
         }
         Print(n);
         if (cp.next() and !cp.next()->starts_with_endl()) {
            out.Write(' ');
         }
      }
      CommentSeq *last = cp.next();
      if (last) {
         last->only_one_endl_at_end();
      }
      out.Write(cp.cmt());
      if (last == 0 or !last->has_endl()) {
         out.EndLine();
      }      
      break;
      
   }
   default:
      out.Write("<node>");
      break;
   }
}

void PrettyPrint(Ast *ast, ostream& out) {
   PrettyPrinter2(out).Print(ast);
}
