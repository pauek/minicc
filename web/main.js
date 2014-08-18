
var initial_program = '#include <iostream>\n\
using namespace std;\n\
\n\
int main() {\n\
   cout << "Hola, MiniC++" << endl;\n\
}\n\
';

var compilado = false;

function setCompilado(new_value) {
   compilado = new_value;
   if (compilado) {
      $("#compile").addClass("pure-button-disabled");
      $("#execute").removeClass("pure-button-disabled");
   } else {
      $("#compile").removeClass("pure-button-disabled");
      $("#execute").addClass("pure-button-disabled");
   }
}

$(document).ready(function () {
   var editor = ace.edit("editor");
   editor.setTheme("ace/theme/monokai");
   editor.setValue(initial_program);
   editor.gotoLine(6, 3);

   var session = editor.getSession();
   session.setMode("ace/mode/c_cpp");
   session.setTabSize(3);

   $("#execute").addClass("pure-button-disabled");

   session.on("change", function () {
      setCompilado(false);
   });

   $("#compile").click(function () {
      var code = editor.getValue();
      var errors = Module.compile(code);
      if (errors == "") {
         setCompilado(true);
      }
      $("#errors > pre").text(errors);
   });
   $("#execute").click(function () {
      $("#output > pre").text("");
      setTimeout(function () {
         $("#output > pre").text(Module.execute(""));
      }, 200);
   });
   $("#reformat").click(function () {
      var code = editor.getValue();
      var new_code = Module.reformat(code);
      editor.setValue(new_code);
      editor.selection.clearSelection();
   });
});