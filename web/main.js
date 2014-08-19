
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

var editor;

$(document).ready(function () {
   editor = CodeMirror.fromTextArea(document.getElementById("editor"), {
      mode: 'text/x-c++src',
      theme: 'default',
      lineNumbers: true,
      styleActiveLine: true,
      matchBrackets: true,
      tabSize: 3,
      indentUnit: 3,
      autofocus: true,
   });
   editor.setValue(initial_program);

   var output = CodeMirror.fromTextArea($('#output > textarea')[0], {
      mode: 'text/x-show-inv',
      readOnly: true,
   });
   var errors = CodeMirror.fromTextArea($('#errors > textarea')[0], {
      mode: 'text/x-show-inv',
      readOnly: true,
   });

   $("#execute").addClass("pure-button-disabled");

   editor.on("change", function () {
      setCompilado(false);
      $('#output').hide();
   });

   $("#compile").click(function () {
      var code = editor.getValue();
      var err = Module.compile(code);
      if (err == "") {
         setCompilado(true);
      }
      errors.setValue(err);
      $('#errors').show();
   });
   $("#execute").click(function () {
      $('#errors').hide();
      $("#output > pre").text("");
      setTimeout(function () {
         var out = Module.execute("");
         var re1 = new RegExp('\n', 'g');
         out = out.replace(re1, '\u21a9\n');
         var re2 = new RegExp(' ', 'g');
         out = out.replace(re2, '\u2423');
         console.log(out);
         $('#output').show();
         output.setValue(out);
      }, 80);
   });
   $("#reformat").click(function () {
      var code = editor.getValue();
      var new_code = Module.reformat(code);
      editor.setValue(new_code);
   });
});