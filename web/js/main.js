
var initial_program = '#include <iostream>\n\
using namespace std;\n\
\n\
int main() {\n\
   int a = 1;\n\
   a = 3;\n\
   int b = 3;\n\
   b = 7;\n\
   cout << "Hola, MiniC++" << endl;\n\
}\n\
';

var cambiado = false;
var compilado = false;

function setCompilado(new_value) {
   compilado = new_value;
   if (compilado) {
      $("#compile").addClass("pure-button-disabled");
      $("#execute").removeClass("pure-button-disabled");
      $("#step").removeClass("pure-button-disabled");
   } else {
      $("#compile").removeClass("pure-button-disabled");
      $("#execute").addClass("pure-button-disabled");
      $("#step").addClass("pure-button-disabled");
      if (mark) {
         mark.clear();
      }
      if (stepper) {
         stepper = null;
      }
   }
}

var editor;
var output, errors;

function setup(init) {
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
   editor.setValue(init);

   output = CodeMirror.fromTextArea($('#output > textarea')[0], {
      mode: 'text/x-show-inv',
      readOnly: true,
   });

   errors = CodeMirror.fromTextArea($('#errors > textarea')[0], {
      mode: 'text/x-show-inv',
      readOnly: true,
   });

   editor.on("change", function () {
      setCompilado(false);
      cambiado = true;
      $('#output').hide();
   });
}

function saveProgram() {
   localStorage['minicc:program'] = editor.getValue();
}

function compile() {
   saveProgram();
   var code = editor.getValue();
   var err = Module.compile(code);
   if (err == "") {
      setCompilado(true);
   }
   errors.setValue(err);
   $('#errors').show();
}

function execute() {
   $('#errors').hide();
   $("#output > pre").text("");
   setTimeout(function () {
      var out = Module.execute("");
      var re1 = new RegExp('\n', 'g');
      out = out.replace(re1, '\u21a9\n');
      var re2 = new RegExp(' ', 'g');
      out = out.replace(re2, '\u2423');
      out += '\u00a7';
      console.log(out);
      $('#output').show();
      output.setValue(out);
   }, 80);
}

var stepper = null, mark;

function step() {
   if (stepper === null) {
      stepper = new Module.Stepper();
   }
   if (mark) {
      mark.clear();
   }
   if (!stepper.finished()) {
      var r = stepper.span();
      var ini = {line: r.ini.lin-1, ch: r.ini.col};
      var fin = {line: r.fin.lin-1, ch: r.fin.col};
      mark = editor.markText(ini, fin, {
         className: "current",
      });
      editor.scrollIntoView({line: fin.line + 3, ch: 0});
      showenv(JSON.parse(stepper.env()));
      stepper.step();
   } else {
      stepper = null;
      showenv(null);
   }
}

function reformat() {
   var code = editor.getValue();
   var new_code = Module.reformat(code);
   editor.setValue(new_code);
}

function resize() {
   var free = $(window).height() - $('header').height();
   $('#content').height(free + 'px');
}

function value_str(value, addClass) {
   var s = '', elem = 'span';
   var classes = [];
   if (value === null) {
      classes.push('unknown');
      s = '?';
   } else if (value instanceof Array) {
      classes.push('array');
      for (var j = 0; j < value.length; j++) {
         s += value_str(value[j], (j == 0 ? "first" : null));
      } 
   } else if (value instanceof Object) {
      classes.push('struct');
      elem = 'div';
      for (var prop in value) {
         s += '<div class="var">' + prop + '&nbsp;';
         s += value_str(value[prop]);
         s += '</div>';
      }
   } else {
      classes.push('value');
      s = value;
   }
   if (addClass) {
      classes.push(addClass);
   }
   var html = '<' + elem + ' class="';
   for (var i = 0; i < classes.length; i++) {
      if (i > 0) {
         html += ' ';
      }
      html += classes[i];
   }
   html += '">' + s + '</' + elem + '>';
   return html;
}

function showenv(env) {
   $('#env').empty();
   if (env === null) {
      return;
   }
   var html = '<table><tr>';
   for (var i = env.length-1; i >= 0; i--) {
      html += '<td><div class="fenv';
      if (i == env.length-1) {
         html += " curr";
      }
      html += '"><h5>' + env[i].func + '</h5>';
      var E = env[i].env;
      for (var prop in E) {
         html += '<div class="var">' + prop + '&nbsp;';
         html += value_str(E[prop]);
         html += '</div>';
      }
      html += '</div></td>';
   }
   html += '</tr></table>';
   $('#env').append(html);
}

$(document).ready(function () {
   if (localStorage['minicc:program']) {
      initial_program = localStorage['minicc:program'];
   }
   setup(initial_program);

   $("#execute").addClass("pure-button-disabled");
   $("#step").addClass("pure-button-disabled");

   $("#compile").click(compile);
   $("#execute").click(execute);
   $('#step').click(step);
   $("#reformat").click(reformat);

   $(window).resize(resize);
   $(window).resize();

   $('#content').split({
      orientation: 'horizontal',
      limit: 200,
   });
   $('#top').split({
      orientation: 'vertical',
      limit: 350,
   });
   $(window).bind('splitter.resize', function () {
      editor.refresh();
   });
   editor.refresh();

   $(window).bind('beforeunload', function () {
      saveProgram();
      if (cambiado) {
         return "Has editado, seguro que quieres salir?";
      }
   });
});
