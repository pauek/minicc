
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
      $("#forwards").removeClass("pure-button-disabled");
      $("#backwards").removeClass("pure-button-disabled");
      stepper.prepare();
   } else {
      $("#compile").removeClass("pure-button-disabled");
      $("#execute").addClass("pure-button-disabled");
      $("#forwards").addClass("pure-button-disabled");
      $("#backwards").addClass("pure-button-disabled");
      stepper.resetHistory();
      stepper.clearMark();
   }
   slider.reset();
   showstate(null);
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

const MIME_TYPE = 'text/plain';

function download_file(filename, text) {
   // http://stackoverflow.com/a/18197511/540869
   // http://html5-demos.appspot.com/static/a.download.html
   window.URL = window.webkitURL || window.URL;
   var blob = new Blob([text], {type: MIME_TYPE});
   var a = document.createElement('a');
   a.download = filename;
   a.href = window.URL.createObjectURL(blob);
   a.dataset.downloadurl = [MIME_TYPE, a.download, a.href].join(':');
   a.click();
}

function download() {
   download_file('programa.cpp', editor.getValue());
}

var stepper = {
   _mark: null,
   _stepper: null,
   _history: null,
   prepare: function () {
      this.resetHistory();
      this._stepper = new Module.Stepper();
   },
   clearMark: function () {
      if (this._mark) {
         this._mark.clear();
         this._mark = null;
      }
   },
   span: function () {
      var span = this._stepper.span();
      return {
         ini: {line: span.ini.lin-1, ch: span.ini.col},
         fin: {line: span.fin.lin-1, ch: span.fin.col}
      };
   },
   setMark: function(span) {
      this.clearMark();
      this._mark = editor.markText(span.ini, span.fin, {
         className: "current",
      });
      editor.scrollIntoView({line: span.fin.line, ch: 0}, 40);
   },
   resetHistory: function () {
      this._history = [];
   },
   finished: function () {
      return this._stepper.finished();
   },
   step: function () {
      var json = this._stepper.state();
      console.log(json);
      this._history.push(JSON.parse(json));
      if (!this._stepper.step()) {
         alert(this._stepper.error());
         return false;
      }
      return true;
   },
   show: function (k) {
      if (k < 0 || k >= this._history.length) {
         return;
      }
      var item = stepper._history[k];
      this.setMark(item.span);
      showstate(item);
   }
};

function reformat() {
   var code = editor.getValue();
   var new_code = Module.reformat(code);
   editor.setValue(new_code);
}

function resize() {
   var free = $(window).height() - $('header').height();
   $('#content').height(free + 'px');
   slider._refreshKnob();
}

function value_str(value, addClass, insert) {
   var s = '', elem = 'div';
   var classes = ['var'];
   if (value === null) {
      classes.push('unknown');
      s = '?';
   } else if (value instanceof Array) {
      classes.push('array');
      elem = 'table';
      s += '<tr>'
      for (var j = 0; j < value.length; j++) {
         s += '<td>';
         s += value_str(value[j], 
                        (j == 0 ? "first" : null), 
                        '<div class="index">' + j + '</div>');
         s += '</td>';
      } 
      s += '</tr>';
   } else if (value instanceof Object) {
      var type = value["<type>"];
      if (type == 'ref') {
         elem = 'div';
         s += '<div class="ref">' + value['ref'] + '</div>';
      } else if (type == 'struct') {
         classes.push('struct');
         elem = 'div';
         s += '<table>';
         for (var prop in value) {
            if (prop == '<type>') {
               continue;
            }
            s += '<tr><td><div class="name">' + prop + '</div></td><td>';
            s += value_str(value[prop]);
            s += '</td></tr>';
         }
         s += '</table>';
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
   html += '">' + s + (insert ? insert : '') + '</' + elem + '>';
   return html;
}

function showstate(S) {
   $('#env').empty();
   $('#status').text('');
   if (S === null) {
      $('#slider .knob').addClass('disabled');
      return;
   }
   $('#slider .knob').removeClass('disabled');
   var env = S.env;
   if (env === null) {
      return;
   }
   var html = '<table><tr>';
   for (var i = env.length-1; i >= 0; i--) {
      html += '<td><div class="fenv';
      if (env[i].tab["<active>"]) {
         html += " active";
      }
      html += '"><h5>' + env[i].name + '</h5>';
      var T = env[i].tab;
      html += '<div class="wrapper"><table>'
      for (var prop in T) {
         if (prop == "<active>") {
            continue;
         }
         html += '<tr><td><div class="name">' + prop + '</div></td><td>';
         html += value_str(T[prop]);
         html += '</td></tr>';
      }
      html += '</table></div>';
      html += '</div></td>';
   }
   html += '</tr></table>';
   $('#env').append(html);
   $('#status').text(S.status);
}

function sliderChange() {
   var value = $('history').value;
}

var dragging = null;

function setupEvents() {
   $('#slider').click(function (ev) {
      slider.click(ev);
   });
   $('#slider .knob').mousedown(function (ev) { 
      var track = $('#slider .track');
      dragging = { 
         orig:  track.position().left,
         width: track.width(),
      };
      return false;
   });
   $('body').mousemove(function (ev) {
      if (dragging) {
         var ratio = (ev.clientX - dragging.orig) / dragging.width;
         slider.setKnob(ratio);
         return false;
      }
   });
   $('body').mouseup(function (ev) {
      dragging = null;
   });
}

var slider = {
   _max: 100,
   _curr: -1,
   _knob: -1,
   curr: function () {
      return this._curr / this.max;
   },
   knob: function () {
      return this._knob;
   },
   top: function () {
      return this._curr == this._knob;
   },
   reset: function () {
      this._max = 100;
      this._curr = -1;
      this._knob = -1;
      this._refreshKnob();
      this._refreshTrack();
   },
   init: function () {
      this._refreshTrack();
      this._refreshKnob();
      setupEvents();
   },
   incr: function() {
      this._knob += 1;
      if (this._knob > this._max * 0.95) {
         this._max += this._max / 2;
      }
      this._refreshKnob();
      if (this._knob > this._curr) {
         this._curr = this._knob;
         this._refreshTrack();
         return true;
      }
      return false;
   },
   decr: function() {
      this._knob -= 1;
      if (this._knob < 0) {
         this._knob = 0;
      }
      this._refreshKnob();
   },
   _refreshTrack: function () {
      var ratio = this._curr / this._max;
      if (ratio > 1.0) {
         ratio = 1.0;
      } else if (ratio < 0.0) {
         ratio = 0.0;
      }
      var track = $('#slider .track');
      var w = track.width();
      var left = track.position().left;
      $('#slider .hl-track').css({width: '' + (ratio * 100) + '%'});
   },
   _refreshKnob: function () {
      var ratio = this._knob / this._max;
      if (ratio > 1.0) {
         ratio = 1.0;
      } else if (ratio < 0.0) {
         ratio = 0.0;
      }
      var track = $('#slider .track');
      var w = track.width();
      var left = track.position().left;
      $('#slider .knob').css({left: ratio * w + left + 'px'});
   },
   setKnob: function (ratio) {
      this._knob = Math.round(this._max * ratio);
      if (this._knob < 0) {
         this._knob = 0;
      }
      var len = stepper._history.length;
      if (this._knob >= len) {
         this._knob = len-1;
      }
      this._refreshKnob();
      stepper.show(this._knob);
   },
   click: function (ev) {
      var track = $('#slider .track');
      var w = track.width();
      var left = track.position().left;
      var ratio = (ev.clientX - left) / w;
      if (ratio < -0.02 || ratio > 1.02) {
         return;
      }
      this.setKnob(ratio);
   }
};

$(document).ready(function () {
   if (localStorage['minicc:program']) {
      initial_program = localStorage['minicc:program'];
   }
   setup(initial_program);

   $("#execute").addClass("pure-button-disabled");
   $("#forwards").addClass("pure-button-disabled");
   $("#backwards").addClass("pure-button-disabled");

   $("#compile").click(compile);
   $("#execute").click(execute);
   $('#forwards').click(function () {
      if (slider.top() && stepper.finished()) {
         return;
      }
      if (slider.incr()) {
         stepper.step();
      }
      stepper.show(slider.knob());
   });
   $('#backwards').click(function () {
      slider.decr();
      stepper.show(slider.knob());
   });
   $("#reformat").click(reformat);
   $("#download").click(download);

   $(window).resize(resize);
   $(window).resize();

   $('#content').split({
      orientation: 'horizontal',
      limit: 70,
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
   slider.init();
   showstate(null);   
});
