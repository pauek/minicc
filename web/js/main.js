
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
   draw_state(null);
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
      draw_state(item);
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


/* Draw functions
 
   These functions draw the state of the environment to a canvas
   using KineticJS. 

   All draw functions return a single object (a group if necessary), and
   the size and position of this object.

       drawX(x, y, data) -> {obj, width, height}

*/

var draw_params = {
   separation: {var_name: 6, value: 6},
   padding: {
      env_title: {top: 7, right: 0, bottom: 7, left: 9},
      env:       {top: 7, right: 7, bottom: 7, left: 15},
      value:     {top: 5, right: 5, bottom: 5, left: 5},
      obj:       {top: 0, right: 0, bottom: 0, left: 0},
   },
   env_min:  {width: 100, height: 10},
};

var paper = null;

function draw_state(S) {
   paper.clear();
   $('#status').text('');
   if (S === null) {
      $('#slider .knob').addClass('disabled');
      return;
   }
   $('#slider .knob').removeClass('disabled');
   var x = 10, y = 10;
   for (var i = S.env.length-1; i >= 0; i--) {
      var env = draw_env(x, y, S.env[i]);
      x += env.width + 20;
   }
}

function draw_env(_x, _y, _env) {
   var x = _x, y = _y;
   var set = paper.g();

   // title
   var P1 = draw_params.padding.env_title;
   var title = draw_text(x + P1.left, y + P1.top, _env.name);
   title.obj.attr({'font-weight': 600, 'fill': '#333'});
   y += P1.top + title.height + P1.bottom;
   set.add(title.obj);

   // vars + values
   var P2 = draw_params.padding.env;
   var env = draw_obj(x + P2.left, y + P2.top, _env.tab);
   y += P2.top + env.height + P2.bottom;
   set.add(env.obj);

   var W = draw_params.env_min.width;
   var H = draw_params.env_min.height;
   var width  = (env.width  < W ? W : env.width) + P2.left + P2.right;
   var height = (env.height < H ? H : env.height) + title.height + P1.top + P1.bottom;
   if (env.count > 0) {
      height += P2.top + P2.bottom;
   }

   // draw background + title
   var active = _env.tab['<active>'];
   var r1 = paper.rect(_x, _y, width, height).attr({
      fill: (active ? 'rgb(255, 255, 160)' : '#f7f7f7'),
      'stroke': 'rgba(0, 0, 0, .2)',
      'stroke-width': 1
   });
   var r2 = paper.rect(_x, _y, width, P1.top + title.height + P1.bottom).attr({
      fill: (active ? 'rgba(150, 150, 0, 0.3)' : 'rgba(0, 0, 0, 0.2)'),
      'stroke': 'rgba(0, 0, 0, 0)',
      'stroke-width': 1
   });
   env.obj.before(r2);
   env.obj.before(r1);
   r2.before(r1);
   r1.after(title.obj);
   // r2.toBack(); r1.toBack();

   return {
      obj:    set,
      width:  width,
      height: height,
      count:  env.count,
   };
}

function draw_value(x0, y0, data, index) {
   var P  = draw_params.padding.value;
   var value = {}, height, width;
   var set = paper.g();
   if (data instanceof Array) {
      value = draw_array(x0, y0, data);
      y += value.height;
      height = value.height;
      width = value.width;
      set.add(value.obj);
      if (index !== undefined) {
         idx = draw_text(x0 + 2.0, y0 + 1.5, "" + index, '#bbb', 9);
         set.add(idx.obj);
      }
   } else {
      var x = x0 + P.left, y = y0 + P.top;
      var fill = 'rgba(255, 255, 255, 0.6)';
      if (data instanceof Object && data['<type>'] === 'char') {
         value = draw_text(x, y, "'" + data['char'] + "'");
         y += value.height;
         fill = 'white';
      } else if (data instanceof Object && data['<type>'] === 'ref') {
         value = draw_text(x, y, data['ref']);
         y += value.height;
         fill = 'white';
      } else if (data instanceof Object) {
         value = draw_obj(x, y, data);
         y += value.height;
      } else {
         value = draw_text(x, y, data, '#55f');
         y += value.height;
         fill = 'white';
      }
      height = value.height + P.top + P.bottom;
      width = value.width + P.left + P.right;
      var rect = paper.rect(x0, y0, width, height).attr({
         'fill': fill,
         'stroke': '#bbb',
         'stroke-width': 1
      });
      set.add(rect);
      set.add(value.obj);
      value.obj.before(rect);
   }
   return {
      obj:    set,
      width:  width,
      height: height
   };
}

function draw_array(x0, y0, array) {
   var height = 0, x = x0;
   var set = paper.g();
   for (var i = 0; i < array.length; i++) {
      var value = draw_value(x, y0, array[i].data, i);
      x += value.width;
      if (value.height > height) {
         height = value.height;
      }
      set.add(value.obj);
   }
   return {
      obj:    set,
      width:  x - x0,
      height: height
   }
}

function draw_obj(_x, _y, obj) {
   var P = draw_params.padding.obj;
   var x = _x + P.left, y = _y + P.top;
   var set = paper.g();

   // draw variable names
   var objs = {names: [], values: [], count: 0};
   var names_width = 0, values_width = 0;
   for (var prop in obj) {
      if (prop == '<type>' || prop == '<active>') {
         continue;
      }
      objs.count++;
      if (objs.count > 1) {
         y += draw_params.separation.value;
      }
      
      // value
      var value = draw_value(x, y, obj[prop].data);
      if (value.width > values_width) {
         values_width = value.width;
      }
      objs.values.push(value);
      set.add(value.obj);

      // name
      var name = draw_text(x, y, prop);
      if (name.width > names_width) {
         names_width = name.width;
      }
      name.dy = (value.height)/2;
      objs.names.push(name);
      set.add(name.obj);

      y += value.height;
   }
   
   // adjust names so that they are aligned to the right
   var dx;
   var sep = draw_params.separation.var_name;
   for (var i = 0; i < objs.count; i++) {
      var n = objs.names[i];
      dx = names_width - n.width;
      n.obj.transform('t' + dx + ' ' + (n.dy + 4 /* why????!!! */));

      var v  = objs.values[i];
      dx = names_width + sep;
      v.obj.transform('t' + dx + ' 0');
   }
   
   return {
      obj:    set,
      width:  names_width + sep + values_width + P.left + P.right,
      height: y - _y + P.top + P.bottom,
      count:  objs.count,
   };
}

function draw_text(x, y, text, fill, fontSize) {
   var text = paper.text(x, y, '' + text).attr({
      'font-family': 'Source Code Pro',
      'font-size': (fontSize ? fontSize : 14),
      'text-anchor': 'start',
      fill: (fill ? fill : '#333'),
   });
   var box = text.getBBox();
   var dy = y - (box.y + 2);
   text.transform('t0,' + dy);
   return {
      obj:    text,
      width:  box.width,
      height: box.height - 3,
   }
}

/* Show state (as html elements). */

function value_str(value, addClass, insert) {
   var s = '', elem = 'div', links = [];
   var classes = ['var'];
   if (value.data === null) {
      classes.push('unknown');
      s = '?';
   } else if (value.data instanceof Array) {
      classes.push('array');
      elem = 'table';
      s += '<tr>'
      for (var j = 0; j < value.data.length; j++) {
         s += '<td>';
         var res = value_str(value.data[j], 
                             (j == 0 ? "first" : null), 
                             '<div class="index">' + j + '</div>');
         links.push.apply(links, res.links);
         s += res.html;
         s += '</td>';
      } 
      s += '</tr>';
   } else if (value.data instanceof Object) {
      var type = value.data["<type>"];
      if (type == 'ref') {
         var addr = value.data['ref']
         elem = 'div';
         classes.push('ref');
         var from = 'ref-' + value.box + '-' + addr;
         var to   = 'box-' + addr;
         s += '<div id="' + from + '" class="endpoint"></div>';
         links.push({from: from, to: to});
      } else if (type == 'struct') {
         classes.push('struct');
         elem = 'div';
         s += '<table>';
         for (var prop in value.data) {
            if (prop == '<type>') {
               continue;
            }
            s += '<tr><td><div class="name">' + prop + '</div></td><td>';
            var res = value_str(value.data[prop]);
            links.push.apply(links, res.links);
            s += res.html;
            s += '</td></tr>';
         }
         s += '</table>';
      }
   } else {
      classes.push('value');
      s = value.data;
   }
   if (addClass) {
      classes.push(addClass);
   }
   var html = '<' + elem + ' id="box-' + value.box + '" class="';
   for (var i = 0; i < classes.length; i++) {
      if (i > 0) {
         html += ' ';
      }
      html += classes[i];
   }
   html += '">' + s + (insert ? insert : '') + '</' + elem + '>';
   return {
      html:  html,
      links: links
   };
}

function showstate(S) {
   var links = [];
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
      if (env.tab["<active>"]) {
         html += " active";
      }
      html += '"><h5>' + env.name + '</h5>';
      var T = env.tab;
      html += '<div class="wrapper"><table>'
      for (var prop in T) {
         if (prop == "<active>") {
            continue;
         }
         html += '<tr><td><div class="name">' + prop + '</div></td><td>';
         var res = value_str(T[prop]);
         links.push.apply(links, res.links);
         html += res.html;
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
   
   paper = new Snap('#env');
   draw_state(null);
});
