
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
   // showstate(null);
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
      // showstate(item);
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

/* Draw functions
 
   These functions draw the state of the environment to a canvas
   using KineticJS. 

   All draw functions return a single object (a group if necessary), and
   the size and position of this object.

       drawX(x, y, data) -> {obj, width, height}

*/

var draw_params = {
   lineHeight: 24,
   var_name_sep: 6,
   value_sep: 7,
   value_padding: 4,
   value_padding_left: 7,
   obj_padding: 1,
   env_padding: 8,
   env_padding_left: 12,
   min_env_width: 100,
   min_env_height: 10,
};

var stage = null;

function draw_text(x, y, text, fill, fontSize) {
   var text = new Kinetic.Text({
      x: x, y: y,
      text: text,
      fontSize: (fontSize ? fontSize : 14),
      fontFamily: 'Source Code Pro',
      fill: (fill ? fill : '#333'),
   });
   return {
      obj:    text,
      width:  text.width(),
      height: text.height()
   };
}

function draw_state(S) {
   stage.destroyChildren();
   $('#status').text('');
   if (S === null) {
      $('#slider .knob').addClass('disabled');
      return;
   }
   $('#slider .knob').removeClass('disabled');
   var layer = new Kinetic.Layer();
   var x = 10, y = 10;
   for (var i = S.env.length-1; i >= 0; i--) {
      var env = draw_env(x, y, S.env[i]);
      layer.add(env.obj);
      x += env.width + 20;
   }
   stage.add(layer);
}

function draw_env(_x, _y, _env) {
   var frame = new Kinetic.Group({x: _x, y: _y, id: _env.name});
   var x = 0, y = 0;

   // title
   var title = draw_text(x+10, y+9, _env.name);
   frame.add(title.obj);
   y += draw_params.lineHeight + 10;
   var title_height = y;

   // vars + values
   var P  = draw_params.env_padding;
   var PL = draw_params.env_padding_left;
   var env = draw_obj(x + PL, y + P, _env.tab);
   frame.add(env.obj);
   y += env.height;

   var W = draw_params.min_env_width;
   var H = draw_params.min_env_height;
   var width  = (env.width  < W ? W : env.width) + 2*P;
   var height = (env.height < H ? H : env.height) + title_height;
   if (env.count > 0) {
      height += 2*P;
   }

   // draw background + title
   var active = _env.tab['<active>'];
   var r1 = new Kinetic.Rect({
      x: 0, y: 0,
      width: width, height: height,
      fill: (active ? 'rgb(255, 255, 160)' : '#f7f7f7'),
   });
   var r2 = new Kinetic.Rect({
      x: 0, y: 0,
      width: width, height: title_height,
      fill: (active ? 'rgba(150, 150, 0, 0.3)' : 'rgba(0,0,0,0.2)'),
   });
   var r3 = new Kinetic.Rect({
      x: 0, y: 0,
      width: width, height: height,
      stroke: (active ? 'rgba(150, 150, 0, 0.8)' : 'rgba(0,0,0,0.3)'),
      strokeWidth: 1
   })
   frame.add(r1); frame.add(r2); frame.add(r3);
   r3.moveToBottom(); r2.moveToBottom();  r1.moveToBottom();

   return {
      obj:    frame,
      width:  width,
      height: height,
      count:  env.count,
   };
}

function draw_value(x, y, data, index) {
   var x0 = x, y0 = y;
   var group = new Kinetic.Group({x: x, y: y});
   var P  = draw_params.value_padding;
   var PL = draw_params.value_padding;
   var fill = 'rgba(255, 255, 255, 0.6)';
   if (data instanceof Object) {
      PL = draw_params.value_padding_left;
   }
   var y0 = y, value = {};
   if (data instanceof Array) {
      value = draw_array(0, 0, data);
      y += value.height;
   } else if (data instanceof Object && data['<type>'] === 'char') {
      value = draw_text(PL, P, "'" + data['char'] + "'");
      y += draw_params.lineHeight;
   } else if (data instanceof Object && data['<type>'] === 'ref') {
      value = draw_text(PL, P, data['ref']);
      y += draw_params.lineHeight;
   } else if (data instanceof Object) {
      value = draw_obj(PL, P, data);
      y += value.height;
   } else {
      value = draw_text(PL, P, data, '#55f');
      y += draw_params.lineHeight;
      fill = 'white';
   }
   var height = value.height;
   var width = value.width;
   if (!(data instanceof Array)) {
      var rect = new Kinetic.Rect({
         x: 0, 
         y: 0,
         width:  value.width + P + PL,
         height: value.height + 2*P,
         fill:   fill,
         stroke: '#777',
         strokeWidth: 0.5
      });
      group.add(rect);
      height = rect.height();
      width = rect.width();
   }
   if (index !== undefined) {
      var idx = draw_text(2.0, 1.5, "" + index, '#bbb', 9);
      group.add(idx.obj);
   }
   group.add(value.obj);
   return {
      obj:    group,
      width:  width,
      height: height
   };
}

function draw_array(x, y, array) {
   var group = new Kinetic.Group({x: x, y: y});
   var height = 0;
   for (var i = 0; i < array.length; i++) {
      var value = draw_value(x, y, array[i].data, i);
      x += value.width;
      group.add(value.obj);
      if (value.height > height) {
         height = value.height;
      }
   }
   return {
      obj:    group,
      width:  x,
      height: height
   }
}

function draw_obj(_x, _y, obj) {
   var group = new Kinetic.Group({x: _x, y: _y});
   var P = draw_params.obj_padding;
   var x = P, y = P;

   // draw variable names
   var objs = {names: [], values: [], count: 0};
   var names_width = 0, values_width = 0;
   for (var prop in obj) {
      if (prop == '<type>' || prop == '<active>') {
         continue;
      }
      objs.count++;
      if (objs.count > 1) {
         y += draw_params.value_sep;
      }

      // value
      var value = draw_value(x, y, obj[prop].data);
      if (value.width > values_width) {
         values_width = value.width;
      }
      group.add(value.obj);
      objs.values.push(value.obj);

      // name
      var name = draw_text(x, y + value.height/2 - 7 /* tamaño fuente? */, prop);
      if (name.width > names_width) {
         names_width = name.width;
      }
      group.add(name.obj);
      objs.names.push(name.obj);

      y += value.height;
   }
   
   // adjust names so that they are aligned to the right
   for (var i = 0; i < objs.count; i++) {
      objs.names[i].width(names_width);
      objs.names[i].align('right');
      var vx = objs.values[i].x();
      objs.values[i].x(vx + names_width + draw_params.var_name_sep);
   }

   return {
      obj:    group,
      width:  names_width + draw_params.var_name_sep + values_width + 2*P,
      height: y + 2*P,
      count:  objs.count,
   };
}

// function draw_obj(_x, _y, obj) {
//    var objgroup = new Kinetic.Group({
//       x: x, y: y,
//    });
//    var x = _x + 3, y = _y + 3;
//    var max_x = x;
//    for (var prop in obj) {
//       if (prop == '<type>') {
//          continue;
//       }
//       var res = drawvar(x, y, prop, obj[prop].data, objgroup);
//       if (res.xmax > max_x) {
//          max_x = res.xmax;
//       }
//       y += draw_params.lineHeight;
//    }
//    var rect = new Kinetic.Rect({
//       x: _x, y: _y,
//       width: max_x + 3,
//       height: y + 6,
//       fill: 'white',
//       stroke: '#777',
//       strokeWidth: 0.5
//    });
//    objgroup.add(rect);
//    rect.moveToBottom();
//    return {
//       objgroup: objgroup,
//       xmax: max_x,
//       y:    y
//    };
// }

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
   
   stage = new Kinetic.Stage({
      container: 'env',
      width: 1000,
      height: 1000
   });
   // showstate(null);
   draw_state(null);
});
