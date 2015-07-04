
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

   editor.on("change", function () {
      setCompilado(false);
      cambiado = true;
      borraErrores();
      $('#output').hide();
   });
}

function borraErrores() {
   var marks = editor.getAllMarks();
   for (var i = 0; i < marks.length; i++) {
      marks[i].clear();
   }
}

function saveProgram() {
   localStorage['minicc:program'] = editor.getValue();
}

function compile() {
   saveProgram();
   var code = editor.getValue();
   var errjson = Module.compile(code);
   if (errjson == "[]") {
      setCompilado(true);
   }
   var errlist = JSON.parse(errjson);
   for (var i = 0; i < errlist.length; i++) {
      var err = errlist[i];
      var ini = {line: err.ini.lin-1, ch: err.ini.col};
      var fin = {line: err.fin.lin-1, ch: err.fin.col};
      editor.markText(ini, fin, {className: "error"});
   }
   $('#errlist').html(errjson);
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
      console.log(json);
      var obj  = JSON.parse(json);
      console.log(obj);
      this._history.push(obj);
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
      var status = $('#status').get()[0];
      var line = editor.getLine(item.span.fin.line);
      editor.addWidget({line: item.span.fin.line, ch: line.length}, status);
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

function render_elem(elem, box, content, extra) {
   var html = '<' + elem + ' id="box-' + box + '" class="var ';
   if (extra && extra.classes) {
      for (var i = 0; i < extra.classes.length; i++) {
         html += ' ' + extra.classes[i];
      }
   }
   html += '">';
   html += content;
   if (extra && extra.insert) {
      html += extra.insert;
   }
   html += '</' + elem + '>';
   return html;
}

function new_extras(classes, insert) {
   return {
      classes: classes || [],
      insert: insert || '',
      add: function (_class, _insert) {
         var classes = this.classes;
         var insert  = this.insert;
         if (_class) {
            classes = this.classes.concat(_class);
         }
         if (_insert) {
            insert += _insert;
         }
         return new_extras(classes, insert);
      },
   };
}

var to_html = function (val, _extras) {
   var extras = _extras || new_extras();
   if (val.data === null) {
      return {
         html:  render_elem('div', val.box, '?', 
                            extras.add('unknown')),
         links: []
      };
   } 
   else if (val.data instanceof Array) {
      return to_html.array(val, extras);
   } 
   else if (val.data instanceof Object) {
      return to_html[val.data["<type>"]](val, extras);
   } 
   else {
      var data = val.data;
      if (typeof(data) == "string") {
         data = '"' + data + '"';
      }
      return {
         html: render_elem('div', val.box, data, 
                           extras.add('value')),
         links: []
      };
   }
}

to_html['char'] = function(val, extras) {
   var links = [];
   var ch = val.data['char'];
   console.log("'" + ch + "'");
   return {
      html: render_elem('div', val.box, "'" + ch + "'", 
                        extras.add('value')),
      links: links,
   };
}

to_html.array = function (val, extras) {
   var links = [];
   var html = '<tr>';
   if (val.data.length == 0) {
      return {
         html: render_elem('div', val.box, '', 
                           extras.add('value empty')),
         links: links,
      };
   }
   for (var j = 0; j < val.data.length; j++) {
      html += '<td>';
      var first = [];
      if (j == 0) {
         first.push("first");
      }
      var insert = '<div class="index">' + j + '</div>';
      var res = to_html(val.data[j], extras.add(first, insert));
      push.apply(links, res.links);
      html += res.html;
      html += '</td>';
   } 
   html += '</tr>';
   return {
      html: render_elem('table', val.box, html, 
                        extras.add('array')),
      links: links,
   }
}

to_html.ref = function (val, extras) {
   extras.classes.push('ref');
   return  {
      html: render_elem('div', val.box, '', extras),
      links: [{from: val.box, to: val.data['ref']}]
   };
}

to_html.list = function (val, extras) {
   var links = [];
   var elems = val.data['<elements>'];
   if (elems.length == 0) {
      return {
         html: render_elem('div', val.box, '', 
                           extras.add('value empty')),
         links: links,
      };
   }
   var html = '<tr>';
   var insert = '<div class="join"></div>';
   for (var i = 0; i < elems.length; i++) {
      html += '<td>';
      
      var res = to_html(elems[i], (i == 0 ? 
                                   extras.add('first') : 
                                   extras.add(null, insert)));
      push.apply(links, res.links);
      html += res.html;
      html += '</td>';
   } 
   html += '</tr>';
   return {
      html: render_elem('table', val.box, html, 
                        extras.add('list')),
      links: links,
   }
}

to_html.struct = function (val, extras) {
   var links = [];
   extras.classes.push('struct');

   var html = '<table>';
   for (var prop in val.data) {
      if (prop == '<type>') {
         continue;
      }
      html += '<tr><td><div class="name">' + prop + '</div></td><td>';
      var res = to_html(val.data[prop]);
      push.apply(links, res.links);
      html += res.html;
      html += '</td></tr>';
   }
   html += '</table>';

   return {
      html: render_elem('div', val.box, html, extras),
      links: links,
   };
}

var push = Array.prototype.push;
var svg; // 

var params = {
   arrow_offset:    22,
   arrow_curvature: 30,
   arrow_cap_height: 8,
   arrow_cap_width:  3,
   arrow_radius:    10,
   through_inc:      5,
};

function position_calculator(origin) {
   function _calc(fx, fy) {
      return function(elem) {
         var offset = elem.offset();
         return {
            x: offset.left - origin.left + elem.outerWidth()  * fx,
            y: offset.top  - origin.top  + elem.outerHeight() * fy
         };
      }
   }
   return {
      topleft: _calc(0.0, 0.0),
      center:  _calc(0.5, 0.5),
      right:   _calc(1.0, 0.5),
      bottom:  _calc(0.5, 1.0),
   }
}

function draw_arrow_cap_right(x0, y0) {
   var h = params.arrow_cap_height;
   var w = params.arrow_cap_width;
   svg.path(Snap.format("M{x0} {y0}L{x1} {y1}L{x2} {y2}L{x3} {y3}Z", {
      x0: x0,          y0: y0,
      x1: x0 + h,      y1: y0 + w,
      x2: x0 + h * .8, y2: y0,
      x3: x0 + h,      y3: y0 - w,
   })).attr({
      fill: '#f00'
   });
}

function draw_arrow_cap_bottom(x0, y0) {
   var h = params.arrow_cap_height;
   var w = params.arrow_cap_width;
   svg.path(Snap.format("M{x0} {y0}L{x1} {y1}L{x2} {y2}L{x3} {y3}Z", {
      x0: x0,     y0: y0,
      x1: x0 + w, y1: y0 + h,
      x2: x0,     y2: y0 + h * .8,
      x3: x0 - w, y3: y0 + h,
   })).attr({
      fill: '#f00'
   });
}

function draw_arrow_far_right(coords, through) {
   coords.t1 = through + 10;
   coords.t2 = through;
   var dx = coords.from.x - through;
   var dy = coords.from.y - coords.to.y;
   coords.c = {
      x: coords.t1 + 10,
      y: coords.to.y + 10*(dy/dx)
   };
   svg.path(Snap.format("M{from.x} {from.y}L{c.x} {c.y}Q{t1} {to.y} {t2} {to.y}H{to.x}", coords)).attr({
      fill: 'none',
      stroke:'rgba(255, 0, 0, .2)',
      strokeWidth: 1.8,
   });
   draw_arrow_cap_right(coords.to.x, coords.to.y);
}

function draw_arrow_two_turns(coords, through) {
   path  = "M{from.x} {from.y}";
   path += "H{a}";
   path += "a{r},{r} 0 0,0 {r},-{r}"
   path += "V{b}";
   path += "a{r},{r} 0 0,0 -{r},-{r}";
   path += "H{to.x}";
   var r = params.arrow_radius;
   coords.r = r;
   coords.a = through - r;
   coords.b = coords.to.y + r;
   svg.path(Snap.format(path, coords)).attr({
      fill: 'none',
      stroke:'rgba(255, 0, 0, .2)',
      strokeWidth: 1.8,
   });
   draw_arrow_cap_right(coords.to.x, coords.to.y);
}

function draw_arrow_one_turn(coords, through) {
   var r = params.arrow_radius;
   path = "M{from.x} {from.y}H{through}a{r},{r} 0 0,0 {r},-{r}V{bto.y}";
   coords.through = coords.bto.x - r;
   coords.r = r;
   svg.path(Snap.format(path, coords)).attr({
      fill: 'none',
      stroke:'rgba(255, 0, 0, .2)',
      strokeWidth: 1.8,
   });
   draw_arrow_cap_bottom(coords.bto.x, coords.bto.y);
}

function draw_arrow(coords, through) {
   if (coords.from.x - coords.to.x > 20) {
      draw_arrow_far_right(coords, through);
   } else if (coords.to.x - coords.from.x > 20) {
      draw_arrow_one_turn(coords, through);
   } else {
      draw_arrow_two_turns(coords, through);
   }
}

function showstate(S) {
   var links = [];
   $('#env').empty();
   $('#status').hide();
   svg = Snap('#refs');
   svg.clear();
   if (S === null) {
      $('#slider .knob').addClass('disabled');
      return;
   }
   $('#slider .knob').removeClass('disabled');
   var env = S.env;
   if (env === null) {
      return;
   }
   var html = '<table>';
   for (var i = env.length-1; i >= 0; i--) {
      env[i].links = [];
      html += '<tr><td><div id="env-' + env[i].name + '" class="fenv';
      if (env[i].tab["<active>"]) {
         html += " active";
      }
      html += '"><h5>' + env[i].name + '</h5>';
      var table = env[i].tab;
      html += '<div class="wrapper"><table>'
      for (var name in table) {
         if (name == "<active>") {
            continue;
         }
         html += '<tr><td><div class="name">' + name + '</div></td><td>';
         var res = to_html(table[name]);
         push.apply(env[i].links, res.links);
         html += res.html;
         html += '</td></tr>';
      }
      html += '</table></div>';
      html += '</div></td></tr>';
   }
   html += '</table>';
   $('#env').append(html);
   $('#status .text').text(S.status);
   $('#status').show();

   // pintar flechas de referencias, punteros y iteradores.
   var origin = $('#env').offset();
   var poscalc = position_calculator(origin);

   for (var i = env.length-1; i >= 0; i--) {
      var links = env[i].links;
      var through = 0.0;
      for (var j = 0; j < links.length; j++) {
         var coords = {
            from: poscalc.center($('#box-' + links[j].from)),
            to:   poscalc.right($('#box-' + links[j].to)),
            bto:  poscalc.bottom($('#box-' + links[j].to)),
         };
         if (through < coords.to.x + params.arrow_offset) {
            through = coords.to.x + params.arrow_offset;
         }
         svg.circle(coords.from.x, coords.from.y, 3.2).attr({fill: '#f00'});
         draw_arrow(coords, through);
         $('#box-' + links[j].to).addClass('hasref');
         
         // avoid same vertical
         through += params.through_inc;
      }
   }
   $('#refs').width($('#env').width());
   $('#refs').height($('#env').height());
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
         orig:  track.offset().left,
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
      var left = track.offset().left;
      var ratio = (ev.clientX - left) / w;
      if (ratio < -0.02 || ratio > 1.02) {
         return;
      }
      this.setKnob(ratio);
   }
};

function forwards() {
   if (slider.top() && stepper.finished()) {
      return;
   }
   if (slider.incr()) {
      stepper.step();
   }
   stepper.show(slider.knob());
}

function backwards() {
   slider.decr();
   stepper.show(slider.knob());
}

$(document).ready(function () {

   if (localStorage["minicc:program"]) {
      initial_program = localStorage["minicc:program"];
   }
   setup(initial_program);

   $("#execute").addClass("pure-button-disabled");
   $("#forwards").addClass("pure-button-disabled");
   $("#backwards").addClass("pure-button-disabled");

   $("#compile").click(compile);     Mousetrap.bind("f9", compile);
   $("#execute").click(execute);     Mousetrap.bind("f5", execute);
   $("#reformat").click(reformat);   Mousetrap.bind("f3", reformat);
   $("#download").click(download);   Mousetrap.bind("f4", download);
   $("#forwards").click(forwards);   Mousetrap.bind("right", forwards);
   $("#backwards").click(backwards); Mousetrap.bind("left", backwards);

   editor.setOption("extraKeys", {
      "F9": compile,
      "F5": execute,
      "F3": reformat,
      "F4": download
   });

   $(window).resize(resize);
   $(window).resize();

   $('#content').split({
      orientation: 'vertical',
      limit: 150,
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
