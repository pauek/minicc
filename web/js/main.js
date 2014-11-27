
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

function bottomPartition() {
   var total = $('#bottom').height();
   var controls = $('#controls').height();
   $('#bottom .scroll').height((total - controls) + 'px');
}

function value_str(value, addClass, insert) {
   var s = '', elem = 'div', links = [];
   var classes = ['var'];
   var id;
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
         push.apply(links, res.links);
         s += res.html;
         s += '</td>';
      } 
      s += '</tr>';
   } else if (value.data instanceof Object) {
      var type = value.data["<type>"];
      if (type == 'ref') {
         elem = 'div';
         classes.push('ref');
         var addr = value.data['ref'];
         links.push({from: value.box, to: addr});
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
            push.apply(links, res.links);
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
            x: offset.left - origin.left + elem.outerWidth() * fx,
            y: offset.top  - origin.top  + elem.outerWidth() * fy
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
   $('#status').text('');
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
   var html = '<table><tr>';
   for (var i = env.length-1; i >= 0; i--) {
      env[i].links = [];
      html += '<td><div id="env-' + env[i].name + '" class="fenv';
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
         var res = value_str(T[prop]);
         push.apply(env[i].links, res.links);
         html += res.html;
         html += '</td></tr>';
      }
      html += '</table></div>';
      html += '</div></td>';
   }
   html += '</tr></table>';
   $('#env').append(html);
   $('#status').text(S.status);

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
      bottomPartition();
   });
   editor.refresh();
   bottomPartition();

   $(window).bind('beforeunload', function () {
      saveProgram();
      if (cambiado) {
         return "Has editado, seguro que quieres salir?";
      }
   });
   slider.init();
   showstate(null);
});
