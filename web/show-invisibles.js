/* CodeMirror mode to show invisible characters in the output */

(function(mod) {
   if (typeof exports == "object" && typeof module == "object") // CommonJS
      mod(require("../../lib/codemirror"));
   else if (typeof define == "function" && define.amd) // AMD
      define(["../../lib/codemirror"], mod);
   else // Plain browser env
      mod(CodeMirror);
})(function(CodeMirror) {
   "use strict";
   
   CodeMirror.defineMode("show-invisible", function(config, parserConfig) {
      var TOKEN_STYLE = {
         '\u2423': 'invisible',
         '\u21a9': 'invisible',
      };
      return {
         token: function (stream) {
            return TOKEN_STYLE[stream.next()];
         }
      };
   });

   CodeMirror.defineMIME('text/x-show-inv', 'show-invisible');
});