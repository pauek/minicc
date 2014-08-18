$(document).ready(function () {
   $("#compile").click(function () {
      var code = $("#code").val();
      console.log(code);
      var errors = Module.compile(code);
      console.log(errors);
      $("#errors").text(errors);
   });
   $("#execute").click(function () {
      var output = Module.execute("");
      $("#output").text(output);
   });
});